{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE OverloadedStrings            #-}
module Handlers.Posts 
    ( getPostsR
    , getPostR
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import DevSite
import Model
import Yesod

import Yesod.Helpers.Auth
import Yesod.Goodies.Markdown
import Yesod.Goodies.Shorten (shorten)
import Helpers.Documents

import Control.Applicative ((<$>), (<*>))
import Data.Time           (getCurrentTime)

import qualified Settings
import qualified Data.Text as T

-- | Used by the new post form
data PostForm = PostForm
    { formSlug  :: T.Text
    , formTitle :: T.Text
    , formTags  :: T.Text
    , formDescr :: Markdown
    }

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "All Posts"
        Settings.addKeywords ["all posts"]
        [hamlet|
            <header>
                <h1>All Posts

            $forall doc <- docs
                ^{shortDocument doc}
            |]

-- | A post
getPostR :: T.Text -> Handler RepHtml
getPostR slug = do
    docs <- siteDocs =<< getYesod
    case lookupDocument slug docs of
        Just doc -> defaultLayout $ longDocument doc (documentNavigation doc docs)
        Nothing  -> defaultLayout $ unpublishedDocument slug
        
-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _    <- requireAuth
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "Manage posts"
        [hamlet|
            <header>
                <h1>Manage Posts
            <article .fullpage>
                ^{runPostForm Nothing}
                ^{documentsList docs}
            |]

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

-- | Edit post
getEditPostR :: T.Text -> Handler RepHtml
getEditPostR slug = do
    _    <- requireAuth
    docs <- siteDocs =<< getYesod

    let mdoc = safeHead $ filter ((==) slug . postSlug . post) docs

    defaultLayout $ do
        Settings.setTitle "Edit post"
        [hamlet|
            <header>
                <h1>Edit Post
            <article .fullpage>
                ^{runPostForm mdoc}
                ^{documentsList docs}
            |]

    where
        safeHead :: [a] -> Maybe a
        safeHead []    = Nothing
        safeHead (x:_) = Just x

postEditPostR :: T.Text -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: T.Text -> Handler RepHtml
getDelPostR slug = do
    _    <- requireAuth
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (key, _) -> do
            -- delete the post and the tags
            runDB $ deleteBy $ UniquePost slug
            runDB $ deleteWhere [TagPostEq key]
            setMessage "post deleted!"
        Nothing -> setMessage "post not found."

    redirect RedirectTemporary ManagePostsR

documentsList :: [Document] -> Widget ()
documentsList []   = return ()
documentsList docs = [hamlet|
    <div .posts_existing>
        <h3>Existing posts:

        <table>
            <tr>
                <th>Title
                <th>Description
                <th>Edit
                <th>Delete

            $forall p <- map post docs
                <tr>
                    <td>
                        <a href="@{PostR $ postSlug p}">#{shorten 20 $ postTitle p}
                    <td>#{markdownToHtml $ shorten 60 $ postDescr p}
                    <td>
                        <a href="@{EditPostR $ postSlug p}">edit
                    <td>
                        <a href="@{DelPostR $ postSlug p}">delete
    |]

updatePost :: PostId -> Post -> Handler ()
updatePost key new = runDB $ update key 
    [ PostSlug  $ postSlug  new
    , PostTitle $ postTitle new
    , PostDescr $ postDescr new
    ]

removeTags :: PostId -> Handler ()
removeTags key = runDB $ deleteWhere [TagPostEq key]

createTags :: PostId -> [T.Text] -> Handler ()
createTags key = mapM_ (go key)
    where
        go :: PostId -> T.Text -> Handler ()
        go key' tag = runDB (insertBy $ Tag key' tag) >>= \_ -> return ()

updateTags :: PostId -> [T.Text] -> Handler ()
updateTags key ts = removeTags key >> createTags key ts

runPostForm :: Maybe Document -> Widget ()
runPostForm mdoc = do
    ((res, form), enctype) <- lift . runFormMonadPost $ postForm mdoc
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> lift $ processFormResult pf

    [hamlet|
        <div .post_input>
            <form enctype="#{enctype}" method="post">
                ^{form}
        |]

-- | Display the new post form inself. If the first argument is Just,
--   then use that to prepopulate the form
postForm :: Maybe Document -> FormMonad (FormResult PostForm, Widget ())
postForm mdoc = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   $ fmap (postSlug   . post) mdoc
    (t          , fiTitle      ) <- stringField   "title:"       $ fmap (postTitle  . post) mdoc
    (ts         , fiTags       ) <- stringField   "tags:"        $ fmap (formatTags . tags) mdoc
    (description, fiDescription) <- markdownField "description:" $ fmap (postDescr  . post) mdoc
    return (PostForm <$> slug <*> t <*> ts <*> description, [hamlet|
        <table>
            ^{fieldRow fiSlug}
            ^{fieldRow fiTitle}
            ^{fieldRow fiTags}
            ^{fieldRow fiDescription}
            <tr .buttons>
                <td colspan="3">
                    <input type="submit" value="Save">
        |])

    where
        fieldRow fi = [hamlet|
            <tr>
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                    <div .tooltip>#{fiTooltip fi}
                <td>
                    ^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;

            |]

        formatTags :: [Tag] -> T.Text
        formatTags = T.intercalate ", " . map tagName

processFormResult :: PostForm -> Handler ()
processFormResult pf = do
    p      <- postFromForm pf
    result <- runDB $ insertBy p

    case result of
        Right k -> do
            -- post was inserted, add the tags
            createTags k (parseTags $ formTags pf)
            setMessage "post created!"

        Left (k, _) -> do
            -- post exists, update
            updatePost k p
            updateTags k (parseTags $ formTags pf)
            setMessage "post updated!"

    redirect RedirectTemporary ManagePostsR

postFromForm :: PostForm -> Handler Post
postFromForm pf = do
    now <- liftIO getCurrentTime
    return Post
        { postSlug  = formSlug  pf
        , postTitle = formTitle pf
        , postDescr = formDescr pf
        , postDate  = now
        }

parseTags :: T.Text -> [T.Text]
parseTags = filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn ","
