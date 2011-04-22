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
import Yesod.Comments.Markdown
import Helpers.Documents

import Control.Applicative ((<$>), (<*>))
import Data.Char           (isSpace)
import Data.List           (intercalate)
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
        addKeywords ["all posts"]
        [hamlet|
            <header>
                <h1>All Posts

            $forall doc <- docs
                ^{shortDocument doc}
            |]

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    docs <- siteDocs =<< getYesod
    case helper slug docs of
        (Nothing , Nothing, Nothing) -> defaultLayout $ unpublishedDocument slug
        (Just doc, mprev  , mnext  ) -> defaultLayout $ longDocument doc (fmap linkFromPost mprev) (fmap linkFromPost mnext)

    where
        -- | Return the desired document, and maybe the post just before 
        --   and just after it in the list so that next/previous links 
        --   can be shown
        helper :: String -> [Document] -> (Maybe Document, Maybe Post, Maybe Post)
        helper _ [] = (Nothing, Nothing, Nothing) -- not found

        helper slug' (d@(Document p _):[]) =
            if postSlug p == slug'
                then (Just d, Nothing, Nothing)
                else helper slug' [] -- not found

        helper slug' (d1@(Document p1 _):d2@(Document p2 _):[]) =
            if postSlug p1 == slug'
                then (Just d1, Nothing, Just p2)
                else if postSlug p2 == slug'
                    then (Just d2, Just p1, Nothing)
                    else helper slug' [] -- not found

        helper slug' (d1@(Document p1 _):d2@(Document p2 _):d3@(Document p3 _):ds) =
            if postSlug  p1 == slug'
                then (Just d1, Nothing, Just p2)
                else if postSlug p2 == slug'
                    then (Just d2, Just p1, Just p3)
                    else helper slug' (d2:d3:ds)

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
getEditPostR :: String -> Handler RepHtml
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

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
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
                    <td>#{shorten 60 $ postDescr p}
                    <td>
                        <a href="@{EditPostR $ postSlug p}">edit
                    <td>
                        <a href="@{DelPostR $ postSlug p}">delete
    |]

    where shorten n s = if length s > n then take n s ++ "..." else s

updatePost :: PostId -> Post -> Handler ()
updatePost key new = runDB $ update key 
    [ PostSlug  $ postSlug  new
    , PostTitle $ postTitle new
    , PostDescr $ postDescr new
    ]

removeTags :: PostId -> Handler ()
removeTags key = runDB $ deleteWhere [TagPostEq key]

createTags :: PostId -> [String] -> Handler ()
createTags key = mapM_ (go key)
    where
        go :: PostId -> String -> Handler ()
        go key' tag = do
            _ <- runDB (insertBy $ Tag key' tag)
            return ()

updateTags :: PostId -> [String] -> Handler ()
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
    (slug       , fiSlug       ) <- stringField   "post slug:"   $ fmap (T.pack . postSlug . post) mdoc
    (title      , fiTitle      ) <- stringField   "title:"       $ fmap (T.pack . postTitle . post) mdoc
    (ts         , fiTags       ) <- stringField   "tags:"        $ fmap (T.pack . formatTags . tags) mdoc
    (description, fiDescription) <- markdownField "description:" $ fmap (Markdown . postDescr . post) mdoc
    return (PostForm <$> slug <*> title <*> ts <*> description, [hamlet|
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

        formatTags :: [Tag] -> String
        formatTags = intercalate ", " . map tagName

processFormResult :: PostForm -> Handler ()
processFormResult pf = do
    p      <- postFromForm pf
    result <- runDB $ insertBy p

    case result of
        Right k -> do
            -- post was inserted, add the tags
            createTags k (parseTags . T.unpack $ formTags pf)
            setMessage "post created!"

        Left (k, _) -> do
            -- post exists, update
            updatePost k p
            updateTags k (parseTags . T.unpack $ formTags pf)
            setMessage "post updated!"

    redirect RedirectTemporary ManagePostsR

postFromForm :: PostForm -> Handler Post
postFromForm pf = do
    now <- liftIO getCurrentTime
    return Post
        { postSlug  = T.unpack $ formSlug pf
        , postTitle = T.unpack $ formTitle pf
        , postDescr = unMarkdown $ formDescr pf
        , postDate  = now
        }

    where
        unMarkdown (Markdown s) = s

-- <https://github.com/fortytools/lounge/blob/master/Handler/Entry.hs#L57>
parseTags :: String -> [String]
parseTags [] = []
parseTags s  = let (l,s') = break (==',') $ dropWhile (==',') s
    in trim l : case s' of
        []      -> []
        (_:s'') -> parseTags s''

    where 
        trim = f . f
        f    = reverse . dropWhile isSpace
