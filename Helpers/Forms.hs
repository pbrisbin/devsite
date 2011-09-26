{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms
    ( runProfileFormGet
    , runProfileFormPost
    , runPostForm
    ) where

import Foundation
import Control.Applicative ((<$>), (<*>))
import Data.Time           (getCurrentTime)

import Data.Text (Text)
import qualified Data.Text as T

data ProfileEditForm = ProfileEditForm
    { formUsername :: Maybe Text
    , formEmail    :: Maybe Text
    }

data PostEditForm = PostEditForm
    { formSlug  :: Text
    , formTitle :: Text
    , formTags  :: Text
    , formDescr :: Markdown
    }

-- | Display the form for user input
runProfileFormGet :: Widget
runProfileFormGet = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift $ runFormPost $ profileEditForm u
    [whamlet|
        <h1>Edit
        <article .fullpage .profile
            <form enctype="#{enctype}" method="post">
                <table>
                    ^{form}
                    <tr>
                        <td>&nbsp;
                        <td .buttons>
                            <input type="submit" value="Save">
        |]

-- | Handle the POST request. This must be a separate call since all 
--   fields are optional so an empty for would be constantly POSTed if 
--   we consolidated the code.
runProfileFormPost :: Handler ()
runProfileFormPost = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileEditForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    where
        saveChanges :: UserId -> ProfileEditForm -> Handler ()
        saveChanges uid ef = do
            runDB $ update uid 
                [ UserName  =. formUsername ef
                , UserEmail =. formEmail    ef
                ]

            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

profileEditForm :: User -> Html -> Form DevSite DevSite (FormResult ProfileEditForm, Widget)
profileEditForm u = renderTable $ ProfileEditForm
    <$> aopt textField   "User name"
        { fsTooltip = Just "comments are attributed to this username"
        } (Just $ userName u)

    <*> aopt emailField  "Email address"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

runPostForm :: Maybe Document -> Widget
runPostForm mdoc = do
    ((res, form), enctype) <- lift $ runFormPost $ postForm mdoc
    case res of
        FormSuccess pf -> lift $ processFormResult pf
        _              -> return ()

    [whamlet|
        <div .post_input>
            <form enctype="#{enctype}" method="post">
                <table>
                    ^{form}
                    <tr>
                        <td>&nbsp;
                        <td .buttons>
                            $maybe _ <- mdoc
                                <input type="submit" value="Update">
                            $nothing
                                <input type="submit" value="Create">
        |]

    where
        processFormResult :: PostEditForm -> Handler ()
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

        postFromForm :: PostEditForm -> Handler Post
        postFromForm pf = do
            now <- liftIO getCurrentTime
            return Post
                { postSlug  = formSlug  pf
                , postTitle = formTitle pf
                , postDescr = formDescr pf
                , postDate  = now
                }

        updatePost :: PostId -> Post -> Handler ()
        updatePost key new = runDB $ update key 
            [ PostSlug  =. postSlug  new
            , PostTitle =. postTitle new
            , PostDescr =. postDescr new
            ]

        updateTags :: PostId -> [Text] -> Handler ()
        updateTags key ts = do
            runDB $ deleteWhere [TagPost ==. key]
            createTags key ts


        createTags :: PostId -> [Text] -> Handler ()
        createTags key = mapM_ (go key)
            where
                go :: PostId -> Text -> Handler ()
                go key' tag = runDB $ do
                    _ <- insertBy $ Tag key' tag
                    return ()

        parseTags :: Text -> [Text]
        parseTags = filter (not . T.null)
                  . map (T.toLower . T.strip) . T.splitOn ","

-- | Display the new post form inself. If the first argument is Just,
--   then use that to prepopulate the form
postForm :: Maybe Document -> Html -> Form DevSite DevSite (FormResult PostEditForm, Widget)
postForm mdoc = renderTable $ PostEditForm
    <$> areq textField     "post slug"   (fmap (postSlug   . post) mdoc)
    <*> areq textField     "title"       (fmap (postTitle  . post) mdoc)
    <*> areq textField     "tags"        (fmap (formatTags . tags) mdoc)
    <*> areq markdownField "description" (fmap (postDescr  . post) mdoc)

    where
        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName
