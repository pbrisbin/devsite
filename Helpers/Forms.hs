{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms
    ( runProfileFormGet
    , runProfileFormPost
    , runPostForm
    ) where

import Foundation
import Yesod.Goodies.Markdown
import Control.Applicative ((<$>), (<*>))
import Data.Time           (getCurrentTime)

import Data.Text (Text)
import qualified Data.Text as T

data ProfileEditForm = ProfileEditForm
    { eUsername :: Maybe Text
    , eEmail    :: Maybe Text
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
                ^{form}
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
                [ UserName  =. eUsername ef
                , UserEmail =. eEmail    ef
                ]

            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

profileEditForm :: User -> Html -> Form DevSite DevSite (FormResult ProfileEditForm, Widget)
profileEditForm u fragment = do
    (fUsername, fiUsername) <- mopt textField   "User name:"     $ Just $ userName u
    (fEmail   , fiEmail   ) <- mopt emailField  "Email address:" $ Just $ userEmail u

    return (ProfileEditForm <$> fUsername <*> fEmail, [whamlet|
            #{fragment}
            <table>
                ^{fieldRow fiUsername "comments are attributed to this username"        }
                ^{fieldRow fiEmail    "never displayed, only used to find your gravatar"}
                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
            |])

    where
        fieldRow :: FieldView s m -> Text -> GWidget s m ()
        fieldRow fv txt = [whamlet|
            <tr ##{fvId fv}>
                <th>
                    <label for="#{fvId fv}">#{fvLabel fv}
                    $maybe tt <- fvTooltip fv
                        <div .tooltip>#{tt}
                <td>^{fvInput fv}
                <td>#{txt}
                <td>
                    $maybe error <- fvErrors fv
                        #{error}
                    $nothing
                        &nbsp;
            |]

runPostForm :: Maybe Document -> Widget
runPostForm mdoc = do
    ((res, form), enctype) <- lift $ runFormPost $ postForm mdoc
    case res of
        FormSuccess pf -> lift $ processFormResult pf
        _              -> return ()

    [whamlet|
        <div .post_input>
            <form enctype="#{enctype}" method="post">
                ^{form}
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
postForm mdoc fragment = do
    (slug       , fiSlug       ) <- mreq textField     "post slug:"   $ fmap (postSlug   . post) mdoc
    (t          , fiTitle      ) <- mreq textField     "title:"       $ fmap (postTitle  . post) mdoc
    (ts         , fiTags       ) <- mreq textField     "tags:"        $ fmap (formatTags . tags) mdoc
    (description, fiDescription) <- mreq markdownField "description:" $ fmap (postDescr  . post) mdoc
    return (PostEditForm <$> slug <*> t <*> ts <*> description, [whamlet|
        #{fragment}
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
        fieldRow fv = [whamlet|
            <tr>
                <th>
                    <label for="#{fvId fv}">#{fvLabel fv}
                    $maybe tt <- fvTooltip fv
                        <div .tooltip>#{tt}
                <td>
                    ^{fvInput fv}
                <td>
                    $maybe error <- fvErrors fv
                        #{error}
                    $nothing
                        &nbsp;

            |]

        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName
