{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms
    ( runProfileFormGet
    , runProfileFormPost
    , runPostForm
    ) where

import DevSite
import Yesod.Helpers.Auth
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
runProfileFormGet :: Widget ()
runProfileFormGet = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift . runFormMonadPost $ editForm u

    [hamlet|
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
    ((res, _   ), _ ) <- runFormMonadPost $ editForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    where
        saveChanges :: UserId -> ProfileEditForm -> Handler ()
        saveChanges uid ef = do
            runDB $ update uid 
                [ UserName  $ eUsername ef
                , UserEmail $ eEmail    ef
                ]

            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

editForm :: User -> FormMonad (FormResult ProfileEditForm, Widget())
editForm u = do
    (fUsername, fiUsername) <- maybeStringField "User name:"     $ Just $ userName u
    (fEmail   , fiEmail   ) <- maybeEmailField  "Email address:" $ Just $ userEmail u

    return (ProfileEditForm <$> fUsername <*> fEmail, [hamlet|
            <table>
                ^{fieldRow fiUsername "comments are attributed to this username"        }
                ^{fieldRow fiEmail    "never displayed, only used to find your gravatar"}
                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
            |])

    where
        fieldRow :: FieldInfo sub y -> Text -> GWidget sub y ()
        fieldRow fi txt = [hamlet|
            <tr ##{fiIdent fi}>
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                    <div .tooltip>#{fiTooltip fi}
                <td>^{fiInput fi}
                <td>#{txt}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
            |]

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
postForm :: Maybe Document -> FormMonad (FormResult PostEditForm, Widget ())
postForm mdoc = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   $ fmap (postSlug   . post) mdoc
    (t          , fiTitle      ) <- stringField   "title:"       $ fmap (postTitle  . post) mdoc
    (ts         , fiTags       ) <- stringField   "tags:"        $ fmap (formatTags . tags) mdoc
    (description, fiDescription) <- markdownField "description:" $ fmap (postDescr  . post) mdoc
    return (PostEditForm <$> slug <*> t <*> ts <*> description, [hamlet|
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

        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName

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
    [ PostSlug  $ postSlug  new
    , PostTitle $ postTitle new
    , PostDescr $ postDescr new
    ]

removeTags :: PostId -> Handler ()
removeTags key = runDB $ deleteWhere [TagPostEq key]

createTags :: PostId -> [Text] -> Handler ()
createTags key = mapM_ (go key)
    where
        go :: PostId -> Text -> Handler ()
        go key' tag = runDB (insertBy $ Tag key' tag) >>= \_ -> return ()

updateTags :: PostId -> [Text] -> Handler ()
updateTags key ts = removeTags key >> createTags key ts

parseTags :: Text -> [Text]
parseTags = filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn ","
