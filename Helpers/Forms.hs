{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms
    ( runProfileFormGet
    , runProfileFormPost
    , runPostForm
    ) where

import Import
import Control.Monad (forM_)
import Yesod.Markdown
import Data.Time (getCurrentTime)
import qualified Data.Text as T

data ProfileEditForm = ProfileEditForm
    { formUsername :: Maybe Text
    , formEmail    :: Maybe Text
    }

data PostEditForm = PostEditForm
    { formSlug  :: Text
    , formTitle :: Text
    , formTags  :: Text
    , formDescr :: Maybe Markdown
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
            redirect $ tm ProfileR

profileEditForm :: User -> Form ProfileEditForm
profileEditForm u = renderBootstrap $ ProfileEditForm
    <$> aopt textField   "User name"
        { fsTooltip = Just "comments are attributed to this username"
        } (Just $ userName u)

    <*> aopt emailField  "Email address"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

runPostForm :: Maybe (Post,[Tag]) -> Widget
runPostForm mpost = do
    ((res, form), enctype) <- lift $ runFormPost $ postForm mpost
    case res of
        FormSuccess pf -> lift $ processFormResult pf
        _              -> return ()

    [whamlet|
        <div .post-input>
            <form enctype="#{enctype}" method="post" .form-stacked>
                ^{form}
                <div .actions>
                    <button .btn type="submit">
                        $maybe _ <- mpost
                            Update
                        $nothing
                            Create
        |]

    where
        processFormResult :: PostEditForm -> Handler ()
        processFormResult pf = do
            p   <- postFromForm pf
            msg <- runDB $ do
                result <- insertBy p

                case result of
                    Right k -> do
                        -- post was inserted, add the tags
                        forM_ (parseTags $ formTags pf) $ \tag -> do
                            insertBy $ Tag k tag

                        return "post created!"

                    Left (Entity k _) -> do
                        -- post exists, update it
                        update k 
                            [ PostSlug  =. postSlug  p
                            , PostTitle =. postTitle p
                            , PostDescr =. postDescr p
                            ]

                        -- remove existing tags
                        deleteWhere [TagPost ==. k]

                        -- add new ones
                        forM_ (parseTags $ formTags pf) $ \tag -> do
                            insertBy $ Tag k tag

                        return "post updated!"

            setMessage msg

            redirect ManagePostsR

        postFromForm :: PostEditForm -> Handler Post
        postFromForm pf = do
            now <- liftIO getCurrentTime
            return Post
                { postSlug  = formSlug  pf
                , postTitle = formTitle pf
                , postDescr = formDescr pf
                , postDate  = now
                }

        parseTags :: Text -> [Text]
        parseTags = filter (not . T.null)
                  . map (T.toLower . T.strip) . T.splitOn ","

-- | Display the new post form inself. If the first argument is Just,
--   then use that to prepopulate the form
postForm :: Maybe (Post,[Tag]) -> Form PostEditForm
postForm = undefined
-- FIXME:
--postForm mpost = renderBootstrap $ PostEditForm
--    <$> areq textField     "Slug"        (fmap (postSlug   . fst) mpost)
--    <*> areq textField     "Title"       (fmap (postTitle  . fst) mpost)
--    <*> areq textField     "Tags"        (fmap (formatTags . snd) mpost)
--    <*> aopt markdownField "Description" (fmap (postDescr  . snd) mpost)
--
--    where
--        formatTags :: [Tag] -> Text
--        formatTags = T.intercalate ", " . map tagName
