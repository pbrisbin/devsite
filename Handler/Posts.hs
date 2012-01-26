module Handler.Posts 
    ( getPostR
    , postPostR
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Import
import Control.Monad (forM)
import Data.Time.Format.Human
import Helpers.Forms

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    (post,tags) <- runDB $ do
        (Entity key val) <- getBy404 $ UniquePost slug
        tags' <- selectList [TagPost ==. key] [Asc TagName]

        return (val, map entityVal tags')

    published <- liftIO $ humanReadableTime $ postDate post

    defaultLayout $ do
        setTitle slug
        addKeywords $ map tagName tags
        $(widgetFile "post/show")

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin

    -- select all (Post, [Tag])
    records <- runDB $ do
        posts <- selectList [] [Desc PostDate]
        tags  <- selectList [] [Asc TagName]

        forM posts $ \post -> do
            let pid   = entityKey post
            let post' = entityVal post
            let tags' = filter ((== pid) . tagPost) $ map entityVal tags

            return (post', tags')

    defaultLayout $ do
        setTitle "Manage posts"
        $(widgetFile "posts/index")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin

    (post,tags) <- runDB $ do
        (Entity key val) <- getBy404 $ UniquePost slug
        tags' <- selectList [TagPost ==. key] [Asc TagName]

        return (val, map entityVal tags')

    published <- liftIO $ humanReadableTime $ postDate post

    defaultLayout $ do
        setTitle "Edit post"
        $(widgetFile "post/edit")

postEditPostR :: Text -> Handler RepHtml
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler RepHtml
getDelPostR slug = do
    requireAdmin

    msg <- runDB $ do
        mentity <- getBy $ UniquePost slug

        case mentity of
            Just (Entity key _) -> do
                delete key
                deleteWhere [TagPost ==. key]
                return "post deleted!"

            _ -> return "post not found!"

    setMessage msg
    redirect ManagePostsR

requireAdmin :: Handler ()
requireAdmin = do
    (_, u) <- requireAuth
    if userAdmin u
        then return ()
        else permissionDenied "User is not an admin"
