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
import Data.Time.Format.Human
import Helpers.Post
import System.Directory (doesFileExist)
import Yesod.Comments (addCommentsAuth)
import Yesod.Links
import Yesod.Markdown

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    (post,tags) <- runDB $ do
        (Entity key val) <- getBy404 $ UniquePost slug
        tags' <- selectList [TagPost ==. key] [Asc TagName]

        return (val, map entityVal tags')

    published <- liftIO $ humanReadableTime $ postDate post

    let file = pandocFile $ postSlug post

    content <- liftIO $ do
        exists <- doesFileExist file
        mkd    <- case (exists, postDescr post) of
            (True, _         ) -> markdownFromFile file
            (_   , Just descr) -> return descr
            _                  -> return "nothing?"

        return $ markdownToHtml mkd

    let (mprev,mnext) = (Nothing,Nothing) :: (Maybe Post, Maybe Post)

    defaultLayout $ do
        setTitle slug
        addKeywords $ map tagName tags
        $(widgetFile "post/show")

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin

    posts <- runDB $ selectList [] [Desc PostDate]

    ((res,form), enctype) <- runFormPost $ postForm Nothing

    case res of
        FormSuccess pf -> upsertPost pf
        _              -> return ()

    defaultLayout $ do
        setTitle "Manage posts"
        $(widgetFile "post/index")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin

    record <- runDB $ do
        (Entity key val) <- getBy404 $ UniquePost slug
        tags' <- selectList [TagPost ==. key] [Asc TagName]

        return (val, map entityVal tags')

    ((res,form), enctype) <- runFormPost $ postForm $ Just record

    case res of
        FormSuccess pf -> upsertPost pf
        _              -> return ()

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
                deleteWhere [TagPost ==. key]
                delete key
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
