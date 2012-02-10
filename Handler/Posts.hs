module Handler.Posts 
    ( getPostR
    , postPostR
    , getManagePostsR
    , postManagePostsR
    , getNewPostR
    , postNewPostR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Import
import Data.Time (getCurrentTime)
import Data.Time.Format.Human
import Helpers.Admin
import Helpers.Post
import Yesod.Comments (addCommentsAuth)
import Yesod.Links

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    (post,tags,mprev,mnext) <- runDB $ do
        (post',tags') <- getPost404 slug
        mprev'        <- getPreviousPost post'
        mnext'        <- getNextPost     post'

        return $ (post',tags',mprev',mnext')

    published   <- liftIO $ humanReadableTime $ postDate post
    content     <- liftIO $ postContent post

    defaultLayout $ do
        setTitle slug
        addKeywords $ map tagName tags
        $(widgetFile "post/show")

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin

    now   <- liftIO $ getCurrentTime
    posts <- runDB  $ selectList [] [Desc PostDate]

    defaultLayout $ do
        setTitle "Manage posts"
        $(widgetFile "post/index")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getNewPostR :: Handler RepHtml
getNewPostR = do
    requireAdmin

    ((res,form), enctype) <- runFormPost $ postForm Nothing

    case res of
        FormSuccess pf -> upsertPost pf
        _              -> return ()

    defaultLayout $ do
        setTitle "New post"
        $(widgetFile "post/new")

postNewPostR :: Handler RepHtml
postNewPostR = getNewPostR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin

    record <- runDB $ getPost404 slug

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
