{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE OverloadedStrings            #-}
module Handler.Posts 
    ( getPostR
    , postPostR
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Foundation
import Helpers.Documents
import Helpers.Forms
import Data.Text (Text)

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    docs <- siteDocs =<< getYesod
    case lookupDocument slug docs of
        Just doc -> pageDocument doc docs
        Nothing  -> unpublishedDocument slug

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Manage posts"
        $(widgetFile "posts_admin/index")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin
    docs <- siteDocs =<< getYesod
    let mdoc = lookupDocument slug docs
    defaultLayout $ do
        setTitle "Edit post"
        $(widgetFile "posts_admin/edit")

postEditPostR :: Text -> Handler RepHtml
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler RepHtml
getDelPostR slug = do
    requireAdmin
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (key, _) -> do
            -- delete the post and the tags
            runDB $ deleteWhere [TagPost ==. key]
            runDB $ deleteBy $ UniquePost slug
            setMessage "post deleted!"
        Nothing -> setMessage "post not found."

    redirect RedirectTemporary ManagePostsR

requireAdmin :: Handler ()
requireAdmin = do
    (_, u) <- requireAuth
    if userAdmin u
        then return ()
        else permissionDenied "User is not an admin"
