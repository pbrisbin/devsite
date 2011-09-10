{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE OverloadedStrings            #-}
module Handler.Posts 
    ( getPostsR
    , getPostR
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

getPostsR :: Handler RepHtml
getPostsR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "All Posts"
        addKeywords ["all posts"]
        addWidget $(widgetFile "posts")

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    docs <- siteDocs =<< getYesod
    case lookupDocument slug docs of
        Just doc -> longDocument doc docs
        Nothing  -> unpublishedDocument slug

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Manage posts"
        addWidget $(widgetFile "manage")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin
    docs <- siteDocs =<< getYesod
    let mdoc = lookupDocument slug docs
    defaultLayout $ do
        setTitle "Edit post"
        addWidget $(widgetFile "editposts")

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
