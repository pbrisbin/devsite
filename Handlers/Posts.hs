{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE OverloadedStrings            #-}
module Handlers.Posts 
    ( getPostsR
    , getPostR
    , postPostR
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import DevSite
import Helpers.Documents
import Helpers.Forms
import Yesod.Helpers.Auth
import Data.Text (Text)

getPostsR :: Handler RepHtml
getPostsR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "All Posts"
        addKeywords ["all posts"]
        [hamlet|
            <header>
                <h1>All Posts

            $forall doc <- docs
                ^{shortDocument doc}
            |]

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    docs <- siteDocs =<< getYesod
    case lookupDocument slug docs of
        Just doc -> defaultLayout $ longDocument doc (documentNavigation doc docs)
        Nothing  -> defaultLayout $ unpublishedDocument slug

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    requireAdmin
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Manage posts"
        [hamlet|
            <header>
                <h1>Manage Posts
            <article .fullpage>
                ^{runPostForm Nothing}
                ^{documentsList docs}
            |]

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    requireAdmin
    docs <- siteDocs =<< getYesod

    let mdoc = safeHead $ filter ((==) slug . postSlug . post) docs

    defaultLayout $ do
        setTitle "Edit post"
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

postEditPostR :: Text -> Handler RepHtml
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler RepHtml
getDelPostR slug = do
    requireAdmin
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (key, _) -> do
            -- delete the post and the tags
            runDB $ deleteBy $ UniquePost slug
            runDB $ deleteWhere [TagPostEq key]
            setMessage "post deleted!"
        Nothing -> setMessage "post not found."

    redirect RedirectTemporary ManagePostsR

requireAdmin :: Handler ()
requireAdmin = do
    (_, u) <- requireAuth
    if userAdmin u
        then return ()
        else permissionDenied "User is not an admin"
