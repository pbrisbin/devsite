{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Posts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Posts 
    ( getPostsR
    , getPostR
    , getTagsR
    , getTagR
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Stats

import DevSite
import Helpers.Posts

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- Post pages

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    logRequest
    curTime <- liftIO getCurrentTime
    posts   <- selectPosts 0
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addKeywords ["pbrisbin", "all posts"]
        addHamlet $ allPostsTemplate curTime posts "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    logRequest
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> defaultLayout $ addPostContent post

-- Tag pages

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    logRequest
    curTime <- liftIO getCurrentTime
    posts   <- selectPosts 0
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addKeywords ["pbrisbin", "all tags"]
        addHamlet $ allPostsTemplate curTime posts "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    logRequest
    curTime <- liftIO getCurrentTime
    posts'  <- getPostsByTag tag
    case posts' of
        []    -> notFound
        posts -> defaultLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addKeywords ["pbrisbin", tag]
            addHamlet $ allPostsTemplate curTime posts ("Tag: " ++ tag)

-- Management pages

-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle $ string "pbrisbin - Manage posts"
        addHamlet [$hamlet| %h1 Manage Posts |]
        runPostForm Nothing

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

-- | Edit post
getEditPostR :: String -> Handler RepHtml
getEditPostR slug = do
    _    <- requireAuth
    post <- getPostBySlug slug
    case post of
        []        -> notFound
        (post':_) -> do
            defaultLayout $ do
                setTitle $ string "pbrisbin - Edit post"
                addHamlet [$hamlet| %h1 Edit Post |]
                runPostForm $ Just post'

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    _ <- requireAuth
    deletePost slug
    setMessage $ [$hamlet| %em post deleted! |]
    redirect RedirectTemporary ManagePostsR
