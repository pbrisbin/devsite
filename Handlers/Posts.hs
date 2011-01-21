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
import DevSite

import Yesod.Helpers.Auth

import Helpers.Posts

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- Post pages

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    curTime <- liftIO getCurrentTime
    posts   <- selectPosts 0
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addHamlet $ allPostsTemplate curTime posts "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> postLayout post

-- Tag pages

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    curTime <- liftIO getCurrentTime
    posts   <- selectPosts 0
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addHamlet $ allPostsTemplate curTime posts "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    curTime <- liftIO getCurrentTime
    posts'  <- getPostsByTag tag
    case posts' of
        []    -> notFound
        posts -> defaultLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addHamlet $ allPostsTemplate curTime posts ("Tag: " ++ tag)

-- Management pages

-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _        <- requireAuth
    postForm <- runPostForm Nothing
    defaultLayout $ do
        setTitle $ string "pbrisbin - Manage posts"
        addHamlet [$hamlet|
            %h1 Manage Posts

            ^postForm^
            |]

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
            postForm <- runPostForm $ Just post'
            defaultLayout $ do
                setTitle $ string "pbrisbin - Edit post"
                addHamlet [$hamlet|
                    %h1 Edit Post

                    ^postForm^
                    |]

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    _ <- requireAuth
    deletePost slug
    setMessage $ [$hamlet| %em post deleted! |]
    redirect RedirectTemporary ManagePostsR
