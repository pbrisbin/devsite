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
import Helpers.Layouts

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    curTime <- liftIO getCurrentTime
    posts'  <- selectPosts 0
    let posts = zip posts' (repeat curTime)
    pageLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addHamlet $ allPostsTemplate posts "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> postLayout post

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    curTime  <- liftIO getCurrentTime
    posts'   <- selectPosts 0
    let posts = zip posts' (repeat curTime)
    pageLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addHamlet $ allPostsTemplate posts "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    curTime <- liftIO getCurrentTime
    posts'  <- getPostsByTag tag
    case posts' of
        []     -> notFound
        posts'' -> do
            let posts = zip posts'' (repeat curTime)
            pageLayout $ do
                setTitle $ string $ "pbrisbin - Tag: " ++ tag
                addHamlet $ allPostsTemplate posts ("Tag: " ++ tag)

-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _        <- requireAuth
    postForm <- runPostForm Nothing

    pageLayout $ do
        setTitle $ string "pbrisbin - Manage posts"
        addHamlet [$hamlet|
        #header
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

            pageLayout $ do
                setTitle $ string "pbrisbin - Edit post"
                addHamlet [$hamlet|
                #header
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
