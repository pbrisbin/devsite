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
    , getManagePostsR
    , postManagePostsR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Yesod
import Yesod.Helpers.Auth

import DevSite
import Helpers.Posts
import Helpers.PostTypes
import Helpers.RssFeed (rssLink)
import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    DevSite _ hposts _ <- getYesod
    posts              <- hposts
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addKeywords ["all posts"]
        [$hamlet| 
            %h1 All Posts 
            $forall posts post
                ^addPostBlock.post^ 
            |]

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    DevSite _ hposts _ <- getYesod
    posts              <- hposts
    case filter ((==) slug . postSlug) posts of
        []       -> notFound
        (post:_) -> defaultLayout $ addPostContent post
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
    _                  <- requireAuth
    DevSite _ hposts _ <- getYesod
    posts              <- hposts
    case filter ((==) slug . postSlug) posts of
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
    setMessage $ [$hamlet| post deleted! |]
    redirect RedirectTemporary ManagePostsR
