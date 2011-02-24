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
    posts <- sitePosts =<< getYesod
    defaultLayout $ do
        setTitle $ string $ Settings.titlePrefix ++ "All Posts"
        addKeywords ["all posts"]
        [$hamlet| 
            %header
                %h1 All Posts

            $forall posts post
                ^addPostBlock.post^ 
            |]

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- sitePosts =<< getYesod
    case filter ((==) slug . postSlug) posts of
        []       -> notFound
        (post:_) -> defaultLayout $ addPostContent post

-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle $ string $ Settings.titlePrefix ++ "Manage posts"
        [$hamlet|
            %header
                %h1 Manage Posts
            %article.fullpage
                ^runPostForm.Nothing^
            |]

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

-- | Edit post
getEditPostR :: String -> Handler RepHtml
getEditPostR slug = do
    _                  <- requireAuth
    posts <- sitePosts =<< getYesod
    case filter ((==) slug . postSlug) posts of
        []        -> notFound
        (post':_) -> do
            defaultLayout $ do
                setTitle $ string $ Settings.titlePrefix ++ "Edit post"
                [$hamlet|
                    %header
                        %h1 Edit Post
                    %article.fullpage
                        ^runPostForm.Just.post'^
                    |]

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    _ <- requireAuth
    deletePost slug
    setMessage $ [$hamlet| post deleted! |]
    redirect RedirectTemporary ManagePostsR
