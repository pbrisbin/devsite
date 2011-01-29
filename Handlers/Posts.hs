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

import DevSite
import Helpers.Posts
import Helpers.RssFeed (rssLink)

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    posts <- selectPosts 0
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addKeywords ["pbrisbin", "all posts"]
        addHamlet [$hamlet| %h1 All Posts |]
        mapM_ addPostBlock posts

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> defaultLayout $ addPostContent post

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    tags <- selectTags
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addKeywords $ "pbrisbin":tags

        addCassius [$cassius|
            h3
                border: none !important
                margin-left: 0px

            .lighter
                color: #909090
            |]

        addJulius [$julius|
            $(function() {
                $("#accordion").accordion({
                collapsible:true,
                autoHeight: false,
                active: false
                });
            });
            |]

        pageContent <- liftHandler $ widgetToPageContent (mapM_ go tags)

        addHamlet [$hamlet|
            %h1 All Tags
            #accordion
                ^pageBody.pageContent^
            |]

    where
        -- each tags section
        go tag = do
            posts     <- liftHandler $ getPostsByTag tag
            postsList <- liftHandler $ widgetToPageContent (mapM_ addPostBlock posts)
            addHamlet [$hamlet|
                %h3 $tag$ 
                    %span.lighter - $show.length.posts$ posts
                %div
                    ^pageBody.postsList^
                |]

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    posts'<- getPostsByTag tag
    case posts' of
        []    -> notFound
        posts -> defaultLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addKeywords ["pbrisbin", tag]
            rssLink (FeedTagR tag) ("rss feed for tag " ++ tag)
            addHamlet [$hamlet| 
                %h1 Tag: $tag$ 
                |]
            mapM_ addPostBlock posts

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
