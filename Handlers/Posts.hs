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

import Control.Monad (forM, liftM)
import Data.List (sortBy)
import Data.Ord (comparing)
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
    -- create a tuple [(tag, [post])] and sort on length [posts]
    tagPosts <- liftM sortTags $ forM tags $ \tag -> do
        posts <- getPostsByTag tag
        return (tag, posts)
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addKeywords $ "pbrisbin":tags

        -- add some styling for this page only
        addCassius [$cassius|
            h3
                cursor:      pointer
                border:      none !important
                outline:     none !important
                margin-left: 0px

            .post_count
                color: #909090
            |]

        -- accordion effect
        addJulius [$julius|
            $(function() {
                $("#accordion").accordion({
                collapsible:true,
                autoHeight: false,
                active: false
                });
            });
            |]

        [$hamlet|
            %h1 All Tags
            #accordion
                $forall tagPosts tagPost
                    %h3 $fst.tagPost$ 
                        %span.post_count - $show.length.snd.tagPost$ posts
                    %div
                        $forall snd.tagPost post
                            ^addPostBlock.post^
            |]
    where
        sortTags = reverse . sortBy (comparing (length . snd))

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
            [$hamlet| 
                %h1 Tag: $tag$ 
                $forall posts post
                    ^addPostBlock.post^
                |]

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
    setMessage $ [$hamlet| post deleted! |]
    redirect RedirectTemporary ManagePostsR
