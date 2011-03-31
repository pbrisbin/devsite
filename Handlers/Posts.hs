{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Text.Blaze (toHtml)

import DevSite
import Helpers.Posts
import Helpers.PostTypes

import qualified Settings

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = do
    posts <- sitePosts =<< getYesod
    defaultLayout $ do
        setTitle $ toHtml $ Settings.titlePrefix ++ "All Posts"
        addKeywords ["all posts"]
        [hamlet|
            <header>
                <h1>All Posts

            $forall post <- posts
                ^{addPostBlock post}
            |]

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- sitePosts =<< getYesod
    case helper slug posts of
        (Nothing  , Nothing, Nothing) -> notFound
        (Just post, mprev  , mnext  ) -> defaultLayout $ do
            addPostContent post
            [hamlet|
                <p .post_nav>
                    <span .left>
                        $maybe prev <- mprev
                            &#9666&nbsp;&nbsp;&nbsp;
                            <a href="@{PostR $ postSlug prev}">#{postTitle prev}
                        $nothing
                            <a href="@{RootR}">Home

                    <span .right>
                        $maybe next <- mnext
                            <a href="@{PostR $ postSlug next}">#{postTitle next}
                            &nbsp;&nbsp;&nbsp;&#9656
                |]

    where
        -- | Return the desired post, and maybe the post just before and 
        --   just after it in the list
        helper _ [] = (Nothing,Nothing,Nothing) -- not found

        helper slug (p1:[]) = if postSlug p1 == slug
            then (Just p1, Nothing, Nothing)
            else helper slug []

        helper slug (p1:p2:[]) = if postSlug p1 == slug
            then (Just p1, Nothing, Just p2)
            else if postSlug p2 == slug
                then (Just p2, Just p1, Nothing)
                else helper slug []

        helper slug (p1:p2:p3:ps) = if postSlug p1 == slug
            then (Just p1, Nothing, Just p2)
            else if postSlug p2 == slug
                then (Just p2, Just p1, Just p3)
                else helper slug (p2:p3:ps)

-- | Manage posts
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle $ toHtml $ Settings.titlePrefix ++ "Manage posts"
        [hamlet|
            <header>
                <h1>Manage Posts
            <article .fullpage>
                ^{runPostForm Nothing}
            |]

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

-- | Edit post
getEditPostR :: String -> Handler RepHtml
getEditPostR slug = do
    _     <- requireAuth
    posts <- sitePosts =<< getYesod
    case filter ((==) slug . postSlug) posts of
        []        -> notFound
        (post':_) -> do
            defaultLayout $ do
                setTitle $ toHtml $ Settings.titlePrefix ++ "Edit post"
                [hamlet|
                    <header>
                        <h1>Edit Post
                    <article .fullpage>
                        ^{runPostForm (Just post')}
                    |]

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    _ <- requireAuth
    deletePost slug
    setMessage "post deleted!"
    redirect RedirectTemporary ManagePostsR
