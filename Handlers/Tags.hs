{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Tags
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Tags
    ( getTagsR
    , getTagR
    ) where

import Yesod
import Yesod.Helpers.RssFeed (rssLink)
import Text.Blaze (toHtml)

import DevSite
import Helpers.Posts
import Helpers.PostTypes
import Data.Char (toLower, toUpper)

import qualified Settings

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    posts <- sitePosts =<< getYesod
    let tagGroups = mkTagGroups posts
    defaultLayout $ do
        setTitle $ toHtml $ Settings.titlePrefix ++ "All Tags"
        addKeywords $ map fst tagGroups
        [$hamlet|
            <header>
                <h1>All Tags

            <article .fullpage>
                <div id="accordion">
                    $forall tagGroup <- tagGroups
                        ^{addTagGroup tagGroup}

            <script src="//ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js">
            <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.9/jquery-ui.min.js">
            <script>
                $(function() {
                    $("#accordion").accordion({
                        collapsible: true,
                        autoHeight:  false,
                        active:      false
                    });
                });

            |]
    where
        addTagGroup :: TagGroup -> Widget ()
        addTagGroup tg = do
            let tag   = fst tg
            let posts = snd tg
            let len   = doShow $ length posts
            [$hamlet|
                <h3>#{proper tag} 
                    <span .post_count>- #{len}
                <div .hidden>
                    $forall post <- posts
                        ^{addPostBlock post}
                |]

        -- tag name -> Tag Name
        proper            = unwords . map capitalize . words

        capitalize []     = []
        capitalize (x:xs) = (toUpper x) : xs

        doShow 1 = "1 post"
        doShow n = show n ++ " posts"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag' = do
    let tag = map toLower tag'
    posts' <- sitePosts =<< getYesod
    case filter (elem tag . postTags) posts' of
        []    -> notFound
        posts -> defaultLayout $ do
            setTitle $ toHtml $ Settings.titlePrefix ++ "Tag: " ++ tag
            addKeywords [tag]
            rssLink (FeedTagR tag) ("rss feed for tag " ++ tag)
            [$hamlet|
                <header>
                    <h1>Tag: #{tag}

                <article .fullpage>
                    $forall post <- posts
                        ^{addPostBlock post}
                |]
