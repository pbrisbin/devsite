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

import DevSite
import Helpers.Posts
import Helpers.RssFeed (rssLink)
import Data.Char (toLower, toUpper)

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    tagGroups <- getTagGroups
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addKeywords $ map fst tagGroups

        addCassius [$cassius|
            h3
                cursor:      pointer
                border:      none !important
                outline:     none !important
                margin-left: 0px

            .post_count
                color: #909090
            |]

        [$hamlet|
            %h1 All Tags
            #accordion
                $forall tagGroups tagGroup
                    ^addTagGroup.tagGroup^

            %script!src="//ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"
            %script!src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.9/jquery-ui.min.js"
            %script
                $$(function() {
                    $$("#accordion").accordion({
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
            let len   = show $ length posts
            [$hamlet|
                %h3 $proper.tag$ 
                    %span.post_count - $len$ posts
                %div.hidden
                    $forall posts post
                        ^addPostBlock.post^
                |]

        -- tag name -> Tag Name
        proper            = unwords . map capitalize . words
        capitalize []     = []
        capitalize (x:xs) = (toUpper x) : xs

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag' = do
    let tag = map toLower tag'
    posts'<- getPostsByTag tag
    case posts' of
        []    -> notFound
        posts -> defaultLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addKeywords [tag]
            rssLink (FeedTagR tag) ("rss feed for tag " ++ tag)
            [$hamlet| 
                %h1 Tag: $tag$ 
                $forall posts post
                    ^addPostBlock.post^
                |]
