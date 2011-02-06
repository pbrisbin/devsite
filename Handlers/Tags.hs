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

import Control.Monad (forM, liftM)
import Data.List (sortBy)
import Data.Ord (comparing)

type Tag      = String
type TagGroup = (Tag, [Post])

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    tagGroups <- getTagGroups
    defaultLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addKeywords $ map fst tagGroups

        -- styling for this page only {{{
        addCassius [$cassius|
            h3
                cursor:      pointer
                border:      none !important
                outline:     none !important
                margin-left: 0px

            .post_count
                color: #909090
            |]
        -- }}}

        -- accordion effect {{{
        addJulius [$julius|
            $(function() {
                $("#accordion").accordion({
                collapsible:true,
                autoHeight: false,
                active: false
                });
            });
            |]
        -- }}}

        [$hamlet|
            %h1 All Tags
            #accordion
                $forall tagGroups tagGroup
                    ^addTagGroup.tagGroup^
            |]
    where
        addTagGroup :: TagGroup -> Widget ()
        addTagGroup tg = do
            let tag   = fst tg
            let posts = snd tg
            let len   = show $ length posts
            [$hamlet|
                %h3 $tag$ 
                    %span.post_count - $len$ posts
                %div
                    $forall posts post
                        ^addPostBlock.post^
                |]

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
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

getTagGroups :: Handler [TagGroup]
getTagGroups = do
    tags <- selectTags
    liftM sortTags $ forM tags $ \tag -> do
        posts <- getPostsByTag tag
        return (tag, posts)

sortTags :: [TagGroup] -> [TagGroup]
sortTags = reverse . sortBy (comparing (length . snd))
