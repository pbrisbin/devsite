-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Feed
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Feed where

import Yesod
import DevSite

import Yesod.Helpers.Stats

import Helpers.Posts
import Helpers.RssFeed

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    logRequest
    results <- selectPosts 10
    case results of
        []    -> notFound
        posts -> feedFromPosts posts

-- | Rss feed, limited to a tag
getFeedTagR :: String -> Handler RepRss
getFeedTagR tag = do
    logRequest
    results <- getPostsByTag tag
    case results of
        []    -> notFound
        posts -> feedFromPosts posts

feedFromPosts :: [Post] -> Handler RepRss
feedFromPosts posts = rssFeed RssFeed
    { rssTitle       = "pbrisbin dot com"
    , rssDescription = string $ "New posts on pbrisbin dot com"
    , rssLanguage    = "en-us"
    , rssLinkSelf    = FeedR
    , rssLinkHome    = RootR
    , rssUpdated     = mostRecent posts
    , rssEntries     = map postToRssEntry posts
    }
    where mostRecent = rssEntryUpdated . postToRssEntry . head

postToRssEntry :: Post -> RssFeedEntry DevSiteRoute
postToRssEntry post = RssFeedEntry
    { rssEntryLink    = PostR $ postSlug post
    , rssEntryUpdated = postDate post
    , rssEntryTitle   = postTitle post
    , rssEntryContent = string $ postDescr post
    }
