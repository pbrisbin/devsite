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

import Helpers.Posts
import Helpers.RssFeed

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    posts <- selectPosts 10
    rssFeed RssFeed
        { rssTitle       = "pbrisbin dot com"
        , rssDescription = string $ "New posts on pbrisbin dot com"
        , rssLanguage    = "en-us"
        , rssLinkSelf    = FeedR
        , rssLinkHome    = RootR
        , rssUpdated     = mostRecent posts
        , rssEntries     = map postToRssEntry posts
        }

    where
        -- note: head of empty list possible
        mostRecent = rssEntryUpdated . postToRssEntry . head

postToRssEntry :: Post -> RssFeedEntry DevSiteRoute
postToRssEntry post = RssFeedEntry
    { rssEntryLink    = PostR $ postSlug post
    , rssEntryUpdated = postDate post
    , rssEntryTitle   = postTitle post
    , rssEntryContent = string $ postDescr post
    }
