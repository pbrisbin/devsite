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
import Helpers.PostTypes
import Helpers.RssFeed

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    posts' <- fmap (take 10) . sitePosts =<< getYesod
    case posts' of
        []    -> notFound
        posts -> feedFromPosts posts

-- | Rss feed, limited to a tag
getFeedTagR :: String -> Handler RepRss
getFeedTagR tag = do
    posts' <- sitePosts =<< getYesod
    case filter (elem tag . postTags) posts' of
        []    -> notFound
        posts -> feedFromPosts posts

feedFromPosts :: [Post] -> Handler RepRss
feedFromPosts posts = rssFeed RssFeed
    { rssTitle       = "pbrisbin dot com"
    , rssDescription = string $ "New posts on pbrisbin dot com"
    , rssLanguage    = "en-us"
    , rssLinkSelf    = FeedR
    , rssLinkHome    = RootR
    -- note: posts is known to be not empty coming in
    , rssUpdated     = postDate $ head posts
    , rssEntries     = map postToRssEntry posts
    }

postToRssEntry :: Post -> RssFeedEntry DevSiteRoute
postToRssEntry post = RssFeedEntry
    { rssEntryLink    = PostR $ postSlug post
    , rssEntryUpdated = postDate post
    , rssEntryTitle   = postTitle post
    , rssEntryContent = string $ postDescr post
    }
