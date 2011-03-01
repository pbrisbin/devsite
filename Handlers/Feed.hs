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
import Yesod.Helpers.RssFeed
import Text.Blaze (toHtml)

import DevSite
import Helpers.PostTypes

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
feedFromPosts posts = rssFeed Feed
    { feedTitle       = "pbrisbin dot com"
    , feedDescription = toHtml $ "New posts on pbrisbin dot com"
    , feedLanguage    = "en-us"
    , feedLinkSelf    = FeedR
    , feedLinkHome    = RootR
    -- note: posts is known to be not empty coming in
    , feedUpdated     = postDate $ head posts
    , feedEntries     = map postToRssEntry posts
    }

postToRssEntry :: Post -> FeedEntry DevSiteRoute
postToRssEntry post = FeedEntry
    { feedEntryLink    = PostR $ postSlug post
    , feedEntryUpdated = postDate post
    , feedEntryTitle   = postTitle post
    , feedEntryContent = toHtml $ postDescr post
    }
