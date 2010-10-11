{-# LANGUAGE TemplateHaskell #-}
--
-- pbrisbin 2010
--
module Handlers where

import Yesod
import DevSite
import Posts
import qualified Settings as S
import Helpers.RssFeed
  
--
-- These handlers don't need Controller to be in scope
--

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    rssFeed RssFeed
        { rssTitle       = "pbrisbin dot com"
        , rssDescription = "New posts on pbrisbin dot com"
        , rssLanguage    = "en-us"
        , rssLinkSelf    = FeedR
        , rssLinkHome    = RootR
        , rssUpdated     = mostRecent
        , rssEntries     = map readRssEntry $ take 10 allPosts
        }
        where
            mostRecent = postDate $ head allPosts

            readRssEntry post = RssFeedEntry
                { rssEntryLink    = PostR $ postSlug post
                , rssEntryUpdated = postDate post
                , rssEntryTitle   = postTitle post
                , rssEntryContent = string $ postDescr post
                }
