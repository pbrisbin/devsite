{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Handler functions for each of the site's routes.
--
-------------------------------------------------------------------------------
module Handlers
    ( getRootR
    , getAboutR
    , getStatsR
    , getPostsR
    , getPostR
    , getTagsR
    , getTagR
    , getFeedR
    , getFaviconR
    , getRobotsR
    ) where

import Yesod
import DevSite
import Posts
import Pkgs
import Stats
import Layouts
import qualified Settings as S
import Helpers.RssFeed

-- | These two TH calls will define runnable functions for every post
--   slug and tag currently in use on the site.
mkPostSlugs
mkPostTags

-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    -- recent posts
    let posts = selectPosts 10

    -- render the page
    setTitle $ string "pbrisbin - Home"
    addHamlet $(S.hamletFile "index")
     
-- | Stats page
getStatsR :: Handler RepHtml
getStatsR = pageLayout $ do
    content <- liftHandler $ statsTemplate myLogFile myTopEntries
    setTitle $ string "pbrisbin - Stats"
    addHamlet $ content

myLogFile :: LogFile
myLogFile = lighttpdLog "/var/log/lighttpd/access.log" ["127.0.0.1", "192.168.0.1", "192.168.0.5", "66.30.118.211"]

myTopEntries :: [(String, String)]
myTopEntries = [ ("posts"         , "^/posts/.*"             )
               , ("xmonad modules", "^/xmonad/docs/.*\\.html")
               ]

-- | About page
getAboutR :: Handler RepHtml
getAboutR = pageLayout $ do
    setTitle $ string "pbrisbin - About"
    addHamlet $(S.hamletFile "about")

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = pageLayout $ do
    setTitle $ string "pbrisbin - All Posts"
    addHamlet $ allPostsTemplate (selectPosts 0) "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug =
    case getPostBySlug slug of
        []       -> notFound
        (post:_) -> postLayout post

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = pageLayout $ do
    setTitle $ string "pbrisbin - All Tags"
    addHamlet $ allPostsTemplate (selectPosts 0) "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = 
    case getPostsByTag tag of
        []    -> notFound
        posts -> pageLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addHamlet $ allPostsTemplate posts ("Tag: " ++ tag)

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = rssFeed RssFeed
    { rssTitle       = "pbrisbin dot com"
    , rssDescription = "New posts on pbrisbin dot com"
    , rssLanguage    = "en-us"
    , rssLinkSelf    = FeedR
    , rssLinkHome    = RootR
    , rssUpdated     = mostRecent
    , rssEntries     = map postToRssEntry $ selectPosts 10
    }
    where
        -- note: head of empty list
        mostRecent = rssEntryUpdated . postToRssEntry . head $ selectPosts 1

postToRssEntry :: Post -> RssFeedEntry DevSiteRoute
postToRssEntry post = RssFeedEntry
    { rssEntryLink    = PostR $ postSlug post
    , rssEntryUpdated = postDate post
    , rssEntryTitle   = postTitle post
    , rssEntryContent = string $ postDescr post
    }

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"
