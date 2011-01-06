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

-- Since posts are now retrieved in the Handler Monad it's no longer
-- easy to create these functions, a solution is still a big todo:
arch           = "arch"
bash           = "bash"
haskell        = "haskell"
linux          = "linux"
mutt           = "mutt"
site_migration = "site_migration"
xmonad         = "xmonad"

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    posts <- selectPosts 10
    defaultLayout $ do
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
getPostsR = do
    allPosts <- selectPosts 0
    pageLayout $ do
        setTitle $ string "pbrisbin - All Posts"
        addHamlet $ allPostsTemplate allPosts "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> postLayout post

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    allPosts <- selectPosts 0
    pageLayout $ do
        setTitle $ string "pbrisbin - All Tags"
        addHamlet $ allPostsTemplate allPosts "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    posts <- getPostsByTag tag
    case posts of
        []     -> notFound
        posts' -> pageLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addHamlet $ allPostsTemplate posts' ("Tag: " ++ tag)

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    posts <- selectPosts 10
    rssFeed RssFeed
        { rssTitle       = "pbrisbin dot com"
        , rssDescription = "New posts on pbrisbin dot com"
        , rssLanguage    = "en-us"
        , rssLinkSelf    = FeedR
        , rssLinkHome    = RootR
        , rssUpdated     = mostRecent posts
        , rssEntries     = map postToRssEntry $ posts
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

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"
