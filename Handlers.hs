{-# LANGUAGE TemplateHaskell #-}
--
-- pbrisbin 2010
--
module Handlers where

import Yesod
import DevSite
import Posts
import Layouts
import Templates
import qualified Settings as S
import Helpers.RssFeed
 
-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    let posts = take 10 allPosts
    setTitle $ string "pbrisbin - Home"
    addBody $(S.hamletFile "index")
     
-- | About page
getAboutR :: Handler RepHtml
getAboutR = pageLayout $ do
    setTitle $ string "pbrisbin - About"
    addBody $(S.hamletFile "about")

-- | All posts
getPostsR :: Handler RepHtml
getPostsR = pageLayout $ do
    setTitle $ string "pbrisbin - All Posts"
    addBody $ allPostsTemplate allPosts "All Posts"

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    case loadPost slug of
        []       -> notFound
        (post:_) -> postLayout post

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = pageLayout $ do
    setTitle $ string "pbrisbin - All Tags"
    addBody  $ allPostsTemplate allPosts "All Tags"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    case getPostsByTag tag of
        []    -> notFound
        posts -> pageLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addBody  $ allPostsTemplate posts ("Tag: " ++ tag)

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

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"
