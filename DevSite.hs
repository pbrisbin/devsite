{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE TemplateHaskell #-}
-- 
-- pbrisbin 2010
--
module Main where

import Yesod
import Yesod.Helpers.Static
import System.Directory
import qualified Text.Hamlet as H
import qualified Text.Cassius as C
import qualified Data.ByteString.Lazy as L

import Data.Time
import Data.Char (toLower)

import DataTypes
import Posts
import qualified Settings as S
import Helpers.RssFeed -- like Y.H.AtomFeed

-- | Automatically generate callable functions for everything under
--   the /static directory
staticFiles "static"

-- | Define all of the routes and handlers
mkYesod "DevSite" [$parseRoutes|
/              RootR    GET
/about         AboutR   GET
/posts         PostsR   GET
/posts/#String PostR    GET
/tags          TagsR    GET
/tags/#String  TagR     GET
/feed          FeedR    GET
/favicon.ico   FaviconR GET
/robots.txt    RobotsR  GET
/static        StaticR Static getStatic
|]

-- | Make my site an instance of Yesod so we can actually use it
instance Yesod DevSite where 
    approot _ = "http://localhost:3000"

    -- | override defaultLayout to provide a fallback overall template
    --   and css file
    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addStyle $(S.cassiusFile "root")
        hamletToRepHtml $(S.hamletFile "root")

    -- | With this, any generated CSS/Java will be placed in a temp file
    --   and served statically rather than being placed inline with the
    --   html
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let statictmp = S.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        liftIO $ L.writeFile (statictmp ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

-- | Make my site an instance of breadcrumbs so that i can simply call
--   the breadcrumbs function to get automagical breadcrumb links
instance YesodBreadcrumbs DevSite where
    -- root is the parent node
    breadcrumb RootR  = return ("root" , Nothing) 

    -- about goes back home
    breadcrumb AboutR = return ("about", Just RootR)

    -- all posts goes back home and individual posts go to all posts
    breadcrumb PostsR       = return ("all posts"  , Just RootR)
    breadcrumb (PostR slug) = return (getTitle slug, Just PostsR)
        where
            getTitle s = 
                case loadPost s of
                    []       -> "?" -- this will never happen
                    (post:_) -> map toLower $ postTitle post

    -- all tags goes back home and individual tags go to all tags
    breadcrumb TagsR        = return ("all tags" , Just RootR)
    breadcrumb (TagR tag)   = return (formTag tag, Just TagsR)
        where
            formTag t = (map toLower t) ++ " tag"

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("%%%", Just RootR)

-- | Drop in replacement for defaultLayout but add breadcrumbs
pageLayout widget = do
    mmesg <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addStyle $(S.cassiusFile "root")
    hamletToRepHtml $(S.hamletFile "page")
        
-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        addPost
        addStyle $(S.cassiusFile "root")
    hamletToRepHtml $(S.hamletFile "post")
        
    where
        addPost = do
            setTitle $ string $ "pbrisbin - " ++ postTitle post
            addBody  $ postContent post

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
    let posts = allPosts
    setTitle $ string "pbrisbin - All Posts"
    addBody $(S.hamletFile "posts/all")

-- | A post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    case loadPost slug of
        []       -> notFound
        (post:_) -> postLayout post
        
-- | All tags
getTagsR :: Handler RepHtml
getTagsR = pageLayout $ do
    let posts = allPosts
    setTitle $ string "pbrisbin - All Tags"
    addBody $(S.hamletFile "posts/all_tags")

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag = do
    case getPostsByTag tag of
        []    -> notFound
        posts -> pageLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addBody $(S.hamletFile "posts/tagged")

-- | favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | robots.txt
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
 
-- | Start the server
main :: IO ()
main = basicHandler 3000 $ DevSite $ s
    where
        s = fileLookupDir S.staticdir typeByExt
