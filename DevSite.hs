{-#LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, TypeSynonymInstances #-}
-- 
-- pbrisbin 2010
--
module Main where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import System.Directory
import qualified Text.Hamlet as H
import qualified Text.Cassius as C
import qualified Data.ByteString.Lazy as L

import Data.Time
import Data.Char (toLower)
import Data.ByteString.UTF8 (toString)

import DataTypes
import Posts
import qualified Settings as S

-- create static file handlers
staticFiles "static"

-- Routes {{{
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

-- }}}

-- Instance {{{
instance Yesod DevSite where 
    approot _ = "http://localhost:3000"

    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addStyle $(S.cassiusFile "root")
        hamletToRepHtml $(S.hamletFile "root")

    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let statictmp = S.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        liftIO $ L.writeFile (statictmp ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodBreadcrumbs DevSite where
    breadcrumb RootR  = return ("root" , Nothing) 
    breadcrumb AboutR = return ("about", Just RootR)

    breadcrumb PostsR       = return ("all posts"  , Just RootR)
    breadcrumb (PostR slug) = return (getTitle slug, Just PostsR)
        where
            getTitle s = 
                case loadPost s of
                    []       -> "?" -- this will never happen
                    (post:_) -> map toLower $ postTitle post

    breadcrumb TagsR        = return ("all tags"     , Just RootR)
    breadcrumb (TagR tag)   = return ("tag: " ++ tag , Just TagsR)

    -- fail noticably so i fix it
    breadcrumb _ = return ("X", Just RootR)

-- }}}

-- Layouts {{{


-- | Like defaultLayout but with crumbs
pageLayout widget = do
    mmesg <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addStyle $(S.cassiusFile "root")
    hamletToRepHtml $(S.hamletFile "page")
        
-- | Each post's template
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

-- }}}

-- Handlers {{{

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

-- | A post page
getPostR :: String -> Handler RepHtml
getPostR slug = do
    case loadPost slug of
        []       -> notFound
        (post:_) -> postLayout post
        
getTagsR :: Handler RepHtml
getTagsR = pageLayout $ do
    let posts = allPosts
    setTitle $ string "pbrisbin - All Tags"
    addBody $(S.hamletFile "posts/all_tags")

-- | A tag page
getTagR :: String -> Handler RepHtml
getTagR tag = do
    case getPostsByTag tag of
        []    -> notFound
        posts -> pageLayout $ do
            setTitle $ string $ "pbrisbin - Tag: " ++ tag
            addBody $(S.hamletFile "posts/tagged")

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"


getFeedR :: Handler RepHtml
getFeedR = notFound

--do
--    atomFeed AtomFeed
--        { atomTitle = "New posts on pbrisbin dot com"
--        , atomLinkSelf = FeedR
--        , atomLinkHome = RootR
--        , atomUpdated = mostRecent
--        , atomEntries = map readAtomEntry allPosts
--        }
--        where
--            mostRecent = let date = read . postDate $ head allPosts :: Day
--                         in UTCTime date $ secondsToDiffTime 0
--
--            readAtomEntry post = AtomFeedEntry
--                { atomEntryLink    = PostR $ postSlug post
--                , atomEntryUpdated = readDate post
--                , atomEntryTitle   = postTitle post
--                , atomEntryContent = string $ postDescr post
--                }
--                where
--                    readDate p = let date = read $ postDate p :: Day
--                                 in UTCTime date $ secondsToDiffTime 0
--                               

-- }}}

-- | Start the server
main :: IO ()
main = basicHandler 3000 $ DevSite $ s
    where
        s = fileLookupDir S.staticdir typeByExt
