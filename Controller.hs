{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
--
-- pbrisbin 2010
--
module Controller where

import Yesod
import Yesod.Helpers.Static
import qualified Settings as S
import qualified Data.ByteString.Lazy as L
import System.Directory
import Data.Char (toLower)

import DevSite
import Posts
import Handlers
import Helpers.RssFeed

-- | Instantiate the Yesod route types
mkYesodDispatch "DevSite" resourcesDevSite

-- | Make my site an instance of Yesod so we can actually use it
instance Yesod DevSite where 
    approot _ = S.approot

    -- | override defaultLayout to provide an overall template and css
    --   file
    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addStyle $(S.cassiusFile "root-css")
        hamletToRepHtml $(S.hamletFile "root-layout")

    -- | With this, any generated CSS/Java will be placed in a temp file
    --   and served statically rather than added directly in the <head>
    --   of the html
    addStaticContent ext' _ content = do
        let fn        = base64md5 content ++ '.' : ext'
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
    breadcrumb TagsR      = return ("all tags" , Just RootR)
    breadcrumb (TagR tag) = return (formTag tag, Just TagsR)
        where
            formTag t = (map toLower t) ++ " tag"

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("%%%", Just RootR)

--
-- These templates require the breadcrumb instance be in scope
--

-- | Drop in replacement for defaultLayout but add breadcrumbs
pageLayout widget = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addStyle $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "page-layout")
        
-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        addPost
        addStyle $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "post-layout")
        
    where
        addPost = do
            setTitle $ string $ "pbrisbin - " ++ postTitle post
            addBody  $ postContent post

-- | A body template for a list of posts and title
allPostsTemplate :: [Post] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$
%hr

#recent_posts
    $forall posts post
        ^postTemplate.post^
|]

-- | A sub template for a single post
postTemplate :: Post -> Hamlet DevSiteRoute
postTemplate arg = [$hamlet|
.recent_post
  %p

    %a!href=@PostR postSlug.arg@ $postTitle.arg$
    \ - $postDescr.arg$ 
    
  %p.small

    Published on $postDate.arg$

    %span!style="float: right;"

      Tags: 

      $forall postTags.arg tag

        %a!href=@TagR tag@ $tag$ 
|]

--
-- These handlers require the above templates be in scope
--

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

-- | Start the server
main :: IO ()
main = basicHandler 3000 $ DevSite $ s
    where
        -- | Files are searched for in /static and served as the content
        --   their extensions signify
        s = fileLookupDir S.staticdir typeByExt
