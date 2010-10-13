{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE TypeFamilies    #-}
{-#LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  DevSite
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- The actual site definition. What routes there are and instances it is
-- a part of.
--
-------------------------------------------------------------------------------
module DevSite where

import Yesod
import Yesod.Helpers.Static
import Yesod.WebRoutes

import qualified Settings as S
import qualified Data.ByteString.Lazy as L

import System.Directory
import Data.Char (toLower)

-- | The main site type
data DevSite = DevSite { getStatic :: Static }
type Handler = GHandler DevSite DevSite
type DWidget = GWidget  DevSite DevSite

-- | Automatically generate callable functions for everything under
--   the /static directory
staticFiles "static"

-- | Define all of the routes and handlers
mkYesodData "DevSite" [$parseRoutes|
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
    approot _ = S.approot

    -- | override defaultLayout to provide an overall template and css
    --   file
    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addStyle $(S.cassiusFile "root-css")
        hamletToRepHtml $(S.hamletFile "root-layout")

    -- | This lets static file server bypass the application code
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a S.staticroot) $ format s
        
        where
            format = formatPathSegments ss
            ss :: Site StaticRoute (String -> Maybe (GHandler Static DevSite ChooseRep))
            ss = getSubSite

    urlRenderOverride _ _ = Nothing

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
    breadcrumb PostsR       = return ("all posts", Just RootR)
    breadcrumb (PostR slug) = return (format slug, Just PostsR)
        where
            -- switch underscores with spaces
            format []         = []
            format ('_':rest) = ' ': format rest
            format (x:rest)   = x  : format rest

    -- all tags goes back home and individual tags go to all tags
    breadcrumb TagsR      = return ("all tags" , Just RootR)
    breadcrumb (TagR tag) = return (format tag, Just TagsR)
        where
            format t = (map toLower t) ++ " tag"

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("%%%", Just RootR)
