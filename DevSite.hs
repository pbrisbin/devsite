{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE TypeFamilies    #-}
{-#LANGUAGE QuasiQuotes     #-}
-- 
-- pbrisbin 2010
--
module DevSite where

import Yesod
import Yesod.Helpers.Static

data DevSite = DevSite { getStatic :: Static }
type Handler = GHandler DevSite DevSite

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
