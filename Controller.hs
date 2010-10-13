--
-- pbrisbin 2010
--
module Controller (withServer) where

import Yesod
import Yesod.Helpers.Static
import DevSite
import Posts
import Layouts
import Templates
import Handlers
import qualified Settings as S

-- | Instantiate the Yesod route types
mkYesodDispatch "DevSite" resourcesDevSite

-- | Allocate resources and create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = toWaiApp (DevSite s) >>= f
    where
        -- | Files are searched for in /static and served as the content
        --   their extensions signify
        s = fileLookupDir S.staticdir typeByExt
