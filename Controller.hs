-------------------------------------------------------------------------------
-- |
-- Module      :  Controller
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Imports my entire Site and defines one method 'withServer' to be used
-- when creating the WAI app.
--
-------------------------------------------------------------------------------
module Controller (withServer) where

import Yesod
import DevSite
import Posts
import Layouts
import Templates
import Handlers
import qualified Settings as S

-- | Instantiate the Yesod route types
mkYesodDispatch "DevSite" resourcesDevSite

-- | Create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = toWaiApp DevSite >>= f
