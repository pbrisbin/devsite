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
-- Imports my Site and defines one method 'withServer' to be used when
-- creating the WAI app.
--
-------------------------------------------------------------------------------
module Controller (withServer) where

import Yesod
import DevSite
import Settings
import Posts
import Handlers

import HashDB
import Yesod.Helpers.Auth
import Database.Persist.GenericSql

-- | Instantiate the Yesod route types
mkYesodDispatch "DevSite" resourcesDevSite

-- | Create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts) p
    runSqlPool (runMigration migrateUsers) p
    let h = DevSite p
    toWaiApp h >>= f
