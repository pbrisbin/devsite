{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Controller (withServer) where

import DevSite
import Model
import Handlers

import Yesod
import Yesod.Helpers.MPC
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.HashDB (migrateUsers)

import Database.Persist.GenericSql

import qualified Settings

-- | Instantiate the Yesod route types
mkYesodDispatch "DevSite" resourcesDevSite

-- | Create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts) p
    runSqlPool (runMigration migrateUsers) p
    f =<< toWaiApp (DevSite p loadDocuments)
