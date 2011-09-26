{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-orphans      #-}
module Application
    ( withDevSite
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Util
import Yesod.Logger (Logger)
import Data.Dynamic (Dynamic, toDyn)
import Control.Monad (forM)
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Database.Persist.GenericSql (runMigration)
import qualified Database.Persist.Base as Base

import Handler.About
import Handler.Feed
import Handler.Posts
import Handler.Profile
import Handler.Root
import Handler.Tags

mkYesodDispatch "DevSite" resourcesDevSite

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

withDevSite :: AppConfig DefaultEnv -> Logger -> (Application -> IO a) -> IO ()
withDevSite conf logger f = do
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            $ either error return . Base.loadConfig

    Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Base.runPool dbconf (runMigration migratePosts)    p
        Base.runPool dbconf (runMigration migrateComments) p
        defaultRunner f $ DevSite conf logger p loadDocuments

    where
        loadDocuments :: Handler [Document]
        loadDocuments = do
            ps <- runDB $ selectList [] [Desc PostDate]
            ts <- runDB $ selectList [] [Asc  TagName ]
            forM ps $ \(k,v) -> do
                let ts' = filter ((== k) . tagPost) $ map snd ts
                return $ Document v ts'

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withDevSite
