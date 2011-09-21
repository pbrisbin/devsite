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
import Yesod.Logger (Logger)
import Database.Persist.GenericSql
import Data.Dynamic (Dynamic, toDyn)
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import Control.Monad (forM)
import Yesod.Comments.Management
import Yesod.Comments.Storage

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
    Settings.withConnectionPool conf $ \p -> do
        runConnectionPool (runMigration migratePosts)    p
        runConnectionPool (runMigration migrateComments) p
        let h = DevSite conf logger p loadDocuments

        tid  <- forkIO $ toWaiApp h >>= f >> return ()
        flag <- newEmptyMVar
        _    <- Signal.installHandler Signal.sigINT (Signal.CatchOnce $ do
            putStrLn "Caught an interrupt"
            killThread tid
            putMVar flag ()) Nothing
        takeMVar flag

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
