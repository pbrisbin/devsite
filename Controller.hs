{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Controller (withServer) where

import DevSite

import Handlers.Root
import Handlers.About
import Handlers.Posts
import Handlers.Tags
import Handlers.Profile
import Handlers.Feed

import Yesod.Helpers.Auth
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Control.Monad (forM)
import Database.Persist.GenericSql
import qualified Settings

mkYesodDispatch "DevSite" resourcesDevSite

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts)    p
    runSqlPool (runMigration migrateComments) p
    f =<< toWaiApp (DevSite p loadDocuments)

    where
        loadDocuments :: Handler [Document]
        loadDocuments = do
            ps <- runDB $ selectList [] [PostDateDesc] 0 0
            ts <- runDB $ selectList [] [TagNameAsc  ] 0 0
            forM ps $ \(k,v) -> do
                let ts' = filter ((== k) . tagPost) $ map snd ts
                return $ Document v ts'
