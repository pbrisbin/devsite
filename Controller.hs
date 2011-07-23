{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Controller (withServer) where

import DevSite
import Handlers

import Yesod.Helpers.Auth
import Yesod.Comments.Storage
import Control.Monad (forM)
import Database.Persist.GenericSql
import qualified Settings

mkYesodDispatch "DevSite" resourcesDevSite

withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts)    p
    runSqlPool (runMigration migrateComments) p
    f =<< toWaiApp (DevSite p loadDocuments)

    where
        loadDocuments :: Handler [Document]
        loadDocuments = do
            ps <- runDB $ selectList [] [PostDateDesc] 0 0
            ts <- fmap (map snd) (runDB $ selectList [] [TagNameAsc] 0 0)
            forM ps $ \(k,v) -> return $ Document v $ filter ((== k) . tagPost) ts
