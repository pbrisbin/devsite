{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Settings
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Settings
    ( approot
    , staticRoot
    , titlePrefix
    , pandocFile
    , withConnectionPool
    ) where

import Yesod hiding (approot)

import Database.Persist.Sqlite
import Language.Haskell.TH.Syntax

import qualified Text.Hamlet  as H
import qualified Text.Cassius as C

titlePrefix :: String
titlePrefix = "pbrisbin - "

pandocFile :: String -> FilePath
pandocFile x = "/srv/http/pandoc/" ++ x ++ ".pdc"

approot :: String
#ifdef PROD
approot = "http://pbrisbin.com"
#else
approot = "http://localhost:3000"
#endif

staticRoot :: String
#ifdef PROD
staticRoot = "/static"
#else
staticRoot = "http://pbrisbin.com/static"
#endif

dataBase :: String
#ifdef PROD
dataBase = "posts.s3db"
#else
dataBase = "dev-posts.s3db"
#endif

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
