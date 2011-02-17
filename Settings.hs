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
    , cssLink
    , pandocFile
    , withConnectionPool
    ) where

import Yesod hiding (approot)

import Database.Persist.Sqlite
import Language.Haskell.TH.Syntax

import qualified Text.Hamlet  as H
import qualified Text.Cassius as C

approot :: String
#ifdef PROD
approot = "http://pbrisbin.com"
#else
approot = "http://localhost:3000"
#endif

cssLink :: String
#ifdef PROD
cssLink = "/static/css/style.css"
#else
cssLink = "//pbrisbin.com/static/css/style.css"
#endif

dataBase :: String
#ifdef PROD
dataBase = "posts.s3db"
#else
dataBase = "dev-posts.s3db"
#endif

pandocFile :: String -> FilePath
#ifdef PROD
pandocFile x = "pandoc/" ++ x ++ ".pdc"
#else
pandocFile x = "/srv/http/pandoc/" ++ x ++ ".pdc"
#endif

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
