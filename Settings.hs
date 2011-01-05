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
-- Static settings used in all modules.
--
-------------------------------------------------------------------------------
module Settings
    ( approot
    , hamletFile
    , cassiusFile
    , withConnectionPool
    ) where

import qualified Text.Hamlet  as H
import qualified Text.Cassius as C
import Language.Haskell.TH.Syntax

import Yesod hiding (approot)
import Database.Persist.Sqlite

-- #define PRODUCTION

approot :: String
#ifdef PRODUCTION
approot = "http://pbrisbin.com"
#else
approot = "http://localhost:3000"
#endif

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
#else
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = C.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = C.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
#endif

dataBase :: String
dataBase = "comments.db3"

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
