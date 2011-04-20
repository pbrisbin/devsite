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
    , setTitle
    , pandocFile
    , withConnectionPool
    ) where

import Control.Monad.IO.Peel   (MonadPeelIO)
import Database.Persist.Sqlite (ConnectionPool(..), withSqlitePool)
import Text.Blaze              (toHtml)

import qualified Yesod

setTitle :: Yesod.Yesod m => String -> Yesod.GWidget s m ()
setTitle = Yesod.setTitle . toHtml . (++) "pbrisbin - " 

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

withConnectionPool :: MonadPeelIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
