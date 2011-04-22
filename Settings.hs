{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( approot
    , staticRoot
    , setTitle
    , pandocFile
    , withConnectionPool
    ) where

import Database.Persist.Sqlite
import qualified Yesod
import qualified Data.Text as T

setTitle :: Yesod.Yesod m => String -> Yesod.GWidget s m ()
setTitle = Yesod.setTitle . Yesod.toHtml . (++) "pbrisbin - " 

pandocFile :: String -> FilePath
pandocFile x = "/srv/http/pandoc/" ++ x ++ ".pdc"

approot :: T.Text
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

dataBase :: T.Text
dataBase = "db.s3db"

withConnectionPool :: Yesod.MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
