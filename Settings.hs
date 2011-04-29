{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( approot
    , staticRoot
    , setTitle
    , addKeywords
    , pandocFile
    , withConnectionPool
    ) where

import Yesod (hamlet)
import Database.Persist.Sqlite
import qualified Yesod as Y
import qualified Data.Text as T

setTitle :: Y.Yesod m => T.Text -> Y.GWidget s m ()
setTitle = Y.setTitle . Y.toHtml . T.append "pbrisbin - "

addKeywords :: [T.Text] -> Y.GWidget s m ()
addKeywords ws = Y.addHamletHead [hamlet|<meta name="keywords" content="#{format ws}">|]
    where 
        -- add some default keywords, and make the comma separated 
        format :: [T.Text] -> T.Text
        format = T.append "patrick brisbin, pbrisbin, brisbin, " . T.intercalate ", "

pandocFile :: T.Text -> FilePath
pandocFile x = T.unpack $ T.concat ["/srv/http/pandoc/", x, ".pdc"]

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

withConnectionPool :: Y.MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
