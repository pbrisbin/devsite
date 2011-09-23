{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( widgetFile
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , staticLink
    , setTitle
    , addKeywords
    , pandocFile
    ) where

import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql

import Yesod (liftIO, MonadControlIO, hamlet)
import Yesod.Default.Config

import qualified Yesod as Y
import qualified Yesod.Default.Util

import Data.Text (Text)
import qualified Data.Text as T

setTitle :: Y.Yesod m => Text -> Y.GWidget s m ()
setTitle = Y.setTitle . Y.toHtml . T.append "pbrisbin - "

addKeywords :: [Text] -> Y.GWidget s m ()
addKeywords ws = Y.addHamletHead [hamlet|<meta name="keywords" content="#{format ws}">|]

    where 
        -- add some default keywords, and make the comma separated list
        format :: [Text] -> Text
        format = T.append "patrick brisbin, pbrisbin, brisbin, " . T.intercalate ", "

pandocFile :: Text -> FilePath
pandocFile x = "pandoc/" ++ T.unpack x ++ ".pdc"

staticLink :: FilePath -> String
staticLink x = "http://pbrisbin.com/static/" ++ x

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

withConnectionPool :: MonadControlIO m => AppConfig DefaultEnv -> (ConnectionPool -> m a) -> m a
withConnectionPool conf f = do
    conf <- liftIO $ loadPostgresql (appEnv conf)
    withPostgresqlPool (pgConnStr conf ) (pgPoolSize conf) f

widgetFile :: FilePath -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
    Yesod.Default.Util.widgetFileDebug
#else
    Yesod.Default.Util.widgetFileProduction
#endif
