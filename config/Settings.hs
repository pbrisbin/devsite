{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , luciusFile
    , widgetFile
    , approot
    , staticRoot
    , setTitle
    , addKeywords
    , pandocFile
    , withConnectionPool
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import qualified Text.Lucius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql
import qualified Data.Text as T

import Yesod (MonadControlIO, hamlet, addWidget, addCassius, addJulius, addLucius)
import qualified Yesod as Y

import Data.Monoid (mempty)
import System.Directory (doesFileExist)

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

globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile = 
#ifdef PROD
  H.cassiusFile . globFile "cassius"
#else
  H.cassiusFileDebug . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile = 
#ifdef PROD
  H.luciusFile . globFile "lucius"
#else
  H.luciusFileDebug . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef PROD
  H.juliusFile . globFile "julius"
#else
  H.juliusFileDebug . globFile "julius"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists (globFile "hamlet")  hamletFile
    let c = unlessExists (globFile "cassius") cassiusFile
    let j = unlessExists (globFile "julius")  juliusFile
    let l = unlessExists (globFile "lucius")  luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]

connStr :: T.Text
#ifdef PROD
connStr = "user=pbrisbin password=password host=localhost port=5432 dbname=pbrisbin"
#else
connStr = "user=pbrisbin password=password host=localhost port=5432 dbname=pbrisbin_dev"
#endif

connCount :: Int
connCount = 100

withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connCount