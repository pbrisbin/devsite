{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , luciusFile
    , textFile
    , widgetFile
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , staticLink
    , setTitle
    , addKeywords
    , pandocFile
    ) where

import qualified Text.Hamlet as S
import qualified Text.Cassius as S
import qualified Text.Julius as S
import qualified Text.Lucius as S
import qualified Text.Shakespeare.Text as S
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql
import Yesod (liftIO, MonadControlIO, addWidget, addCassius, addJulius, addLucius, whamletFile,hamlet)
import Yesod.Default.Config
import qualified Yesod as Y
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
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

globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = S.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile = 
#ifdef DEVELOPMENT
    S.cassiusFileDebug . globFile "cassius"
#else
    S.cassiusFile . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile = 
#ifdef DEVELOPMENT
    S.luciusFileDebug . globFile "lucius"
#else
    S.luciusFile . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef DEVELOPMENT
    S.juliusFileDebug . globFile "julius"
#else
    S.juliusFile . globFile "julius"
#endif

textFile :: FilePath -> Q Exp
textFile =
#ifdef DEVELOPMENT
    S.textFileDebug . globFile "text"
#else
    S.textFile . globFile "text"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = whenExists (globFile "hamlet" ) (whamletFile . globFile "hamlet")
    let c = whenExists (globFile "cassius") cassiusFile
    let j = whenExists (globFile "julius" ) juliusFile
    let l = whenExists (globFile "lucius" ) luciusFile

    [| addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l |]

    where
        whenExists tofn f = do
            e <- qRunIO $ doesFileExist $ tofn x
            if e then f x else [| mempty |]
