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
    , loadConfig
    , AppEnvironment(..)
    , AppConfig(..)
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
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql

import Yesod (liftIO, MonadControlIO, addWidget, addCassius, addJulius, addLucius, whamletFile,hamlet)
import qualified Yesod as Y
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
import Data.Text (Text, pack, concat)
import qualified Data.Text as T
import Prelude hiding (concat)
import Data.Object
import qualified Data.Object.Yaml as YAML
import Control.Monad (join)

data AppEnvironment = Development | Production
                    deriving (Eq, Show, Read, Enum, Bounded)

data AppConfig = AppConfig
    { appEnv             :: AppEnvironment
    , appPort            :: Int
    , connectionPoolSize :: Int
    , appRoot            :: Text
    } deriving (Show)

loadConfig :: AppEnvironment -> IO AppConfig
loadConfig env = do
    allSettings <- (join $ YAML.decodeFile ("config/settings.yml" :: String)) >>= fromMapping
    settings <- lookupMapping (show env) allSettings
    hostS <- lookupScalar "host" settings
    port <- fmap read $ lookupScalar "port" settings
    connectionPoolSizeS <- lookupScalar "connectionPoolSize" settings
    return $ AppConfig
        { appEnv  = env
        , appPort = port
        , appRoot = pack $ hostS ++ addPort port
        , connectionPoolSize = read connectionPoolSizeS
        }

    where
        addPort :: Int -> String
#ifdef PRODUCTION
        addPort _ = ""
#else
        addPort p = ":" ++ (show p)
#endif

setTitle :: Y.Yesod m => Text -> Y.GWidget s m ()
setTitle = Y.setTitle . Y.toHtml . T.append "pbrisbin - "

addKeywords :: [Text] -> Y.GWidget s m ()
addKeywords ws = Y.addHamletHead [hamlet|<meta name="keywords" content="#{format ws}">|]
    where 
        -- add some default keywords, and make the comma separated 
        format :: [Text] -> Text
        format = T.append "patrick brisbin, pbrisbin, brisbin, " . T.intercalate ", "

pandocFile :: Text -> FilePath
pandocFile x = T.unpack $ T.concat ["/srv/http/pandoc/", x, ".pdc"]

staticLink :: FilePath -> String
staticLink x =
#ifdef PRODUCTION
    "/static/" ++ x
#else
    "http://pbrisbin.com/static/" ++ x
#endif

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

withConnectionPool :: MonadControlIO m => AppConfig -> (ConnectionPool -> m a) -> m a
withConnectionPool conf f = do
    cs <- liftIO $ loadConnStr (appEnv conf)
    withPostgresqlPool cs (connectionPoolSize conf) f
  where
    -- | The database connection string. The meaning of this string is backend-
    -- specific.
    loadConnStr :: AppEnvironment -> IO Text
    loadConnStr env = do
        allSettings <- (join $ YAML.decodeFile ("config/postgresql.yml" :: String)) >>= fromMapping
        settings <- lookupMapping (show env) allSettings
        database <- lookupScalar "database" settings :: IO Text

        connPart <- fmap concat $ (flip mapM) ["user", "password", "host", "port"] $ \key -> do
          value <- lookupScalar key settings
          return $ [st| #{key}=#{value} |]
        return $ [st|#{connPart} dbname=#{database}|]

globFile :: String -> String -> FilePath
globFile kind x = kind ++ "/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = S.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile = 
#ifdef PRODUCTION
  S.cassiusFile . globFile "cassius"
#else
  S.cassiusFileDebug . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile = 
#ifdef PRODUCTION
  S.luciusFile . globFile "lucius"
#else
  S.luciusFileDebug . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef PRODUCTION
  S.juliusFile . globFile "julius"
#else
  S.juliusFileDebug . globFile "julius"
#endif

textFile :: FilePath -> Q Exp
textFile =
#ifdef PRODUCTION
    S.textFile . globFile "text"
#else
    S.textFileDebug . globFile "text"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = whenExists (globFile "hamlet")  (whamletFile . globFile "hamlet")
    let c = whenExists (globFile "cassius") cassiusFile
    let j = whenExists (globFile "julius")  juliusFile
    let l = whenExists (globFile "lucius")  luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    whenExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]
