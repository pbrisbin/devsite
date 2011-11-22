{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , setTitle
    , addKeywords
    , pandocFile
    ) where

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Data.Text (Text)
import qualified Data.Text as T

import Yesod hiding (setTitle)
import Yesod.Default.Config
import qualified Yesod
import qualified Yesod.Default.Util

type PersistConfig = PostgresConf

setTitle :: Yesod m => Text -> GWidget s m ()
setTitle = Yesod.setTitle . toHtml . T.append "pbrisbin - "

addKeywords :: [Text] -> GWidget s m ()
addKeywords ws = addHamletHead [hamlet|<meta name="keywords" content="#{format ws}">|]

    where 
        -- add some default keywords, and make the comma separated list
        format :: [Text] -> Text
        format = T.append "patrick brisbin, pbrisbin, brisbin, " . T.intercalate ", "

pandocFile :: Text -> FilePath
pandocFile x = "pandoc/" ++ T.unpack x ++ ".pdc"

staticDir :: FilePath
staticDir = "static"

staticRoot :: AppConfig DefaultEnv -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

widgetFile :: FilePath -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
    Yesod.Default.Util.widgetFileDebug
#else
    Yesod.Default.Util.widgetFileProduction
#endif
