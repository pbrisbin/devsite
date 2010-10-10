{-# LANGUAGE CPP #-}
--
-- pbrisbin 2010
--
module Settings where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import Language.Haskell.TH.Syntax

approot :: String
#ifdef PRODUCTION
approot = "http://localhost:3000"
#else
approot = "http://localhost:3000"
#endif

staticdir :: FilePath
staticdir = "static"

staticroot :: String
staticroot = approot ++ "/static"

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
#else
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
#endif
