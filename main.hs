{-# LANGUAGE CPP #-}
import Application (withDevSite)
import qualified Settings

#ifdef PRODUCTION
import Yesod.Logger (makeLogger)
import Network.Wai.Handler.FastCGI  (run)
import Settings (AppEnvironment(..))
#else
import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)
import Network.Wai.Handler.Warp     (run)
import Network.Wai.Middleware.Debug (debugHandle)
import Settings (AppConfig(..), AppEnvironment(..))
#endif

main :: IO ()
main = do
    l <- makeLogger
#ifdef PRODUCTION
    c <- Settings.loadConfig Production
    withDevSite c l $ run
#else
    c <- Settings.loadConfig Development
    logString l $ "Application launched, listening on port " ++ show (appPort c)
    withDevSite c l $ run (appPort c) . debugHandle (logHandle l)
    flushLogger l

    where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger
#endif
