{-# LANGUAGE CPP #-}
import Application (withDevSite)
import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)
import Settings (AppConfig(..), AppEnvironment(..))
import qualified Settings

#ifdef PRODUCTION
import Network.Wai.Handler.FastCGI  (run)
#else
import Network.Wai.Handler.Warp     (run)
import Network.Wai.Middleware.Debug (debugHandle)
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
