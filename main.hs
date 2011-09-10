-- compile to fastcgi
import Application  (withDevSite)
import Yesod.Logger (makeLogger)
import Network.Wai.Handler.FastCGI (run)

import Settings (AppEnvironment(..))
import qualified Settings

main :: IO ()
main = do
    l <- makeLogger
    c <- Settings.loadConfig Production
    withDevSite c l $ run
