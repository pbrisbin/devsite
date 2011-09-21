import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withDevSite)

main :: IO ()
main = defaultMain fromArgs withDevSite
