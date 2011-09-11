import Yesod.Main  (defaultMain, fromArgs)
import Application (withDevSite)

main :: IO ()
main = defaultMain fromArgs withDevSite
