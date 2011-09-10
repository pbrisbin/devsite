import Application (withDevSite)
import Yesod.Logger (makeLogger)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

import Settings (AppConfig(..), AppEnvironment(..), loadConfig)

main :: IO ()
main = do
    l <- makeLogger
    c <- loadConfig Production
    p <- getArgsWithDefault $ appPort c
    withDevSite c l $ run p

    where
        getArgsWithDefault :: Int -> IO Int
        getArgsWithDefault def = return . go =<< getArgs

            where
                go :: [String] -> Int
                go ("-p"    :portS:_) = read portS
                go ("--port":portS:_) = read portS
                go _                  = def
