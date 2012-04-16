{-# LANGUAGE OverloadedStrings #-}
module Shellfile where

import Shelly

import Control.Monad (when)
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as T

binary   = "dist/build/devsite/devsite"
binary_f = "./dist/build/devsite/devsite"

main :: IO ()
main = do
    args <- getArgs

    shelly $ silently $ do
        when ("build" `elem` args || "deploy" `elem` args) $ do
            run_ "cabal" ["clean"]
            run_ "cabal" ["configure"]
            run_ "cabal" ["build"]
            
        when ("stop" `elem` args || "deploy" `elem` args) $ do
            pids <- fmap T.lines $ run "pgrep" ["-f", binary]
            run_ "kill" pids

        when ("start" `elem` args || "deploy" `elem` args) $ do
            -- bg only available in my fork FYI
            bg $ run_ binary_f ["-p", "3001", "production"]
            bg $ run_ binary_f ["-p", "3002", "production"]
            bg $ run_ binary_f ["-p", "3003", "production"]
