{-# LANGUAGE OverloadedStrings #-}
module Shellfile where

import Shelly

import Control.Monad (forM_, when)
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as T

binary   = "dist/build/devsite/devsite"
binary_f = "./dist/build/devsite/devsite"

main :: IO ()
main = do
    args <- getArgs

    shelly $ silently $ do
        onArgs args ["build", "deploy"] $ do
            mapM_ (run_ "cabal") [ ["clean"]
                                 , ["configure"]
                                 , ["build"]
                                 ]
            
        onArgs args ["stop", "deploy"] $ do
            run_ "kill" . T.lines =<< run "pgrep" ["-f", binary]

        onArgs args ["start", "deploy"] $ do
            forM_ ["3001", "3002", "3003"] $ \p ->
                -- bg only available in my fork
                bg $ run_ binary_f ["-p", p, "production"]

    where
        -- | When any of the ons are present in the list of args, the
        --   action is run.
        onArgs :: (Eq a, Monad m) => [a] -> [a] -> m () -> m ()
        onArgs args ons =
            when (any id $ map (`elem` args) ons)
