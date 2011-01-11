{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Root
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Root (getRootR) where

import Yesod
import DevSite

import Helpers.Pkgs
import Helpers.Posts

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- Since posts are now retrieved in the Handler Monad it's no longer
-- easy to create these functions, a solution is still a big todo:
arch           = "arch"
bash           = "bash"
haskell        = "haskell"
linux          = "linux"
mutt           = "mutt"
site_migration = "site_migration"
xmonad         = "xmonad"

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    curTime <- liftIO getCurrentTime
    posts'  <- selectPosts 10
    let posts = zip posts' (repeat curTime)
    defaultLayout $ do
        setTitle $ string "pbrisbin - Home"
        addHamlet $(Settings.hamletFile "index")
