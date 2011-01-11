{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.About
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.About (getAboutR) where

import Yesod
import DevSite

import Helpers.Layouts

import qualified Settings

-- | About page
getAboutR :: Handler RepHtml
getAboutR = pageLayout $ do
    setTitle $ string "pbrisbin - About"
    addHamlet $(Settings.hamletFile "about")
