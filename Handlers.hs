{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers 
    ( getFaviconR
    , getRobotsR
    , module Handlers.Root
    , module Handlers.About
    , module Handlers.Feed
    , module Handlers.Tags
    --, module Handlers.Posts
    ) where

import Yesod
import DevSite

import Handlers.Root
import Handlers.About
import Handlers.Feed
import Handlers.Tags
--import Handlers.Posts

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)
