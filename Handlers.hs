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
    , module Handlers.Posts
    , module Handlers.Tags
    , module Handlers.Feed
    ) where

import Yesod
import DevSite

import Handlers.Root
import Handlers.About
import Handlers.Posts
import Handlers.Tags
import Handlers.Feed

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"
