{-# LANGUAGE OverloadedStrings #-}
module Handlers 
    ( getFaviconR
    , getRobotsR
    , module X
    ) where

import DevSite

import Handlers.Root    as X
import Handlers.About   as X
import Handlers.Posts   as X
import Handlers.Profile as X
import Handlers.Tags    as X
import Handlers.Feed    as X

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)
