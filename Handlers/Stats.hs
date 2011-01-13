-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Stats
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Stats (getStatsR) where

import Yesod
import DevSite
import Helpers.Stats
import Helpers.Layouts

getStatsR :: Handler RepHtml
getStatsR = pageLayout $ do
    content <- liftHandler $ statsTemplate myLogFile myTopEntries
    setTitle $ string "pbrisbin - Stats"
    addHamlet content

myLogFile :: LogFile
myLogFile = lighttpdLog 
    -- access log location
    "/var/log/lighttpd/access.log" 

    -- blacklist
    [ "127.0.0.1"     -- localhost
    , "192.168.0.1"   -- router
    , "192.168.0.5"   -- lan ip
    , "66.30.118.211" -- ext ip
    ]

myTopEntries :: [(String, String)]
myTopEntries = [ ("posts", "^/posts/.*") ]
