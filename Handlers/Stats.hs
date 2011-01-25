{-# LANGUAGE QuasiQuotes #-}
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
import Yesod.Helpers.Stats.Widgets

import DevSite

getStatsR :: Handler RepHtml
getStatsR = defaultLayout $ do
    setTitle $ string "pbrisbin - Stats"
    addKeywords ["pbrisbin", "stats"]
    addBreadcrumbs

    addHamlet [$hamlet| 
        %h1 Stats
        %h3 General statistics
        |]

    overallStats 

    addHamlet [$hamlet| %h3 Popular requests |]

    topRequests 10 ("posts", "^/posts/.*")
    topRequests 5  ("tags" , "^/tags/.*" )
