{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Helpers.Layouts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Helpers.Layouts
    ( pageLayout
    , postLayout
    ) where

import Yesod
import DevSite

import Helpers.Posts

import qualified Settings

-- | Like defaultLayout, just with breadcrumbs, used with any top level
--   pages
pageLayout :: Widget () -> Handler RepHtml
pageLayout widget = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addCassius $(Settings.cassiusFile "root-css")
    hamletToRepHtml $(Settings.hamletFile "page-layout")
        
-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg       <- getMessage
    (t, h)      <- breadcrumbs
    postContent <- loadPostContent post

    pc <- widgetToPageContent $ do
        setTitle $ string $ "pbrisbin - " ++ postTitle post
        addCassius $(Settings.cassiusFile "root-css")
    hamletToRepHtml $(Settings.hamletFile "post-layout")
