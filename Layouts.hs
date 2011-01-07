{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Layouts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Functions defined to be used in place of 'defaultLayout' where an
-- alternative but still generic page layout should be used.
--
-------------------------------------------------------------------------------
module Layouts
    ( pageLayout
    , postLayout
    ) where

import Yesod
import DevSite
import Posts
import qualified Settings as S

-- | Like defaultLayout, just with breadcrumbs, used with any top level
--   pages
pageLayout :: Widget () -> Handler RepHtml
pageLayout widget = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addCassius $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "page-layout")
        
-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg       <- getMessage
    (t, h)      <- breadcrumbs
    postContent <- loadPostContent post

    pc <- widgetToPageContent $ do
        setTitle $ string $ "pbrisbin - " ++ postTitle post
        addCassius $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "post-layout")
