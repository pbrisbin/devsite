{-# LANGUAGE TemplateHaskell #-}
--
-- pbrisbin 2010
--
module Layouts where

import Yesod
import DevSite
import Posts
import qualified Settings as S

-- | Drop in replacement for defaultLayout but add breadcrumbs
pageLayout :: DWidget () -> Handler RepHtml
pageLayout widget = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addStyle $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "page-layout")
        
-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        addPost
        addStyle $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "post-layout")
        
    where
        addPost = do
            setTitle $ string $ "pbrisbin - " ++ postTitle post
            addBody  $ postContent post
