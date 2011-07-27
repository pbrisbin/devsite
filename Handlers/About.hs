{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.About (getAboutR) where

import DevSite

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About"
    addKeywords ["about"]
    addWidget $(widgetFile "about")
