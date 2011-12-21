{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.About (getAboutR) where

import Foundation

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About"
    addKeywords ["about"]
    $(widgetFile "about")
