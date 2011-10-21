{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root (getRootR) where

import Foundation
import Helpers.Documents

getRootR :: Handler RepHtml
getRootR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Home"
        addKeywords ["home", "haskell", "bash", "mutt", "xmonad", "arch linux"]
        addWidget $(widgetFile "homepage")
