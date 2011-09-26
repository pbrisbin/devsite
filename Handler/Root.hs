{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root (getRootR) where

import Foundation
import Helpers.Documents
import Data.Text (Text)

getRootR :: Handler RepHtml
getRootR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Home"
        addKeywords ["home", "haskell", "bash", "mutt", "xmonad", "arch linux"]
        addWidget $(widgetFile "homepage")

    where
        -- helps OverloadedStrings determine the type
        link'' :: Text -> GWidget s DevSite ()
        link'' = link

        code :: Text -> GWidget s DevSite ()
        code t = [whamlet|<code>#{t}|]
