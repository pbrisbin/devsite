{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Root (getRootR) where

import DevSite
import Yesod.Goodies.Links
import Helpers.Documents
import Data.Text (Text)

-- | Home page
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
        code t = [hamlet|<code>#{t}|]
