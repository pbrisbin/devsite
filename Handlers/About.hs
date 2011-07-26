{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handlers.About (getAboutR) where

import DevSite

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    -- three validation images used
    let img1 = staticRoot ++ "/images/valid-html5.png"
    let img2 = staticRoot ++ "/images/valid-css.png"
    let img3 = staticRoot ++ "/images/valid-rss.png"

    setTitle "About"
    addKeywords ["about"]
    addWidget $(widgetFile "about")
