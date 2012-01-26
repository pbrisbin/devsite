module Handler.About (getAboutR) where

import Import
import Settings.StaticFiles

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About"
    addKeywords ["about"]
    $(widgetFile "about")
