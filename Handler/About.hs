module Handler.About (getAboutR) where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    setTitle "About"
    addKeywords ["about"]
    $(widgetFile "about")
