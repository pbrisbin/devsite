module Handler.Archives (getArchivesR) where

import Import

getArchivesR :: Handler RepHtml
getArchivesR = do
    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "archives")
