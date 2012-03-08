module Handler.Archives (getArchivesR) where

import Import
import Helpers.Post

getArchivesR :: Handler RepHtml
getArchivesR = do
    now   <- liftIO $ getCurrentTime
    posts <- runDB $ selectList [PostDraft !=. True] [Desc PostDate]

    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "archives")
