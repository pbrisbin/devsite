module Handler.Archives (getArchivesR) where

import Import
import Helpers.Post

getArchivesR :: Handler RepHtml
getArchivesR = do
    posts <- runDB $ selectList [] [Desc PostDate]

    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "archives")

    where
        postWidget :: Post -> Widget
        postWidget post = do
            published <- liftIO $ postPublished post
            $(widgetFile "post/_archive_row_item")
