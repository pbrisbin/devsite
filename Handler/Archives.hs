module Handler.Archives (getArchivesR) where

import Import
import Helpers.Admin
import Data.Time (getCurrentTime)
import Data.Time.Format.Human

getArchivesR :: Handler RepHtml
getArchivesR = do
    now     <- liftIO $ getCurrentTime
    isAdmin <- maybeAdmin

    posts <- runDB $ selectList [PostDraft !=. True] [Desc PostDate]

    defaultLayout $ do
        setTitle "Archives"
        $(widgetFile "archives")
