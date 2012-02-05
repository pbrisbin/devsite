module Handler.Users (getUsersR) where

import Import
import Helpers.Admin
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

getUsersR :: Handler RepHtml
getUsersR = do
    requireAdmin

    records <- runDB $ do
        users <- selectList [] [Asc UserId]
        creds <- selectList [] []

        forM users $ \user -> do
            let uid  = entityKey user
            let cred = head $ filter ((== uid) . identUser . entityVal) creds

            return (user,cred)

    defaultLayout $ do
        setTitle "All users"
        $(widgetFile "user/index")

userWidget :: UserId -> User -> Ident -> Widget
userWidget uid u i = $(widgetFile "user/_row_item")
