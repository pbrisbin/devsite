module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    ) where

import Import
import Data.Maybe (fromMaybe)
import Helpers.Profile
import Network.Gravatar

getProfileR :: Handler RepHtml
getProfileR = do
    (Entity _ user) <- requireAuth

    let username = fromMaybe "" $ userName user
    let email    = fromMaybe "" $ userEmail user
    let pic      = gravatar gravatarOpts email

    defaultLayout $ do
        setTitle "View profile"
        $(widgetFile "profile/show")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = def
            { gSize    = Just $ Size 128
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = do
    (Entity _ u)         <- requireAuth
    ((_, form), enctype) <- runFormPost $ profileForm u

    defaultLayout $ do
        setTitle "Edit profile"
        $(widgetFile "profile/edit")

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (Entity uid u)    <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileForm u
    case res of
        FormSuccess ef -> saveProfile uid ef
        _              -> return ()

    getEditProfileR
