module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    ) where

import Import
import Data.Maybe (fromMaybe)
import Helpers.Profile
import Network.Gravatar
import Yesod.Comments.Management

getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth

    let username = fromMaybe "" $ userName u
    let email    = fromMaybe "" $ userEmail u
    let pic      = gravatarImg email gravatarOpts

    defaultLayout $ do
        setTitle "View profile"
        $(widgetFile "profile/show")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = defaultOptions
            { gSize    = Just $ Size 128
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = do
    (_, u)               <- requireAuth
    ((_, form), enctype) <- runFormPost $ profileForm u

    defaultLayout $ do
        setTitle "Edit profile"
        $(widgetFile "profile/edit")

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileForm u
    case res of
        FormSuccess ef -> saveProfile uid ef
        _              -> return ()

    getEditProfileR
