{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    ) where

import DevSite
import Helpers.Forms
import Yesod.Helpers.Auth
import Yesod.Goodies.Gravatar
import Data.Maybe (fromMaybe)

getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth

    let username = fromMaybe "" $ userName u
    let email    = fromMaybe "" $ userEmail u
    let pic      = gravatarImg email gravatarOpts

    defaultLayout $ do
        setTitle "View profile"
        addWidget $(widgetFile "profile")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = defaultOptions
            { gSize    = Just $ Size 48
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = defaultLayout $ do
    setTitle "Edit profile"
    runProfileFormGet

postEditProfileR :: Handler RepHtml
postEditProfileR = runProfileFormPost >> getEditProfileR
