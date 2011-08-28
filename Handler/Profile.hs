{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    ) where

import Foundation
import Helpers.Forms
import Yesod.Comments.Management
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
postEditProfileR = do
    runProfileFormPost
    getEditProfileR
