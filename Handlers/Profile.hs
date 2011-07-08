{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    ) where

import DevSite
import Model
import Yesod
import Yesod.Helpers.Auth
import Yesod.Goodies.Gravatar
import Control.Applicative ((<$>), (<*>))
import Data.Maybe          (fromMaybe)
import qualified Data.Text as T
import qualified Settings

data EditForm = EditForm
    { eUsername :: Maybe T.Text
    , eEmail    :: Maybe T.Text
    }

getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth

    let username = fromMaybe "" $ userName u
    let email    = fromMaybe "" $ userEmail u
    let pic      = gravatarImg email gravatarOpts

    defaultLayout $ do
        Settings.setTitle "View profile"
        [hamlet|
            <h1>You
            <article .fullpage .profile>
                <div .gravatar>
                    <a title="change your profile picture at gravatar" href="http://gravatar.com/emails/">
                        <img src="#{pic}">

                <table>
                    <tr>
                        <th>User name:
                        <td>#{username}
                    <tr>
                        <th>Email address:
                        <td>#{email}
                    <tr>
                        <td #edit-button colspan="2">
                            <a href="@{EditProfileR}">edit
        |]

        where
            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultOptions
                { gSize    = Just $ Size 48
                , gDefault = Just MM
                }

getEditProfileR :: Handler RepHtml 
getEditProfileR = defaultLayout $ do
    Settings.setTitle "Edit profile"
    [hamlet|
        <h1>Edit
        <article .fullpage .profile
            ^{showForm}
        |]

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormMonadPost $ editForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    -- we should never get here since all fields are optional
    getEditProfileR

showForm :: Widget ()
showForm = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift . runFormMonadPost $ editForm u

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]

editForm :: User -> FormMonad (FormResult EditForm, Widget())
editForm u = do
    (fUsername, fiUsername) <- maybeStringField "User name:"     $ Just $ userName u
    (fEmail   , fiEmail   ) <- maybeEmailField  "Email address:" $ Just $ userEmail u

    return (EditForm <$> fUsername <*> fEmail, [hamlet|
            <table>
                ^{fieldRow fiUsername}
                ^{fieldRow fiEmail}
                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
            |])
    where

        fieldRow fi = [hamlet|
            <tr ##{fiIdent fi}>
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                    <div .tooltip>#{fiTooltip fi}
                <td>
                    ^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
            |]

saveChanges :: UserId -> EditForm -> Handler ()
saveChanges uid ef = do
    runDB $ update uid 
        [ UserName  $ eUsername ef
        , UserEmail $ eEmail    ef
        ]

    tm <- getRouteToMaster
    redirect RedirectTemporary $ tm ProfileR
