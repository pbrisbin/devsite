{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Helpers.Auth.HashDB
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A yesod-auth AuthPlugin designed to look users up in Persist where
-- their user id's and a sha1 hash of their password will already be
-- stored.
--
-- > echo -n 'MyPassword' | sha1sum
--
-- can be used to get the hash which should be inserted in the database
-- before using this in your webapp.
--
-------------------------------------------------------------------------------
module Helpers.Auth.HashDB
    ( authHashDB
    , getAuthIdHashDB
    , UserId
    , migrateUsers
    ) where

import Yesod
import Yesod.Helpers.Auth

import Control.Applicative ((<$>),(<*>))
import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

import System.IO

-- | Computer the sha1 of a string and return it as a string
sha1String :: String -> String
sha1String = showDigest . sha1 . pack

-- | Generate data base instances for a valid user
share2 mkPersist (mkMigrate "migrateUsers") [$persist|
User
    idUser   String Eq
    password String
    UniqueUser idUser
Ident
    ident String Asc
    user UserId Eq
    UniqueIdent ident
|]

-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser :: (YesodPersist y, 
                 PersistBackend (YesodDB y (GHandler sub y))) 
             => (String, String) 
             -> GHandler sub y Bool
validateUser (user,password) = runDB (getBy $ UniqueUser user) >>= \dbUser ->
    case dbUser of
        -- user not found
        Nothing          -> return False
        -- validate password
        Just (_, sqlUser) -> return $ sha1String password == userPassword sqlUser

login :: AuthRoute
login = PluginR "hashdb" ["login"]

postLoginR :: (YesodAuth y,
               YesodPersist y, 
               PersistBackend (YesodDB y (GHandler Auth y)))
           => GHandler Auth y ()
postLoginR = do
    (user, password) <- runFormPost' $ (,)
        <$> stringInput "username"
        <*> stringInput "password"

    isValid <- validateUser (user,password)
    if isValid
        then setCreds True $ Creds "hashdb" user []
        else do
            setMessage $ string "Invalid username/password"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

-- | Use this as getAuthId in your instance declaration by passing your
--   AuthR data type
getAuthIdHashDB :: (Key User ~ AuthId master,
                    PersistBackend (YesodDB master (GHandler sub master)),
                    YesodPersist master,
                    YesodAuth master)
                => (AuthRoute -> Route master)
                -> Creds m
                -> GHandler sub master (Maybe UserId)
getAuthIdHashDB authR creds = do
    muid <- maybeAuth
    x <- runDB $ getBy $ UniqueIdent $ credsIdent creds
    case (x, muid) of
        -- Logged in and identified
        (Just _, Just (uid, _)) -> return $ Just uid

        -- Identified but not logged in
        (Just (_, i), Nothing) -> return $ Just $ identUser i

        -- Logged in but not identified
        (Nothing, Just (uid, _)) -> do
            setMessage $ [$hamlet| %em identifier added |]
            runDB $ insert $ Ident (credsIdent creds) uid
            return $ Just uid

        -- Not logged in and not identified
        (Nothing, Nothing) -> do
            -- is there already a user but with no identifier?
            y <- runDB $ getBy $ UniqueUser (credsIdent creds)
            case y of
                Just (uid, _) -> do
                    setMessage $ [$hamlet| %em identifier added |]
                    runDB $ insert $ Ident (credsIdent creds) uid
                    return $ Just uid
                Nothing -> do
                    setMessage $ [$hamlet| %em user not found |]
                    redirect RedirectTemporary $ authR LoginR

authHashDB :: (YesodAuth y,
               YesodPersist y, 
               PersistBackend (YesodDB y (GHandler Auth y)))
           => AuthPlugin y
authHashDB = AuthPlugin "hashdb" dispatch $ \tm ->
    [$hamlet|
    #header
        %h1 Login

    #login
        %form!method=post!action=@tm.login@
            %table
                %tr
                    %th Username:
                    %td
                        %input!name=username
                %tr
                    %th Password:
                    %td
                        %input!type=password!name=password
                %tr
                    %td &nbsp;
                    %td
                        %input!type=submit!value="Login"
    |]
    where
        dispatch "POST" ["login"] = postLoginR >>= sendResponse
        dispatch _ _              = notFound
