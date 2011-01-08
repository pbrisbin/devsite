{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HashDB where

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

printErr :: String -> GHandler sub y ()
printErr = liftIO . hPutStrLn stderr

-- | Give a (user,password) in plaintext, validate them against the
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
        then setCreds True $ Creds "hashdb" user [] -- wtf?
        else do
            setMessage $ string "Invalid username/password"
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

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
