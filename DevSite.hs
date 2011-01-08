{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  DevSite
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- The actual site definition. What routes there are and instances it is
-- a part of.
--
-------------------------------------------------------------------------------
module DevSite where

import Yesod hiding (lift)
import Yesod.Form.Core
import Yesod.Helpers.Auth
import Data.Char (toLower)
import Language.Haskell.TH.Syntax
import Database.Persist.GenericSql
import qualified Settings as S

import HashDB

-- | The main site type
data DevSite = DevSite { connPool :: ConnectionPool }

type Handler     = GHandler DevSite DevSite
type Widget      = GWidget  DevSite DevSite
type FormMonad a = GFormMonad DevSite DevSite a

-- | Define all of the routes and handlers
mkYesodData "DevSite" [$parseRoutes|
/      RootR  GET
/stats StatsR GET
/about AboutR GET

/manage                ManagePostR GET POST
/manage/edit/#String   EditPostR   GET POST
/manage/delete/#String DelPostR    GET

/posts         PostsR GET
/posts/#String PostR  GET
/tags          TagsR  GET
/tags/#String  TagR   GET

/feed        FeedR    GET
/favicon.ico FaviconR GET
/robots.txt  RobotsR  GET

/auth AuthR Auth getAuth
|]

-- | Make my site an instance of Yesod so we can actually use it
instance Yesod DevSite where 
    approot _ = S.approot

    -- | handle authentication
    authRoute _ = Just $ AuthR LoginR

    -- | override defaultLayout to provide an overall template and css
    --   file
    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addCassius $(S.cassiusFile "root-css")
        hamletToRepHtml $(S.hamletFile "root-layout")

-- | Make my site an instance of breadcrumbs so that i can simply call
--   the breadcrumbs function to get automagical breadcrumb links
instance YesodBreadcrumbs DevSite where
    -- root is the parent node
    breadcrumb RootR  = return ("root" , Nothing) 

    -- about goes back home
    breadcrumb AboutR = return ("about", Just RootR)

    -- stats goes back home
    breadcrumb StatsR = return ("stats", Just RootR)

    -- all posts goes back home and individual posts go to all posts
    breadcrumb PostsR       = return ("all posts", Just RootR)
    breadcrumb (PostR slug) = return (format slug, Just PostsR)

        where
            -- switch underscores with spaces
            format []         = []
            format ('_':rest) = ' ': format rest
            format (x:rest)   = x  : format rest

    -- all tags goes back home and individual tags go to all tags
    breadcrumb TagsR      = return ("all tags", Just RootR)
    breadcrumb (TagR tag) = return (format tag, Just TagsR)

        where
            -- lowercase it
            format t = map toLower t ++ " tag"

    -- management pages
    breadcrumb ManagePostR      = return ("manage posts", Just RootR)
    breadcrumb (EditPostR slug) = return ("edit post", Just ManagePostR)

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("%%%", Just RootR)

-- | Make my site an instance of Persist so that i can store post
--   metatdata in a db
instance YesodPersist DevSite where
    type YesodDB DevSite = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

-- | Handle authentication with my custom HashDB plugin
instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    loginDest _  = ManagePostR
    logoutDest _ = RootR

    -- todo: move this code to HashDB itself?
    getAuthId creds = do
        muid <- maybeAuth
        -- is there already an identier?
        x <- runDB $ getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (_, i), Nothing)   -> return $ Just $ identUser i
            (Nothing, Nothing)       -> do
                -- is there already a user with no identifier?
                y <- runDB $ getBy $ UniqueUser (credsIdent creds)
                case y of
                    Just (uid, _) -> do
                        setMessage $ [$hamlet| %em identifier added |]
                        runDB $ insert $ Ident (credsIdent creds) uid
                        return $ Just uid
                    Nothing -> do
                        setMessage $ [$hamlet| %em unhandled case |]
                        redirect RedirectTemporary $ AuthR LoginR
            (Nothing, Just (uid, _)) -> do
                setMessage $ [$hamlet| %em identifier added |]
                runDB $ insert $ Ident (credsIdent creds) uid
                return $ Just uid
            otherwise                -> do
                setMessage $ [$hamlet| %em unhandled case |]
                redirect RedirectTemporary $ AuthR LoginR

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [authHashDB]

-- | This footer template needs to be in scope everywhere, so we'll
--   define it here
footerTemplate :: Hamlet DevSiteRoute
footerTemplate = [$hamlet|
                 %p
                     %a!href=@RootR@ pbrisbin
                     \ dot com 2010 
                     %span!style="float: right;"
                         powered by 
                         %a!href="http://docs.yesodweb.com/" yesod
                 |]
