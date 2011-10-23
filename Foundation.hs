{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Foundation
    ( DevSite(..)
    , DevSiteRoute(..)
    , resourcesDevSite
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Yesod.Goodies
    , module Settings
    , module Model
    , AuthRoute(..)
    ) where

import Model
import Yesod hiding (setTitle)
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Default.Config
import Yesod.Logger (Logger, logLazyText)
import Yesod.Goodies hiding (NotFound)
import Yesod.RssFeed (rssLink)
import Yesod.Comments hiding (userName, userEmail)
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Data.Maybe (fromMaybe)
import Database.Persist.GenericSql
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Persist.Base as Base

import Settings ( setTitle
                , addKeywords
                , staticLink
                , pandocFile
                , widgetFile
                )

import qualified Settings as Settings

data DevSite = DevSite
    { settings  :: AppConfig DefaultEnv
    , getLogger :: Logger
    , connPool  :: Base.PersistConfigPool Settings.PersistConfig
    , siteDocs  :: GHandler DevSite DevSite [Document]
    }

mkYesodData "DevSite" $(parseRoutesFile "config/routes")

instance Yesod DevSite where 
    approot      = appRoot . settings
    authRoute _  = Just $ AuthR LoginR
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        muid   <- maybeAuth
        mmesg  <- getMessage
        (t, h) <- breadcrumbs
        let mgrav = fmap getGravatar muid
        pc <- widgetToPageContent $ do
            rssLink FeedR "rss feed"
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

        where
            getGravatar :: (UserId, User) -> String
            getGravatar (_,u) = let email = fromMaybe "" $ userEmail u
                                in  gravatarImg email gravatarOpts

            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultOptions
                { gSize    = Just $ Size 12
                , gDefault = Just MM
                }

            -- external links used in sidebar
            github, aurPkgs, xmonadDocs, haskellDocs :: Link DevSite
            github      = Link (External "https://github.com/pbrisbin") "my projects on github" "github"
            aurPkgs     = Link (External "https://aur.archlinux.org/packages.php?K=brisbin33&SeB=m") "my aur packages" "aur packages"
            xmonadDocs  = Link (External "/xmonad/docs") "xmonad haddocks" "xmonad docs"
            haskellDocs = Link (External "/haskell/docs/html") "haskell haddocks" "haskell docs"

    messageLogger y loc level msg =
        formatLogMessage loc level msg >>= logLazyText (getLogger y)

instance YesodBreadcrumbs DevSite where
    breadcrumb RootR        = return ("home"       , Nothing       ) 
    breadcrumb AboutR       = return ("about"      , Just RootR    )
    breadcrumb ArchivesR    = return ("archives"   , Just RootR    )
    breadcrumb (PostR slug) = return (T.map go slug, Just ArchivesR)
        -- switch underscores with spaces
        where go :: Char -> Char
              go '_' = ' '
              go  x  =  x

    breadcrumb (TagR tag)    = return (T.toLower tag , Just ArchivesR   )
    breadcrumb ProfileR      = return ("profile"     , Just RootR       )
    breadcrumb EditProfileR  = return ("edit"        , Just ProfileR    )
    breadcrumb ManagePostsR  = return ("manage posts", Just RootR       )
    breadcrumb (EditPostR _) = return ("edit post"   , Just ManagePostsR)
    breadcrumb (AuthR _)     = return ("login"       , Just RootR       )

    breadcrumb (CommentsAdminR OverviewR)   = return ("your comments", Just RootR                     )
    breadcrumb (CommentsAdminR (ViewR _ _)) = return ("view comment" , Just $ CommentsAdminR OverviewR)
    breadcrumb (CommentsAdminR (EditR _ _)) = return ("edit comment" , Just $ CommentsAdminR OverviewR)

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("404", Just RootR)

instance YesodPersist DevSite where
    type YesodPersistBackend DevSite = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Base.runPool (undefined :: Settings.PersistConfig) f

instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    loginDest  _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (_, i) -> do
                updateFromSreg (credsExtra creds) $ identUser i
                return $ Just $ identUser i

            Nothing -> do
                uid <- insert $ User Nothing Nothing False
                _   <- insert $ Ident (credsIdent creds) uid
                updateFromSreg (credsExtra creds) uid 
                return $ Just uid

        where
            -- updates username/email with values returned by openid
            -- unless values exist there already
            updateFromSreg :: PersistBackend SqlPersist m
                           => [(Text,Text)] -- ^ the @credsExtra@ returned from open id
                           -> UserId        -- ^ the user id to update
                           -> SqlPersist m ()
            updateFromSreg keys uid = maybe (return ()) go =<< get uid

                where
                    go :: PersistBackend SqlPersist m => User -> SqlPersist m ()
                    go u = do
                        case (userName u, lookup "openid.sreg.nickname" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserName =. val]
                            _                       -> return ()

                        case (userEmail u, lookup "openid.sreg.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserEmail =. val]
                            _                       -> return ()

    authPlugins = [ authOpenIdExtended 
                        [("openid.sreg.optional","nickname,email")] ]

    loginHandler = defaultLayout $ do
        setTitle "Login"
        addWidget $(widgetFile "login")

instance RenderMessage DevSite FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodComments DevSite where
    getComment       = getCommentPersist
    storeComment     = storeCommentPersist
    updateComment    = updateCommentPersist
    deleteComment    = deleteCommentPersist
    loadComments     = loadCommentsPersist
    displayUser  uid = maybe' "anonymous" userName  =<< runDB (get uid)
    displayEmail uid = maybe' ""          userEmail =<< runDB (get uid)

maybe' :: Monad m => b -> (a -> Maybe b) -> Maybe a -> m b
maybe' c f = return . fromMaybe c . maybe Nothing f

instance YesodLinked DevSite where
    type Linked = DevSite

instance IsLink DevSiteRoute where
    toLink r@(RootR)                    = Link (Internal r) "go home"                "home"
    toLink r@(AboutR)                   = Link (Internal r) "about pbrisbin dot com" "about"
    toLink r@(ArchivesR)                = Link (Internal r) "archives of all posts"  "archives"
    toLink r@(FeedR)                    = Link (Internal r) "subscribe via rss"      "subscribe"
    toLink r@(ManagePostsR)             = Link (Internal r) "manage posts"           "manage posts"
    toLink r@(AuthR LoginR)             = Link (Internal r) "login"                  "login"
    toLink r@(AuthR LogoutR)            = Link (Internal r) "logout"                 "logout"
    toLink r@ProfileR                   = Link (Internal r) "manage your profile"    "your profile"
    toLink r@(CommentsAdminR OverviewR) = Link (Internal r) "manage your comments"   "your comments"

    -- fail noticably
    toLink r = Link (Internal r) "invalid use of `link'" "FIXME"
    
instance IsLink Post where
    toLink p = Link (Internal $ PostR $ postSlug p) (postTitle p) (postTitle p)

instance IsLink Tag where
    toLink t = Link (Internal $ TagR $ tagName t) (tagName t) (tagName t)

instance IsLink Document where
    toLink = toLink . post
