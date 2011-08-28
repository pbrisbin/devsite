{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE TypeFamilies          #-}
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
    , module Settings
    , module Model
    , AuthRoute(..)
    ) where

import Model
import Yesod hiding (setTitle)
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Logger (Logger, logLazyText)
import Yesod.Goodies.Links
import Yesod.RssFeed
import Yesod.Comments hiding (userName, userEmail)
import Yesod.Comments.Management
import Yesod.Comments.Storage
import Data.Maybe (fromMaybe)
import Database.Persist.GenericSql
import Web.ClientSession (getKey)
import qualified Data.Text as T

import Settings ( setTitle
                , addKeywords
                , staticLink
                , hamletFile
                , cassiusFile
                , luciusFile
                , juliusFile
                , widgetFile
                , pandocFile
                )

import qualified Settings

data DevSite = DevSite
    { settings  :: Settings.AppConfig
    , getLogger :: Logger
    , connPool  :: ConnectionPool
    , siteDocs  :: GHandler DevSite DevSite [Document]
    }

mkYesodData "DevSite" $(parseRoutesFile "config/routes")

instance Yesod DevSite where 
    approot      = Settings.appRoot . settings
    authRoute _  = Just $ AuthR LoginR
    encryptKey _ = fmap Just $ getKey "client_session_key.aes"

    defaultLayout widget = do
        muid   <- maybeAuth
        mmesg  <- getMessage
        (t, h) <- breadcrumbs
        pc <- widgetToPageContent $ do
            rssLink FeedR "rss feed"
            addWidget $(widgetFile "sidebar")
            widget
        hamletToRepHtml $(hamletFile "default-layout")

        where
            -- external links used in sidebar
            github, aurPkgs, xmonadDocs, haskellDocs :: Link DevSite
            github      = Link (External "https://github.com/pbrisbin") "my projects on github" "github"
            aurPkgs     = Link (External "https://aur.archlinux.org/packages.php?K=brisbin33&SeB=m") "my aur packages" "aur packages"
            xmonadDocs  = Link (External "/xmonad/docs") "xmonad haddocks" "xmonad docs"
            haskellDocs = Link (External "/haskell/docs/html") "haskell haddocks" "haskell docs"

    messageLogger y loc level msg =
        formatLogMessage loc level msg >>= logLazyText (getLogger y)

instance YesodBreadcrumbs DevSite where
    breadcrumb RootR        = return ("home"       , Nothing    ) 
    breadcrumb AboutR       = return ("about"      , Just RootR )
    breadcrumb PostsR       = return ("all posts"  , Just RootR )
    breadcrumb (PostR slug) = return (T.map go slug, Just PostsR)
        -- switch underscores with spaces
        where go :: Char -> Char
              go '_' = ' '
              go  x  =  x

    breadcrumb TagsR         = return ("all tags"    , Just RootR       )
    breadcrumb (TagR tag)    = return (T.toLower tag , Just TagsR       )
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
            $ fmap connPool getYesod >>= Settings.runConnectionPool f

instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    loginDest  _ = ProfileR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (_, i) -> return $ Just $ identUser i
            Nothing       -> do
                uid <- insert $ User
                    { userName  = Nothing
                    , userEmail = Nothing
                    , userAdmin = False
                    }
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

        {-muid <- maybeAuth-}
        {-x    <- runDB $ getBy $ UniqueIdent $ credsIdent creds-}
        {-case (x, muid) of-}
            {-(Just (_, i), Nothing      ) -> return $ Just $ identUser i-}
            {-(Nothing    , Just (uid, _)) -> do-}
                {-_ <- runDB $ insert $ Ident (credsIdent creds) uid-}
                {-return $ Just uid-}

            {-(Nothing, Nothing) -> runDB $ do-}
                {-uid <- insert $ User-}
                    {-{ userName  = Nothing-}
                    {-, userEmail = Nothing-}
                    {-, userAdmin = False-}
                    {-}-}
                {-_ <- insert $ Ident (credsIdent creds) uid-}
                {-return $ Just uid-}

            {-(Just _, Just _) -> do -- this shouldn't happen-}
                {-setMessage "That identifier is already attached to an account."-}
                {-redirect RedirectTemporary ProfileR-}

    authPlugins = [ authOpenId ]

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

-- | Make isLink instances for each route in the site
instance IsLink DevSiteRoute where
    toLink r@(RootR)                    = Link (Internal r) "go home"                  "home"
    toLink r@(AboutR)                   = Link (Internal r) "about pbrisbin dot com"   "about"
    toLink r@(PostsR)                   = Link (Internal r) "all posts"                "all posts"
    toLink r@(TagsR)                    = Link (Internal r) "all posts grouped by tag" "all tags"
    toLink r@(FeedR)                    = Link (Internal r) "subscribe via rss"        "subscribe"
    toLink r@(ManagePostsR)             = Link (Internal r) "manage posts"             "manage posts"
    toLink r@(AuthR LoginR)             = Link (Internal r) "login"                    "login"
    toLink r@(AuthR LogoutR)            = Link (Internal r) "logout"                   "logout"
    toLink r@(CommentsAdminR OverviewR) = Link (Internal r) "manage your comments"     "comments"

    -- fail noticably
    toLink r = Link (Internal r) "invalid use of `link'" "FIXME"
    
-- | Link directly to a post
instance IsLink Post where
    toLink p = Link (Internal $ PostR $ postSlug p) (postTitle p) (postTitle p)

-- | Link directly to a tag
instance IsLink Tag where
    toLink t = Link (Internal $ TagR $ tagName t) (tagName t) (tagName t)

-- | Link directly to a document (goes to its post)
instance IsLink Document where
    toLink = toLink . post

-- | This is dangerous but useful, it assumes a link to raw text is
--   meant as a tag. There is no guarantee the tag exists
instance IsLink T.Text where
    toLink t = Link (Internal $ TagR $ T.toLower t) t t
