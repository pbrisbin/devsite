{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation
    ( DevSite (..)
    , Route (..)
    , DevSiteMessage (..)
    , resourcesDevSite
    , Handler
    , Widget
    , Form
    , DB
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod hiding (setTitle)
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, setTitle, addKeywords, pandocFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.Gravatar
import Yesod.RssFeed
import Yesod.Links
import Yesod.Comments hiding (Form, userName, userEmail)
import Yesod.Comments.Storage
import Yesod.Comments.Management
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data DevSite = DevSite
    { settings :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "DevSite" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype DevSiteRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route DevSite = DevSiteRoute
-- * Creates the value resourcesDevSite which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- DevSite. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the DevSiteRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "DevSite" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm DevSite DevSite (FormResult x, Widget)

type DB x = YesodDB DevSite DevSite x

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod DevSite where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        muid   <- maybeAuth
        (t, h) <- breadcrumbs
        let mgrav = fmap getGravatar muid

        pc <- widgetToPageContent $ do
            rssLink FeedR "rss feed"
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

        where
            getGravatar :: Entity User -> String
            getGravatar (Entity _ u) = let email = fromMaybe "" $ userEmail u
                                       in  gravatar gravatarOpts email

            gravatarOpts :: GravatarOptions
            gravatarOpts = def
                { gSize    = Just $ Size 20
                , gDefault = Just MM
                }

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfHeadAsync $ loadJsYepnope $ Right $ StaticR js_modernizr_js

    -- Authorization
    isAuthorized ManagePostsR  _ = authorizeAdmin
    isAuthorized NewPostR      _ = authorizeAdmin
    isAuthorized (EditPostR _) _ = authorizeAdmin
    isAuthorized (DelPostR  _) _ = authorizeAdmin
    isAuthorized UsersR        _ = authorizeAdmin

    isAuthorized _ _ = return Authorized

authorizeAdmin :: GHandler s DevSite AuthResult
authorizeAdmin = do
    mu <- maybeAuth

    return $ case mu of
        Just (Entity _ u) ->
            if userAdmin u
                then Authorized
                else Unauthorized "Administrative rights required"

        _ -> AuthenticationRequired

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
    breadcrumb NewPostR      = return ("new post"    , Just ManagePostsR)
    breadcrumb (EditPostR _) = return ("edit post"   , Just ManagePostsR)
    breadcrumb UsersR        = return ("users"       , Just RootR       )
    breadcrumb (AuthR _)     = return ("login"       , Just RootR       )

    breadcrumb (CommentsAdminR OverviewR)   = return ("your comments", Just RootR                     )
    breadcrumb (CommentsAdminR (ViewR _ _)) = return ("view comment" , Just $ CommentsAdminR OverviewR)
    breadcrumb (CommentsAdminR (EditR _ _)) = return ("edit comment" , Just $ CommentsAdminR OverviewR)

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("404", Just RootR)

-- How to run database actions.
instance YesodPersist DevSite where
    type YesodPersistBackend DevSite = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (Entity _ i) -> do
                updateFromAx (credsExtra creds) $ identUser i
                return $ Just $ identUser i

            Nothing -> do
                uid <- insert $ User Nothing Nothing False
                _   <- insert $ Ident (credsIdent creds) uid
                updateFromAx (credsExtra creds) uid
                return $ Just uid

        where
            -- updates username/email with values returned by openid
            -- unless values exist there already
            updateFromAx :: [(Text, Text)] -> UserId -> YesodDB s DevSite ()
            updateFromAx keys uid = maybe (return ()) go =<< get uid

                where
                    go :: User -> YesodDB s DevSite ()
                    go u = do
                        case (userName u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserName =. (parseNick val)]
                            _                       -> return ()

                        case (userEmail u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserEmail =. val]
                            _                       -> return ()

                    -- we'll request only email and parse the first
                    -- portion as our username.
                    parseNick :: Maybe Text -> Maybe Text
                    parseNick = fmap (T.takeWhile (/= '@'))

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authOpenIdExtended
                        -- tested to work with at least google
                        [ ("openid.ax.mode"       , "fetch_request"                         )
                        , ("openid.ax.required"   , "email"                                 )
                        , ("openid.ax.type.email" , "http://schema.openid.net/contact/email")
                        , ("openid.ns.ax"         , "http://openid.net/srv/ax/1.0"          )
                        , ("openid.ns.ax.required", "email"                                 )
                        , ("openid.ui.icon"       , "true"                                  )
                        ] ]

    authHttpManager = httpManager

    loginHandler = defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
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

instance IsLink (Route DevSite) where
    toLink r@(RootR)                    = Link (Internal r) "go home"                "home"
    toLink r@(AboutR)                   = Link (Internal r) "about pbrisbin dot com" "about"
    toLink r@(ArchivesR)                = Link (Internal r) "archives of all posts"  "archives"
    toLink r@(FeedR)                    = Link (Internal r) "subscribe via rss"      "subscribe"
    toLink r@(ManagePostsR)             = Link (Internal r) "manage posts"           "manage posts"
    toLink r@(UsersR)                   = Link (Internal r) "known users"            "known users"
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
