module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, hamletFile)
import Model
import Text.Jasmine (minifym)
import System.Log.FastLogger (Logger)

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.Gravatar
import Yesod.RssFeed
import Yesod.Links
import Yesod.Comments
import Yesod.Comments.Storage
import Yesod.Comments.Management
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv ()
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type DB x = YesodDB App x

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (60 * 60 * 24 * 10) -- 10 days
        "config/client_session_key.aes"

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

            $(combineStylesheets 'StaticR
                [ css_bootstrap_min_css
                , css_bootstrap_responsive_min_css
                ])

            $(combineScripts 'StaticR
                [ js_jquery_min_js
                , js_bootstrap_min_js
                ])

            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

        where
            getGravatar :: Entity User -> String
            getGravatar (Entity _ u) = let email = fromMaybe "" $ userEmail u
                                       in  gravatar gravatarOpts email

            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultConfig
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

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody
    -- FIXME
    --jsLoader _ = BottomOfHeadAsync $ loadJsYepnope $ Right $ StaticR js_modernizr_js

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    -- Authorization
    isAuthorized ManagePostsR  _ = authorizeAdmin
    isAuthorized NewPostR      _ = authorizeAdmin
    isAuthorized (EditPostR _) _ = authorizeAdmin
    isAuthorized (DelPostR  _) _ = authorizeAdmin
    isAuthorized UsersR        _ = authorizeAdmin

    isAuthorized _ _ = return Authorized

authorizeAdmin :: HandlerT App IO AuthResult
authorizeAdmin = do
    mu <- maybeAuth

    return $ case mu of
        Just (Entity _ u) ->
            if userAdmin u
                then Authorized
                else Unauthorized "Administrative rights required"

        _ -> AuthenticationRequired

instance YesodBreadcrumbs App where
    breadcrumb RootR        = return ("home"       , Nothing       )
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

    breadcrumb (CommentsAdminR CommentsR)            = return ("your comments" , Just RootR                     )
    breadcrumb (CommentsAdminR (EditCommentR _ _))   = return ("edit comment"  , Just $ CommentsAdminR CommentsR)
    breadcrumb (CommentsAdminR (DeleteCommentR _ _)) = return ("delete comment", Just $ CommentsAdminR CommentsR)

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("404", Just RootR)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

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
            updateFromAx :: [(Text, Text)] -> UserId -> DB ()
            updateFromAx keys uid = maybe (return ()) go =<< get uid

                where
                    go :: User -> DB ()
                    go u = do
                        case (userName u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserName =. parseNick val]
                            _                       -> return ()

                        case (userEmail u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserEmail =. val]
                            _                       -> return ()

                    -- we'll request only email and parse the first
                    -- portion as our username.
                    parseNick :: Maybe Text -> Maybe Text
                    parseNick = fmap (T.takeWhile (/= '@'))

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authOpenId Claimed
                        -- tested to work with at least google
                        [ ("openid.ax.mode"       , "fetch_request"                         )
                        , ("openid.ax.required"   , "email"                                 )
                        , ("openid.ax.type.email" , "http://schema.openid.net/contact/email")
                        , ("openid.ns.ax"         , "http://openid.net/srv/ax/1.0"          )
                        , ("openid.ns.ax.required", "email"                                 )
                        , ("openid.ui.icon"       , "true"                                  )
                        ] ]

    authHttpManager = httpManager

    loginHandler = lift $ defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler ()
getExtra = fmap (appExtra . settings) getYesod

instance YesodComments App where
    commentStorage = persistStorage

    userDetails uid = do
        mu <- runDB $ get uid
        return $ case mu of
            Just u -> do
                let name = toPathPiece uid

                Just $ case (userName u, userEmail u) of
                    (Just uname, Just email) -> UserDetails name uname email
                    (_,          Just email) -> UserDetails name "unknown" email
                    (Just uname, _         ) -> UserDetails name uname ""
                    _                        -> UserDetails name "unknown" ""

            _ -> Nothing

    threadRoute = \thread -> PostR thread

    editRoute   = Just $ \thread cid -> CommentsAdminR $ EditCommentR thread cid
    deleteRoute = Just $ \thread cid -> CommentsAdminR $ DeleteCommentR thread cid

maybe' :: Monad m => b -> (a -> Maybe b) -> Maybe a -> m b
maybe' c f = return . fromMaybe c . maybe Nothing f

instance YesodLinked App where
    type Linked = App

instance IsLink (Route App) where
    toLink r@(RootR)                    = Link (Internal r) "go home"                "home"
    toLink r@(ArchivesR)                = Link (Internal r) "archives of all posts"  "archives"
    toLink r@(FeedR)                    = Link (Internal r) "subscribe via rss"      "subscribe"
    toLink r@(ManagePostsR)             = Link (Internal r) "manage posts"           "manage posts"
    toLink r@(UsersR)                   = Link (Internal r) "known users"            "known users"
    toLink r@(AuthR LoginR)             = Link (Internal r) "login"                  "login"
    toLink r@(AuthR LogoutR)            = Link (Internal r) "logout"                 "logout"
    toLink r@ProfileR                   = Link (Internal r) "manage your profile"    "your profile"
    toLink r@(CommentsAdminR CommentsR) = Link (Internal r) "manage your comments"   "your comments"

    -- fail noticably
    toLink r = Link (Internal r) "invalid use of `link'" "FIXME"

instance IsLink Post where
    toLink p = Link (Internal $ PostR $ postSlug p) (postTitle p) (postTitle p)

instance IsLink Tag where
    toLink t = Link (Internal $ TagR $ tagName t) (tagName t) (tagName t)
