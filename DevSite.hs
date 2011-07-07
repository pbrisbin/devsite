{-# OPTIONS -fno-warn-orphans  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module DevSite where

import Yesod
import Model
import Yesod.Form.Core (GFormMonad)
import Yesod.Goodies.Links
import Yesod.Helpers.RssFeed
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Facebook
import Yesod.Comments hiding (userName, userEmail)
import Yesod.Comments.Storage
import Data.Maybe (fromMaybe)
import Database.Persist.GenericSql
import qualified Settings
import qualified Data.Text as T

-- | The main site type
data DevSite = DevSite
    { connPool :: ConnectionPool
    , siteDocs :: Handler [Document]
    }

type Handler     = GHandler DevSite DevSite
type Widget      = GWidget  DevSite DevSite
type FormMonad a = GFormMonad DevSite DevSite a

-- | Define all of the routes and handlers
mkYesodData "DevSite" [parseRoutes|
    /      RootR  GET
    /about AboutR GET

    /manage                ManagePostsR GET POST
    /manage/edit/#T.Text   EditPostR    GET POST
    /manage/delete/#T.Text DelPostR     GET

    /posts         PostsR GET
    /posts/#T.Text PostR  GET
    /tags          TagsR  GET
    /tags/#T.Text  TagR   GET

    /feed         FeedR    GET
    /feed/#T.Text FeedTagR GET

    /auth        AuthR Auth getAuth
    /favicon.ico FaviconR GET
    /robots.txt  RobotsR  GET
    |]

instance Yesod DevSite where 
    approot _   = Settings.approot
    authRoute _ = Just $ AuthR LoginR

    defaultLayout widget = do
        let cssLink = Settings.staticRoot ++ "/css/style.css"

        sb <- widgetToPageContent sideBar
        pc <- widgetToPageContent $ do
            rssLink FeedR "rss feed"
            widget
        hamletToRepHtml [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="pbrisbin dot com">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                    <link rel="stylesheet" href="#{cssLink}">
                <body>
                    ^{pageBody sb}
                    ^{pageBody pc}

                    <footer>
                        <p>
                            <small>
                                <a href="@{RootR}">pbrisbin
                                \ dot com 2010 
                                <span .float_right>
                                    powered by 
                                    <a href="http://docs.yesodweb.com/">yesod
                                    \ - #{yesodVersion}

                    <script>
                        var _gaq = _gaq || [];
                        _gaq.push(['_setAccount', 'UA-22304237-1']);
                        _gaq.push(['_trackPageview']);

                        (function() {
                            var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                            ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
                        })();
            |]

        where
            sideBar :: GWidget s DevSite ()
            sideBar = do
                mmesg    <- lift getMessage
                (t, h)   <- lift breadcrumbs
                --loggedin <- lift $ fmap isJust maybeAuthId

                let feedIcon = Settings.staticRoot ++ "/images/feed.png"

                [hamlet|
                    <aside>
                        $maybe mesg <- mmesg
                            <div .message>
                                <p>#{mesg}

                        <div .breadcrumbs>
                            <p>
                                $forall node <- h
                                    <a href="@{fst node}">#{snd node} 
                                    \ / 
                                \ #{t}
                        <nav>
                            <ul>
                                <li>^{link RootR}
                                <li>^{link AboutR}
                                <li>^{link PostsR}
                                <li>^{link TagsR}
                                <li .extra>^{link' github}
                                <li .extra>^{link' aurPkgs}
                                <li .extra>^{link' xmonadDocs}
                                <li .extra>^{link' haskellDocs}
                                <li .extra>
                                    <img src="#{feedIcon}" .icon>
                                    \ ^{link FeedR}
                    |]

                    where
                        github      = Link (External "https://github.com/pbrisbin") "my projects on github" "github"
                        aurPkgs     = Link (External "http://aur.archlinux.org/packages.php?K=brisbin33&amp;SeB=m") "my aur packages" "aur packages"
                        xmonadDocs  = Link (External "/xmonad/docs") "xmonad haddocks" "xmonad docs"
                        haskellDocs = Link (External "/haskell/docs/html") "haskell haddocks" "haskell docs"

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
    breadcrumb ManagePostsR  = return ("manage posts", Just RootR       )
    breadcrumb (EditPostR _) = return ("edit post"   , Just ManagePostsR)
    breadcrumb (AuthR _)     = return ("login"       , Just RootR       )

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("404", Just RootR)

-- | Make my site an instance of Persist so that i can store post
--   metadata in a db
instance YesodPersist DevSite where
    type YesodDB DevSite = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= runSqlPool db

-- | Handle authentication with my custom HashDB plugin
instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    loginDest  _ = RootR
    logoutDest _ = RootR

    getAuthId creds = do
        muid <- maybeAuth
        x    <- runDB $ getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (_, i), Nothing      ) -> return $ Just $ identUser i
            (Nothing    , Just (uid, _)) -> do
                _ <- runDB $ insert $ Ident (credsIdent creds) uid
                return $ Just uid

            (Nothing, Nothing) -> runDB $ do
                uid <- insert $ User
                    { userName  = Nothing
                    , userEmail = Nothing
                    , userAdmin = False
                    }
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

            (Just _, Just _) -> do -- this shouldn't happen
                setMessage "That identifier is already attached to an account."
                redirect RedirectTemporary RootR

    authPlugins = [ authOpenId
                  , authFacebook "" "" [] -- TODO
                  ]

    loginHandler = defaultLayout [hamlet|
        <h1>Log in
        <div .login>
            <p>TODO
        |]

instance YesodComments DevSite where
    getComment       = getCommentPersist
    storeComment     = storeCommentPersist
    deleteComment    = deleteCommentPersist
    loadComments     = loadCommentsPersist
    displayUser  uid = return . maybe' "anonymous" userName  =<< runDB (get uid)
    displayEmail uid = return . maybe' ""          userEmail =<< runDB (get uid)

maybe' :: b -> (a -> Maybe b) -> Maybe a -> b
maybe' c f = fromMaybe c . maybe Nothing f

-- | Make isLink instances for each route in the site
instance IsLink DevSiteRoute where
    toLink r@(RootR)         = Link (Internal r) "go home" "home"
    toLink r@(AboutR)        = Link (Internal r) "about pbrisbin dot com" "about"
    toLink r@(PostsR)        = Link (Internal r) "all posts" "all posts"
    toLink r@(TagsR)         = Link (Internal r) "all posts grouped by tag" "all tags"
    toLink r@(FeedR)         = Link (Internal r) "subscribe via rss" "subscribe"
    toLink r@(ManagePostsR)  = Link (Internal r) "manage posts" "manage posts"
    toLink r@(AuthR LoginR)  = Link (Internal r) "login" "login"
    toLink r@(AuthR LogoutR) = Link (Internal r) "logout" "logout"

    -- fail noticably
    toLink r = Link (Internal r) "invalid use of `link'" "FIXME"
    
instance YesodLinked DevSite where
    type Linked = DevSite

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
