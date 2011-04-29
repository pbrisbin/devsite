{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module DevSite where

import Yesod
import Model
import Helpers.AlbumArt
import Yesod.Form.Core (GFormMonad)
import Yesod.Helpers.MPC
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.HashDB
import Yesod.Helpers.RssFeed
import Data.Maybe (isJust)
import Database.Persist.GenericSql
import qualified Settings
import qualified Data.Text as T
import qualified Network.MPD as MPD

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

    /favicon.ico FaviconR GET
    /robots.txt  RobotsR  GET

    /auth     AuthR Auth getAuth
    /apps/mpc MpcR  MPC  getMPC
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
                loggedin <- lift $ fmap isJust maybeAuthId

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
                                <li>
                                    <a href="@{RootR}">home
                                <li>
                                    <a href="@{AboutR}">about
                                <li>
                                    <a href="@{PostsR}">posts
                                <li>
                                    <a href="@{TagsR}">tags
                                <li .extra>
                                    <a href="https://github.com/pbrisbin">github
                                <li .extra>
                                    <a href="http://aur.archlinux.org/packages.php?K=brisbin33&amp;SeB=m">aur packages
                                <li .extra>
                                    <a href="/xmonad/docs">xmonad docs
                                <li .extra>
                                    <a href="/haskell/docs/html">haskell docs
                                <li .extra>
                                    <img src="#{feedIcon}" .icon>
                                    \ 
                                    <a href="@{FeedR}">subscribe

                                $if loggedin
                                    <li .extra>
                                        <a href="@{ManagePostsR}">manage posts
                                    <li>
                                        <a href="@{MpcR StatusR}">mpd
                                    <li>
                                        <a href="@{AuthR LogoutR}">logout
                                $else
                                    <li>
                                        <a href="@{AuthR LoginR}">login
                    |]

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
    breadcrumb (MpcR  _)     = return ("mpc"         , Just RootR       )

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
    getAuthId    = getAuthIdHashDB AuthR 
    authPlugins  = [authHashDB]

-- | In-browser mpd controls
instance YesodMPC DevSite where
    withMPD        = liftIO . MPD.withMPDEx "192.168.0.5" 6600 ""
    authHelper     = fmap (const ()) requireAuth
    albumArtHelper = getAlbumUrl
