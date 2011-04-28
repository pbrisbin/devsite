{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module DevSite where

import Yesod
import Yesod.Form.Core (GFormMonad)
import Yesod.Comments.Markdown
import Yesod.Helpers.MPC
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.HashDB
import Yesod.Helpers.RssFeed

import Data.Time
import System.Locale

import Control.Monad (forM)
import Data.Char     (isSpace)
import Data.Maybe    (isJust)

import Database.Persist.GenericSql

import Model
import Helpers.AlbumArt

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
                    <aside>
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
--   metatdata in a db
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
    mpdConfig      = return . Just $ MpdConfig "192.168.0.5" 6600 ""
    authHelper     = requireAuth >>= \_ -> return ()
    albumArtHelper = getAlbumUrl

-- | Add a list of words to the html head as keywords
addKeywords :: [T.Text] -> Widget ()
addKeywords keywords = addHamletHead [hamlet|
    <meta name="keywords" content="#{format keywords}">
    |]
    where 
        -- add some default keywords, then make the comma separated 
        -- string
        format :: [T.Text] -> T.Text
        format = T.append "patrick brisbin, pbrisbin, brisbin, " . T.intercalate ", "

-- | Add navigation
sideBar :: GWidget s DevSite ()
sideBar = do
    mmesg    <- lift getMessage
    (t, h)   <- lift breadcrumbs
    loggedin <- lift maybeAuthId >>= return . isJust

    let feedIcon = Settings.staticRoot ++ "/images/feed.png"

    [hamlet|
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

loadDocuments :: Handler [Document]
loadDocuments = do
    ps <- runDB $ selectList [] [PostDateDesc] 0 0
    ts <- fmap (map snd) (runDB $ selectList [] [TagNameAsc] 0 0)
    forM ps $ \(k,v) -> return $ Document v $ filter ((== k) . tagPost) ts

-- | Shorten a variety of string-like types adding ellipsis
class Shorten a where shorten :: Int -> a -> a

instance Shorten String where
    shorten n s = if length s > n then take (n - 3) s ++ "..." else s

instance Shorten T.Text where
    shorten n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t

instance Shorten Markdown where
    shorten n (Markdown s) = Markdown $ shorten n s

-- | A generalized internal link
data Link m = Link
    { linkRoute :: Route m
    , linkTitle :: T.Text
    , linkText  :: T.Text
    }

-- | How to display a general link
showLink :: Link m -> GWidget s m ()
showLink (Link r t x) = [hamlet|<a title="#{t}" href="@{r}">#{x}|]

-- | Items that can be linked to within this app
class Linkable a where
    link :: a -> Link DevSite

instance Linkable Post where
    link p = Link (PostR $ postSlug p) (postTitle p) (postTitle p)

instance Linkable Tag where
    link t = Link (TagR $ tagName t) (tagName t) (tagName t)

instance Linkable Document where
    link = link . post

-- | A convenience helper for links specific to this app
linkTo :: Linkable a => a -> GWidget s DevSite ()
linkTo = showLink . link

-- | Based on 
--   <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs> 
--   <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTimeDiff :: UTCTime -> GHandler s m String
humanReadableTimeDiff t = return . helper . flip diffUTCTime t =<< liftIO getCurrentTime

    where
        minutes :: NominalDiffTime -> Double
        minutes n = realToFrac $ n / 60

        hours :: NominalDiffTime -> Double
        hours   n = minutes n / 60

        days :: NominalDiffTime -> Double
        days    n = hours n / 24

        weeks :: NominalDiffTime -> Double
        weeks   n = days n / 7

        years :: NominalDiffTime -> Double
        years   n = days n / 365

        i2s :: RealFrac a => a -> String
        i2s n = show m where m = truncate n :: Int

        trim = f . f where f = reverse . dropWhile isSpace

        old           = utcToLocalTime utc t
        dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
        thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
        previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

        helper d 
            | d         < 1  = "just now"
            | d         < 60 = i2s d ++ " seconds ago"
            | minutes d < 2  = "one minute ago"
            | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
            | hours d   < 2  = "one hour ago"
            | hours d   < 24 = "about " ++ i2s (hours d) ++ " hours ago"
            | days d    < 5  = "at " ++ dow
            | days d    < 10 = i2s (days d)  ++ " days ago"
            | weeks d   < 2  = i2s (weeks d) ++ " week ago"
            | weeks d   < 5  = i2s (weeks d) ++ " weeks ago"
            | years d   < 1  = "on " ++ thisYear
            | otherwise      = "on " ++ previousYears
