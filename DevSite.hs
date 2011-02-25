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
-------------------------------------------------------------------------------
module DevSite where

import Yesod
import Yesod.Markdown
import Yesod.Helpers.Auth
import Yesod.Helpers.MPC
import Yesod.Form.Core (GFormMonad(..))

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Database.Persist.GenericSql

import Helpers.AlbumArt
import Helpers.PostTypes
import Helpers.RssFeed
import Helpers.Auth.HashDB

import qualified Settings

-- | The main site type
data DevSite = DevSite
    { connPool  :: ConnectionPool
    , sitePosts :: Handler [Post]
    }

type Handler     = GHandler DevSite DevSite
type Widget      = GWidget  DevSite DevSite
type FormMonad a = GFormMonad DevSite DevSite a

-- | Define all of the routes and handlers
mkYesodData "DevSite" [$parseRoutes|
    /      RootR  GET
    /about AboutR GET

    /manage                ManagePostsR GET POST
    /manage/edit/#String   EditPostR    GET POST
    /manage/delete/#String DelPostR     GET

    /posts         PostsR GET
    /posts/#String PostR  GET
    /tags          TagsR  GET
    /tags/#String  TagR   GET

    /feed         FeedR    GET
    /feed/#String FeedTagR GET

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
        hamletToRepHtml [$hamlet|
            !!!
            %html!lang="en"
                %head
                    %meta!charset="utf-8"
                    %title $pageTitle.pc$
                    %meta!name="description"!content="pbrisbin dot com"
                    %meta!name="author"!content="Patrick Brisbin"
                    ^pageHead.pc^
                    %link!rel="stylesheet"!href=$cssLink$
                %body
                    %aside
                        ^pageBody.sb^

                    ^pageBody.pc^

                    %footer
                        %p
                            %small
                                %a!href=@RootR@ pbrisbin
                                \ dot com 2010 
                                %span.float_right
                                    powered by 
                                    %a!href="http://docs.yesodweb.com/" yesod
            |]

instance YesodBreadcrumbs DevSite where
    breadcrumb RootR  = return ("home" , Nothing   ) 
    breadcrumb AboutR = return ("about", Just RootR)

    breadcrumb PostsR       = return ("all posts", Just RootR )
    breadcrumb (PostR slug) = return (format slug, Just PostsR)
        where
            -- switch underscores with spaces
            format []         = []
            format ('_':rest) = ' ': format rest
            format (x:rest)   = x  : format rest

    breadcrumb TagsR      = return ("all tags", Just RootR     )
    breadcrumb (TagR tag) = return (map toLower tag, Just TagsR)

    breadcrumb ManagePostsR     = return ("manage posts", Just RootR    )
    breadcrumb (EditPostR slug) = return ("edit post", Just ManagePostsR)

    -- subsites
    breadcrumb (AuthR _) = return ("login", Just RootR)
    breadcrumb (MpcR  _) = return ("mpc"  , Just RootR)

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("404", Just RootR)

-- | Make my site an instance of Persist so that i can store post
--   metatdata in a db
instance YesodPersist DevSite where
    type YesodDB DevSite = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

-- | Handle authentication with my custom HashDB plugin
instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    loginDest  _ = RootR
    logoutDest _ = RootR
    getAuthId    = getAuthIdHashDB AuthR 
    showAuthId _ = showIntegral
    readAuthId _ = readIntegral
    authPlugins  = [authHashDB]

-- | In-browser mpd controls
instance YesodMPC DevSite where
    mpdConfig      = return . Just $ MpdConfig "192.168.0.5" 6600 ""
    authHelper     = return . const () =<< requireAuth
    albumArtHelper = getAlbumUrl

-- | Add a list of words to the html head as keywords
addKeywords :: [String] -> Widget ()
addKeywords keywords = addHamletHead [$hamlet| 
    %meta!name="keywords"!content=$format.keywords$
    |]
    where 
        -- add some default keywords, then make the comma separated 
        -- string
        format :: [String] -> Html
        format = string 
               . intercalate ", " 
               . (:) "patrick brisbin" 
               . (:) "pbrisbin"
               . (:) "brisbin" 

-- | Add navigation
sideBar :: GWidget s DevSite ()
sideBar = do
    mmesg    <- liftHandler getMessage
    (t, h)   <- liftHandler breadcrumbs
    loggedin <- liftHandler maybeAuthId >>= return . isJust
    let feedIcon = Settings.staticRoot ++ "/images/feed.png"
    [$hamlet|
        $maybe mmesg mesg
            .message
                %p $mesg$

        .breadcrumbs
            %p
                $forall h node
                    %a!href=@fst.node@ $snd.node$ 
                    \ / 
                \ $t$
        %nav
            %ul
                %li
                    %a!href=@RootR@  home
                %li
                    %a!href=@AboutR@ about
                %li
                    %a!href=@PostsR@ posts
                %li
                    %a!href=@TagsR@  tags
                %li
                    %a!href="https://github.com/pbrisbin" github
                %li
                    %a!href="http://aur.archlinux.org/packages.php?K=brisbin33&amp;SeB=m" aur packages
                %li
                    %a!href="/xmonad/docs" xmonad docs
                %li
                    %a!href="/haskell/docs/html" haskell docs
                %li
                    %img.icon!src=$feedIcon$
                    \ 
                    %a!href=@FeedR@ subscribe

                $if loggedin
                    %li
                        %a!href=@ManagePostsR@  manage posts
                    %li
                        %a!href=@MpcR.StatusR@  mpd
                    %li
                        %a!href=@AuthR.LogoutR@ logout
                $else
                    %li.fade_in
                        %a!href=@AuthR.LoginR@  login
        |]

-- | Render from markdown, yesod-style
markdownToHtml :: Markdown -> GHandler s DevSite Html
markdownToHtml = (writePandoc yesodDefaultWriterOptions <$>) 
               . localLinks 
               . parseMarkdown yesodDefaultParserStateTrusted
