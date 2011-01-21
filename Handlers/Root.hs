{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Root
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Root (getRootR) where

import Yesod
import DevSite

import Helpers.Pkgs
import Helpers.Posts

import Data.Time.Clock (getCurrentTime)

import qualified Settings

-- Since posts are now retrieved in the Handler Monad it's no longer
-- easy to create these functions, a solution is still a big todo:
site_migration = "site_migration"

-- tags
arch    = "Arch"
bash    = "Bash"
haskell = "Haskell"
linux   = "Linux"
mutt    = "Mutt"
xmonad  = "XMonad"

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    curTime <- liftIO getCurrentTime
    posts   <- selectPosts 10
    defaultLayout $ do
        setTitle $ string "pbrisbin - Home"
        addHamlet [$hamlet|
        #header
            %p
                %a!href=@AboutR@ about
                \ | 
                %a!href=@StatsR@ stats
                \ | 
                %a!href="/xmonad/docs/" xmonad modules
                \ | 
                %a!href="https://github.com/pbrisbin/dotfiles" dotfiles
                \ | 
                %a!href="https://github.com/pbrisbin/scripts" scripts

                %span.float_right
                    %a!href="#Recent_Posts" recent posts
                    \ | 
                    %a!href=@PostsR@ all posts

        %h1 pbrisbin dot com

        %p

            Welcome to pbrisbin dot com. You'll find it's mostly [GNU/]
      
            %a!href=@TagR.linux@ Linux
      
            -related geekery here and some of the information presented 
            is specific to the [amazing] distribution known as 
      
            %a!href=@TagR.arch@ Arch
      
            \ Linux. Some of my favorite topics are the 
      
            %a!href=@TagR.xmonad@ XMonad
      
            \ window manager, 
      
            %a!href=@TagR.haskell@ haskell
      
            \ in general, 
      
            %a!href=@TagR.bash@ Bash
      
            \ scripting (or just general command-line adventures), and 
            the great email client 
      
            %a!href=@TagR.mutt@ Mutt
      
            \.

        %p Enjoy the site.

        %h3 Current Desktop

        %p

            %a!href="http://xmonad.org/" XMonad 
          
            \ is a type of tiling window manager. It makes me happy. If 
            you've never used a tiling window manager, the idea is 
            simple: automatically resize any open windows to fill the 
            whole monitor with no gaps and no overlap. Usually you get a 
            bunch of virtual workspaces to spread out your windows by 
            category too. And there's always a floating layer available 
            if you want to pop any windows out of the tile and 
            move/resize them freely (or have them start that way by 
            default). It's a very efficient way to work with your 
            computer as you waste no screen space, you can quickly 
            navigate to or rearrange your windows, and you can do all of 
            it without ever touching the mouse. 

        .fixed

            %p

                %a!href="/static/screenshots/current_desktop.png"

                    %img!src="/static/screenshots/current_desktop-thumb.png"!id="current-desktop"!alt="Most recent desktop shot"

        %h3 About Arch

        %p

            If you haven't noticed, my desktop runs on Arch linux. 
      
            %a!href="http://www.archlinux.org/" Arch
      
            \ is an amazing DIY distro that gives the user the barest of 
            bare installs; from there, you can build your system, piece 
            by piece. There is no hand-holding, and the user is expected 
            to install, maintain, and configure his entire system on his 
            own. Through appropriate use of the amazing Arch wiki, 
            forums, and IRC, this isn't so bad - it simply takes 
            reading, and time. The benefit being not only can you make 
            it exactly what you want, but you'll learn more than you 
            could imagine about the inner workings of your system. This 
            transparency and required effort can lead to both 
            fulfillment and frustration; this is a feature, not a bug. 

        %p

            If you want to try a distro that expects a lot from its 
            users while still offering a great package manager and 
            simple transparent tools for maintaining 

            %em your

            \ system, please try Arch. Be sure to read the Beginners 
            guide before installing, and do some research (manpage, 
            wiki, forum, google, and IRC) before asking questions - 
            Though most Archers are extremely friendly, showing that 
            you've done some research before asking the same often 
            repeated questions will go a long way in earning the respect 
            of the group; and, at least in the beginning, I'll bet you 
            find your question's already been asked and answered 
            somewhere.

        %h3 Aur Packages

        ^pkgsTemplate.allPkgs^

        %h3 

            %a#Recent_Posts!href="#Recent_Posts" Recent Posts

        #recent_posts
            $forall posts post
                ^(postTemplate.curTime).post^

        %p.small
            %a!href=@PostsR@ all posts

            \...
        |]
