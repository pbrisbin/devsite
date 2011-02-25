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
    posts <- fmap (take 10) . sitePosts =<< getYesod
    defaultLayout $ do
        setTitle $ string $ Settings.titlePrefix ++ "Home"
        addKeywords ["home", "haskell", "bash", "mutt", "xmonad", "arch linux"]
        addHamlet [$hamlet|
            %header
                %h1 pbrisbin dot com
        
            %article.fullpage
                %p
                    Welcome to pbrisbin dot com. You'll find it's mostly 
                    [GNU/]
              
                    %a!href=@TagR.linux@ Linux
              
                    -related geekery here and some of the information 
                    presented is specific to the [amazing] distribution 
                    known as 
              
                    %a!href=@TagR.arch@ Arch
              
                    \ Linux. Some of my favorite topics are the 
              
                    %a!href=@TagR.xmonad@ XMonad
              
                    \ window manager, 
              
                    %a!href=@TagR.haskell@ haskell
              
                    \ in general, 
              
                    %a!href=@TagR.bash@ Bash
              
                    \ scripting (or just general command-line 
                    adventures), and the great email client 
              
                    %a!href=@TagR.mutt@ Mutt
              
                    \.

                %p 
                    This site does use HTML5 and CSS3 fairly heavily.

                %p
                    No, it's nothing sexy like 

                    %code &lt;video&gt;

                    \ or 

                    %code &lt;canvas&gt;

                    , but I do use some of the newer semantic tags like 

                    %code &lt;aside&gt;

                    \ and 

                    %code &lt;nav&gt;

                    \. For this reason, the site might not style 
                    properly in older browsers. I'm sorry.

                %p
                    If you're curius, 
                    
                    %a!href="static/fileshare/chromium.png" here's
                    
                    \ what this page looks like in chromium.

                %p
                    Enjoy the site!

                %h3 

                    %a#Recent_Posts!href="#Recent_Posts" Recent Posts
                |]

        -- show recent posts
        mapM_ addPostBlock posts

        addHamlet [$hamlet|
            %p
                %small
                    %a!href=@PostsR@ all posts
                    \...
            |]
