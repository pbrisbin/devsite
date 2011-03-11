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
import Text.Blaze (toHtml)

import DevSite
import Helpers.Posts

import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    posts <- fmap (take 10) . sitePosts =<< getYesod
    defaultLayout $ do
        setTitle $ toHtml $ Settings.titlePrefix ++ "Home"
        addKeywords ["home", "haskell", "bash", "mutt", "xmonad", "arch linux"]
        [$hamlet|
            <header>
                <h1>
                    <span .title_one>pbrisbin
                    <span .title_two>dot
                    <span .title_three>com

            <article .fullpage>
                <p>
                    Welcome to pbrisbin dot com. You'll find it's mostly 
                    [GNU/]

                    <a href="@{TagR "linux"}">Linux

                    -related geekery here and some of the information 
                    presented is specific to the [amazing] distribution 
                    known as 

                    <a href="@{TagR "arch"}">Arch

                    \ Linux. Some of my favorite topics are the 

                    <a href="@{TagR "xmonad"}">XMonad

                    \ window manager, 

                    <a href="@{TagR "haskell"}">haskell

                    \ in general, 

                    <a href="@{TagR "bash"}">Bash

                    \ scripting (or just general command-line 
                    adventures), and the great email client 

                    <a href="@{TagR "mutt"}">Mutt

                    \.

                <p>
                    This site does use HTML5 and CSS3 fairly heavily.

                <p>
                    No, it's nothing sexy like 

                    <code>&lt;video&gt;

                    \ or 

                    <code>&lt;canvas&gt;

                    , but I do use some of the newer semantic tags like 

                    <code>&lt;aside&gt;

                    \ and 

                    <code>&lt;nav&gt;

                    \. For this reason, the site might not style 
                    properly in older browsers. I'm sorry.

                <h3>
                    <a id="Recent_Posts" href="#Recent_Posts">Recent Posts

                $forall post <- posts
                    ^{addPostBlock post}

                <p>
                    <small>
                        <a href="@{PostsR}">all posts
                        \...
            |]
