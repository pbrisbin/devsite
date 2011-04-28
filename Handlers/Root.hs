{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import DevSite
import Yesod
import Helpers.Documents
import Helpers.Links
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "Home"
        addKeywords ["home", "haskell", "bash", "mutt", "xmonad", "arch linux"]
        [hamlet|
            <header>
                <h1>
                    <span .title_one>pbrisbin
                    <span .title_two>dot
                    <span .title_three>com

            <article .fullpage>
                <p>
                    Welcome to pbrisbin dot com. You'll find it's mostly 
                    [GNU/] ^{linkToText "Linux"}-related geekery here 
                    and some of the information presented is specific to 
                    the [amazing] distribution known as 
                    ^{linkToText "Arch"} Linux. Some of my favorite 
                    topics are the ^{linkToText "XMonad"} window 
                    manager, ^{linkToText "haskell"} in general, 
                    ^{linkToText "bash"} scripting (or just general 
                    command-line adventures), and the great email client 
                    ^{linkToText "mutt"}.

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

                $forall doc <- take 10 docs
                    ^{shortDocument doc}

                <p>
                    <small>
                        <a href="@{PostsR}">all posts
                        \...
            |]
