{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import DevSite
import Yesod.Goodies.Links
import Helpers.Documents
import Data.Text (Text)

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        setTitle "Home"
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
                    [GNU/]^{link'' "Linux"}-related geekery here and 
                    some of the information presented is specific to the 
                    [amazing] distribution known as ^{link'' "Arch"} 
                    Linux. Some of my favorite topics are the 
                    ^{link'' "XMonad"} window manager, 
                    ^{link'' "haskell"} in general, ^{link'' "bash"} 
                    scripting (or just general command-line adventures), 
                    and the great email client ^{link'' "mutt"}.

                <p>
                    This site does use HTML5 and CSS3 fairly heavily.

                <p>
                    No, it's nothing sexy like ^{code "<video>"} or 
                    ^{code "<canvas>"}, but I do use some of the newer 
                    semantic tags like ^{code "<aside>"} and 
                    ^{code "<nav>"}. For this reason, the site might not 
                    style properly in older browsers. I'm sorry.

                <h3>
                    <a id="Recent_Posts" href="#Recent_Posts">Recent Posts

                $forall doc <- take 10 docs
                    ^{shortDocument doc}

                <p>
                    <small>^{link PostsR}...
            |]
            
            where
                -- helps OverloadedStrings determine the type
                link'' :: Text -> GWidget s DevSite ()
                link'' = link

                code :: Text -> GWidget s DevSite ()
                code t = [hamlet|<code>#{t}|]
