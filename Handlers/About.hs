{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.About (getAboutR) where

import DevSite
import Yesod
import qualified Settings

-- | About page
getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    Settings.setTitle "About"
    addKeywords ["about"]

    let v1 = Settings.staticRoot ++ "/images/valid-html5.png"
    let v2 = Settings.staticRoot ++ "/images/valid-css.png"
    let v3 = Settings.staticRoot ++ "/images/valid-rss.png"

    [hamlet|
        <header>
            <h1>About

        <article .fullpage>
            <p>
                This place is to serve as a little slice of the internet 
                where I can keep crap, link people to my crap, and 
                practice making more better crap.

            <h3>Framework

            <p>
                This new version of the site is powered by the 

                <a href="http://docs.yesodweb.com/">Yesod

                \ framework. It's an extremely powerful framework 
                written in Haskell.

            <p>
                The source for the site is available in my 

                <a href="http://github.com/pbrisbin/devsite">
                    git repo

                \ if anyone's interested.

            <p>
                There are also a 

                <a href="http://github.com/pbrisbin/yesod-comments">
                    number

                \ 

                <a href="http://github.com/pbrisbin/yesod-mpc">of

                \ 

                <a href="http://github.com/pbrisbin/yesod-statistics">
                    extensions

                \ which I've written for the framework. Some I use here, 
                some I don't.

            <h3>About Arch

            <p>
                If you haven't noticed, my desktop runs on Arch linux. 

                <a href="http://www.archlinux.org/">Arch

                \ is an amazing DIY distro that gives the user the 
                barest of bare installs; from there, you can build your 
                system, piece by piece. There is no hand-holding, and 
                the user is expected to install, maintain, and configure 
                his entire system on his own.  Through appropriate use 
                of the amazing Arch wiki, forums, and IRC, this isn't so 
                bad - it simply takes reading, and time. The benefit 
                being not only can you make it exactly what you want, 
                but you'll learn more than you could imagine about the 
                inner workings of your system. This transparency and 
                required effort can lead to both fulfillment and 
                frustration; this is a feature, not a bug.

            <p>
                If you want to try a distro that expects a lot from its 
                users while still offering a great package manager and 
                simple transparent tools for maintaining 

                <em>your

                \ system, please try Arch. Be sure to read the Beginners 
                guide before installing, and do some research (manpage, 
                wiki, forum, google, and IRC) before asking questions - 
                Though most Archers are extremely friendly, showing that 
                you've done some research before asking the same often 
                repeated questions will go a long way in earning the 
                respect of the group; and, at least in the beginning, 
                I'll bet you find your question's already been asked and 
                answered somewhere.

            <h3>About Me

            <p>
                I hold a degree in Aerospace Engineering from BU, but I 
                currently work as an 

                <a href="http://en.wikipedia.org/wiki/Enterprise_resource_planning">
                    ERP

                \ Consultant writing 

                <a href="http://en.wikipedia.org/wiki/X++">X++ 

                \ code for 

                <a href="http://en.wikipedia.org/wiki/Microsoft_Dynamics_AX">
                    Microsoft Dynamics AX

                \. I've been using Arch Linux for a few years now and do 
                pretty much everything you see around the site as my 
                main hobby.

            <h3>Site Validations

            <p>
                All of my pages are valid HTML5. My stylesheet (with the 
                exception of some vendor-extensions) is valid CSS3. My 
                RSS is valid level 2.0.

            <p .centered>
                <img src=#{v1} alt="Valid HTML5" .validation>
                &nbsp;
                <img src=#{v2} alt="Valid CSS" .validation>
                &nbsp;
                <img src=#{v3} alt="Valid RSS" .validation>
    |]
