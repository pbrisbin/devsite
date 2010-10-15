{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Posts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- The master list of all posts known to the site plus some helper
-- functions for finding them.
--
-------------------------------------------------------------------------------
module Posts
    ( Post (..)
    , loadPost
    , getPostsByTag
    , allPosts
    ) where

import Yesod
import DevSite
import qualified Settings as S

-- | The datatype of a Post
data Post = Post
    { postTitle   :: String
    , postSlug    :: String
    , postDate    :: String
    , postDescr   :: String
    , postContent :: HamletContent
    , postTags    :: [String]
    }

-- | A convenience synonymn
type HamletContent = Hamlet (Route DevSite)

-- | Locate posts with a given slug
loadPost :: String -> [Post]
loadPost slug = filter ((== slug) . postSlug) allPosts

-- | Locate posts with a given tag
getPostsByTag :: String -> [Post]
getPostsByTag tag = filter ((elem tag) . postTags) allPosts

-- | The master list of all posts known to the site
allPosts :: [Post]
allPosts =
    [ Post
        "Site Migration"
        "site_migration"
        "Sat, 09 Oct 2010 23:31:43 -0400"
        "Finally, I've successfully migrated the site onto some sort of framework. It's taken a lot of effort but I'm very excited for the result. In this post I'll explain what I did, why I did it, and a little bit about the framework itself: Yesod."
        $(S.hamletFile "posts/site_migration")
        ["Haskell", "Website"]
    
    , Post
        "PHP Authentication"
        "php_authentication"
        "Fri, 01 Oct 2010 21:29:03 -0400"
        "Recently had the chance to write some php pages that needed to be authenticated. Nothing too crazy or secure required, so I decided to just let PHP do the work. Simply redirect the browser to prompt for credentials then do what you want with PHP_AUTH_USER and PHP_AUTH_PW. Simple but affective."
        $(S.hamletFile "posts/php_authentication")
        ["PHP", "Website"]

    , Post
        "XMonad Modules"
        "xmonad_modules"
        "Mon, 30 Aug 2010 22:38:22 -0400"
        "Writing an xmonad.hs can be fun; so much so, that eventually, one file just isn't enough. I've recently modularized parts of my config into separate files under ./lib. In exchange for any breakage this might cause to those that use this particular config, I'm also maintaining haddock documentation for all the modules. Read on for the full apology/announcement and a link to the module documentation."
        $(S.hamletFile "posts/xmonad_modules")
        ["Haskell", "XMonad", "Dzen"]

    , Post
        "Haskell RSS Reader"
        "rss_reader"
        "Sun, 15 Aug 2010 11:51:28 -0400"
        "Just finished writing an RSS Reader using haskell to show aggregated feed items in a ticker-text style dzen bar. I've definitely got some polishing to do, but I'm really happy with how it turned out. Check out the details and let me know what you think."
        $(S.hamletFile "posts/rss_reader")
        ["Haskell", "XMonad", "Dzen"]

    , Post
        "Web Preview"
        "web_preview"
        "Mon, 26 Jul 2010 19:47:51 -0400"
        "I've been enjoying Jumanji as my web browser of choice lately. Unfortunately, this meant it wasn't as convenient for me to use my uzbl-auto-refresh script as a live preview of sorts for my web pages as I wrote them. This motivated me to do something entirely different: I archived all of my Uzbl scripts and configurations and started fresh. I now have one simple webpreview script which spawns and refreshes a minimal uzbl-core instance used solely for previewing web pages as I write them. I'm really happy with the result."
        $(S.hamletFile "posts/web_preview")
        ["Website", "Arch", "Uzbl"]

    , Post
        "Scratchpad Everything"
        "scratchpad_everything"
        "Sun, 13 Jun 2010 20:46:21 -0400"
        "A follow up on my recent XMonad Scratchpad post. I've replaced the terminal-specific scratchpad extension with a more general one that allows any arbitrary application to share the scratchpad paradigm. I find this works really well for a volume mixer, resource monitor, etc."
        $(S.hamletFile "posts/scratchpad_everything")
        ["Haskell", "XMonad"]

    , Post
        "Raw Audio"
        "raw_audio"
        "Thu, 27 May 2010 21:05:54 -0400"
        "Information ragarding my latest hobby: Android development. I've written a simple app called Raw Audio, which just loads a user-entered URI using the built-in MediaPlayer() class. This lets me pick up my mpd stream from anywhere. On this page, I go through the Classes method by method and explain what they do."
        $(S.hamletFile "posts/raw_audio")
        ["Java", "Android"]

    , Post
        "MapToggle"
        "maptoggle"
        "Sat, 08 May 2010 21:08:21 -0400"
        "A nice vim snippet to toggle settings on a key press. This trick has been on my main page for a while, but now it's getting its on home."
        $(S.hamletFile "posts/maptoggle")
        ["Vim"]

    , Post
        "HTPC"
        "httpc"
        "Sat, 01 May 2010 11:22:59 -0400"
        "Details on my recent HTPC build; hardware I got, software I installed, and even some screenshots."
        $(S.hamletFile "posts/htpc")
        ["HTPC","Arch","Linux"]

    , Post
        "Controlling MPlayer"
        "controlling_mplayer"
        "Thu, 08 Apr 2010 19:55:59 -0400"
        "A simple, bindable setup to control a running MPlayer through the use of a fifo."
        $(S.hamletFile "posts/controlling_mplayer")
        ["Arch", "Linux","Bash"]

    , Post
        "Irssi"
        "irssi"
        "Sat, 20 Mar 2010 00:27:55 -0400"
        "Outlining my current irssi setup: config, theme, scripts, etc. Hat-tip to rson for the post idea."
        $(S.hamletFile "posts/irssi")
        ["Arch", "Linux", "IRC"]

    , Post
        "Automounting"
        "automounting"
        "Mon, 11 Jan 2010 21:25:30 -0500"
        "A dead simple, totally independant, easy to setup way to automount the occasional flashdrive."
        $(S.hamletFile "posts/automounting")
        ["Arch", "Linux"]

    , Post
        "Backups"
        "backups"
        "Sun, 03 Jan 2010 12:15:32 -0500"
        "Here I outline my backup strategy; how I do it, why it works for me, and even why it might not work for you."
        $(S.hamletFile "posts/backups")
        ["Arch", "Linux"]

    , Post
        "XMonad's IM Layout"
        "xmonads_im_layout"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "A How-to on setting up a layout in XMonad devoted to an IM client. Using one of the best contrib modules, IMLayout."
        $(S.hamletFile "posts/xmonads_im_layout")
        ["XMonad", "Haskell", "Arch"]

    , Post
        "Using Two IMAP Accounts in Mutt"
        "two_accounts_in_mutt"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "A follow up on using offlineimap/msmtp/mutt for gmail. Here I describe how I added a second account (GMX) into the mix."
        $(S.hamletFile "posts/two_accounts_in_mutt")
        ["Mutt", "Offlineimap", "GMail"]

    , Post
        "Aurget"
        "aurget"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "All about my AUR helper app, Arch linux users only."
        $(S.hamletFile "posts/aurget")
        ["Linux", "Arch", "AUR"]

    ]
    {-
    , Post
        "Dvdcopy"
        "dvdcopy"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "A script to copy a standard DVD9 to a DVD5, decrypting along the way. Just wraps standard tools like growisofs, and mencoder."
        $(S.hamletFile "posts/dvdcopy")

    , Post
        "Screen Tricks"
        "screen_tricks"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "Here I describe how I setup some environment variables and bash aliases to add to the versatility that is the great program screen."
        $(S.hamletFile "posts/screen_tricks")

    , Post
        "Wifi Pipe"
        "wifi_pipe"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "I rewrote the great tool wifi-select to output to an openbox menu. Now you can right-click on your desktop and see all available wifi hotspots. Click to connect."
        $(S.hamletFile "posts/wifi_pipe")

    , Post
        "Text From CLI"
        "text_from_cli"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "Leveraging the power of command line emailing in linux, I show a quick way to get a text message out from the commandline. Set up alerts or bug your friends, whatever works."
        $(S.hamletFile "posts/text_from_cli")

    , Post
        "Downgrade"
        "downgrade"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "A script to easily downgrade packages to a version in your cache or the A.R.M. Arch users only."
        $(S.hamletFile "posts/downgrade")

    , Post
        "Status Bars in XMonad"
        "xmonad_statusbars"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "One of the hardest things for new XMonad users is setting up the status bars. This is mostly due to the myriad options available; here, I outline step-by-step how I do it."
        $(S.hamletFile "posts/xmonad_statusbars")
    , Post
        "Goodsong"
        "goodsong"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "This script allows you to immediately log a currently playing song as 'good'. You can then later, play a random 'good' song, build a playlist of 'good' songs, etc."
        $(S.hamletFile "posts/goodsong")

    , Post
        "Mutt + Gmail + Offlineimap"
        "mutt_gmail_offlineimap"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "A How-to describing the setup required for a convenient offlineimap + mutt + msmtp email solution on linux."
        $(S.hamletFile "posts/mutt_gmail_offlineimap")
    ]
    -}
