{-# LANGUAGE QuasiQuotes #-}
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
    , loadPostContent
    , getPostBySlug
    , getPostsByTag
    , allPosts
    , mkPostSlugs
    , mkPostTags
    , allPostsTemplate
    , postTemplate
    ) where

import Yesod
import DevSite

import Data.Char (toLower)
import Data.List (nub)
import System.Directory (doesFileExist)
import Language.Haskell.TH.Syntax
import Text.Pandoc

-- | The datatype of a Post
data Post = Post
    { postTitle :: String   -- ^ The Title
    , postSlug  :: String   -- ^ the_slug, also used as pandoc filename
    , postDate  :: String   -- ^ the post date in format of `date -R`
    , postDescr :: String   -- ^ description used for post listing and RSS feed
    , postTags  :: [String] -- ^ list of tags that apply to the post
    }

-- | Locate posts with a given slug
getPostBySlug :: String -> [Post]
getPostBySlug slug = filter ((== slug) . postSlug) allPosts

-- | Locate posts with a given tag
getPostsByTag :: String -> [Post]
getPostsByTag tag = filter (elem tag . postTags) allPosts

-- | Use TH to define functions for each post's slug for use in hamlet
--   templates
mkPostSlugs :: Q [Dec]
mkPostSlugs = mkConstants $ map postSlug allPosts

-- | Use TH to define functions for each tag for use in hamlet templates
mkPostTags :: Q [Dec]
mkPostTags = mkConstants $ nub . concat $ map postTags allPosts

-- | Load a post's pandoc file and convert it to html, return not found
--   if the pdc file doesn't exist
loadPostContent :: Post -> IO Html
loadPostContent p = do
    let fileName = "pandoc/" ++ postSlug p ++ ".pdc"
    markdown <- do
        exists <- doesFileExist fileName
        if exists
            then readFile fileName
            else return "File not found"
    return $ preEscapedString
           $ writeHtmlString defaultWriterOptions
           $ readMarkdown defaultParserState markdown

-- | A body template for a list of posts, you can also provide the title
allPostsTemplate :: [Post] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$

#posts
    $forall posts post
        ^postTemplate.post^
|]

-- | The sub template for a single post
postTemplate :: Post -> Hamlet DevSiteRoute
postTemplate arg = [$hamlet|
.post
  %p

    %a!href=@PostR.postSlug.arg@ $postTitle.arg$
    \ - $postDescr.arg$ 
    
  %p.small

    Published on $postDate.arg$

    %span!style="float: right;"

      Tags: 

      $forall postTags.arg tag

        %a!href=@TagR.tag@ $tag$ 
|]

-- | The master list of all posts known to the site
allPosts :: [Post]
allPosts =
    [ Post
        "Android Receiver"
        "android_receiver"
        "Sat, 11 Dec 2010 16:28:24 -0500"
        ( "Android notifier is a great little app I just recently found "
        ++ "on the marketplace. What it does is use your wifi network or a "
        ++ "bluetooth connection to send out a broadcast when certain events "
        ++ "happen on your phone. Things like incoming calls or SMS "
        ++ "messages. The Linux application designed to receive these events "
        ++ "and display a desktop notification were overkill for me. This "
        ++ "post describes my simpler reciever using just netcat, dzen, and "
        ++ "bash.")
        ["Linux", "Android", "Bash"]
    
    , Post
        "Vim Registers"
        "vim_registers"
        "Sun, 07 Nov 2010 14:05:42 -0500"
        ( "When you use an extremely powerful text editor such as vi, vim, or "
        ++ "emacs, there are often times where you'll discover a feature or command "
        ++ "that literally changes the way you write text. It's not a very large "
        ++ "leap to say that, for a developer, that can be life-changing. "
        ++ "I've recently made one such discovery via vim's :help registers "
        ++ "command. So I'd like to boil it down a bit and share it here. ")
        ["Linux", "Vim"]
    
    , Post
        "Site Migration"
        "site_migration"
        "Sat, 09 Oct 2010 23:31:43 -0400"
        ("Finally, I've successfully migrated the site onto some sort of "
        ++ "framework. It's taken a lot of effort but I'm very excited for "
        ++ "the result. In this post I'll explain what I did, why I did it, "
        ++ "and a little bit about the framework itself: Yesod.")
        ["Haskell", "Website"]
    
    , Post
        "PHP Authentication"
        "php_authentication"
        "Fri, 01 Oct 2010 21:29:03 -0400"
        ("Recently had the chance to write some php pages that needed to "
        ++ "be authenticated. Nothing too crazy or secure required, so I "
        ++ "decided to just let PHP do the work. Simply redirect the browser "
        ++ "to prompt for credentials then do what you want with "
        ++ "PHP_AUTH_USER and PHP_AUTH_PW. Simple but affective.")
        ["PHP", "Website"]

    , Post
        "XMonad Modules"
        "xmonad_modules"
        "Mon, 30 Aug 2010 22:38:22 -0400"
        ("Writing an xmonad.hs can be fun; so much so, that eventually, "
        ++ "one file just isn't enough. I've recently modularized parts of "
        ++ "my config into separate files under ./lib. In exchange for any "
        ++ "breakage this might cause to those that use this particular "
        ++ "config, I'm also maintaining haddock documentation for all the "
        ++ "modules. Read on for the full apology/announcement and a link to "
        ++ "the module documentation.")
        ["Haskell", "XMonad", "Dzen"]

    , Post
        "Haskell RSS Reader"
        "rss_reader"
        "Sun, 15 Aug 2010 11:51:28 -0400"
        ("Just finished writing an RSS Reader using haskell to show "
        ++ "aggregated feed items in a ticker-text style dzen bar. I've "
        ++ "definitely got some polishing to do, but I'm really happy with "
        ++ "how it turned out. Check out the details and let me know what "
        ++ "you think.")
        ["Haskell", "XMonad", "Dzen"]

    , Post
        "Web Preview"
        "web_preview"
        "Mon, 26 Jul 2010 19:47:51 -0400"
        ("I've been enjoying Jumanji as my web browser of choice lately. "
        ++ "Unfortunately, this meant it wasn't as convenient for me to use "
        ++ "my uzbl-auto-refresh script as a live preview of sorts for my "
        ++ "web pages as I wrote them. This motivated me to do something "
        ++ "entirely different: I archived all of my Uzbl scripts and "
        ++ "configurations and started fresh. I now have one simple "
        ++ "webpreview script which spawns and refreshes a minimal uzbl-core "
        ++ "instance used solely for previewing web pages as I write them. "
        ++ "I'm really happy with the result.")
        ["Website", "Arch", "Uzbl"]

    , Post
        "Scratchpad Everything"
        "scratchpad_everything"
        "Sun, 13 Jun 2010 20:46:21 -0400"
        ("A follow up on my recent XMonad Scratchpad post. I've replaced "
        ++ "the terminal-specific scratchpad extension with a more general "
        ++ "one that allows any arbitrary application to share the "
        ++ "scratchpad paradigm. I find this works really well for a volume "
        ++ "mixer, resource monitor, etc.")
        ["Haskell", "XMonad"]

    , Post
        "Raw Audio"
        "raw_audio"
        "Thu, 27 May 2010 21:05:54 -0400"
        ("Information ragarding my latest hobby: Android development. "
        ++ "I've written a simple app called Raw Audio, which just loads a "
        ++ "user-entered URI using the built-in MediaPlayer() class. This "
        ++ "lets me pick up my mpd stream from anywhere. On this page, I go "
        ++ "through the Classes method by method and explain what they do.")
        ["Java", "Android"]

    , Post
        "MapToggle"
        "maptoggle"
        "Sat, 08 May 2010 21:08:21 -0400"
        ("A nice vim snippet to toggle settings on a key press. This "
        ++ "trick has been on my main page for a while, but now it's getting "
        ++ "its on home.")
        ["Vim"]

    , Post
        "HTPC"
        "htpc"
        "Sat, 01 May 2010 11:22:59 -0400"
        ("Details on my recent HTPC build; hardware I got, software I "
        ++ "installed, and even some screenshots.")
        ["Home Theatre","Arch","Linux"]

    , Post
        "XMonad Scratchpad"
        "xmonad_scratchpad"
        "Sat, 10 Apr 2010 00:34:24 -0400"
        ("How to add a scratchpad terminal to your XMonad setup. If "
        ++ "you've ever used Quake or some other drop-down terminal, you "
        ++ "know how useful these are. XMonad's contrib module makes it easy "
        ++ "to add one of these natively to your WM.")
        ["Haskell", "XMonad"]

    , Post
        "Controlling MPlayer"
        "controlling_mplayer"
        "Thu, 08 Apr 2010 19:55:59 -0400"
        ("A simple, bindable setup to control a running MPlayer through "
        ++ "the use of a fifo.")
        ["Arch", "Linux","Bash"]

    , Post
        "Irssi"
        "irssi"
        "Sat, 20 Mar 2010 00:27:55 -0400"
        ("Outlining my current irssi setup: config, theme, scripts, etc. "
        ++ "Hat-tip to rson for the post idea.")
        ["Arch", "Linux", "IRC"]

    , Post
        "Automounting"
        "automounting"
        "Mon, 11 Jan 2010 21:25:30 -0500"
        ("A dead simple, totally independant, easy to setup way to "
        ++ "automount the occasional flashdrive.")
        ["Arch", "Linux"]

    , Post
        "Backups"
        "backups"
        "Sun, 03 Jan 2010 12:15:32 -0500"
        ("Here I outline my backup strategy; how I do it, why it works "
        ++ "for me, and even why it might not work for you.")
        ["Arch", "Linux"]

    , Post
        "XMonad's IM Layout"
        "xmonads_im_layout"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A How-to on setting up a layout in XMonad devoted to an IM "
        ++ "client. Using one of the best contrib modules, IMLayout.")
        ["XMonad", "Haskell", "Arch"]

    , Post
        "Using Two IMAP Accounts in Mutt"
        "two_accounts_in_mutt"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A follow up on using offlineimap/msmtp/mutt for gmail. Here I "
        ++ "describe how I added a second account (GMX) into the mix.")
        ["Mutt", "Offlineimap", "GMail"]

    , Post
        "Aurget"
        "aurget"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        "All about my AUR helper app, Arch linux users only."
        ["Linux", "Arch", "AUR"]

    , Post
        "Dvdcopy"
        "dvdcopy"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A script to copy a standard DVD9 to a DVD5, decrypting along "
        ++ "the way. Just wraps standard tools like growisofs, and "
        ++ "mencoder.")
        ["Arch", "Linux", "Bash"]

    , Post
        "Screen Tricks"
        "screen_tricks"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("Here I describe how I setup some environment variables and bash "
        ++ "aliases to add to the versatility that is the great program "
        ++ "screen.")
        ["Arch", "Linux", "Screen"]

    , Post
        "Text From CLI"
        "text_from_cli"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("Leveraging the power of command line emailing in linux, I show "
        ++ "a quick way to get a text message out from the commandline. Set "
        ++ "up alerts or bug your friends, whatever works.")
        ["Arch", "Linux"]

    , Post
        "Downgrade"
        "downgrade"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A script to easily downgrade packages to a version in your "
        ++ "cache or the A.R.M. Arch users only.")
        ["Arch", "Linux"]

    , Post
        "Wifi Pipe"
        "wifi_pipe"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("I rewrote the great tool wifi-select to output to an openbox "
        ++ "menu. Now you can right-click on your desktop and see all "
        ++ "available wifi hotspots. Click to connect.")
        ["Arch", "Linux", "Openbox", "Bash"]

    , Post
        "Status Bars in XMonad"
        "xmonad_statusbars"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("One of the hardest things for new XMonad users is setting up "
        ++ "the status bars. This is mostly due to the myriad options "
        ++ "available; here, I outline step-by-step how I do it.")
        ["XMonad", "Haskell", "Dzen"]

    , Post
        "Goodsong"
        "goodsong"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("This script allows you to immediately log a currently playing "
        ++ "song as 'good'. You can then later, play a random 'good' song, "
        ++ "build a playlist of 'good' songs, etc.")
        ["Arch", "Linux", "Bash"]

    , Post
        "Mutt + Gmail + Offlineimap"
        "mutt_gmail_offlineimap"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A How-to describing the setup required for a convenient "
        ++ "offlineimap + mutt + msmtp email solution on linux.")
        ["Mutt", "Offlineimap", "GMail"]
    , Post
        "Display Manager"
        "display_manager"
        "Sat, 05 Dec 2009 18:50:44 -0500"
        ("A simple approach to a display manager. Just drop a few lines "
        ++ "in ~/.bashrc to conditionally startx")
        ["Bash", "Linux", "Arch"]
    ]
