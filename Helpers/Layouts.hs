{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Helpers.Layouts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Helpers.Layouts
    ( pageLayout
    , postLayout
    ) where

import Yesod
import DevSite

import Data.Time.Clock (getCurrentTime)

import Helpers.Posts

import qualified Settings

-- | Like defaultLayout, just with breadcrumbs, used with any top level
--   pages
pageLayout :: Widget () -> Handler RepHtml
pageLayout widget = do
    mmesg  <- getMessage
    (t, h) <- breadcrumbs
    pc <- widgetToPageContent $ do
        widget
        addCassius $(Settings.cassiusFile "root-css")
    hamletToRepHtml [$hamlet|
    !!!
    %html!lang="en"
        %head
            %meta!name="author"!content="pbrisbin"
            %meta!name="keywords"!content="pbrisbin, arch linux, bash, xmonad, mutt"
            %meta!name="description"!content="pbrisbin dot com"
            %meta!http-equiv="Content-Type"!content="text/html; charset=UTF-8"
            %title $pageTitle.pc$
            %link!rel="alternate"!type="application/rss+xml"!title="rss feed"!href=@FeedR@
            ^pageHead.pc^
        %body
            #header
                %p
                    $forall h node
                        %a!href=@fst.node@ $snd.node$ 
                        \ / 
                        \ $t$

                    $maybe mmesg msg
                        #message 
                            %p.centered $msg$
            #body
                ^pageBody.pc^
            #footer
                ^footerTemplate^
    |]

-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    curTime     <- liftIO getCurrentTime
    mmesg       <- getMessage
    (t, h)      <- breadcrumbs
    postContent <- loadPostContent post

    let prettyTime = string . humanReadableTimeDiff curTime $ postDate post

    pc <- widgetToPageContent $ do
        setTitle $ string $ "pbrisbin - " ++ postTitle post
        addCassius $(Settings.cassiusFile "root-css")
    hamletToRepHtml [$hamlet|
    !!!
    %html!lang="en"
        %head
            %meta!name="author"!content="pbrisbin"
            %meta!name="keywords"!content="pbrisbin, arch linux, bash, haskell, xmonad, mutt"
            %meta!name="description"!content="pbrisbin dot com"
            %meta!http-equiv="Content-Type"!content="text/html; charset=UTF-8"
            %title $pageTitle.pc$
            %link!rel="alternate"!type="application/rss+xml"!title="rss feed"!href=@FeedR@
            ^pageHead.pc^
        %body

            #header
                %p
                    $forall h node
                        %a!href=@fst.node@ $snd.node$ 
                        \ / 
                    \ $t$

                    %span!style="float: right;"
                        Tags: 
                            $forall postTags.post tag
                                %a!href=@TagR.tag@ $tag$ 

            #body
                %h1 $postTitle.post$

                $maybe mmesg msg
                    #message
                        %p.centered $msg$

                $postContent$

                %p.small
                    %em Published $prettyTime$

                %h3 
                    %a!href="#Comments"!id="Comments" Comments

                #disqus_thread
                    %script!type="text/javascript"
                        var disqus_shortname = 'pbrisbin'; 
                        var disqus_identifier = $postSlug.post$; 

                    %script!type="text/javascript"!src="http://pbrisbin.disqus.com/embed.js"

                    %noscript Sadly, javascript is required for comments on this site.

            #footer
                ^footerTemplate^
    |]
