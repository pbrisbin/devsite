{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.About
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.About (getAboutR) where

import Yesod
import DevSite

import Helpers.Layouts

import qualified Settings

-- | About page
getAboutR :: Handler RepHtml
getAboutR = pageLayout $ do
    setTitle $ string "pbrisbin - About"
    addHamlet [$hamlet|
    %h1 About

    %p

      This place is to serve as a little slice of the internet where I can 
      keep crap, link people to my crap, and practice making more better 
      crap.

    %h3 Framework

    %p

      This new version of the site is powered by the 

      %a!href="http://docs.yesodweb.com/" Yesod

      \ framework. It's a web framework built on haskell. 

    %p

      The haskell source for this very site is available in my 

      %a!href="http://github.com/pbrisbin/devsite" git repo

      \ as well.

    %h3 About Me

    %p

      I hold a degree in Aerospace Engineering from BU, but I currently work as an 
      
      %a!href="http://en.wikipedia.org/wiki/Enterprise_resource_planning" ERP
      
      \ Consultant writing 
      
      %a!href="http://en.wikipedia.org/wiki/X++" X++
      
      \ code for 
      
      %a!href="http://en.wikipedia.org/wiki/Microsoft_Dynamics_AX" Microsoft Dynamics AX
      
      \. I've been using Arch Linux for a few years now and do pretty much 
      everything you see around the site as my main hobby.

    %h3 Site Validations

    %p

      All of my pages have been validated as HTML5, my CSS is valid level 
      2.1, and my RSS is valid 2.0.

    %p.centered

      %img.validation!src="/static/images/valid-html5.png"!alt="Valid HTML5"

      &nbsp;

      %img.validation!src="/static/images/valid-css.png"!alt="Valid CSS"

      &nbsp;

      %img.validation!src="/static/images/valid-rss.png"!alt="Valid RSS"
    |]