{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Templates
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Drop in templates, for now only one.
--
-------------------------------------------------------------------------------
module Comments.Templates (defaultTemplate) where

import Yesod
import Comments.Core

import Text.Hamlet      (HamletValue, ToHtml)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | Template for the entire comments section
defaultTemplate :: (HamletValue a, ToHtml b) => [Comment] -> a -> b -> a
defaultTemplate comments form enctype = [$hamlet|
#comments
    %h4 Add a comment:
    %form!enctype=$enctype$!method="post"
        %table
            ^form^
            %tr
                %td
                    &nbsp;
                %td
                    %input!type="submit"!value="Add comment"
    %p 
        %em when using html, assume your text will be wrapped in &lt;p&gt &lt;/p&gt;

    %h4 Showing $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^
|]

-- | Sub template for a single comment
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = 
    let date = formatTime defaultTimeLocale "%a, %b %d at %H:%S" $ timeStamp comment
    in [$hamlet|
%p
    On 
    %strong $date$
    , 
    %strong $userName.comment$
    \ wrote:

%blockquote
    %p
        $content.comment$
|]
