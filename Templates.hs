{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Settings
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Small hamlet sub-templates centralized for use in more than one page.
--
-------------------------------------------------------------------------------
module Templates
    ( allPostsTemplate
    , postTemplate
    ) where

import Yesod
import DevSite
import Posts

-- | A body template for a list of posts, you can also provide the title
allPostsTemplate :: [Post] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$
%hr

#posts
    $forall posts post
        ^postTemplate.post^
|]

-- | A sub template for a single post
postTemplate :: Post -> Hamlet DevSiteRoute
postTemplate arg = [$hamlet|
.post
  %p

    %a!href=@PostR postSlug.arg@ $postTitle.arg$
    \ - $postDescr.arg$ 
    
  %p.small

    Published on $postDate.arg$

    %span!style="float: right;"

      Tags: 

      $forall postTags.arg tag

        %a!href=@TagR tag@ $tag$ 
|]
