{-# LANGUAGE QuasiQuotes     #-}
--
-- pbrisbin 2010
--
module Templates where

import Yesod
import DevSite
import Posts

-- | A body template for a list of posts, provide the title
allPostsTemplate :: [Post] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$
%hr

#recent_posts
    $forall posts post
        ^postTemplate.post^
|]

-- | A sub template for a single post
postTemplate :: Post -> Hamlet DevSiteRoute
postTemplate arg = [$hamlet|
.recent_post
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
