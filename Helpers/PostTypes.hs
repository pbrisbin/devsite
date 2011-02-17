-------------------------------------------------------------------------------
-- |
-- Module      :  Helpers.PostTypes
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- These types need to be shared between DevSite and Posts so this
-- separate module prevents a circular import
--
-------------------------------------------------------------------------------
module Helpers.PostTypes
    ( Post(..)
    , Tag
    , TagGroup
    ) where

import Data.Time (UTCTime(..))

type Tag = String

data Post = Post
    { postSlug  :: String
    , postDate  :: UTCTime
    , postTitle :: String
    , postDescr :: String
    , postTags  :: [Tag]
    }

-- | A tag name and the list of posts that have that tag
type TagGroup = (Tag, [Post])
