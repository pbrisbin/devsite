{-# LANGUAGE QuasiQuotes #-}
module Helpers.Links
    ( Link(..)
    , showLink
    , Linkable(..)
    , linkTo
    , linkToText
    ) where

import Yesod
import DevSite
import Model
import qualified Data.Text as T

-- | A generalized internal link
data Link m = Link
    { linkRoute :: Route m
    , linkTitle :: T.Text
    , linkText  :: T.Text
    }

-- | How to display a general link
showLink :: Link m -> GWidget s m ()
showLink (Link r t x) = [hamlet|<a title="#{t}" href="@{r}">#{x}|]

-- | Items that can be linked to within this app
class Linkable a where
    link :: a -> Link DevSite

instance Linkable Post where
    link p = Link (PostR $ postSlug p) (postTitle p) (postTitle p)

instance Linkable Tag where
    link t = Link (TagR $ tagName t) (tagName t) (tagName t)

instance Linkable Document where
    link = link . post

-- | This is unsafe but useful. It basically says any link to raw 
--   text is assumed to be a tag. There is no assurance the tag exists
instance Linkable T.Text where
    link t = Link (TagR $ T.toLower t) t t

-- | A convenience helper for links specific to this app
linkTo :: Linkable a => a -> GWidget s DevSite ()
linkTo = showLink . link

-- | This helps where OverloadedStrings can't figure it out
linkToText :: T.Text -> GWidget s DevSite ()
linkToText = linkTo
