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

-- | Either an interal route or an external url
data Destination m = Internal (Route m) | External T.Text

-- | A generalized link
data Link m = Link
    { linkDest  :: Destination m
    , linkTitle :: T.Text
    , linkText  :: T.Text
    }

-- | How to display a link
showLink :: Link m -> GWidget s m ()
showLink (Link (Internal i) t x) = [hamlet|<a title="#{t}" href="@{i}">#{x}|]
showLink (Link (External e) t x) = [hamlet|<a title="#{t}" href="#{e}">#{x}|]

-- | Items that can be linked to within this app
class Linkable a where
    link :: a -> Link DevSite

instance Linkable Post where
    link p = Link (Internal $ PostR $ postSlug p) (postTitle p) (postTitle p)

instance Linkable Tag where
    link t = Link (Internal $ TagR $ tagName t) (tagName t) (tagName t)

instance Linkable Document where
    link = link . post

-- | This is unsafe but useful. It basically says any link to raw 
--   text is assumed to be a tag. There is no assurance the tag exists
instance Linkable T.Text where
    link t = Link (Internal $ TagR $ T.toLower t) t t

-- | A convenience helper for links specific to this app
linkTo :: Linkable a => a -> GWidget s DevSite ()
linkTo = showLink . link

-- | This helps where OverloadedStrings can't figure it out
linkToText :: T.Text -> GWidget s DevSite ()
linkToText = linkTo
