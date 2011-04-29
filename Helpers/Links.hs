{-# LANGUAGE QuasiQuotes #-}
-- |
-- 
-- Generic representation of an internal (type-safe) or external 
-- (text-only) link that can be placed within a widget.
--
module Helpers.Links
    ( Destination(..)
    , Link(..)
    , showLink
    , Linkable(..)
    , tagLink
    ) where

import DevSite
import Model

import Yesod (GWidget, Route, hamlet)
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

-- | Show a link to a type in this application
class Linkable a where
    link :: a -> GWidget s DevSite ()

instance Linkable Post where
    link p = showLink $ Link (Internal $ PostR $ postSlug p) (postTitle p) (postTitle p)

instance Linkable Tag where
    link t = showLink $ Link (Internal $ TagR $ tagName t) (tagName t) (tagName t)

instance Linkable Document where
    link = link . post

-- | This is unsafe but useful. It basically says any link to raw 
--   text is assumed to be a tag. There is no assurance the tag exists
instance Linkable T.Text where
    link t = showLink $ Link (Internal $ TagR $ T.toLower t) t t

-- | The explicit type helps OverloadedStrings figure it out
tagLink :: T.Text -> GWidget s DevSite ()
tagLink = link
