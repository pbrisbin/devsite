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
    ) where

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
