{-# LANGUAGE QuasiQuotes  #-}
module Helpers.Links
    ( Destination(..)
    , Link(..)
    , link
    ) where

import Yesod (GWidget, Route, hamlet)
import qualified Data.Text as T

-- | An internal route or external url
data Destination m = Internal (Route m) | External T.Text

-- | A link to a 'Destination' with supplied titles and text to be used 
--   when showing the html.
data Link m = Link
    { linkDest  :: Destination m
    , linkTitle :: T.Text
    , linkText  :: T.Text
    }

-- | Show a general link
link :: Link m -> GWidget s m ()
link (Link (Internal i) t x) = [hamlet|<a title="#{t}" href="@{i}">#{x}|]
link (Link (External e) t x) = [hamlet|<a title="#{t}" href="#{e}">#{x}|]
