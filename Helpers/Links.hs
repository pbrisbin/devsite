{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.Links
    ( YesodLinked(..)
    , Destination(..)
    , Link(..)
    , IsLink(..)
    , link
    , link'
    ) where

import Yesod
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

-- | A type family instance used to generalize the widgets that show a 
--   @'Route' m@ where @m@ is your foundation type.
--
--   > instance YesodLinked MyApp where
--   >     type Linked = MyApp
--
class Yesod m => YesodLinked m where
    type Linked

-- | A general way to link to any type in your application
class IsLink a where
    toLink :: a -> Link Linked

-- | The widget for any @'IsLink'@ type
link :: IsLink a => a -> GWidget s Linked ()
link = link' . toLink

link' :: Link m -> GWidget s m ()
link' (Link (Internal i) t x) = [hamlet|<a title="#{t}" href="@{i}">#{x}|]
link' (Link (External e) t x) = [hamlet|<a title="#{t}" href="#{e}">#{x}|]
