{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Shorten (Shorten(..)) where

import Yesod.Comments.Markdown
import qualified Data.Text as T

-- | Shorten a variety of string-like types adding ellipsis
class Shorten a where shorten :: Int -> a -> a

instance Shorten [Char] where
    shorten n s = if length s > n then take (n - 3) s ++ "..." else s

instance Shorten T.Text where
    shorten n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t

instance Shorten Markdown where
    shorten n (Markdown s) = Markdown $ shorten n s
