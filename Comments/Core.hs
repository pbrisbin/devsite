{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Core
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Core data types.
--
-------------------------------------------------------------------------------
module Comments.Core 
    ( Comment(..)
    , CommentForm(..)
    , CommentStorage(..)
    ) where

import Yesod
import Data.Time.Clock (UTCTime)

-- | The actual comment data type
data Comment = Comment
    { thread    :: String
    , timeStamp :: UTCTime
    , ipAddress :: String
    , userName  :: String
    , content   :: Html
    } deriving Show

-- | The form data type, this is used to gather the comment from the
--   user and is handed off to commentFromForm just before storeComment.
data CommentForm = CommentForm
    { formUser    :: String
    , formComment :: Textarea
    , formIsHtml  :: Bool
    }

-- | A data type to represent your backend. Provides an abstract
--   interface for use by the code here.
data CommentStorage = CommentStorage
    { storeComment :: (Yesod m) => Comment -> GHandler s m ()
    , loadComments :: (Yesod m) => String  -> GHandler s m [Comment]
    }
