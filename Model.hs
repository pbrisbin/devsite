{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Model where

import Yesod.Goodies.Markdown
import Yesod.Persist

import Data.List (nub, sortBy)
import Data.Ord  (comparing)
import Data.Text (Text)
import Data.Time (UTCTime)

share [mkPersist, mkMigrate "migratePosts"] $(persistFile "config/models")

data Document = Document
    { post :: Post
    , tags :: [Tag]
    }

instance Eq Document where
    (Document p1 _) == (Document p2 _) = postSlug p1 == postSlug p2

data Collection = Collection
    { name      :: Text
    , documents :: [Document]
    }

-- | Sorted by length descending
collectByTagName :: [Document] -> [Collection]
collectByTagName docs = reverse . sortBy (comparing (length . documents))
                      . map (\tag -> collect docs tag (hasTag tag))
                      . nub . map tagName $ concatMap tags docs

collect :: [Document] -> Text -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

hasTag :: Text -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
