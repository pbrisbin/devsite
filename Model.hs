{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Model where

import Yesod.Persist
import Yesod.Goodies.Markdown
import Data.List (nub)
import Data.Time (UTCTime)
import Data.Text (Text)

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

collect :: [Document] -> Text -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

collectByTagName :: [Document] -> [Collection]
collectByTagName docs = map (\tag -> collect docs tag (hasTag tag))
                      . nub . map tagName $ concatMap tags docs

hasTag :: Text -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
