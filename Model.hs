{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Model where

import Yesod.Persist
import Data.List (nub)
import Data.Time (UTCTime)

-- | Generate data base instances for post meta-data
share2 mkPersist (mkMigrate "migratePosts") [persist|
    Post
        slug  String  Eq Update
        date  UTCTime Desc
        title String Update
        descr String Update
        UniquePost slug

    Tag
        post PostId Eq
        name String Asc
        UniqueTag post name
    |]

data Document = Document
    { post :: Post
    , tags :: [Tag]
    }

data Collection = Collection
    { name      :: String
    , documents :: [Document]
    }

collect :: [Document] -> String -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

collectByTagName :: [Document] -> [Collection]
collectByTagName docs = map (\tag -> collect docs tag (hasTag tag)) . nub . map tagName $ concatMap tags docs

hasTag :: String -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
