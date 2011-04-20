{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Model where

import Yesod.Persist
import Yesod.Markdown (Markdown)

import Data.List                   (group, concatMap)
import Data.Time                   (UTCTime)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

-- | Generate data base instances for post meta-data
share2 mkPersist (mkMigrate "migratePosts") [persist|
    Post
        slug  String  Eq
        date  UTCTime Desc
        title String
        descr String
        UniquePost slug

    Tag
        post PostId
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
collectByTagName docs =
    -- faster than nub on sorted lists
    let uniqueTags = map head . group . map tagName $ concatMap tags docs
    in  map (\tag -> collect docs tag (hasTag tag)) uniqueTags

hasTag :: String -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
