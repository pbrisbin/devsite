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

import qualified Data.Text as T

-- | Generate data base instances for post meta-data
share2 mkPersist (mkMigrate "migratePosts") [persist|
    Post
        slug  T.Text  Eq      Update
        date  UTCTime    Desc
        title T.Text          Update
        descr Markdown        Update

        UniquePost slug

    Tag
        post PostId Eq
        name T.Text    Asc

        UniqueTag post name

    User
        name  T.Text Maybe Update
        email T.Text Maybe Update
        admin Bool default=false Eq Update

    Ident
        ident T.Text Asc
        user  UserId Eq

        UniqueIdent ident
    |]

data Document = Document
    { post :: Post
    , tags :: [Tag]
    }

instance Eq Document where
    (Document p1 _) == (Document p2 _) = postSlug p1 == postSlug p2

data Collection = Collection
    { name      :: T.Text
    , documents :: [Document]
    }

collect :: [Document] -> T.Text -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

collectByTagName :: [Document] -> [Collection]
collectByTagName docs = map (\tag -> collect docs tag (hasTag tag)) . nub . map tagName $ concatMap tags docs

hasTag :: T.Text -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
