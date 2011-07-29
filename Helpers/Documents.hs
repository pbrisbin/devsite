{-# LANGUAGE TemplateHaskell #-}
module Helpers.Documents
    ( lookupDocument
    , documentNavigation
    , shortDocument
    , longDocument
    , unpublishedDocument
    , documentsList
    , humanReadableTime
    ) where

import DevSite

import Yesod.Goodies.Links
import Yesod.Goodies.Markdown
import Yesod.Goodies.Time
import Yesod.Goodies.Shorten (shorten)
import Yesod.Comments

import Control.Monad    (unless)
import Data.Text        (Text)
import System.Directory (doesFileExist)

shortDocument :: Document -> Widget ()
shortDocument d@(Document p _) = addWidget $(widgetFile "shortdocument")

lookupDocument :: Text -> [Document] -> Maybe Document
lookupDocument slug docs =
    case filter ((== slug) . postSlug . post) docs of
        []    -> Nothing
        (x:_) -> Just x

documentNavigation :: Document -> [Document] -> (Maybe Document, Maybe Document)
documentNavigation d (d1:d2:[])
    -- exactly 2 items
    | d == d1   = (Nothing, Just d2)
    | d == d2   = (Just d1, Nothing)
    | otherwise = (Nothing, Nothing)

documentNavigation d (d1:d2:d3:ds)
    -- 3+ items
    | d == d1   = (Nothing, Just d2)
    | d == d2   = (Just d1, Just d3)
    | otherwise = documentNavigation d (d2:d3:ds)

-- 0 or 1 item
documentNavigation _ _ = (Nothing, Nothing)

longDocument :: Document -> (Maybe Document, Maybe Document) -> Widget ()
longDocument d@(Document p ts) (mprev, mnext) = do
    let file = pandocFile $ postSlug p

    documentContent <- lift . liftIO $ do
        exists <- doesFileExist file
        if exists
            then markdownFromFile file
            else return $ postDescr p

    setTitle $ postTitle p
    addKeywords $ postTitle p : map tagName ts
    addWidget $(widgetFile "longdocument")

documentInfo :: Document -> Widget ()
documentInfo (Document p ts) = do
    timeDiff <- lift $ humanReadableTime $ postDate p
    addWidget $(widgetFile "documentinfo")

-- | if the post is not found in the db
unpublishedDocument :: Text -> Widget ()
unpublishedDocument slug = do
    let file = pandocFile $ slug
    exists <- lift . liftIO $ doesFileExist file
    unless exists (lift notFound)
    documentContent <- lift . liftIO $ markdownFromFile file
    setTitle slug
    addWidget $(widgetFile "unpublished")

documentsList :: [Document] -> Widget ()
documentsList []   = return ()
documentsList docs = addWidget $(widgetFile "documentslist")
