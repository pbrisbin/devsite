{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.Documents
    ( lookupDocument
    , documentsList
    , pageDocument
    , inlineDocument
    , unpublishedDocument
    ) where

import Foundation
import Yesod.Comments

import Control.Monad    (unless)
import Data.Text        (Text)
import System.Directory (doesFileExist)

lookupDocument :: Text -> [Document] -> Maybe Document
lookupDocument slug docs =
    case filter ((== slug) . postSlug . post) docs of
        []    -> Nothing
        (x:_) -> Just x

pageDocument :: Document -> [Document] -> Handler RepHtml
pageDocument doc@(Document p ts) docs = do
    let (mprev, mnext) = documentNavigation doc docs

    dContent <- documentContent doc

    defaultLayout $ do
        setTitle $ postTitle p
        addKeywords $ postTitle p : map tagName ts
        addWidget $(widgetFile "document/page")

    where
        -- find next/previous in a list of documents
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

inlineDocument :: Document -> Widget
inlineDocument doc@(Document p _) = do
    dContent <- lift $ documentContent doc
    addWidget $(widgetFile "document/inline")

-- | if the post is not found in the db
unpublishedDocument :: Text -> Handler RepHtml
unpublishedDocument slug = do
    let file = pandocFile $ slug
    exists <- liftIO $ doesFileExist file
    unless exists notFound

    dContent <- liftIO $ markdownFromFile file

    defaultLayout $ do
        setTitle slug
        addWidget $(widgetFile "document/unpublished")

documentContent :: Document -> Handler Html
documentContent (Document p _) = do
    let file = pandocFile $ postSlug p

    mkd <- liftIO $ do
        exists <- doesFileExist file
        if exists
            then markdownFromFile file
            else return $ postDescr p

    return $ markdownToHtml mkd

published :: Document -> Widget
published (Document p _) = do
    timeDiff <- lift . liftIO . humanReadableTime $ postDate p
    addWidget $(widgetFile "document/_published")

taggedWith :: Document -> Widget
taggedWith (Document _ []) = [whamlet|empty|]
taggedWith (Document _ tags) = addWidget $(widgetFile "document/_tagged_with")

-- | Used in admin page
documentsList :: [Document] -> Widget
documentsList []   = return ()
documentsList docs = addWidget $(widgetFile "posts_admin/_list")
