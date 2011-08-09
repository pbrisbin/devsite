{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tags
    ( getTagsR
    , getTagR
    ) where

import DevSite
import Helpers.Documents
import Yesod.Helpers.RssFeed (rssLink)
import Data.Text (Text)
import qualified Data.Text as T

getTagsR :: Handler RepHtml
getTagsR = do
    docs <- siteDocs =<< getYesod
    let collections = collectByTagName docs
    defaultLayout $ do
        setTitle "All Tags"
        addKeywords $ map name collections
        addWidget $(widgetFile "tags")

    where
        proper :: Text -> Text
        proper = T.unwords . map capitalize . T.words

        capitalize :: Text -> Text
        capitalize w = let (x,xs) = T.splitAt 1 w
            in (T.toUpper x) `T.append` xs

        helper 1 = "1 post"
        helper n = show n ++ " posts"

getTagR :: Text -> Handler RepHtml
getTagR tag' = do
    let tag = T.toLower tag'
    docs' <- siteDocs =<< getYesod
    case filter (hasTag tag) docs' of
        []   -> notFound
        docs -> defaultLayout $ do
            rssLink (FeedTagR tag) ("rss feed for tag " ++ T.unpack tag)
            setTitle $ "Tag: " `T.append` tag
            addKeywords [tag]
            addWidget $(widgetFile "tag")
