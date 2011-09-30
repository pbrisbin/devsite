{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Tags
    ( getTagR
    ) where

import Foundation
import Helpers.Documents
import Yesod.RssFeed (rssLink)
import Data.Text (Text)
import qualified Data.Text as T

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
