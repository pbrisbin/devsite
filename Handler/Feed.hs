{-# LANGUAGE OverloadedStrings #-}
module Handler.Feed 
    ( getFeedR
    , getFeedTagR
    ) where

import Foundation
import Yesod.RssFeed
import Data.Text (Text)
import System.Directory (doesFileExist)

getFeedR :: Handler RepRss
getFeedR = do
    docs' <- siteDocs =<< getYesod
    case docs' of
        []   -> notFound
        docs -> feedFromDocs $ take 10 docs

-- | Limited to a tag
getFeedTagR :: Text -> Handler RepRss
getFeedTagR tag = do
    docs' <- siteDocs =<< getYesod
    case filter (hasTag tag) docs' of
        []   -> notFound
        docs -> feedFromDocs docs

feedFromDocs :: [Document] -> Handler RepRss
feedFromDocs docs = do
    entries <- mapM docToRssEntry docs
    rssFeed Feed
        { feedTitle       = "pbrisbin dot com"
        , feedDescription = "New posts on pbrisbin dot com"
        , feedLanguage    = "en-us"
        , feedLinkSelf    = FeedR
        , feedLinkHome    = RootR
        , feedUpdated     = postDate . post $ head docs
        , feedEntries     = entries
        }

docToRssEntry :: Document -> Handler (FeedEntry DevSiteRoute)
docToRssEntry (Document p _) = do
    let file = pandocFile $ postSlug p

    mkd <- liftIO $ do
        exists <- doesFileExist file
        if exists
            then markdownFromFile file
            else return $ postDescr p

    return FeedEntry
        { feedEntryLink    = PostR $ postSlug p
        , feedEntryUpdated = postDate  p
        , feedEntryTitle   = postTitle p
        , feedEntryContent = plainText $ mkd
        }

        where
            plainText :: Markdown -> Html
            plainText (Markdown s) = toHtml s
