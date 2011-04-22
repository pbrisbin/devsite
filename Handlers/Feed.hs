{-# LANGUAGE OverloadedStrings #-}
module Handlers.Feed 
    ( getFeedR
    , getFeedTagR
    ) where

import DevSite
import Model
import Yesod
import Yesod.Helpers.RssFeed
import Text.Blaze (toHtml)

import qualified Data.Text as T

-- | Rss feed
getFeedR :: Handler RepRss
getFeedR = do
    docs' <- siteDocs =<< getYesod
    case docs' of
        []   -> notFound
        docs -> feedFromDocs $ take 10 docs

-- | Rss feed, limited to a tag
getFeedTagR :: String -> Handler RepRss
getFeedTagR tag = do
    docs' <- siteDocs =<< getYesod
    case filter (hasTag tag) docs' of
        []   -> notFound
        docs -> feedFromDocs docs

feedFromDocs :: [Document] -> Handler RepRss
feedFromDocs docs = rssFeed Feed
    { feedTitle       = "pbrisbin dot com"
    , feedDescription = "New posts on pbrisbin dot com"
    , feedLanguage    = "en-us"
    , feedLinkSelf    = FeedR
    , feedLinkHome    = RootR
    -- note: posts is known to be not empty coming in
    , feedUpdated     = postDate . post $ head docs
    , feedEntries     = map docToRssEntry docs
    }

docToRssEntry :: Document -> FeedEntry DevSiteRoute
docToRssEntry (Document p _) = FeedEntry
    { feedEntryLink    = PostR $ postSlug p
    , feedEntryUpdated = postDate  p
    , feedEntryTitle   = T.pack $ postTitle p
    , feedEntryContent = toHtml $ postDescr p
    }
