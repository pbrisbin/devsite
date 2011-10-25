{-# LANGUAGE OverloadedStrings #-}
module Handler.Feed 
    ( getFeedR
    , getFeedTagR
    ) where

import Foundation
import Yesod.RssFeed
import Data.Text           (Text)
import System.Directory    (doesFileExist)
import Text.Blaze          (preEscapedText)
import Text.Blaze.Internal (HtmlM (Append))

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

-- | Note: does not gracefully handle a post with no pandoc or in-db
--   content
docToRssEntry :: Document -> Handler (FeedEntry DevSiteRoute)
docToRssEntry (Document p _) = do
    let file = pandocFile $ postSlug p

    mkd <- liftIO $ do
        exists <- doesFileExist file
        case (exists, postDescr p) of
            (True, _         ) -> markdownFromFile file
            (_   , Just descr) -> return descr
            _                  -> return $ Markdown ""

    return FeedEntry
        { feedEntryLink    = PostR $ postSlug p
        , feedEntryUpdated = postDate  p
        , feedEntryTitle   = postTitle p
        , feedEntryContent = cdata mkd
        }

        where
            -- Rss validation warnings on "relative links" that I have
            -- in the markdown. Shows as one big blob (no line-breaks)
            -- in Google reader. Looks great in snownews.
            plainText :: Markdown -> Html
            plainText (Markdown s) = toHtml s

            -- Should appear as formatted HTML in readers that support
            -- that.Rss validation errors on script tage used by
            -- markdown conversion to obfuscate an email. Looks pretty
            -- bad in snownews (but readable) -- not sure about Google
            -- yet.
            cdata :: Markdown -> Html
            cdata mkd = let prefix  = preEscapedText "<![CDATA["
                            content = markdownToHtml mkd
                            suffix  = preEscapedText "]]>" in Append prefix $ Append content suffix
