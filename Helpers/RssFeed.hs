{-# LANGUAGE QuasiQuotes #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.RssFeed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Generating rss news feeds. In a very similar fashion as
-- Yesod.Helpers.AtomFeed.
--
---------------------------------------------------------

module Helpers.RssFeed
    ( RssFeed (..)
    , RssFeedEntry (..)
    , rssFeed
    , RepRss (..)
    ) where

import Yesod

-- | This would normally be added in Yesod.Content
typeRss :: ContentType
typeRss = "application/rss+xml"

newtype RepRss = RepRss Content
instance HasReps RepRss where
    chooseRep (RepRss c) _ = return (typeRss, c)

rssFeed :: RssFeed (Route master) -> GHandler sub master RepRss
rssFeed = fmap RepRss . hamletToContent . template

data RssFeed url = RssFeed
    { rssTitle       :: String
    , rssLinkSelf    :: url
    , rssLinkHome    :: url
    , rssDescription :: String
    , rssLanguage    :: String
    , rssUpdated     :: String
    , rssEntries     :: [RssFeedEntry url]
    }

data RssFeedEntry url = RssFeedEntry
    { rssEntryLink    :: url
    , rssEntryUpdated :: String
    , rssEntryTitle   :: String
    , rssEntryContent :: Html
    }

template :: RssFeed url -> Hamlet url
template arg = [$xhamlet|
%rss!version="2.0"!xmlns:atom="http://www.w3.org/2005/Atom"

    %channel
        %atom:link!href=@rssLinkSelf.arg@!rel="self"!type="application/rss+xml"
        %title         $rssTitle.arg$
        %link          @rssLinkHome.arg@
        %description   $rssDescription.arg$
        %lastBuildDate $rssUpdated.arg$
        %language      $rssLanguage.arg$

        $forall rssEntries.arg entry
            ^entryTemplate.entry^
|]

entryTemplate :: RssFeedEntry url -> Hamlet url
entryTemplate arg = [$xhamlet|
%item
    %title       $rssEntryTitle.arg$
    %link        @rssEntryLink.arg@
    %guid        @rssEntryLink.arg@
    %pubDate     $rssEntryUpdated.arg$
    %description $rssEntryContent.arg$
|]
