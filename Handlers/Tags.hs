{-# LANGUAGE QuasiQuotes #-}
module Handlers.Tags
    ( getTagsR
    , getTagR
    ) where

import DevSite
import Model
import Helpers.Documents
import Yesod
import Yesod.Helpers.RssFeed (rssLink)
import Data.Char             (toLower,toUpper)
import Data.List             (sortBy)
import Data.Ord              (comparing)
import qualified Settings

-- | All tags
getTagsR :: Handler RepHtml
getTagsR = do
    docs <- siteDocs =<< getYesod
    let collections = sortByLength $ collectByTagName docs
    defaultLayout $ do
        Settings.setTitle "All Tags"
        addKeywords $ map name collections
        [hamlet|
            <header>
                <h1>All Tags

            <article .fullpage>
                <div id="accordion">
                    $forall collection <- collections
                        ^{addCollection collection}

            <script src="//ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js">
            <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.9/jquery-ui.min.js">
            <script>
                $(function() {
                    $("#accordion").accordion({
                        collapsible: true,
                        autoHeight:  false,
                        active:      false
                    });
                });

            |]

    where
        sortByLength :: [Collection] -> [Collection]
        sortByLength = reverse . sortBy (comparing (length . documents))

addCollection :: Collection -> Widget ()
addCollection collection = do
    let len = helper . length $ documents collection
    [hamlet|
        <h3>#{proper $ name collection} 
            <span .post_count>- #{len}
        <div .hidden>
            $forall doc <- documents collection
                ^{shortDocument doc}
        |]

    where
        -- tag name -> Tag Name
        proper = unwords . map capitalize . words

        capitalize []     = []
        capitalize (x:xs) = (toUpper x) : xs

        helper 1 = "1 post"
        helper n = show n ++ " posts"

-- | A tag
getTagR :: String -> Handler RepHtml
getTagR tag' = do
    let tag = map toLower tag'
    docs' <- siteDocs =<< getYesod
    case filter (hasTag tag) docs' of
        []   -> notFound
        docs -> defaultLayout $ do
            Settings.setTitle $ "Tag: " ++ tag
            addKeywords [tag]
            rssLink (FeedTagR tag) ("rss feed for tag " ++ tag)
            [hamlet|
                <header>
                    <h1>Tag: #{tag}

                <article .fullpage>
                    $forall doc <- docs
                        ^{shortDocument doc}
                |]
