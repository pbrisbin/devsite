{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tags
    ( getTagsR
    , getTagR
    ) where

import DevSite
import Model
import Helpers.Documents
import Yesod
import Yesod.Helpers.RssFeed (rssLink)
import Data.List             (sortBy)
import Data.Ord              (comparing)

import qualified Data.Text as T
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
        proper :: T.Text -> T.Text
        proper = T.unwords . map capitalize . T.words

        capitalize :: T.Text -> T.Text
        capitalize w = let (x,xs) = T.splitAt 1 w
            in (T.toUpper x) `T.append` xs

        helper 1 = "1 post"
        helper n = show n ++ " posts"

-- | A tag
getTagR :: T.Text -> Handler RepHtml
getTagR tag' = do
    let tag = T.toLower tag'
    docs' <- siteDocs =<< getYesod
    case filter (hasTag tag) docs' of
        []   -> notFound
        docs -> defaultLayout $ do
            Settings.setTitle $ "Tag: " `T.append` tag
            addKeywords [tag]
            rssLink (FeedTagR tag) ("rss feed for tag " ++ T.unpack tag)
            [hamlet|
                <header>
                    <h1>Tag: #{tag}

                <article .fullpage>
                    $forall doc <- docs
                        ^{shortDocument doc}
                |]
