{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Helpers.Documents
    ( lookupDocument
    , documentNavigation
    , shortDocument
    , longDocument
    , unpublishedDocument
    , humanReadableTime
    ) where

import DevSite
import Model

import Yesod
import Yesod.Goodies.Links
import Yesod.Goodies.Markdown
import Yesod.Goodies.Time
import Control.Monad    (unless)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Settings

shortDocument :: Document -> Widget ()
shortDocument d@(Document p _) = [hamlet|
    <article>
        <p>^{link d}
        #{markdownToHtml $ postDescr p}
        ^{documentInfo d}
    |]

-- todo: recombine these two?

lookupDocument :: T.Text -> [Document] -> Maybe Document
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
    let file = Settings.pandocFile $ postSlug p

    documentContent <- lift . liftIO $ do
        exists <- doesFileExist file
        if exists
            then markdownFromFile file
            else return $ postDescr p

    Settings.setTitle $ postTitle p
    Settings.addKeywords $ postTitle p : map tagName ts

    addJulius [julius|
        var disqus_shortname  = 'pbrisbin';
        var disqus_identifier = '#{postSlug p}';
        var disqus_title      = '#{postTitle p}';
        |]

    [hamlet|
        <header>
            <h1>#{postTitle p}

        <article .fullpage>
            #{markdownToHtml $ documentContent}
            ^{documentInfo d}

        <h3>
            <a href="#Comments" id="Comments">Comments

        <div id="disqus_thread">
            <script type="text/javascript" src="http://pbrisbin.disqus.com/embed.js">
            <noscript>
                <p>
                    <small>
                        <em>Sadly, javascript is required for comments on this site.

        <p .post_nav>
            <span .left>
                $maybe prev <- mprev
                    &#9666&nbsp;&nbsp;&nbsp;
                    ^{link prev}
                $nothing
                    <a href="@{RootR}">Home

            <span .right>
                $maybe next <- mnext
                    ^{link next}
                    &nbsp;&nbsp;&nbsp;&#9656
        |]

documentInfo :: Document -> Widget ()
documentInfo (Document p ts) = do
    timeDiff <- lift $ humanReadableTime $ postDate p
    [hamlet|
        <footer>
            <p>
                <small>
                    published #{timeDiff}

                    $if not $ null ts
                        <span .tag_list>
                            tags: 

                            $forall tag <- init ts
                                ^{link tag}, 

                            ^{link $ last ts}
        |]

-- | if the post is not found in the db
unpublishedDocument :: T.Text -> Widget ()
unpublishedDocument slug = do
    let file = Settings.pandocFile $ slug
    exists <- lift . liftIO $ doesFileExist file
    unless exists (lift notFound)
    documentContent <- lift . liftIO $ markdownFromFile file
    Settings.setTitle slug
    [hamlet|
        <header>
            <h1>unpublished: #{slug}
        <article .fullpage>#{markdownToHtml $ documentContent}
        |]
