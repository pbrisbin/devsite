{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Helpers.Documents
    ( shortDocument
    , longDocument
    , unpublishedDocument
    ) where

import Yesod
import DevSite
import Model

import Yesod.Markdown   (Markdown(..))
import Control.Monad    (unless)
import Data.Time        (getCurrentTime)
import System.Directory (doesFileExist)

import qualified Settings

-- | The sub template for a single post
shortDocument :: Document -> Widget ()
shortDocument (Document p ts) = [hamlet|
    <article>
        <p>
            <a href="@{PostR $ postSlug p}">#{postTitle p}
        #{markdownToHtml $ Markdown $ postDescr p}
        ^{docInfo p ts}
    |]

longDocument :: Document -> Widget ()
longDocument (Document p ts) = do
    let file = Settings.pandocFile $ postSlug p

    documentContent <- lift . liftIO $ do
        exists <- doesFileExist file
        if exists
            then readFile file
            else return $ postDescr p

    Settings.setTitle $ postTitle p
    addKeywords $ postTitle p : map tagName ts

    addJulius [julius|
        var disqus_shortname  = 'pbrisbin';
        var disqus_identifier = '#{postSlug p}';
        var disqus_title      = '#{postTitle p}';
        |]

    [hamlet|
        <header>
            <h1>#{postTitle p}

        <article .fullpage>
            #{markdownToHtml $ Markdown $ documentContent}
            ^{docInfo p ts}

        <h3>
            <a href="#Comments" id="Comments">Comments

        <div id="disqus_thread">
            <script type="text/javascript" src="http://pbrisbin.disqus.com/embed.js">
            <noscript>
                <p>
                    <small>
                        <em>Sadly, javascript is required for comments on this site.
        |]

docInfo :: Post -> [Tag] -> Widget ()
docInfo p ts = do
    timeDiff <- lift $ humanReadableTimeDiff $ postDate p
    [hamlet|
        <footer>
            <p>
                <small>
                    published #{timeDiff}

                    $if not $ null ts
                        <span .tag_list>
                            tags: 

                            $forall tag <- init ts
                                <a href="@{TagR $ tagName tag}">#{tagName tag}
                                , 

                            <a href="@{TagR $ tagName $ last ts}">#{tagName $ last $ ts}
        |]

-- | if the post is not found in the db
unpublishedDocument :: String -> Widget ()
unpublishedDocument slug = do
    let file = Settings.pandocFile $ slug
    exists <- lift . liftIO $ doesFileExist file

    unless exists (lift notFound)

    now <- liftIO getCurrentTime
    longDocument $ Document (Post slug now ("unpublished: " ++ slug) []) []
