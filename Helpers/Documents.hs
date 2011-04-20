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
shortDocument (Document post tags) = [hamlet|
    <article>
        <p>
            <a href="@{PostR $ postSlug post}">#{postTitle post}
        #{markdownToHtml $ Markdown $ postDescr post}
        ^{docInfo post tags}
    |]

longDocument :: Document -> Widget ()
longDocument (Document post tags) = do
    let file = Settings.pandocFile $ postSlug post

    documentContent <- lift . liftIO $ do
        exists <- doesFileExist file
        if exists
            then readFile file
            else return $ postDescr post

    Settings.setTitle $ postTitle post
    addKeywords $ postTitle post : map tagName tags

    addJulius [julius|
        var disqus_shortname  = 'pbrisbin';
        var disqus_identifier = '#{postSlug post}';
        var disqus_title      = '#{postTitle post}';
        |]

    [hamlet|
        <header>
            <h1>#{postTitle post}

        <article .fullpage>
            #{markdownToHtml $ Markdown $ documentContent}
            ^{docInfo post tags}

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
docInfo post tags = do
    timeDiff <- lift $ humanReadableTimeDiff $ postDate post
    [hamlet|
        <footer>
            <p>
                <small>
                    published #{timeDiff}

                    $if not $ null tags
                        <span .tag_list>
                            tags: 

                            $forall tag <- init tags
                                <a href="@{TagR $ tagName tag}">#{tagName tag}
                                , 

                            <a href="@{TagR $ tagName $ last tags}">#{tagName $ last $ tags}
        |]

-- | if the post is not found in the db
unpublishedDocument :: String -> Widget ()
unpublishedDocument slug = do
    let file = Settings.pandocFile $ slug
    exists <- lift . liftIO $ doesFileExist file

    unless exists (lift notFound)

    now <- liftIO getCurrentTime
    longDocument $ Document (Post slug now ("unpublished: " ++ slug) []) []
