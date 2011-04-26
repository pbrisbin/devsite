{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Helpers.Documents
    ( Link(..)
    , linkFromPost
    , linkFromDocument
    , showLink
    , shortDocument
    , longDocument
    , unpublishedDocument
    ) where

import Yesod
import DevSite
import Model

import Yesod.Comments.Markdown
import Control.Monad    (unless)
import System.Directory (doesFileExist)

import qualified Data.Text as T
import qualified Settings

-- | A link
data Link a = Link
    { route :: a
    , title :: T.Text
    }

linkFromPost :: Post -> Link DevSiteRoute
linkFromPost p = Link (PostR $ postSlug p) (postTitle p)

linkFromDocument :: Document -> Link DevSiteRoute
linkFromDocument = linkFromPost . post

showLink :: Link DevSiteRoute -> Widget ()
showLink l = [hamlet|<a title="#{title l}" href="@{route l}">#{title l}|]

-- | The sub template for a single post
shortDocument :: Document -> Widget ()
shortDocument (Document p ts) = [hamlet|
    <article>
        <p>^{showLink $ linkFromPost p}
        #{markdownToHtml $ postDescr p}
        ^{docInfo p ts}
    |]

longDocument :: Document                  -- ^ document to display
             -> Maybe (Link DevSiteRoute) -- ^ maybe route to previous post
             -> Maybe (Link DevSiteRoute) -- ^ maybe route to next post
             -> Widget ()
longDocument (Document p ts) mprev mnext = do
    let file = Settings.pandocFile $ postSlug p

    documentContent <- lift . liftIO $ do
        exists <- doesFileExist file
        if exists
            then markdownFromFile file
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
            #{markdownToHtml $ documentContent}
            ^{docInfo p ts}

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
                    ^{showLink prev}
                $nothing
                    <a href="@{RootR}">Home

            <span .right>
                $maybe next <- mnext
                    ^{showLink next}
                    &nbsp;&nbsp;&nbsp;&#9656
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
