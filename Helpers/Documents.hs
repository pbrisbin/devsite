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
import Yesod.Comments.Markdown
import Helpers.Links
import Control.Monad    (unless)
import Data.Char        (isSpace)
import System.Directory (doesFileExist)

import Data.Time
import System.Locale

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
documentNavigation _ []  = (Nothing, Nothing)
documentNavigation _ [_] = (Nothing, Nothing)

documentNavigation d (d1:d2:[]) =
    if d == d1
        then (Nothing, Just d2)
        else if d == d2
            then (Just d1, Nothing)
            else (Nothing, Nothing)

documentNavigation d (d1:d2:d3:ds) =
    if d == d1
        then (Nothing, Just d2)
        else if d == d2
            then (Just d1, Just d3)
            else documentNavigation d (d2:d3:ds)

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

-- | Based on 
--   <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs> 
--   <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTime :: UTCTime -> GHandler s m String
humanReadableTime t = return . helper . flip diffUTCTime t =<< liftIO getCurrentTime

    where
        minutes :: NominalDiffTime -> Double
        minutes n = realToFrac $ n / 60

        hours :: NominalDiffTime -> Double
        hours   n = minutes n / 60

        days :: NominalDiffTime -> Double
        days    n = hours n / 24

        weeks :: NominalDiffTime -> Double
        weeks   n = days n / 7

        years :: NominalDiffTime -> Double
        years   n = days n / 365

        i2s :: RealFrac a => a -> String
        i2s n = show m where m = truncate n :: Int

        trim = f . f where f = reverse . dropWhile isSpace

        old           = utcToLocalTime utc t
        dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
        thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
        previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

        helper d 
            | d         < 1  = "just now"
            | d         < 60 = i2s d ++ " seconds ago"
            | minutes d < 2  = "one minute ago"
            | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
            | hours d   < 2  = "one hour ago"
            | hours d   < 24 = "about " ++ i2s (hours d) ++ " hours ago"
            | days d    < 5  = "at " ++ dow
            | days d    < 10 = i2s (days d)  ++ " days ago"
            | weeks d   < 2  = i2s (weeks d) ++ " week ago"
            | weeks d   < 5  = i2s (weeks d) ++ " weeks ago"
            | years d   < 1  = "on " ++ thisYear
            | otherwise      = "on " ++ previousYears
