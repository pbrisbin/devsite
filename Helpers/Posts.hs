{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Helpers.Posts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Helpers.Posts
    ( Post (..)
    , loadPostContent
    , selectPosts
    , insertPost
    , deletePost
    , getPostBySlug
    , getPostsByTag
    , runPostForm
    , allPostsTemplate
    , postTemplate
    , addPostContent
    , migratePosts
    , humanReadableTimeDiff
    ) where

import DevSite

import Yesod
import Yesod.Markdown
import Helpers.RssFeed

import Data.Time
import System.Locale

import Data.Char           (isSpace)
import Data.List           (intercalate)
import Data.Maybe          (isJust, fromJust)
import System.Directory    (doesFileExist)
import Control.Applicative ((<$>), (<*>))

import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

import Language.Haskell.TH.Syntax

import qualified Settings

-- | The data type of a single post
data Post = Post
    { postSlug  :: String
    , postDate  :: UTCTime
    , postTitle :: String
    , postDescr :: String
    , postTags  :: [String]
    }

-- | Used in the new post form
data PostForm = PostForm
    { formSlug  :: String
    , formTitle :: String
    , formTags  :: String
    , formDescr :: Textarea
    }

-- | Generate data base instances for post meta-data
share2 mkPersist (mkMigrate "migratePosts") [$persist|
SqlPost
    slug        String
    date        UTCTime Desc
    title       String
    descr       String
    UniqueSqlPost slug
SqlTag
    post SqlPostId Eq
    name String Asc
|]

-- | Select n recent posts from the database and return them
selectPosts :: Int -> Handler [Post]
selectPosts n = mapM go =<< runDB (selectList [] [SqlPostDateDesc] n 0)

    where
        go :: (Key SqlPost, SqlPost) -> Handler Post
        go (sqlPostKey, sqlPost) = do
            -- tags for this post
            sqlTags <- runDB $ selectList [SqlTagPostEq sqlPostKey] [SqlTagNameAsc] 0 0
            return Post
                { postSlug  = sqlPostSlug  sqlPost
                , postDate  = sqlPostDate  sqlPost
                , postTitle = sqlPostTitle sqlPost
                , postDescr = sqlPostDescr sqlPost
                , postTags  = fmap (sqlTagName . snd) sqlTags
                }

-- | Insert a post into the database
insertPost :: Post -> Handler ()
insertPost post = do
    let sqlPost = SqlPost
            { sqlPostSlug  = postSlug post
            , sqlPostDate  = postDate post
            , sqlPostTitle = postTitle post
            , sqlPostDescr = postDescr post
            }

    -- insert the Post record
    sqlPostKey <- runDB $ insert sqlPost

    -- insert each tag record
    mapM_ (go sqlPostKey) $ postTags post
    
    where
        go :: SqlPostId -> String -> Handler SqlTagId
        go key tag = runDB (insert $ SqlTag key tag)

-- | Delete an existing post by slug
deletePost :: String -> Handler ()
deletePost slug = do
    sqlPost <- runDB $ getBy $ UniqueSqlPost slug
    case sqlPost of
        Just (sqlPostKey, _) -> do
            -- delete the post and the tags
            runDB $ deleteBy $ UniqueSqlPost slug
            runDB $ deleteWhere [SqlTagPostEq sqlPostKey]
        Nothing -> return ()

-- | Locate posts with a given slug
getPostBySlug :: String -> Handler [Post]
getPostBySlug slug = do
    allPosts <- selectPosts 0
    return $ filter ((== slug) . postSlug) allPosts

-- | Locate posts with a given tag
getPostsByTag :: String -> Handler [Post]
getPostsByTag tag = do
    allPosts <- selectPosts 0
    return $ filter (elem tag . postTags) allPosts

-- | Load a post's pandoc file and convert it to html, return not found
--   if the pdc file doesn't exist
loadPostContent :: Post -> Handler Html
loadPostContent p = do
    let fileName = Settings.pandocFile $ postSlug p
    markdown <- do
        exists <- liftIO $ doesFileExist fileName
        if exists
            then liftIO $ readFile fileName
            else return "File not found"
    (writePandoc yesodDefaultWriterOptions <$>) 
        . localLinks 
        . parseMarkdown yesodDefaultParserState 
        $ Markdown markdown

-- | Convert form input into a Post and update the db. If the first
--   argument is Just, this is an edit of an existing Post. If the first
--   argument is Nothing, then it's an insert
updatePostFromForm :: Maybe Post -> PostForm -> Handler ()
updatePostFromForm p pf = do
    postDate' <- if isJust p 
        -- preserve original publish date
        then return $ postDate $ fromJust p
        else liftIO getCurrentTime
    let post = Post
            { postSlug  = formSlug pf
            , postTitle = formTitle pf
            , postDescr = unTextarea $ formDescr pf
            , postDate  = postDate'
            , postTags  = parseTags $ formTags pf
            }
    if isJust p
        then do
            -- delete the original and insert a new version
            deletePost (postSlug post)
            insertPost post
            setMessage $ [$hamlet| %em post updated! |]
        else do
            insertPost post
            setMessage $ [$hamlet| %em post added! |]

    redirect RedirectTemporary ManagePostsR

-- | minor changes to 
--   <https://github.com/fortytools/lounge/blob/master/Handler/Entry.hs#L57>
parseTags :: String -> [String]
parseTags [] = []
parseTags s  = let (l,s') = break (==',') $ dropWhile (==',') s
    in trim l : case s' of
        []      -> []
        (_:s'') -> parseTags s''

    where 
        trim  = trim' . trim' 
        trim' = reverse . dropWhile isSpace

-- | Run the post form and insert or update based on the entered data
runPostForm :: Maybe Post -> Widget ()
runPostForm post = do
    ((res, form), enctype) <- liftHandler . runFormMonadPost $ postForm post
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> liftHandler $ updatePostFromForm post pf

    managePostTemplate title form enctype

    where title = if isJust post then "Edit post:" else "Add new post:"

-- | Display the new post form inself. If the first argument is Just,
--   then use that to prepopulate the form
postForm :: Maybe Post -> FormMonad (FormResult PostForm, Widget ())
postForm post = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   $ fmap postSlug  post
    (title      , fiTitle      ) <- stringField   "title:"       $ fmap postTitle post
    (tags       , fiTags       ) <- stringField   "tags:"        $ fmap (formatTags . postTags) post
    (description, fiDescription) <- textareaField "description:" $ fmap (Textarea . postDescr)  post
    return (PostForm <$> slug <*> title <*> tags <*> description, [$hamlet|
        %table
            ^fieldRow.fiSlug^
            ^fieldRow.fiTitle^
            ^fieldRow.fiTags^
            ^fieldRow.fiDescription^
            %tr
                %td
                    &nbsp;
                %td!colspan="2"
                    %input!type="submit"!value=$buttonText$
        |])

    where
        fieldRow fi = [$hamlet|
            %tr
                %th
                    %label!for=$fiIdent.fi$ $fiLabel.fi$
                    .tooltip $fiTooltip.fi$
                %td
                    ^fiInput.fi^
                %td
                    $maybe fiErrors.fi error
                        $error$
                    $nothing
                        &nbsp;
            |]

        formatTags = intercalate ", "
        buttonText = string $ if isJust post then "Update post" else "Add post"


-- | The overall template showing the input box and a list of existing
--   posts
managePostTemplate :: String -> Widget () -> Enctype -> Widget ()
managePostTemplate title form enctype = do
    posts <- liftHandler $ selectPosts 0
    [$hamlet|
    .post_input
        %h3 $string.title$

        %form!enctype=$enctype$!method="post"
            ^form^

    .posts_existing
        %h3 Existing posts:

        %table
            %tr
                %th Title
                %th Description
                %th Edit
                %th Delete

            $forall posts post
                %tr
                    %td 
                        %a!href=@PostR.postSlug.post@ $shortenShort.postTitle.post$
                    %td $shortenLong.postDescr.post$
                    %td
                        %a!href=@EditPostR.postSlug.post@ edit
                    %td 
                        %a!href=@DelPostR.postSlug.post@ delete
    |]

    where 
        shortenLong  = shorten 40 
        shortenShort = shorten 15 
        shorten n s  = if length s > n then take n s ++ "..." else s

-- | A body template for a list of posts, you can also provide the title
allPostsTemplate :: UTCTime -- ^ current time
                 -> [Post]  -- ^ posts to show
                 -> String  -- ^ title
                 -> Hamlet DevSiteRoute
allPostsTemplate curTime posts title = [$hamlet|
    %h1 $title$

    $forall posts post
        ^(postTemplate.curTime).post^
    |]


-- | The sub template for a single post
postTemplate :: UTCTime -- ^ current time
             -> Post    -- ^ post to show
             -> Hamlet DevSiteRoute
postTemplate curTime post = [$hamlet|
    .post
        %p
            %a!href=@PostR.postSlug.post@ $postTitle.post$
            \ - $postDescr.post$ 
        
        %p.small
            Published $formatDateTime.postDate.post$

            %span.float_right
                Tags: 

                $forall postTags.post tag
                    %a!href=@TagR.tag@ $tag$ 
    |]
    where
        formatDateTime :: UTCTime -> String
        formatDateTime = humanReadableTimeDiff curTime

-- | Add post content to the body tag
addPostContent :: Post -> Widget ()
addPostContent post = do
    curTime     <- liftHandler $ liftIO getCurrentTime
    postContent <- liftHandler $ loadPostContent post

    let prettyTime = string . humanReadableTimeDiff curTime $ postDate post

    setTitle . string $ "pbrisbin - " ++ postTitle post
    addKeywords $ ["pbrisbin", postTitle post] ++ postTags post

    addJulius [$julius|
        var disqus_shortname  = 'pbrisbin';
        var disqus_identifier = '%postSlug.post%';
        var disqus_title      = '%postTitle.post%';
        |]

    addPostBreadcrumbs post
    addHamlet [$hamlet|
        %h1 $postTitle.post$

        $postContent$

        %p.small
            %em Published $prettyTime$
        %h3 
            %a!href="#Comments"!id="Comments" Comments

        #disqus_thread
            %script!type="text/javascript"!src="http://pbrisbin.disqus.com/embed.js"
            %noscript 
                %p.small
                    %em Sadly, javascript is required for comments on this site.
    |]

-- | Add breadcrumsb and post tags together to a page
addPostBreadcrumbs :: Post -> Widget ()
addPostBreadcrumbs post = do
    (t, h) <- liftHandler breadcrumbs
    addHamlet [$hamlet|
        #breadcrumbs
            %p
                $forall h node
                    %a!href=@fst.node@ $snd.node$ 
                    \ / 
                \ $t$

                %span.float_right
                    Tags: 
                    $forall postTags.post tag
                        %a!href=@TagR.tag@ $tag$ 
    |]

-- <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>
-- <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

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

    old = utcToLocalTime utc oldTime

    trim = f . f where f = reverse . dropWhile isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper  d | d < 1          = "just now"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = "about " ++ i2s (hours d) ++ " hours ago"
              | days d < 5     = "at " ++ dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = "on " ++ thisYear
              | otherwise      = "on " ++ previousYears
