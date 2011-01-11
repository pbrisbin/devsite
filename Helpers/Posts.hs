{-# LANGUAGE QuasiQuotes                #-}
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
    , getPostBySlug
    , getPostsByTag
    , allPostsTemplate
    , postTemplate
    , migratePosts
    , runPostForm
    , deletePost
    ) where

import DevSite

import Yesod
import Yesod.Markdown

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
    (writePandoc yesodDefaultWriterOptions <$>) . localLinks . parseMarkdown yesodDefaultParserState $ Markdown markdown

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
            , postTags  = parseCSL $ formTags pf
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

-- | Take a comma-separated list of tags like "foo, bar, baz" and parse
--   that into a real haskell list
parseCSL :: String -> [String]
parseCSL = filter (/= []) . parseCSL' [] []
    where
        parseCSL' :: [String] -> String -> String -> [String]
        parseCSL' acc1 acc2 []         = acc1 ++ [trim acc2]
        parseCSL' acc1 acc2 (',':rest) = parseCSL' (acc1 ++ [trim acc2]) [] rest
        parseCSL' acc1 acc2 (x:rest)   = parseCSL' acc1 (acc2 ++ [x]) rest

        trim = f . f where f = reverse . dropWhile isSpace

-- | Run the post form and insert or update based on the entered data
runPostForm :: Maybe Post -> Handler (Hamlet DevSiteRoute)
runPostForm post = do
    ((res, form), enctype) <- runFormMonadPost $ postForm post
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> do
            updatePostFromForm post pf

    -- this feels kludgy...
    return . pageBody =<< widgetToPageContent (managePostTemplate title form enctype)

    where

        title = if isJust post then "Edit post:" else "Add new post:"

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
        %tr
            %th
                %label!for=$fiIdent.fiSlug$ $fiLabel.fiSlug$
                .tooltip $fiTooltip.fiSlug$
            %td
                ^fiInput.fiSlug^
            %td.errors
                $maybe fiErrors.fiSlug error
                    $error$
                $nothing
                    &nbsp;
        %tr
            %th
                %label!for=$fiIdent.fiTitle$ $fiLabel.fiTitle$
                .tooltip $fiTooltip.fiTitle$
            %td
                ^fiInput.fiTitle^
            %td.errors
                $maybe fiErrors.fiTitle error
                    $error$
                $nothing
                    &nbsp;
        %tr
            %th
                %label!for=$fiIdent.fiTags$ $fiLabel.fiTags$
                .tooltip $fiTooltip.fiTags$
            %td
                ^fiInput.fiTags^
            %td.errors
                $maybe fiErrors.fiTags error
                    $error$
                $nothing
                    &nbsp;
        %tr
            %th
                %label!for=$fiIdent.fiDescription$ $fiLabel.fiDescription$
                .tooltip $fiTooltip.fiDescription$
            %td
                ^fiInput.fiDescription^
            %td.errors
                $maybe fiErrors.fiDescription error
                    $error$
                $nothing
                    &nbsp;
        %tr
            %td
                &nbsp;
            %td!colspan="2"
                %input!type="submit"!value=$buttonText$
    |])

    where

        formatTags = intercalate ", "
        buttonText = string $ if isJust post then "Update post" else "Add post"

-- | The overall template showing the input box and a list of existing
--   posts
managePostTemplate :: String -> Widget () -> Enctype -> Widget ()
managePostTemplate title form enctype = do
    posts <- liftHandler $ selectPosts 0
    [$hamlet|
    #post_input
        %h3 $string.title$

        %form!enctype=$enctype$!method="post"
            ^form^

    #post_existing
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
        shortenLong  = shorten 50 
        shortenShort = shorten 20
        shorten n s  = if length s > n then take n s ++ "..." else s

-- | A body template for a list of posts, you can also provide the title
allPostsTemplate :: [(Post, UTCTime)] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$

#posts
    $forall posts post
        ^postTemplate.post^
|]

-- | The sub template for a single post
postTemplate :: (Post, UTCTime) -> Hamlet DevSiteRoute
postTemplate (post, curTime) = [$hamlet|
.post
  %p

    %a!href=@PostR.postSlug.post@ $postTitle.post$
    \ - $postDescr.post$ 
    
  %p.small

    Published $formatDateTime.postDate.post$

    %span!style="float: right;"

      Tags: 

      $forall postTags.post tag

        %a!href=@TagR.tag@ $tag$ 
|]
    where
        formatDateTime :: UTCTime -> String
        formatDateTime = humanReadableTimeDiff curTime

-- https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs
-- https://github.com/snoyberg/haskellers/blob/master/LICENSE
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
    hours   n = (minutes n) / 60

    days :: NominalDiffTime -> Double
    days    n = (hours n) / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = (days n) / 7

    years :: NominalDiffTime -> Double
    years   n = (days n) / 365

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
