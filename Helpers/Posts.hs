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
    ( loadPosts
    , mkTagGroups
    , loadPostContent
    , runPostForm
    , insertPost
    , deletePost
    , addPostBlock
    , addPostContent
    , migratePosts
    , humanReadableTimeDiff
    ) where

import DevSite
import Helpers.PostTypes

import Yesod
import Yesod.Markdown
import Yesod.Form.Core

import Data.Time
import System.Locale

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (liftM, forM)
import Data.Char           (isSpace)
import Data.List           (nub, intercalate, sortBy, concatMap)
import Data.Maybe          (isJust, fromJust)
import Data.Ord            (comparing)
import System.Directory    (doesFileExist)

import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

import Language.Haskell.TH.Syntax

import qualified Settings

-- | Used by the new post form
data PostForm = PostForm
    { formSlug  :: String
    , formTitle :: String
    , formTags  :: String
    , formDescr :: Markdown
    }

-- | Generate data base instances for post meta-data
share2 mkPersist (mkMigrate "migratePosts") [$persist|
SqlPost
    slug        String Eq
    date        UTCTime Desc
    title       String
    descr       String
    UniqueSqlPost slug
SqlTag
    post SqlPostId Eq
    name String Asc
|]

-- | Select posts from the db, most recent first
loadPosts :: Handler [Post]
loadPosts = mapM go =<< runDB (selectList [] [SqlPostDateDesc] 0 0)
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

-- | Return all tags sorted by number of posts.
mkTagGroups :: [Post] -> [TagGroup]
mkTagGroups posts = sortByNumPosts . go . nub $ concatMap postTags posts 
        where 
            go :: [Tag] -> [TagGroup] 
            go []     = []
            go (t:ts) = (t, filter (elem t . postTags) posts) : go ts

            sortByNumPosts :: [TagGroup] -> [TagGroup]
            sortByNumPosts = reverse . sortBy (comparing (length . snd))


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

-- | Load a post's pandoc file and convert it to html, return the post 
--   decription text if the pdc file doesn't exist
loadPostContent :: Post -> Handler Html
loadPostContent p = do
    let fileName = Settings.pandocFile $ postSlug p
    markdown <- liftIO $ do
        exists <- doesFileExist fileName
        if exists
            then readFile fileName
            else return $ postDescr p
    markdownToHtml $ Markdown markdown

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
            , postDescr = fromMarkdown $ formDescr pf
            , postDate  = postDate'
            , postTags  = parseTags $ formTags pf
            }
    if isJust p
        then do
            -- delete the original and insert a new version
            deletePost (postSlug post)
            insertPost post
            setMessage $ [$hamlet| post updated! |]
        else do
            insertPost post
            setMessage $ [$hamlet| post added! |]

    redirect RedirectTemporary ManagePostsR

    where
        fromMarkdown (Markdown s) = s

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
    (description, fiDescription) <- markdownField "description:" $ fmap (Markdown . postDescr)  post
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
    posts <- liftHandler $ sitePosts =<< getYesod
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
        shortenLong  = shorten 60
        shortenShort = shorten 20
        shorten n s  = if length s > n then take n s ++ "..." else s

-- | The sub template for a single post
addPostBlock :: Post -> Widget ()
addPostBlock post = do
    postDescription <- liftHandler . markdownToHtml . Markdown $ postDescr post
    [$hamlet|
        .post
            .post_title
                %p
                    %a!href=@PostR.postSlug.post@ $postTitle.post$

            .post_description 
                $postDescription$

            .post_info
                ^postInfo.post^

        |]

-- | A sub template for the posted time and tags
postInfo :: Post -> Widget ()
postInfo post = do
    curTime <- liftHandler $ liftIO getCurrentTime
    [$hamlet|
        %p
            published $(humanReadableTimeDiff.curTime).postDate.post$

            %span.tag_list
                tags: 

                $forall init.postTags.post tag
                    %a!href=@TagR.tag@ $tag$
                    , 

                %a!href=@TagR.last.postTags.post@ $last.postTags.post$
        |]

-- | Add post content to the body tag
addPostContent :: Post -> Widget ()
addPostContent post = do
    curTime     <- liftHandler $ liftIO getCurrentTime
    postContent <- liftHandler $ loadPostContent post

    let prettyTime = string . humanReadableTimeDiff curTime $ postDate post

    setTitle . string $ "pbrisbin - " ++ postTitle post
    addKeywords $ postTitle post : postTags post

    addJulius [$julius|
        var disqus_shortname  = 'pbrisbin';
        var disqus_identifier = '%postSlug.post%';
        var disqus_title      = '%postTitle.post%';
        |]

    [$hamlet|
        %h1 $postTitle.post$

        $postContent$

        .post_info_page
            ^postInfo.post^

        %h3 
            %a!href="#Comments"!id="Comments" Comments

        #disqus_thread
            %script!type="text/javascript"!src="http://pbrisbin.disqus.com/embed.js"
            %noscript 
                %p.small
                    %em Sadly, javascript is required for comments on this site.
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
