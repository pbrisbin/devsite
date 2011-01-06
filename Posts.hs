{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Posts
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- The master list of all posts known to the site plus some helper
-- functions for finding them.
--
-------------------------------------------------------------------------------
module Posts
    ( Post (..)
    , loadPostContent
    , selectPosts
    , insertPost
    , getPostBySlug
    , getPostsByTag
    --, mkPostSlugs
    --, mkPostTags
    , allPostsTemplate
    , postTemplate
    , migratePosts
    ) where

import Yesod
import DevSite

import System.Directory (doesFileExist)
import Language.Haskell.TH.Syntax
import Text.Pandoc

import Data.Time.Clock  (UTCTime)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

-- | The data type of a single post
data Post = Post
    { postSlug  :: String
    , postDate  :: UTCTime
    , postTitle :: String
    , postDescr :: String
    , postTags  :: [String]
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

-- | Use TH to define functions for each post's slug for use in hamlet
--   templates
--mkPostSlugs :: Q [Dec]
--mkPostSlugs = mkConstants $ map postSlug allPosts

-- | Use TH to define functions for each tag for use in hamlet templates
--mkPostTags :: Q [Dec]
--mkPostTags = mkConstants $ nub . concat $ map postTags allPosts

-- | Load a post's pandoc file and convert it to html, return not found
--   if the pdc file doesn't exist
loadPostContent :: Post -> IO Html
loadPostContent p = do
    let fileName = "pandoc/" ++ postSlug p ++ ".pdc"
    markdown <- do
        exists <- doesFileExist fileName
        if exists
            then readFile fileName
            else return "File not found"
    return $ preEscapedString
           $ writeHtmlString defaultWriterOptions
           $ readMarkdown defaultParserState markdown

-- | A body template for a list of posts, you can also provide the title
allPostsTemplate :: [Post] -> String -> Hamlet DevSiteRoute
allPostsTemplate posts title = [$hamlet|
%h1 $title$

#posts
    $forall posts post
        ^postTemplate.post^
|]

-- | The sub template for a single post
postTemplate :: Post -> Hamlet DevSiteRoute
postTemplate arg = [$hamlet|
.post
  %p

    %a!href=@PostR.postSlug.arg@ $postTitle.arg$
    \ - $postDescr.arg$ 
    
  %p.small

    Published on $format.postDate.arg$

    %span!style="float: right;"

      Tags: 

      $forall postTags.arg tag

        %a!href=@TagR.tag@ $tag$ 
|]

    where
        format :: UTCTime -> String
        format = formatTime defaultTimeLocale rfc822DateFormat

        -- | An alternative to System.Local.rfc822DateFormat, this one agrees
        --   with the output of `date -R`
        rfc822DateFormat :: String
        rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %z"
