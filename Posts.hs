{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    , getNewPostR
    , postNewPostR
    ) where

import Yesod
import DevSite

import System.Directory (doesFileExist)
import Control.Applicative ((<$>), (<*>))
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

-- | Used in the new post form
data PostForm = PostForm
    { formSlug  :: String
    , formDescr :: Textarea
    , formTags  :: String
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

getNewPostR :: Handler RepHtml
getNewPostR = do
    postForm <- runPostForm

    defaultLayout $ do
        setTitle $ string "Add New Post"
        addHamlet [$hamlet|
        #header
            %h1 Add New Post
        #body
            ^postForm^
        |]

postNewPostR :: Handler RepHtml
postNewPostR = getNewPostR

postFromForm :: PostForm -> Handler Post
postFromForm = undefined

runPostForm :: Handler (Hamlet DevSiteRoute)
runPostForm = do
    ((res, form), enctype) <- runFormMonadPost addPostForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> do
            postFromForm pf >>= insertPost
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary PostsR

    return . pageBody =<< widgetToPageContent (addPostTemplate form enctype)

addPostForm :: FormMonad (FormResult PostForm, Widget ())
addPostForm = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   Nothing
    (description, fiDescription) <- textareaField "description:" Nothing
    (tags       , fiTags       ) <- stringField   "tags:"        Nothing
    return (PostForm <$> slug <*> description <*> tags, [$hamlet|
    %table
        %tr
            %td
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
            %td
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
            %td
                &nbsp;
            %td!colspan="2"
                %input!type="submit"!value="Add post"
    |])

addPostTemplate :: Widget () -> Enctype -> Widget ()
addPostTemplate form enctype = do
    posts <- liftHandler $ selectPosts 0
    [$hamlet|
    #input
        %h3 Add a new post:

        %form!enctype=$enctype$!method="post"
            ^form^

    #existing
        %table
            %tr
                %td Title
                %td Description
                %td Posted on

            $forall posts post
                %tr
                    %td $postTitle.post$
                    %td $postDescr.post$
                    %td $formatDateTime.postDate.post$
    |]

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

    Published on $formatDateTime.postDate.arg$

    %span!style="float: right;"

      Tags: 

      $forall postTags.arg tag

        %a!href=@TagR.tag@ $tag$ 
|]

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale rfc822DateFormat
    where
        -- | An alternative to System.Local.rfc822DateFormat, this one agrees
        --   with the output of `date -R`
        rfc822DateFormat :: String
        rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %z"
