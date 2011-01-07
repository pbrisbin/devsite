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
-- Logic regarding all the posts on this site: how they're stored, how
-- they're retrieved, routes and templates for working with them.
--
-------------------------------------------------------------------------------
module Posts
    ( Post (..)
    , loadPostContent
    , selectPosts
    , insertPost
    , getPostBySlug
    , getPostsByTag
    , allPostsTemplate
    , postTemplate
    , migratePosts
    , getNewPostR
    , postNewPostR
    , getDelPostR
    ) where

import DevSite

import Yesod
import Yesod.Markdown

import Data.Char (isSpace)
import System.Directory (doesFileExist)
import Control.Applicative ((<$>), (<*>))
import Language.Haskell.TH.Syntax

import Data.Time.Clock  (UTCTime, getCurrentTime)
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
    let fileName = "pandoc/" ++ postSlug p ++ ".pdc"
    markdown <- do
        exists <- liftIO $ doesFileExist fileName
        if exists
            then liftIO $ readFile fileName
            else return "File not found"
    (writePandoc yesodDefaultWriterOptions <$>) . localLinks . parseMarkdown yesodDefaultParserState $ Markdown markdown

-- | Display the add new post form, todo: authentication for use of this
--   page
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

-- | POST is just GET
postNewPostR :: Handler RepHtml
postNewPostR = getNewPostR

getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    deletePost slug
    setMessage $ [$hamlet| %em post deleted! |]
    redirect RedirectTemporary PostsR
    
-- | Convert the entered post into the correct data type by parsing the
--   Tag list and adding a time stamp
postFromForm :: PostForm -> Handler Post
postFromForm pf = do
    currentTime <- liftIO getCurrentTime
    return Post
        { postSlug  = formSlug pf
        , postTitle = formTitle pf
        , postDescr = unTextarea $ formDescr pf
        , postDate  = currentTime
        , postTags  = parseCSL $ formTags pf
        }

-- | Take a comma-separated list of tags like "foo, bar, baz" and parse
--   that into a real haskell list
parseCSL :: String -> [String]
parseCSL = filter (/= []) . parseCSL' [] []
    where
        parseCSL' :: [String] -> String -> String -> [String]
        parseCSL' acc1 acc2 []         = acc1
        parseCSL' acc1 acc2 (',':rest) = parseCSL' (acc1 ++ [trim acc2]) [] rest
        parseCSL' acc1 acc2 (x:rest)   = parseCSL' acc1 (acc2 ++ [x]) rest

        trim = f . f where f = reverse . dropWhile isSpace

-- | Run the new post page and insert on successfully POST
runPostForm :: Handler (Hamlet DevSiteRoute)
runPostForm = do
    ((res, form), enctype) <- runFormMonadPost addPostForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> do
            postFromForm pf >>= insertPost
            setMessage $ [$hamlet| %em new post added! |]
            redirect RedirectTemporary PostsR

    -- this feels kludgy...
    return . pageBody =<< widgetToPageContent (addPostTemplate form enctype)

-- | The new post form itself
addPostForm :: FormMonad (FormResult PostForm, Widget ())
addPostForm = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   Nothing
    (title      , fiTitle      ) <- stringField   "title:"       Nothing
    (tags       , fiTags       ) <- stringField   "tags:"        Nothing
    (description, fiDescription) <- textareaField "description:" Nothing
    return (PostForm <$> slug <*> title <*> tags <*> description, [$hamlet|
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
                %input!type="submit"!value="Add post"
    |])

-- | The overall template showing the input box and a list of existing
--   posts
addPostTemplate :: Widget () -> Enctype -> Widget ()
addPostTemplate form enctype = do
    posts <- liftHandler $ selectPosts 0
    [$hamlet|
    #input
        %h3 Add a new post:

        %form!enctype=$enctype$!method="post"
            ^form^

    #existing
        %h3 Existing posts

        %table
            %tr
                %th Title
                %th Description
                %th Delete

            $forall posts post
                %tr
                    %td 
                        %a!href=@PostR.postSlug.post@ $postTitle.post$
                    %td $shorten.postDescr.post$
                    %td 
                        %a!href=@DelPostR.postSlug.post@ delete
    |]

    where shorten s = if length s > 30 then take 30 s ++ "..." else s

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
