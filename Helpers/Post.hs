module Helpers.Post
    ( postForm
    , upsertPost
    , postMarkdown
    , postContent
    , getPost404
    , getPreviousPost
    , getNextPost
    , postListing
    , inlinePost
    , formattedTags

    -- * Time helpers
    , UTCTime(..)
    , getCurrentTime
    , humanReadableTime
    , humanReadableTime'

    -- * Markdown helpers
    , Markdown(..)
    , markdownToHtml
    ) where

import Import
import Prelude (init, head, last)
import Yesod.Markdown
import Yesod.Links
import Control.Monad (when)
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Format.Human
import Database.Persist.GenericSql (rawSql)
import System.Directory (doesFileExist)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T

data PostForm = PostForm
    { pfSlug     :: Text
    , pfTitle    :: Text
    , pfTags     :: Text
    , pfDescr    :: Maybe Markdown
    , pfDraft    :: Bool
    , pfWasDraft :: Bool
    }

postForm :: Maybe (Post,[Tag]) -> Form PostForm
postForm mpost = renderBootstrap $ PostForm
    <$> areq textField     "Slug"        (fmap (postSlug   . fst) mpost)
    <*> areq textField     "Title"       (fmap (postTitle  . fst) mpost)
    <*> areq textField     "Tags"        (fmap (formatTags . snd) mpost)
    <*> aopt markdownField "Description" (fmap (postDescr  . fst) mpost)
    <*> areq checkBoxField "Draft?"      (Just $ isDraft mpost)
    <*> pure (isDraft mpost) -- store prev value

    where
        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName

        -- use draft value when present, defult True on Nothing
        isDraft :: Maybe (Post,[Tag]) -> Bool
        isDraft = maybe True (postDraft . fst)

upsertPost :: PostForm -> Handler ()
upsertPost pf = do
    now <- liftIO getCurrentTime

    let post = Post
                { postSlug  = pfSlug  pf
                , postTitle = pfTitle pf
                , postDescr = pfDescr pf
                , postDate  = now
                , postDraft = pfDraft pf
                }

    runDB $ do
        result <- insertBy post

        pid <- case result of
            -- inserted, do nothing for now
            Right k -> fmap (const k) $
                lift $ setMessage "post created!"

            -- exists, update it
            Left (Entity k _) -> fmap (const k) $ do
                update k $
                    [ PostSlug  =. pfSlug  pf
                    , PostTitle =. pfTitle pf
                    , PostDescr =. pfDescr pf
                    , PostDraft =. pfDraft pf
                    ] ++
                    -- update date if publishing
                    [ PostDate =. now | pfWasDraft pf, not $ pfDraft pf ]

                -- remove existing tags
                deleteWhere [ TagPost ==. k ]

                lift $ setMessage "post updated!"

        -- insert the new tags
        mapM_ insertBy (parseTags pid $ pfTags pf)

    redirect ManagePostsR

    where
        parseTags :: PostId -> Text -> [Tag]
        parseTags pid = map (Tag pid) . filter (not . T.null)
                      . map (T.toLower . T.strip) . T.splitOn ","

-- | Content as Html, use postMarkdown to get at the raw Markdown for
--   conversion to String or Text
postContent :: Post -> IO Html
postContent = fmap markdownToHtml . postMarkdown

postMarkdown :: Post -> IO Markdown
postMarkdown post = do
    let file = pandocFile $ postSlug post

    exists <- doesFileExist file

    case (exists, postDescr post) of
        (True, _         ) -> markdownFromFile file
        (_   , Just descr) -> return descr
        _                  -> return $ Markdown "nothing?"

getPost404 :: Text -> DB (Post,[Tag])
getPost404 slug = do
    results <- rawSql [st| SELECT ??, ?? FROM "Post"
                           JOIN "Tag" ON "Tag".post = "Post".id
                           WHERE "Post".slug = ? |] [toPersistValue slug]

    when (null results) $ lift notFound

    return ( entityVal . fst $ head results
           , map (entityVal . snd) results
           )

getNextPost :: Post -> DB (Maybe Post)
getNextPost post = getPostBy [ PostDraft !=. True
                             , PostSlug  !=. postSlug post
                             , PostDate  <.  postDate post
                             ] [Desc PostDate]

getPreviousPost :: Post -> DB (Maybe Post)
getPreviousPost post = getPostBy [ PostDraft !=. True
                                 , PostSlug  !=. postSlug post
                                 , PostDate  >.  postDate post
                                 ] [Asc PostDate]

getPostBy :: [Filter Post] -> [SelectOpt Post] -> DB (Maybe Post)
getPostBy filters sorts = do
    posts <- selectList filters $ sorts ++ [LimitTo 1]
    return $ case posts of
        ((Entity _ p):_) -> Just p
        _                -> Nothing

postListing :: [(Post,[Tag])] -> Widget
postListing records = $(widgetFile "post/_listing")

inlinePost :: Post -> [Tag] -> Widget
inlinePost post tags = do
    (published,content) <- liftIO $ do
        published' <- humanReadableTime $ postDate post
        content'   <- postContent post

        return (published',content')

    $(widgetFile "post/_inline")

formattedTags :: PostId -> [Tag] -> Text
formattedTags postId = T.intercalate ", "
                     . map tagName
                     . filter ((== postId) . tagPost)
