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

    -- * Time helpers
    , UTCTime(..)
    , getCurrentTime
    , humanReadableTime
    , humanReadableTime'

    -- * Markdown helpers
    , Markdown(..)
    , markdownToHtml
    , markdownToString
    , markdownToText
    ) where

import Import
import Prelude (init, head, last)
import Yesod.Markdown
import Yesod.Links
import Control.Monad (forM_, when)
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

    msg <- runDB $ do
        result <- insertBy post

        case result of
            Right k -> do
                -- post was inserted, add the tags
                forM_ (parseTags $ pfTags pf) $ \tag -> do
                    insertBy $ Tag k tag

                return "post created!"

            Left (Entity k _) -> do
                -- post exists, update it
                update k $
                    [ PostSlug  =. pfSlug  pf
                    , PostTitle =. pfTitle pf
                    , PostDescr =. pfDescr pf
                    , PostDraft =. pfDraft pf

                    -- update post date if were changing a post from
                    -- draft to not draft.
                    ] ++ dateUpd (pfWasDraft pf) (pfDraft pf) now

                -- remove existing tags
                deleteWhere [TagPost ==. k]

                -- add new ones
                forM_ (parseTags $ pfTags pf) $ \tag -> do
                    insertBy $ Tag k tag

                return "post updated!"

    setMessage msg

    redirect ManagePostsR

    where
        parseTags :: Text -> [Text]
        parseTags = filter (not . T.null)
                  . map (T.toLower . T.strip) . T.splitOn ","

        dateUpd True False date = [PostDate =. date]
        dateUpd _    _     _    = []

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

getPost404 :: Text -> YesodDB DevSite DevSite (Post,[Tag])
getPost404 slug = do
    results <- rawSql [st| SELECT ??, ?? FROM "Post"
                           JOIN "Tag" ON "Tag".post = "Post".id
                           WHERE "Post".slug = ? |] [toPersistValue slug]

    when (null results) $ lift notFound

    return ( entityVal . fst $ head results
           , map (entityVal . snd) results
           )

getNextPost :: Post -> YesodDB DevSite DevSite (Maybe Post)
getNextPost post = getPostBy [ PostDraft !=. True
                             , PostSlug  !=. postSlug post
                             , PostDate  <.  postDate post
                             ] [Desc PostDate]

getPreviousPost :: Post -> YesodDB DevSite DevSite (Maybe Post)
getPreviousPost post = getPostBy [ PostDraft !=. True
                                 , PostSlug  !=. postSlug post
                                 , PostDate  >.  postDate post
                                 ] [Asc PostDate]

getPostBy :: [Filter Post] -> [SelectOpt Post]
          -> YesodDB DevSite DevSite (Maybe Post)
getPostBy filters sorts = do
    posts <- selectList filters $ sorts ++ [LimitTo 1]
    return $ case posts of
        ((Entity _ p):_) -> Just p
        _                -> Nothing

markdownToString :: Markdown -> String
markdownToString (Markdown s) = s

markdownToText :: Markdown -> Text
markdownToText = T.pack . markdownToString

postListing :: [(Post,[Tag])] -> Widget
postListing records = $(widgetFile "post/_listing")

inlinePost :: Post -> [Tag] -> Widget
inlinePost post tags = do
    (published,content) <- liftIO $ do
        published' <- humanReadableTime $ postDate post
        content'   <- postContent post

        return (published',content')

    $(widgetFile "post/_inline")
