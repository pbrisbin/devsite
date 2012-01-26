module Helpers.Post
    ( postForm
    , upsertPost
    ) where

import Import
import Control.Monad (forM_)
import Yesod.Markdown
import Data.Time (getCurrentTime)
import qualified Data.Text as T

data PostForm = PostForm
    { pfSlug  :: Text
    , pfTitle :: Text
    , pfTags  :: Text
    , pfDescr :: Maybe Markdown
    }

postForm :: Maybe (Post,[Tag]) -> Form PostForm
postForm mpost = renderBootstrap $ PostForm
    <$> areq textField     "Slug"        (fmap (postSlug   . fst) mpost)
    <*> areq textField     "Title"       (fmap (postTitle  . fst) mpost)
    <*> areq textField     "Tags"        (fmap (formatTags . snd) mpost)
    <*> aopt markdownField "Description" (fmap (postDescr  . fst) mpost)

    where
        formatTags :: [Tag] -> Text
        formatTags = T.intercalate ", " . map tagName

upsertPost :: PostForm -> Handler ()
upsertPost pf = do
    now <- liftIO getCurrentTime

    let post = Post
                { postSlug  = pfSlug  pf
                , postTitle = pfTitle pf
                , postDescr = pfDescr pf
                , postDate  = now
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
                update k 
                    [ PostSlug  =. pfSlug  pf
                    , PostTitle =. pfTitle pf
                    , PostDescr =. pfDescr pf
                    ]

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
