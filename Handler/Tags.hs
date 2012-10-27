module Handler.Tags (getTagR) where

import Import
import Helpers.Post
import Yesod.RssFeed (rssLink)
import Yesod.Paginator
import Control.Monad (forM)
import qualified Data.Text as T

getTagR :: Text -> Handler RepHtml
getTagR tag = do
    (records,widget) <- runDB $ do
        tags <- fmap (map entityVal) $ selectList [] []

        let pids = map tagPost $ filter ((== T.toLower tag) . tagName) tags

        (posts,widget') <- selectPaginated 5 [PostDraft !=. True, PostId <-. pids] [Desc PostDate]

        records' <- forM posts $ \(Entity pid post) ->
            return (post, filter ((== pid) . tagPost) $ tags)

        return (records',widget')

    defaultLayout $ do
        rssLink (FeedTagR tag) ("rss feed for tag " `T.append` tag)
        setTitle $ toHtml $ "Tag: " `T.append` tag
        addKeywords [tag]
        $(widgetFile "tag")
