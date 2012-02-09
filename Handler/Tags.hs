module Handler.Tags (getTagR) where

import Import
import Control.Monad (forM)
import Helpers.Post
import Yesod.RssFeed (rssLink)
import qualified Data.Text as T

getTagR :: Text -> Handler RepHtml
getTagR tag = do
    records <- runDB $ do
        tags <- fmap (map entityVal) $ selectList [] []

        let pids = map tagPost $ filter ((== T.toLower tag) . tagName) tags

        posts <- selectList [PostDraft !=. True, PostId <-. pids] [Desc PostDate]

        forM posts $ \(Entity pid post) ->
            return (post, filter ((== pid) . tagPost) $ tags)

    defaultLayout $ do
        rssLink (FeedTagR tag) ("rss feed for tag " ++ T.unpack tag)
        setTitle $ "Tag: " `T.append` tag
        addKeywords [tag]
        $(widgetFile "tag")
