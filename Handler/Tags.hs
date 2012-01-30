module Handler.Tags (getTagR) where

import Import
import Control.Monad (forM)
import Helpers.Post
import Yesod.RssFeed (rssLink)
import qualified Data.Text as T

getTagR :: Text -> Handler RepHtml
getTagR tag = do
    records <- runDB $ do
        tags <- selectList [TagName ==. T.toLower tag] []

        let pids = map (tagPost . entityVal) tags

        posts <- selectList [PostDraft !=. True, PostId <-. pids] [Desc PostDate]

        forM posts $ \post -> do
            let pid   = entityKey post
            let post' = entityVal post
            let tags' = filter ((== pid) . tagPost) $ map entityVal tags

            return (post', tags')

    defaultLayout $ do
        rssLink (FeedTagR tag) ("rss feed for tag " ++ T.unpack tag)
        setTitle $ "Tag: " `T.append` tag
        addKeywords [tag]
        $(widgetFile "tag")
