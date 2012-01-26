module Handler.Tags (getTagR) where

import Import
import Control.Monad (forM)
import Data.Time.Format.Human
import System.Directory (doesFileExist)
import Yesod.Links
import Yesod.Markdown
import Yesod.RssFeed (rssLink)
import qualified Data.Text as T

getTagR :: Text -> Handler RepHtml
getTagR tag = do
    records <- runDB $ do
        tags <- selectList [TagName ==. T.toLower tag] []

        let pids = map (tagPost . entityVal) tags

        posts <- selectList [PostId <-. pids] [Desc PostDate]

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

postWidget :: Post -> [Tag] -> Widget
postWidget post tags = do
    published <- lift $ liftIO $ humanReadableTime $ postDate post

    let file = pandocFile $ postSlug post

    content <- liftIO $ do
        exists <- doesFileExist file
        mkd    <- case (exists, postDescr post) of
            (True, _         ) -> markdownFromFile file
            (_   , Just descr) -> return descr
            _                  -> return ""

        return $ markdownToHtml mkd

    $(widgetFile "post/_inline")
