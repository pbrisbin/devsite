module Handler.Root (getRootR) where

import Import
import Control.Monad (forM)
import Helpers.Post
import Yesod.Links

getRootR :: Handler RepHtml
getRootR = do
    -- select 5 recent (Post, [Tag]) records
    records <- runDB $ do
        posts <- selectList [PostDraft !=. True] [Desc PostDate, LimitTo 5]

        let pids = map entityKey posts
        tags <- selectList [TagPost <-. pids] []

        forM posts $ \post -> do
            let pid   = entityKey post
            let post' = entityVal post
            let tags' = filter ((== pid) . tagPost) $ map entityVal tags

            return (post', tags')

    defaultLayout $ do
        setTitle "Home"
        addKeywords ["home", "haskell", "yesod", "bash", "mutt", "xmonad", "arch linux"]
        $(widgetFile "root")

postWidget :: Post -> [Tag] -> Widget
postWidget post tags = do
    published <- liftIO $ postPublished post
    content   <- liftIO $ postContent post

    $(widgetFile "post/_inline")
