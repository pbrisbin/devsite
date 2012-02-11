module Handler.Root (getRootR) where

import Import
import Helpers.Post
import Yesod.Paginator
import Control.Monad (forM)

getRootR :: Handler RepHtml
getRootR = do
    (records,widget) <- runDB $ do
        (posts,widget') <- selectPaginated 5 [PostDraft !=. True] [Desc PostDate]

        let pids = map entityKey posts

        tags <- selectList [TagPost <-. pids] []

        records' <- forM posts $ \post -> do
            let pid   = entityKey post
            let post' = entityVal post
            let tags' = filter ((== pid) . tagPost) $ map entityVal tags

            return (post', tags')

        return (records',widget')

    defaultLayout $ do
        setTitle "Home"
        addKeywords ["home", "haskell", "yesod", "bash", "mutt", "xmonad", "arch linux"]
        $(widgetFile "root")
