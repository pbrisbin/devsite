module Handler.Search (getSearchXmlR) where

import Import
import Control.Monad (liftM,forM)
import Helpers.Post
import System.Directory (doesFileExist)
import Yesod.Markdown
import Text.Hamlet (xshamlet)
import qualified Data.Text as T

getSearchXmlR :: Handler RepXml
getSearchXmlR = do
    posts <- runDB $ selectList [] []

    xml <- liftIO $ do
        nodes <- forM posts $ \post -> do
            xmlNode (entityKey post) (entityVal post)

        return $ mconcat nodes 

    liftM RepXml . htmlToContent $ [xshamlet|#{xml}|]

htmlToContent = hamletToContent . const

xmlNode :: PostId -> Post -> IO Html
xmlNode pid post = do
    let file = pandocFile $ postSlug post

    exists <- doesFileExist file
    mkd    <- case (exists, postDescr post) of
        (True, _         ) -> markdownFromFile file
        (_   , Just descr) -> return descr
        _                  -> return $ Markdown "nothing?"

    let content = markdownToText mkd

    return $
        [xshamlet|
            <document>
                <id>#{toPathPiece pid}
                <title>#{postTitle post}
                <body>#{content}
            |]

    where
        markdownToText :: Markdown -> Text
        markdownToText (Markdown s) = T.pack s
