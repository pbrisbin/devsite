module Handler.Search (getSearchXmlR) where

import Import
import Control.Monad (forM)
import System.Directory (doesFileExist)
import Yesod.Markdown
import Text.Hamlet (xshamlet)
import qualified Data.Text as T

-- | This does not scale.
--
--   I could not get the streaming approach outlined on the yesod site
--   now that we've moved away from enumerators, etc. This is a naive
--   approach, selecting all documents out of the database and building
--   xml via hamlet. It's plenty fast enough for a small site like this.
--
getSearchXmlR :: Handler RepXml
getSearchXmlR = do
    posts <- runDB $ selectList [] []

    blocks <- liftIO $ forM posts $ \post -> do
        docBlock (entityKey post) (entityVal post)

    fmap RepXml $ htmlToContent $ mconcat blocks

    where
        htmlToContent :: Html -> Handler Content
        htmlToContent = hamletToContent . const

docBlock :: PostId -> Post -> IO Html
docBlock pid post = do
    let file = pandocFile $ postSlug post

    exists <- doesFileExist file
    mkd    <- case (exists, postDescr post) of
        (True, _         ) -> markdownFromFile file
        (_   , Just descr) -> return descr
        _                  -> return $ Markdown "nothing?"

    return $
        [xshamlet|
            <document>
                <id>#{toPathPiece pid}
                <title>#{postTitle post}
                <body>#{markdownToText mkd}
            |]

    where
        markdownToText :: Markdown -> Text
        markdownToText (Markdown s) = T.pack s
