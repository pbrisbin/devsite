module Handler.Search
    ( getSearchXmlR
    , getSearchR
    ) where

import Import
import Helpers.Search
import Helpers.Post
import Control.Monad (forM)
import Text.Hamlet (xshamlet)

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

getSearchR :: Text -> Handler RepJson
getSearchR qstring = do
    results <- executeSearch qstring

    objects <- forM results $ \result -> do
        return $ object [ ("slug"   , resultSlug    result)
                        , ("title"  , resultTitle   result)
                        , ("excerpt", resultExcerpt result)
                        ]

    jsonToRepJson $ array objects

docBlock :: PostId -> Post -> IO Html
docBlock pid post = do
    markdown <- postMarkdown post
    return $
        [xshamlet|
            <document>
                <id>#{toPathPiece pid}
                <title>#{postTitle post}
                <body>#{markdownToText markdown}
            |]
