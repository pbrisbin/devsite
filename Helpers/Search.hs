module Helpers.Search
    ( SearchResult (..)
    , executeSearch
    ) where

import Import
import Helpers.Post
import Control.Monad          (forM)
import Database.Persist.Store (PersistValue(PersistInt64))

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text                  as T

import Text.Search.Sphinx
import Text.Search.Sphinx.Types
import qualified Text.Search.Sphinx.ExcerptConfiguration as E

sport :: Int
sport = 9312

index :: String
index = "pbrisbin-idx"

data SearchResult = SearchResult
    { resultSlug    :: Text
    , resultTitle   :: Text
    , resultExcerpt :: Text
    }

executeSearch :: Text -> Handler [SearchResult]
executeSearch text = do
    res <- liftIO $ query config index (T.unpack text)

    case res of
        Ok sres -> do
            let pids = map (Key . PersistInt64 . documentId) $ matches sres

            posts <- runDB $ selectList [PostId <-. pids] []

            forM posts $ \(Entity _ post) -> do
                excerpt <- liftIO $ do
                    context <- do
                        markdown <- postMarkdown post
                        return $ markdownToString markdown

                    buildExcerpt context (T.unpack text)

                return $ SearchResult
                            { resultSlug    = postSlug post
                            , resultTitle   = postTitle post
                            , resultExcerpt = excerpt
                            }

        _ -> return []

    where
        config :: Configuration
        config = defaultConfig
            { port   = sport
            , mode   = Any
            }

buildExcerpt :: String -- ^ context
             -> String -- ^ search string
             -> IO Text
buildExcerpt context qstring = do
    excerpt <- buildExcerpts config [concatMap escapeChar context] index qstring
    return $ case excerpt of
        Ok bss -> T.pack $ C8.unpack $ L.concat bss
        _      -> ""

    where
        config :: E.ExcerptConfiguration
        config = E.altConfig { E.port = sport }

        escapeChar :: Char -> String
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar c   = [c]
