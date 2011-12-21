{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Archives
    ( getArchivesR
    ) where

import Foundation
import Data.Text (Text)
import qualified Data.Text as T

getArchivesR :: Handler RepHtml
getArchivesR = do
    docs <- siteDocs =<< getYesod

    tagCollections  <- return $ collectByTagName docs
    dateCollections <- liftIO $ collectByTimeBucket docs

    defaultLayout $ do
        setTitle "Archives"
        addKeywords $ map name $ tagCollections ++ dateCollections
        $(widgetFile "archives")

    where
        proper :: Text -> Text
        proper = T.unwords . map capitalize . T.words

        capitalize :: Text -> Text
        capitalize w = let (x,xs) = T.splitAt 1 w
            in (T.toUpper x) `T.append` xs

        helper 1 = "1 post"
        helper n = show n ++ " posts"
