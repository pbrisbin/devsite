-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Storage
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Some pre-built backend definitions.
--
-------------------------------------------------------------------------------
module Comments.Storage 
    ( testDB
    , fileDB
    ) where

import Yesod
import Comments.Core
import Data.List.Split  (wordsBy)
import Data.Time.Clock  (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Data.Maybe       (mapMaybe)
import System.IO        (hPutStrLn, stderr)
import System.Locale    (defaultTimeLocale)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- | For use during testing, always loads no comments and prints the
--   comment to stderr as "store"
testDB :: CommentStorage
testDB = CommentStorage
    { storeComment = liftIO . hPutStrLn stderr . show
    , loadComments = \_ -> return []
    }

-- | A simple flat file storage method, this is way unreliable, probably
--   wicked slow, but dead simple to setup/use
fileDB :: FilePath -> CommentStorage
fileDB f = CommentStorage
    { storeComment = storeComment'
    , loadComments = loadComments'
    }
    where
        storeComment' comment = do
            -- a comment with a literal '|' in it will be lost...
            let str = concat [ thread comment,                  "|"
                             , formatTime' $ timeStamp comment, "|"
                             , ipAddress comment,               "|"
                             , userName comment,                "|"
                             , htmlToString $ content comment, "\n"
                             ]
            liftIO $ appendFile f str

        formatTime'  = formatTime defaultTimeLocale "%s"
        htmlToString = L.unpack . renderHtml

        loadComments' id = do
            contents <- liftIO $ readFile f
            return $ mapMaybe (readComment id) (lines contents)

        readComment id' s = 
            case wordsBy (=='|') s of
                [t, ts, ip, user, html] -> 
                    if t == id'
                        then
                            case readTime ts of
                                Just utc -> Just
                                    Comment
                                        { thread    = t
                                        , timeStamp = utc
                                        , ipAddress = ip
                                        , userName  = user
                                        , content   = preEscapedString html
                                        }
                                _ -> Nothing
                        else Nothing
                _ -> Nothing

        readTime :: String -> Maybe UTCTime
        readTime = parseTime defaultTimeLocale "%s"

--persistentDB :: CommentStorage
--persistentDB = undefined
