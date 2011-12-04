{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE EmptyDataDecls             #-}
module Model where

import Yesod.Goodies
import Yesod.Persist

import Data.List (nub, sortBy)
import Data.Ord  (comparing)
import Data.Text (Text)
import Data.Time (getCurrentTime, UTCTime, addUTCTime)

share [mkPersist sqlSettings, mkMigrate "migratePosts"] $(persistFile "config/models")

data Document = Document
    { post :: Post
    , tags :: [Tag]
    }

instance Eq Document where
    (Document p1 _) == (Document p2 _) = postSlug p1 == postSlug p2

data Collection = Collection
    { name      :: Text
    , documents :: [Document]
    }

-- | Time bucket in the past
data TimeBucket = TimeBucket
    { tbStart :: UTCTime -- ^ nearest to now (later)
    , tbEnd   :: UTCTime -- ^ futhest from now (earlier)
    , tbName  :: Text
    } deriving Show

-- | Sorted by length descending
collectByTagName :: [Document] -> [Collection]
collectByTagName docs = reverse . sortBy (comparing (length . documents))
                      . map (\tag -> collect docs tag (hasTag tag))
                      . nub . map tagName $ concatMap tags docs

collectByTimeBucket :: [Document] -> IO [Collection]
collectByTimeBucket docs = do
    tbs <- timeBuckets
    return . filter (not . null . documents)
           $ map (\b -> collect docs (tbName b) (inBucket b . postDate . post)) tbs

    where
        inBucket :: TimeBucket -> UTCTime -> Bool
        inBucket (TimeBucket from to _) t = t <= from && t >= to

timeBuckets :: IO [TimeBucket]
timeBuckets = do
    now <- getCurrentTime
    return $ [ TimeBucket                    now  (  1 `hoursBefore`  now) "Just now"
             , TimeBucket ( 1 `hoursBefore`  now) (  1 `daysBefore`   now) "Today"
             , TimeBucket ( 1 `daysBefore`   now) (  1 `weeksBefore`  now) "This week"
             , TimeBucket ( 1 `weeksBefore`  now) (  2 `weeksBefore`  now) "Last week"
             , TimeBucket ( 2 `weeksBefore`  now) (  3 `weeksBefore`  now) "Two weeks ago"
             , TimeBucket ( 3 `weeksBefore`  now) (  4 `weeksBefore`  now) "Three weeks ago"
             , TimeBucket ( 1 `monthsBefore` now) (  2 `monthsBefore` now) "Last month"
             , TimeBucket ( 2 `monthsBefore` now) (  3 `monthsBefore` now) "Two months ago"
             , TimeBucket ( 3 `monthsBefore` now) (  4 `monthsBefore` now) "Three months ago"
             , TimeBucket ( 4 `monthsBefore` now) (  5 `monthsBefore` now) "Four months ago"
             , TimeBucket ( 5 `monthsBefore` now) (  6 `monthsBefore` now) "Five months ago"
             , TimeBucket ( 6 `monthsBefore` now) (  7 `monthsBefore` now) "Six months ago"
             , TimeBucket ( 7 `monthsBefore` now) (  8 `monthsBefore` now) "Seven months ago"
             , TimeBucket ( 8 `monthsBefore` now) (  9 `monthsBefore` now) "Eight months ago"
             , TimeBucket ( 9 `monthsBefore` now) ( 10 `monthsBefore` now) "Nine months ago"
             , TimeBucket (10 `monthsBefore` now) ( 11 `monthsBefore` now) "Ten months ago"
             , TimeBucket (11 `monthsBefore` now) ( 12 `monthsBefore` now) "Eleven months ago"
             , TimeBucket ( 1 `yearsBefore`  now) (  2 `yearsBefore`  now) "Last year"
             , TimeBucket ( 2 `yearsBefore`  now) (  3 `yearsBefore`  now) "Two years ago"
             , TimeBucket ( 3 `yearsBefore`  now) (  4 `yearsBefore`  now) "Three years ago"
             , TimeBucket ( 4 `yearsBefore`  now) (  5 `yearsBefore`  now) "Four years ago"
             , TimeBucket ( 5 `yearsBefore`  now) (  6 `yearsBefore`  now) "Five years ago"
             , TimeBucket ( 6 `yearsBefore`  now) (999 `yearsBefore`  now) "Older"
             ]

-- | Calculate time differences
secondsBefore, minutesBefore, hoursBefore, daysBefore,
    weeksBefore, monthsBefore, yearsBefore :: Integer -> UTCTime -> UTCTime
secondsBefore n = addUTCTime (fromInteger $ -1 * n)

minutesBefore n t = secondsBefore (n*60) t
hoursBefore   n t = minutesBefore (n*60) t
daysBefore    n t = hoursBefore   (n*24) t
weeksBefore   n t = daysBefore    (n*7)  t
monthsBefore  n t = weeksBefore   (n*4)  t
yearsBefore   n t = monthsBefore  (n*12) t

collect :: [Document] -> Text -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

hasTag :: Text -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
