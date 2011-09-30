{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
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
timeBuckets = mapM toBucket $ 
    [ ((secondAgo 0 ),(hourAgo  1 ),"Just now"         )
    , ((hourAgo   1 ),(dayAgo   1 ),"Today"            )
    , ((dayAgo    1 ),(weekAgo  1 ),"This week"        )
    , ((weekAgo   1 ),(weekAgo  2 ),"Last week"        )
    , ((weekAgo   2 ),(weekAgo  3 ),"Two weeks ago"    )
    , ((weekAgo   3 ),(weekAgo  4 ),"Three weeks ago"  )
    , ((monthAgo  1 ),(monthAgo 2 ),"Last month"       )
    , ((monthAgo  2 ),(monthAgo 3 ),"Two months ago"   )
    , ((monthAgo  3 ),(monthAgo 4 ),"Three months ago" )
    , ((monthAgo  4 ),(monthAgo 5 ),"Four months ago"  )
    , ((monthAgo  5 ),(monthAgo 6 ),"Five months ago"  )
    , ((monthAgo  6 ),(monthAgo 7 ),"Six months ago"   )
    , ((monthAgo  7 ),(monthAgo 8 ),"Seven months ago" )
    , ((monthAgo  8 ),(monthAgo 9 ),"Eight months ago" )
    , ((monthAgo  9 ),(monthAgo 10),"Nine months ago"  )
    , ((monthAgo  10),(monthAgo 11),"Ten months ago"   )
    , ((monthAgo  11),(monthAgo 12),"Eleven months ago")
    , ((yearAgo   1 ),(yearAgo  2 ),"Last year"        )
    , ((yearAgo   2 ),(yearAgo  3 ),"Two years ago"    )
    , ((yearAgo   3 ),(yearAgo  4 ),"Three years ago"  )
    , ((yearAgo   4 ),(yearAgo  5 ),"Four years ago"   )
    , ((yearAgo   5 ),(yearAgo  6 ),"Five years ago"   )
    ]

    where
        toBucket :: (IO UTCTime,IO UTCTime,Text) -> IO TimeBucket
        toBucket (fnFrom,fnTo,label) = do
            from <- fnFrom
            to   <- fnTo
            return $ TimeBucket from to label

secondAgo, minuteAgo, hourAgo, dayAgo,
    weekAgo, monthAgo, yearAgo :: Integer -> IO UTCTime
secondAgo n = return . addUTCTime (fromInteger $ -1 * n) =<< getCurrentTime

minuteAgo = secondAgo . (*60)
hourAgo   = minuteAgo . (*60)
dayAgo    = hourAgo   . (*24)
weekAgo   = dayAgo    . (*7)
monthAgo  = weekAgo   . (*4)
yearAgo   = monthAgo  . (*12)

collect :: [Document] -> Text -> (Document -> Bool) -> Collection
collect docs n p = Collection n (filter p docs)

hasTag :: Text -> Document -> Bool
hasTag t d = t `elem` (map tagName $ tags d)
