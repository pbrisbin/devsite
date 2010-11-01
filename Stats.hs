{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Stats
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provide a page of _basic_ access statistics by parsing your server's
-- log file. A parser for lighttpd's default output is provided because,
-- well, that's what I use.
--
-------------------------------------------------------------------------------
module Stats 
    ( LogFile(..)
    , TopEntry(..)
    , lighttpdLog
    , statsTemplate
    ) where

import Yesod
import DevSite

import Control.Monad      (liftM)
import Data.Function      (on)
import Data.List          (nub, sortBy, group, sort)
import Data.Maybe         (mapMaybe)
import Data.Time.Clock    (getCurrentTime)
import Data.Time.Format   (formatTime)
import System.Locale      (defaultTimeLocale)
import Text.Regex.Posix   ((=~))
import Text.Hamlet        (HamletValue(..))

data LogEntry = LogEntry 
    { ipAddress   :: String
    , dateStamp   :: String
    , requestFile :: String
    }

data LogFile = LogFile
    { logFilePath  :: FilePath
    , readLogEntry :: String -> Maybe LogEntry 
    }

data TopEntry = TopEntry
    { entryTitle     :: String
    , totalDownloads :: Int
    , downloadCounts :: [(String,Int)]
    }

readLog :: LogFile -> IO [LogEntry]
readLog lf = do
    contents <- readFile $ logFilePath lf
    return $ mapMaybe (readLogEntry lf) $ lines contents

-- | Contruct a logfile based on file path and a list of IPs to exclude
--   setup to read the default lighttpd access log output format
lighttpdLog :: FilePath -> [String] -> LogFile
lighttpdLog file blacklist = LogFile
    { logFilePath  = file
    , readLogEntry = \xs -> 
        case (words xs) of
            -- assumes no spaces in filenames
            (i:_: _:d:tz:"\"GET":f: _:"200":rest) -> 
                if notBlacklisted i 
                    then Just $ LogEntry i (d ++ " " ++ tz) f
                    else Nothing
            otherwise -> Nothing
    }
    where
        notBlacklisted :: String -> Bool
        notBlacklisted = not . flip elem blacklist
        
-- | The main page template, a table inside a .stats div
--statsTemplate :: (HamletValue a, Yesod m) 
--              => LogFile            -- ^ your log file type
--              -> [(String,String)]  -- ^ list of top entries to find/print
--              -> GHandler s m (Hamlet a)
statsTemplate :: LogFile -> [(String,String)] -> Handler (Hamlet DevSiteRoute)
statsTemplate lf tes = do
    timeNow    <- liftIO getCurrentTime
    logEntries <- liftIO $ readLog lf

    let topEntries   = map (getTopEntry logEntries) tes
    let periodFrom   = head $ map dateStamp logEntries
    let uniqueVisits = show . length $ uniqueIps logEntries
    let frequentIp   = fst . head $ frequentIps logEntries

    return [$hamlet|
    %h1 Site Statistics
    %hr

    .stats
        %h3 General statistics
        %table
            %tr
                %td stats generated:
                %td 
                    %strong $formatTime'.timeNow$
            %tr
                %td log period from:
                %td 
                    %strong $periodFrom$
            %tr
                %td!colspan="2" &nbsp;
            %tr
                %td unique visits:
                %td 
                    %strong $uniqueVisits$
            %tr
                %td!colspan="2" &nbsp;
            %tr
                %td most frequent IP:
                %td 
                    %strong $frequentIp$

        %h3 Files accessed
        $forall topEntries topEntry
            ^topEntryTemplate.topEntry^
|]
    where
        uniqueIps   = nub . map ipAddress
        frequentIps = frequency . map ipAddress
        formatTime' = formatTime defaultTimeLocale "[%d/%b/%Y:%H:%M:%S %z]"

-- | A template for printing table rows of a top entry
topEntryTemplate :: TopEntry -> Hamlet DevSiteRoute
topEntryTemplate arg = let counts = take 10 $ downloadCounts arg in
    [$hamlet|
        .stats_top_entry
            %p
               total $entryTitle.arg$ downloaded: 
               %strong $show.totalDownloads.arg$
            
            %table
                $forall counts count
                    %tr
                        %td $show.snd.count$
                        %td 
                            %a!href=$fst.count$ $fst.count$
    |]

-- | Given log file contents and a string tuple, return the top entry
--   listing for the given tuple
getTopEntry :: [LogEntry] -> (String,String) -> TopEntry
getTopEntry logEntries (s,r) = TopEntry
        { entryTitle     = s
        , totalDownloads = totalDownloaded r logEntries
        , downloadCounts = frequentDownloads r logEntries
        }
    where
        totalDownloaded s   = length . filter (isDownloadOf s)
        frequentDownloads s = frequency . map requestFile . filter (isDownloadOf s)
        isDownloadOf s e    = requestFile e =~ s :: Bool

-- | Ex: frequency [a, b, c, b, a, a] -> [(3, a), (2, b), (1,c)]
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = reverse . sortBy (compare `on` snd) .  map (\xs' -> (head xs', length xs')) . group . sort
