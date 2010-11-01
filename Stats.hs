{-# LANGUAGE QuasiQuotes #-}
--
--
--
module Stats where

import Yesod

import Control.Monad      (liftM)
import Data.Function      (on)
import Data.List          (nub, sortBy, group, sort)
import Data.Maybe         (mapMaybe)
import Data.Time.Clock    (getCurrentTime)
import Data.Time.Format   (formatTime)
import System.Locale      (defaultTimeLocale)
import Text.Regex.Posix   ((=~))
import Text.Hamlet        (HamletValue(..))

------- USER CONFIGURATION
myLogFile :: LogFile
myLogFile = LogFile
    { logFilePath  = "/var/log/lighttpd/access.log"
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
        notBlacklisted = not . flip elem ["192.168.0.5", "66.30.118.211"]

myTopEntries :: [(String, String)]
myTopEntries = [ ("posts"         , "^/posts/.*"             )
               , ("xmonad modules", "^/xmonad/docs/.*\\.html")
               ]
-------

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

pageTemplate :: (HamletValue a, Yesod m) 
             => LogFile 
             -> [(String,String)] 
             -> GHandler s m (Hamlet a)
pageTemplate lf tes = do
    timeNow    <- liftIO getCurrentTime
    logEntries <- liftIO $ readLog lf
    topEntries <- liftIO $ mapM (getTopEntry lf) tes

    let periodFrom   = head $ map dateStamp logEntries
    let uniqueVisits = show . length $ uniqueIps logEntries
    let frequentIp   = fst . head $ frequentIps logEntries

    return [$hamlet|
    .stats
        %h3 General statistics
        %table
            %tr
                %td stats generated:
                %td $formatTime'.timeNow$
            %tr
                %td log period from:
                %td $periodFrom$
            %tr
                %td!colspan="2" &nbsp;
            %tr
                %td unique visits:
                %td $uniqueVisits$
            %tr
                %td!colspan="2" &nbsp;
            %tr
                %td most frequent IP:
                %td $frequentIp$

        %h3 File access
        %table
            $forall topEntries topEntry
                ^topEntryTemplate.topEntry^
|]
    where
        uniqueIps   = nub . map ipAddress
        frequentIps = frequency . map ipAddress
        formatTime' = formatTime defaultTimeLocale "[%d/%b/%Y:%H:%M:%S %z]"

topEntryTemplate :: (HamletValue a) => TopEntry -> Hamlet a
topEntryTemplate arg = [$hamlet|
    %tr
        %td!colspan="2" total $entryTitle.arg$ downloaded: $show.totalDownloads.arg$
    
    $forall downloadCounts.arg count
        %tr
            %td $show.snd.count$
            %td $fst.count$
|]

getTopEntry :: LogFile -> (String,String) -> IO TopEntry
getTopEntry lf (s,r) = do
    logEntries <- readLog lf
    return $ TopEntry
        { entryTitle     = s
        , totalDownloads = totalDownloaded r logEntries
        , downloadCounts = frequentDownloads r logEntries
        }
    where
        totalDownloaded s   = length . filter (isDownloadOf s)
        frequentDownloads s = frequency . map requestFile . filter (isDownloadOf s)
        isDownloadOf s e    = requestFile e =~ s :: Bool

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = reverse . sortBy (compare `on` snd) .  map (\xs' -> (head xs', length xs')) . group . sort
