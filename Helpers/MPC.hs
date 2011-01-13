{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Helpers.MPC
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-- A Yesod subsite allowing in-browser controls for MPD.
--
-- Usage:
--
-- > -- default connection, no authentication
-- > instance YesodMPC MyApp where
-- >     mpdConfig  = Nothing
-- >     authHelper = return ()
-- >
-- > -- custom connection, using YesodAuth
-- > instance YesodMPC MyApp where
-- >     mpdConfig = Just . return $ MpdConfig "192.168.0.5" 6601 "Pa$$word"
-- >     authHelper = do
-- >         _ <- requireAuth
-- >         return ()
--
-- Since each page load and redirect makes a new connection to MPD (and 
-- I've yet to figure out to prevent this), I recommend you adjust your 
-- settings in /etc/mpd.conf to prevent "MPD Connection timeout" errors 
-- from appearing on the page during rapid "next" events
--
-- > connection_timeout	        "15" # was 60
-- > max_connections	        "30" # was 10
--
-- seem to work for me.
--
---------------------------------------------------------
module Helpers.MPC where

import Yesod


import Text.Hamlet
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax hiding (lift)

import System.IO

import qualified Data.Map as M
import qualified Network.MPD as MPD

-- | Customize your connection to MPD
data MpdConfig = MpdConfig
    { mpdHost     :: String
    , mpdPort     :: Integer
    , mpdPassword :: String
    }

data MPC = MPC

getMPC :: a -> MPC
getMPC = const MPC

class Yesod m => YesodMPC m where
    mpdConfig  :: Maybe (GHandler s m MpdConfig) -- ^ Just your connection info or Nothing
    authHelper :: GHandler s m ()                -- ^ some form of requireAuth or return ()

mkYesodSub "MPC" 
    [ ClassP ''YesodMPC [ VarT $ mkName "master" ]
    ] 
    [$parseRoutes|
    /           StatusR GET
    /prev       PrevR   GET
    /pause      PauseR  GET
    /next       NextR   GET

    /play/#Int   PlayR GET
    /delete/#Int DelR  GET
    |]

-- | Wrap MPD.withMPD or MPD.withMPDEx depending on the users mpd 
--   configuration
withMPD :: YesodMPC m => MPD.MPD a -> GHandler MPC m (MPD.Response a)
withMPD f = case mpdConfig of
    Nothing     -> liftIO $ MPD.withMPD f
    Just config -> do
        conf <- config
        liftIO $ MPD.withMPDEx (mpdHost conf) (mpdPort conf) (mpdPassword conf) f

-- | This is the main landing page. present now playing info and simple 
--   prev, pause, next controls. todo:s include playlist and library 
--   support, more advanced controls, maybe some album art
getStatusR :: YesodMPC m => GHandler MPC m RepHtml
getStatusR = do
    authHelper
    toMaster <- getRouteToMaster
    playing  <- getNowPlaying
    playlist <- formattedPlaylist toMaster 10
    controls <- playerControls toMaster
    defaultLayout $ do
        setTitle $ string "MPD"

        -- addCassius... {{{
        addCassius [$cassius|
        .state, .artist, .album
            color: #808080

        .year
            color: #707070

        .title
            color: #ffffff
            font-size: 200%
            padding-left: 10px

        .controls table
            margin-left: auto
            margin-right: auto
            padding: 5px
            padding-top: 20px
            padding-bottom: 20px

        .playlist table
            margin-left: auto
            margin-right: auto

        .playlist th
            border-bottom: solid 1px #909090

        .playlist td
            padding-left: 5px
            padding-right: 5px

        tr.current
            color: #ffffff

        td.playlist_button
            text-align: center
            font-size: 75%
            width: 20px

        |]
        -- }}}

        addHamlet [$hamlet| 
        %h1 MPD 
        ^playing^
        ^playlist^
        ^controls^
        %p.small
            %em
                %a!href="https://github.com/pbrisbin/devsite/blob/master/Helpers/MPC.hs" source code
        |]

-- | Previous
getPrevR :: YesodMPC m => GHandler MPC m RepHtml
getPrevR = authHelper >> actionRoute MPD.previous

-- | Smart play/pause button
getPauseR :: YesodMPC m => GHandler MPC m RepHtml
getPauseR = authHelper >> getPlayPause >>= actionRoute
    where
        -- | return the correct function give the current state
        getPlayPause :: YesodMPC m => GHandler MPC m (MPD.MPD ())
        getPlayPause = do
            result <- withMPD MPD.status
            case result of
                Right status -> case MPD.stState status of
                    MPD.Playing -> return $ MPD.pause True
                    MPD.Stopped -> return $ MPD.play Nothing
                    MPD.Paused  -> return $ MPD.play Nothing
                -- meh, should probably handle err better
                Left err -> return $ MPD.play Nothing

-- | Next
getNextR :: YesodMPC m => GHandler MPC m RepHtml
getNextR = authHelper >> actionRoute MPD.next

-- | Play a specific song in playlist
getPlayR :: YesodMPC m => Int -> GHandler MPC m RepHtml
getPlayR pid = do
    authHelper
    actionRoute $ MPD.playId pid

getDelR :: YesodMPC m => Int -> GHandler MPC m RepHtml
getDelR pid = do
    authHelper
    actionRoute $ MPD.deleteId pid

-- | Execute any mpd action then redirect back to the main status page
actionRoute :: YesodMPC m => MPD.MPD a -> GHandler MPC m RepHtml
actionRoute f = do
    toMaster <- getRouteToMaster
    _        <- withMPD f
    redirect RedirectTemporary $ toMaster StatusR

-- | Return formatted now playing information
getNowPlaying :: YesodMPC m => GHandler MPC m (Hamlet a)
getNowPlaying = do
    result <- withMPD MPD.currentSong
    state  <- getCurrentState
    case result of
        Left err          -> return [$hamlet| %em $string.show.err$ |]
        Right Nothing     -> return [$hamlet| %em -/-               |]
        Right (Just song) -> return $ formatState state song
    where
        -- output state
        getCurrentState :: YesodMPC m => GHandler MPC m String
        getCurrentState = do
            result <- withMPD MPD.status
            case result of
                Left err     -> return $ show err
                Right status -> case MPD.stState status of
                    MPD.Playing -> return "playing"
                    MPD.Stopped -> return "stopped"
                    MPD.Paused  -> return "paused"

        -- parse a Songs metatdata into a table
        formatState :: String -> MPD.Song -> Hamlet a
        formatState state song = let 
            title  = getTag MPD.Title  song
            artist = getTag MPD.Artist song
            album  = getTag MPD.Album  song
            year   = getTag MPD.Date   song
            in [$hamlet|
            .nowplaying
                %p
                    %span.state  $state$
                    \ / 
                    %span.artist $artist$
                    \ - 
                    %span.album  $album$
                    \ 
                    %span.year   ($year$)

                %p
                    %span.title $title$
            |]

-- | The control links themselves
playerControls :: (YesodMPC m, HamletValue a) => (MPCRoute -> HamletUrl a) -> GHandler MPC m a
playerControls toMaster = return [$hamlet|
    .controls
        %table
            %tr
                %td
                    %a!href=@toMaster.PrevR@  [ << ]
                %td
                    %a!href=@toMaster.PauseR@ [ || ]
                %td
                    %a!href=@toMaster.NextR@  [ >> ]
    |]

-- | A formatted play list, limited, auto-centered/highlighted on now 
--   playing, and with links to play and remove the entries
formattedPlaylist :: (YesodMPC m, HamletValue a)
                  => (MPCRoute -> HamletUrl a) -- ^ route to master
                  -> Int                       -- ^ limit display
                  -> GHandler MPC m a
formattedPlaylist toMaster limit = do
    cid    <- liftM (fromMaybe 0) currentId -- id of currently playing song
    result <- withMPD $ MPD.playlistInfo (Just $ (cid - limit `div` 2, cid + limit `div` 2))
    case result of
        Left err    -> return [$hamlet| %em $string.show.err$ |]
        Right songs -> do
            return [$hamlet|
            .playlist
                %table
                    %tr
                        %th #
                        %th Artist
                        %th Title
                        %th Play
                        %th Remove
                    $forall songs song
                        ^formatSong.song^
            |]
            where
                formatSong song = let
                    artist = getTag MPD.Artist song
                    title  = getTag MPD.Title  song
                    pid    = fromMaybe 0 . fmap snd $ MPD.sgIndex song
                    in [$hamlet| 
                    %tr.$clazz.pid$
                        %td $string.show.pid$
                        %td $artist$
                        %td $title$
                        %td.playlist_button
                            %a!href=@toMaster.PlayR.pid@ |>
                        %td.playlist_button
                            %a!href=@toMaster.DelR.pid@  X
                    |]
                    where
                        clazz x = if x == cid
                            then string "current"
                            else string "not_current"

-- | Return maybe the id of the currently playing song
currentId :: YesodMPC m => GHandler MPC m (Maybe Int)
currentId = do
    result <- withMPD $ MPD.currentSong
    case result of
        Left _            -> return Nothing
        Right Nothing     -> return Nothing
        Right (Just song) -> return . fmap snd $ MPD.sgIndex song

-- | Get the first instance of the given tag in the the passed song, 
--   return "N/A" if it's not found
getTag :: MPD.Metadata -> MPD.Song -> Html
getTag tag = string . head . M.findWithDefault ["N/A"] tag . MPD.sgTags

-- | print debug info out to stderr from within the GHandler monad
debug :: (Yesod m) => String -> GHandler s m ()
debug = liftIO . hPutStrLn stderr
