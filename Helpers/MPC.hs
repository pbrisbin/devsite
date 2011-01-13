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
---------------------------------------------------------
module Helpers.MPC where

import Yesod
import Text.Hamlet
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Data.Map as M
import qualified Network.MPD as MPD

data MpdConfig = MpdConfig
    { mpdHost     :: String
    , mpdPort     :: Integer
    , mpdPassword :: String
    }

data MPC = MPC

getMPC :: a -> MPC
getMPC = const MPC

class Yesod m => YesodMPC m where
    mpdConfig :: Maybe (GHandler s m MpdConfig)

mkYesodSub "MPC" 
    [ ClassP ''YesodMPC [ VarT $ mkName "master" ]
    ] 
    [$parseRoutes|
    /           StatusR GET
    /prev       PrevR   GET
    /pause      PauseR  GET
    /next       NextR   GET
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
    toMaster       <- getRouteToMaster
    nowPlayingInfo <- getNowPlaying
    currentState   <- getCurrentState
    defaultLayout $ do
        setTitle $ string "MPD"

        -- fine tune the table layouts
        addCassius [$cassius|
        .nowplaying table
            margin-left: auto
            margin-right: auto

        .nowplaying th
            width: 100px
            text-align: right

        .nowplaying td
            width: 300px
            text-align: center

        td.offset
            width: 100px

        .controls table
            margin-left: auto
            margin-right: auto
            padding: 5px
            padding-top: 20px
            padding-bottom: 20px
        |]

        addHamlet [$hamlet| 
        %h1 MPD 
        %p 
            ^currentState^
        ^nowPlayingInfo^
        ^playerControls.toMaster^
        |]
        where
            getCurrentState :: YesodMPC m => GHandler MPC m (Hamlet a)
            getCurrentState = do
                result <- withMPD MPD.status
                case result of
                    Right status -> case MPD.stState status of
                        MPD.Playing -> return [$hamlet| %em currently playing |]
                        MPD.Stopped -> return [$hamlet| %em currently stopped |]
                        MPD.Paused  -> return [$hamlet| %em currently paused  |]

-- | Previous
getPrevR :: YesodMPC m => GHandler MPC m RepHtml
getPrevR = actionRoute MPD.previous

-- | Smart play/pause button
getPauseR :: YesodMPC m => GHandler MPC m RepHtml
getPauseR = getPlayPause >>= actionRoute
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
getNextR = actionRoute MPD.next

-- | Execute an action then redirect back to the main status page
actionRoute :: YesodMPC m => MPD.MPD a -> GHandler MPC m RepHtml
actionRoute f = do
    toMaster <- getRouteToMaster
    _        <- withMPD f
    redirect RedirectTemporary $ toMaster StatusR

-- | Return formatted now playing information
getNowPlaying :: YesodMPC m => GHandler MPC m (Hamlet a)
getNowPlaying = do
    result <- withMPD MPD.currentSong
    case result of
        Left err          -> return [$hamlet| %em $string.show.err$ |]
        Right Nothing     -> return [$hamlet| %em nothing playing   |]
        Right (Just song) -> return . parseTags $ MPD.sgTags song
    where
        -- parse a Songs metatdata into a table
        parseTags :: M.Map MPD.Metadata [String] -> Hamlet a
        parseTags tags = let 
            title  = head $ M.findWithDefault ["N/A"] MPD.Title  tags 
            artist = head $ M.findWithDefault ["N/A"] MPD.Artist tags
            album  = head $ M.findWithDefault ["N/A"] MPD.Album  tags
            year   = head $ M.findWithDefault ["N/A"] MPD.Date   tags
            in [$hamlet|
            .nowplaying
                %table
                    %tr
                        %th Title:
                        %td $title$
                        %td.offset &nbsp;
                    %tr
                        %th Artist:
                        %td $artist$
                        %td.offset &nbsp;
                    %tr
                        %th Album:
                        %td $album$
                        %td.offset &nbsp;
                    %tr
                        %th Year:
                        %td $year$
                        %td.offset &nbsp;
            |]

-- | The control links themselves
playerControls :: (HamletValue a) => (MPCRoute -> HamletUrl a) -> a
playerControls toMaster = [$hamlet|
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
