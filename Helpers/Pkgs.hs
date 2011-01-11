{-# LANGUAGE QuasiQuotes #-}
---------------------------------------------------------
--
-- Module        : Helpers.Pkgs
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Unstable
-- Portability   : portable
--
---------------------------------------------------------
module Helpers.Pkgs 
    ( allPkgs
    , pkgsTemplate
    , pkgTemplate
    ) where

import Yesod
import DevSite

data AurPkg url = AurPkg
    { pkgId    :: Int
    , pkgName  :: String
    , pkgDescr :: String
    , homePage :: url
    }

pkgsTemplate :: [AurPkg url] -> Hamlet url
pkgsTemplate args = [$hamlet|
#aur_pkgs
    %table
        $forall args arg
            ^pkgTemplate.arg^
|]

pkgTemplate :: AurPkg url -> Hamlet url
pkgTemplate arg = 
    let link = "http://aur.archlinux.org/packages.php?ID=" ++ show (pkgId arg)
    in [$hamlet|
.aur_pkg
    %tr
        %td 
            %a!href=@homePage.arg@ $pkgName.arg$
        %td 
            $pkgDescr.arg$
        %td [
            %a!href=$link$ pkg
            ]
|]

allPkgs =
    [ AurPkg 31933 "aurget"    "A simple Pacman-like interface to the AUR"                                                (PostR "aurget") 
    , AurPkg 31937 "downgrade" "Bash script for downgrading one or more packages to a version in your cache or the A.R.M" (PostR "downgrade")
    , AurPkg 42433 "dvdcopy"   "Provides a means to easily backup a DVD9 to DVD5, among other things"                     (PostR "dvdcopy")
    ]
