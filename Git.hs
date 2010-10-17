{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Git
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- An attempt to provide a conventient way to reference files you keep
-- in git as routes, i.e.:
--
-- > %p
-- > 
-- >     Check out my new script in my
-- >
-- >     %a!href@GitR.scripts.aurget@ git repo
-- >
-- >     \.
--
-------------------------------------------------------------------------------
module Git where

import Data.List (intercalate)

data GitRepo = GitRepo
    { gitUser  :: String
    , gitRepo  :: String
    , gitPath  :: FilePath
    , gitFiles :: [String]
    }

loadGitRepo :: FilePath -> GitRepo
loadGitRepo = undefined

exampleRepo = GitRepo "pbrisbin" "scripts" "/home/patrick/.bin"
    [ "abuild"
    , "archive"
    , "aurget"
    , "automount"
    , "backup"
    , "bashnotify"
    ]

formatLink :: GitRepo -> String -> String
formatLink r f = intercalate "/" $ [ "http://github.com", gitUser r, gitRepo r, "blob", "master", f ]
