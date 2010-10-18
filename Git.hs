{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
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

import Yesod hiding (lift)
import Yesod.WebRoutes

import Data.List (intercalate)

data Git = Git
data GitRoute = GitRoute [String] deriving (Eq, Show, Read)
type instance Route Git = GitRoute

-- | Fully describe a git repo
data GitRepo = GitRepo
    { gitUser  :: String   -- ^ The username
    , gitRepo  :: String   -- ^ The repo name
    , gitPath  :: FilePath -- ^ It's local path
    , gitFiles :: [String] -- ^ A list of the files contained
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

-- | Given a repo name and file name format a proper github link to the
--   file's page
formatLink :: GitRepo -- ^ The Repo
           -> String  -- ^ The filename
           -> String  -- ^ The resulting link
formatLink r f = intercalate "/" $ [ "http://github.com", gitUser r, gitRepo r, "blob", "master", f ]
