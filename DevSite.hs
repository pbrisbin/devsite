{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  DevSite
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- The actual site definition. What routes there are and instances it is
-- a part of.
--
-------------------------------------------------------------------------------
module DevSite where

import Yesod hiding (lift)
import Yesod.WebRoutes
import qualified Settings as S

import Control.Monad (liftM)
import Data.Char (toLower)
import Language.Haskell.TH.Syntax

import Comments
import System.IO

-- | The main site type
data DevSite = DevSite
type Handler = GHandler DevSite DevSite
type DWidget = GWidget  DevSite DevSite

-- | Define all of the routes and handlers
mkYesodData "DevSite" [$parseRoutes|
/              RootR    GET
/about         AboutR   GET
/posts         PostsR   GET
/posts/#String PostR    GET POST
/tags          TagsR    GET
/tags/#String  TagR     GET
/feed          FeedR    GET
/favicon.ico   FaviconR GET
/robots.txt    RobotsR  GET
|]

-- | Make my site an instance of Yesod so we can actually use it
instance Yesod DevSite where 
    approot _ = S.approot

    -- | override defaultLayout to provide an overall template and css
    --   file
    defaultLayout widget = do
        mmesg  <- getMessage
        pc     <- widgetToPageContent $ do
            widget
            addStyle $(S.cassiusFile "root-css")
        hamletToRepHtml $(S.hamletFile "root-layout")

-- | Make my site an instance of breadcrumbs so that i can simply call
--   the breadcrumbs function to get automagical breadcrumb links
instance YesodBreadcrumbs DevSite where
    -- root is the parent node
    breadcrumb RootR  = return ("root" , Nothing) 

    -- about goes back home
    breadcrumb AboutR = return ("about", Just RootR)

    -- all posts goes back home and individual posts go to all posts
    breadcrumb PostsR       = return ("all posts", Just RootR)
    breadcrumb (PostR slug) = return (format slug, Just PostsR)

        where
            -- switch underscores with spaces
            format []         = []
            format ('_':rest) = ' ': format rest
            format (x:rest)   = x  : format rest

    -- all tags goes back home and individual tags go to all tags
    breadcrumb TagsR      = return ("all tags", Just RootR)
    breadcrumb (TagR tag) = return (format tag, Just TagsR)

        where
            -- lowercase it
            format t = (map toLower t) ++ " tag"

    -- be sure to fail noticably so i fix it when it happens
    breadcrumb _ = return ("%%%", Just RootR)

-- | All comments on post pages
instance YesodComments DevSite where
    idFromRoute  (PostR slug) = slug
    idFromRoute  _            = ""
    storeComment (PostR slug) = hPutStrLn stderr . show
    storeComment _            = return $ return ()
    loadComments (PostR slug) = return $ []
    loadComments _            = return $ []

-- | This footer template needs to be in scope everywhere, so we'll
--   define it here
footerTemplate :: Hamlet DevSiteRoute
footerTemplate = [$hamlet|
#footer
    %hr
    %p
        %a!href=@RootR@ pbrisbin

        \ dot com 2010 

        %span!style="float: right;"
            powered by 
            
            %a!href="http://docs.yesodweb.com/" yesod
|]

-- | Template haskell to automate function declarations for use in
--   hamlet files
--
--   > mkConstant "foo"
--
--   is equivalent to writing directly,
--
--   > foo = "foo"
--
mkConstant :: String -> Q [Dec]
mkConstant s = do
    exp <- lift s
    let name = mkName $ cleanString s
    return [ FunD name [ Clause [] (NormalB exp) [] ] ]

-- | Similar but for lists
mkConstants :: [String] -> Q [Dec]
mkConstants []     = return []
mkConstants (s:ss) = do
    exp <- lift s
    let name = mkName $ cleanString s
    mkConstants ss >>= (\rest -> return $ FunD name [ Clause [] (NormalB exp) [] ] : rest)

-- | Clean a string before it becomes a function name
cleanString :: String -> String
cleanString = map (toLower . doClean)
    where
        doClean '.' = '_'
        doClean '-' = '_'
        doClean c   = c
