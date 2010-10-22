{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Trying to provide a generic Comments interface for a Yesod
-- application. Ideally, one should make one's app an instance of
-- Comments by defining only loadComments and storeComment; that part's
-- a todo still...
--
-------------------------------------------------------------------------------
module Comments where

import Yesod
import Control.Applicative ((<$>), (<*>))
import Text.Hamlet         (HamletValue, ToHtml)
import System.IO

-- | todo: commentDate
--   todo: commentIp
--   todo: commentContent should be Html
data Comment = Comment
    { commentUser    :: String
    , commentContent :: String
    }

-- | todo: interface with a DB? flat file?
storeComment :: String -> Comment -> IO ()
storeComment id comment = hPutStrLn stderr $ -- just echo it for testing purposes
    "storing comment " ++ (commentContent comment) ++ " from " ++ (commentUser comment) ++ "..."

-- | todo: interface with a DB? flat file?
loadComments :: String -> [Comment]
loadComments id = -- returning example comments for now
    [ Comment "joe" "Some comment"
    , Comment "jim" "Some other comment"
    ]

-- | todo: text area
commentForm :: Maybe Comment -> Form s m Comment
commentForm comment = fieldsToTable $ Comment
    <$> stringField "name"    (fmap commentUser comment)
    <*> stringField "comment" (fmap commentContent comment)

-- | Do everything required to handle comments and return only the
--   hamlet to be added to the page
getCommentsHamlet :: (Yesod m) => String -> GHandler s m (Hamlet (Route m))
getCommentsHamlet id = do
    -- load existing comments for this page
    let comments = loadComments id

    -- read any form entry
    (res, form, enctype) <- runFormPost $ commentForm Nothing
    case res of
        FormMissing         -> return ()
        FormFailure _       -> return ()
        FormSuccess comment -> liftIO $ storeComment id comment

    -- set the overal form content
    pc <- widgetToPageContent $ commentsTemplate comments form enctype
    return $ pageBody pc

-- | Template for the entire comments piece
commentsTemplate :: (HamletValue a, ToHtml b) => [Comment] -> a -> b -> a
commentsTemplate comments form enctype = [$hamlet|
%h3 Comments

#comments
    $forall comments comment
        ^commentTemplate.comment^

    %form!enctype=$enctype$!method="post"
        %table
            ^form^
            %tr
                %td!colspan=2
                    %input!type=submit
|]

-- | Sub template for a single comment
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = [$hamlet|
%p
    $commentUser.comment$

%blockquote
    $commentContent.comment$
|]
