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

-- | A Comment datatype; I'm cheating and making most things Strings
--   because it's way easier to deal with.
data Comment = Comment
    { commentIp      :: String
    , commentTime    :: String
    , commentUser    :: String
    , commentContent :: Html
    , commentHtml    :: Bool
    } deriving Show

-- | todo:
storeComment :: String -> Comment -> IO ()
storeComment id comment = hPutStrLn stderr $ show comment

-- | todo: 
loadComments :: String -> IO [Comment]
loadComments id = return $ 
    [ Comment "192.168.0.1" "1287765561" "joe" (preEscapedString "<p>With HTML, i <strong>have</strong> formatting</p>") True
    , Comment "192.168.0.2" "1287765568" "jim" (string           "But Non html is so > html"                           ) False
    ]

-- | The input Form, todo: customize it - no table
commentForm :: Maybe Comment -> Form s m Comment
commentForm comment = fieldsToTable $ Comment
    <$> hiddenField   ""         (fmap commentIp      comment)
    <*> hiddenField   ""         (fmap commentTime    comment)
    <*> stringField   "name:"    (fmap commentUser    comment)
    <*> htmlField     "comment:" (fmap commentContent comment)
    <*> boolField     "html?:"   (fmap commentHtml    comment)

-- | Initialize the comment form with current time and user's ip
initComment :: Maybe Comment
initComment = Just $ Comment getUserIp getCurrentTime "" "" False
    where
        -- todo:
        getUserIp      = "192.168.0.1"
        getCurrentTime = "1287765561"

-- | Do everything required to handle comments and return only the
--   hamlet to be added to the page, redirect to r after a POST
getCommentsHamlet :: (Yesod m) 
                  => String  -- ^ The id to pass to loadComments
                  -> Route m -- ^ the route to redirect to after posting
                  -> GHandler s m (Hamlet (Route m)) 
getCommentsHamlet id r = do
    -- load existing comments for this page
    comments <- liftIO $ loadComments id

    -- read any form entry
    (res, form, enctype) <- runFormPost $ commentForm initComment
    case res of
        FormMissing         -> return ()
        FormFailure _       -> return ()
        FormSuccess comment -> do
            liftIO $ storeComment id comment
            -- redirect to prevent accidental reposts and
            -- to clear the form data
            redirect RedirectTemporary $ r

    -- return the overall form content
    pc <- widgetToPageContent $ commentsTemplate comments form enctype
    return $ pageBody pc

-- | Template for the entire comments section
commentsTemplate :: (HamletValue a, ToHtml b) => [Comment] -> a -> b -> a
commentsTemplate comments form enctype = [$hamlet|
#comments
    $forall comments comment
        ^commentTemplate.comment^

    %form!enctype=$enctype$!method="post"
        %table
            ^form^
            %tr
                %td
                    %input!type=submit
                %td
                    %input!type=reset
|]

-- | Sub template for a single comment
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = [$hamlet|
%p
    %strong $commentUser.comment$
    \ wrote:

%blockquote
    %p
        $commentContent.comment$
|]
