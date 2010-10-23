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
import Yesod.Form.Core            (FieldProfile(..), requiredFieldHelper)
import Control.Applicative        ((<$>), (<*>))
import Data.Time.Clock            (UTCTime, getCurrentTime)
import Data.Time.Format           (parseTime, formatTime)
import Text.Hamlet                (HamletValue, ToHtml, toHtml)
import Text.HTML.SanitizeXSS      (sanitizeXSS)
import System.Locale              (defaultTimeLocale)
import System.IO                  (hPutStrLn, stderr)

-- | A Comment datatype; I'm cheating and making most things Strings
--   because it's way easier to deal with.
data Comment = Comment
    { commentIp      :: String
    , commentTime    :: String
    , commentUser    :: String
    , commentContent :: Textarea
    , commentHtml    :: Bool
    } deriving Show

-- | todo:
storeComment :: String -> Comment -> IO ()
storeComment id comment = hPutStrLn stderr $ show comment

-- | todo:
loadComments :: String -> IO [Comment]
loadComments id = return $ 
    [ Comment "192.168.0.1" "1287765561" "joe" (Textarea "hello\r\nw<strong>or</strong>ld") True
    , Comment "192.168.0.2" "1287765568" "jim" (Textarea "hello<br />world"               ) True
    , Comment "192.168.0.3" "1287765568" "pat" (Textarea "hello\r\nworld"                 ) False
    , Comment "192.168.0.4" "1287765568" "dan" (Textarea "hello world"                    ) False
    ]

-- | Add the time and Ip to the comment, this is called immediately
--   before storeComment
addTimeAndIp :: Comment -> IO Comment
addTimeAndIp comment = do
    utc <- getCurrentTime
    let s = formatTime defaultTimeLocale "%s" utc
    return $ comment
        { commentTime = s
        -- todo: the ip
        , commentIp   = "192.168.0.1"
        }

-- | Used to initialize the form so we can define time/ip later (when
--   saving) without having to make it a Maybe type
initComment :: Maybe Comment
initComment = Just $ Comment "X" "X" "" (Textarea "") False

-- | The input Form, todo: customize it - no table
commentForm :: Maybe Comment -> Form s m Comment
commentForm comment = fieldsToTable $ Comment
    <$> hiddenField    ""         (fmap commentIp      comment)
    <*> hiddenField    ""         (fmap commentTime    comment)
    <*> stringField    "name:"    (fmap commentUser    comment)
    <*> textareaField' "comment:" (fmap commentContent comment)
    <*> boolField      "html?"    (fmap commentHtml    comment)

-- | just like textareaField but with a bigger entry box
textareaField' :: FormFieldSettings -> FormletField sub y Textarea
textareaField' = requiredFieldHelper textareaFieldProfile'

textareaFieldProfile' :: FieldProfile sub y Textarea
textareaFieldProfile' = FieldProfile
    { fpParse  = Right . Textarea
    , fpRender = unTextarea
    , fpWidget = \theId name val _isReq -> addBody [$hamlet|
%textarea#$theId$!name=$name$!cols=60!rows=10 $val$
|]
    }

-- | Do everything required to handle comments and return only the
--   hamlet to be added to the page, redirect to r after a POST
getCommentsHamlet :: (Yesod m) 
                  => String  -- ^ The id to pass to loadComments
                  -> Route m -- ^ the route to redirect to after posting
                  -> GHandler s m (Hamlet (Route m)) 
getCommentsHamlet id r = do
    -- load existing comments for this page
    comments <- liftIO $ loadComments id

    -- read the form entry
    (res, form, enctype) <- runFormPost $ commentForm initComment
    case res of
        FormMissing         -> return ()
        FormFailure _       -> return ()
        FormSuccess comment -> do
            liftIO $ do
                saveComment <- addTimeAndIp comment
                storeComment id saveComment
            -- redirect to prevent accidental reposts and
            -- to clear the form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary $ r

    -- return the overall form content; todo: this is a _hack_
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
                    &nbsp;
                %td
                    %input!type=submit 
                    %input!type=reset

    %p 
        %em when using html, assume your text is already wrapped in &lt;p&gt;
|]

-- | Sub template for a single comment
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = 
    let html = formatHtml comment
        date = parseDate (commentTime comment)
    in [$hamlet|
%p
    On 
    %strong $date$
    , 
    %strong $commentUser.comment$
    \ wrote:

%blockquote
    %p
        $html$
|]
    where
        formatHtml c =
            if (commentHtml c)
                -- comment is html, sanitize and render
                then preEscapedString  . sanitizeXSS . unTextarea $ commentContent c
                -- comment is plain text, translate and render
                else toHtml $ commentContent c

        parseDate s = case (parseTime defaultTimeLocale "%s" s :: Maybe UTCTime) of
            Just utc -> formatTime defaultTimeLocale "%a, %b %d at %H:%S" utc
            Nothing  -> "???"
