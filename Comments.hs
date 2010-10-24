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
import Data.List.Split            (wordsBy)
import Data.Time.Clock            (UTCTime, getCurrentTime)
import Data.Time.Format           (parseTime, formatTime)
import Data.Maybe                 (mapMaybe)
import Text.Hamlet                (HamletValue, ToHtml, toHtml)
import Text.HTML.SanitizeXSS      (sanitizeXSS)
import System.Locale              (defaultTimeLocale)
import System.IO                  (hPutStrLn, stderr)

-- | The actual comment data type, this is what must be stored and
--   loaded in your instance functions.
data Comment = Comment
    { thread    :: String
    , timeStamp :: UTCTime
    , ipAddress :: String
    , userName  :: String
    , content   :: Html
    } deriving Show

-- | The form data type, this is used to gather the comment from the
--   user and is handed off to commentFromForm just before storeComment.
data CommentForm = CommentForm
    { formUser    :: String
    , formComment :: Textarea
    , formIsHtml  :: Bool
    }

-- | Instantiate your app for comments
class YesodComments y where
    idFromRoute  :: Route y -> String
    storeComment :: Route y -> Comment -> IO ()
    loadComments :: Route y -> IO [Comment]

-- | Cleans form input and create a comment type to be stored
commentFromForm :: String -> CommentForm -> IO Comment
commentFromForm id cf = do
    timeNow <- getCurrentTime
    ip      <- getRequestIp
    if formIsHtml cf
        then return $ Comment id timeNow ip (formUser cf) (preEscapedString . sanitizeXSS . stripNewLines . unTextarea $ formComment cf)
        else return $ Comment id timeNow ip (formUser cf) (toHtml $ formComment cf)
    where
        -- todo: how to get the ip?
        getRequestIp = return "0.0.0.0"

        stripNewLines []               = []
        stripNewLines ('\r':'\n':rest) = ' ' : stripNewLines rest
        stripNewLines ('\n':rest)      = ' ' : stripNewLines rest
        stripNewLines (x:rest)         = x   : stripNewLines rest

-- | The input form, todo: add validation on the username
commentForm :: Maybe CommentForm -> Form s m CommentForm
commentForm cf = fieldsToTable $ CommentForm
    <$> stringField  "name:"    (fmap formUser    cf)
    <*> commentField "comment:" (fmap formComment cf)
    <*> boolField    "html?"    (fmap formIsHtml  cf)

-- | A copy of textareaField but with a larger entry box
commentField :: FormFieldSettings -> FormletField sub y Textarea
commentField = requiredFieldHelper textareaFieldProfile'

textareaFieldProfile' :: FieldProfile sub y Textarea
textareaFieldProfile' = FieldProfile
    { fpParse  = Right . Textarea
    , fpRender = unTextarea
    , fpWidget = \theId name val _isReq -> addBody [$hamlet|
%textarea#$theId$!name=$name$!cols=50!rows=6 $val$
|]
    }

-- | todo: The ghc-inferred type signature
commentsForm rThread rRedirect = do
    -- load existing comments for this route
    comments <- liftIO $ loadComments rThread

    -- read/load the form for this route
    (res, form, enctype) <- runFormPost $ commentForm Nothing
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> do
            liftIO $ do
                comment <- commentFromForm (idFromRoute rThread) cf
                storeComment rThread comment
            -- redirect to prevent accidental reposts and to clear the
            -- form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary $ rRedirect

    -- return it as a widget
    --return $ commentsTemplate comments form enctype
    -- return it as hamlet; this is a _hack_
    pc <- widgetToPageContent $ commentsTemplate comments form enctype
    return $ pageBody pc


-- | Template for the entire comments section
commentsTemplate :: (HamletValue a, ToHtml b) => [Comment] -> a -> b -> a
commentsTemplate comments form enctype = [$hamlet|
#comments

    %h4 $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^

    %h4 Add a comment:

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
    let date = formatTime defaultTimeLocale "%a, %b %d at %H:%S" $ timeStamp comment
    in [$hamlet|
%p
    On 
    %strong $date$
    , 
    %strong $userName.comment$
    \ wrote:

%blockquote
    %p
        $content.comment$
|]
