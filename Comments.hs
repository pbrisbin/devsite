{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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
module Comments 
    ( commentsForm
    , CommentStorage(..)
    , testDB
    , fileDB
    ) where

import Yesod
import Yesod.Form.Core            (FieldProfile(..), requiredFieldHelper)
import Control.Applicative        ((<$>), (<*>))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List                  (intercalate)
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

-- | A data type to represent your backend. Provides an abstract
--   interface for use by the code here.
data CommentStorage = CommentStorage
    { storeComment :: (Yesod m) => Comment -> GHandler s m ()
    , loadComments :: (Yesod m) => String  -> GHandler s m [Comment]
    }

-- | For use during testing, always loads no comments and prints the
--   comment to stderr as "store"
testDB :: CommentStorage
testDB = CommentStorage
    { storeComment = liftIO . hPutStrLn stderr . show
    , loadComments = (\_ -> return $ [])
    }

-- | A simple flat file storage method, this is way unreliable, probably
--   wicked slow, but dead simple to setup/use
fileDB :: FilePath -> CommentStorage
fileDB f = CommentStorage
    { storeComment = storeComment'
    , loadComments = loadComments'
    }
    where
        storeComment' comment = do
            let str = concat [ (thread comment),                  "|"
                             , (formatTime' $ timeStamp comment), "|"
                             , (ipAddress comment),               "|"
                             , (userName comment),                "|"
                             , (htmlToString $ content comment), "\n"
                             ]
            liftIO $ appendFile f str

        formatTime'  = formatTime defaultTimeLocale "%s"
        htmlToString = unpack . renderHtml

        loadComments' id = do
            contents <- liftIO $ readFile f
            let strings = lines contents
            return $ mapMaybe (readComment id) strings

        readComment id' s = 
            case (wordsBy (=='|') s) of
                [t, ts, ip, user, html] -> 
                    if t == id'
                        then
                            case readTime ts of
                                Just utc -> Just $ 
                                    Comment
                                        { thread    = t
                                        , timeStamp = utc
                                        , ipAddress = ip
                                        , userName  = user
                                        , content   = preEscapedString html
                                        }
                                _ -> Nothing
                        else Nothing
                _ -> Nothing

        readTime :: String -> Maybe UTCTime
        readTime = parseTime defaultTimeLocale "%s"

-- | Cleans form input and create a comment type to be stored
commentFromForm :: String -> CommentForm -> IO Comment
commentFromForm thread cf = do
    timeNow <- getCurrentTime
    ip      <- getRequestIp
    if formIsHtml cf
        then return $ Comment thread timeNow ip (formUser cf) (preEscapedString . sanitizeXSS . stripNewLines . unTextarea $ formComment cf)
        else return $ Comment thread timeNow ip (formUser cf) (toHtml . Textarea . stripReturn . unTextarea $ formComment cf)
    where
        -- todo: how to get the ip?
        getRequestIp = return "0.0.0.0"

        -- with html formatting \r\n and \n should always become space
        stripNewLines []               = []
        stripNewLines ('\r':'\n':rest) = ' ' : stripNewLines rest
        stripNewLines ('\n':rest)      = ' ' : stripNewLines rest
        stripNewLines (x:rest)         = x   : stripNewLines rest

        -- \n will become <br>, \r should be disgarded
        stripReturn []          = []
        stripReturn ('\r':rest) =     stripReturn rest
        stripReturn (x:rest)    = x : stripReturn rest

-- | The input form, todo: add validation on the username
commentForm :: Maybe CommentForm -> Form s m CommentForm
commentForm cf = fieldsToTable $ CommentForm
    <$> stringField  "name:"    (fmap formUser    cf)
    <*> commentField "comment:" (fmap formComment cf)
    <*> boolField    "html?"    (fmap formIsHtml  cf)

-- | todo: A copy of stringField but with custom validation
--userField :: String -> FormletField sub y String
--userField label initial = GForm $ do
--    userId   <- newFormIdent
--    userName <- newFormIdent
--    env      <- askParams
--
--    let res = undefined
--        case env of
--            [] -> FormMissing
--            _  ->
--                case (lookup userName env) of
--                    Just userString -> undefined -- validate string
--                    _               -> FormFailure "Username required."
--
--    let userValue = fromMaybe "" $ lookup userName env `mplus` initial
--    let fi = FieldInfo
--        { fiLabel = label
--        , fiTooltip = ""
--        , fiIdent = userId
--        , fiInput = [$hamlet| test |]
--        , fiErrors =
--            case res of
--                FormFailure [x] -> Just $ string x
--                _               -> Nothing
--        }
--    return (res, [fi], UrlEncoded)

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

-- | Provides a single call to retrieve the html for the comments
--   section of a page
commentsForm :: (Yesod m)
             => CommentStorage -- ^ how you store your comments
             -> String         -- ^ the id for the thread you're requesting
             -> Route m        -- ^ a route to redirect to after a POST
             -> GHandler s m (Hamlet (Route m))
commentsForm db thread r = do
    -- POST if needed
    (res, form, enctype) <- runFormPost $ commentForm Nothing
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> do
--            liftIO $ do
--                comment <- commentFromForm thread cf
--                storeComment db $ comment

            comment <- liftIO $ commentFromForm thread cf
            storeComment db $ comment
            -- redirect to prevent accidental reposts and to clear the
            -- form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary $ r

    -- load existing comments
    --comments <- liftIO $ loadComments db $ thread
    comments <- loadComments db $ thread

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
        %em when using html, assume your text will be wrapped in &lt;p&gt &lt;/p&gt;
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
