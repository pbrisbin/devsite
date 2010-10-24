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
-- application.
--
-------------------------------------------------------------------------------
module Comments 
    ( commentsForm
    , CommentStorage(..)
    , fileDB
    , testDB
    ) where

import Yesod
import Yesod.Form.Core            (FieldProfile(..), requiredFieldHelper)
import Control.Applicative        ((<$>), (<*>))
import Data.List                  (intercalate)
import Data.List.Split            (wordsBy)
import Data.Time.Clock            (UTCTime, getCurrentTime)
import Data.Time.Format           (parseTime, formatTime)
import Data.Maybe                 (mapMaybe)
import Network.Wai                (remoteHost)
import Text.Hamlet                (HamletValue, ToHtml, toHtml)
import Text.HTML.SanitizeXSS      (sanitizeXSS)
import System.Locale              (defaultTimeLocale)
import System.IO                  (hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- | The actual comment data type
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
            -- a comment with a literal '|' in it will be lost...
            let str = concat [ (thread comment),                  "|"
                             , (formatTime' $ timeStamp comment), "|"
                             , (ipAddress comment),               "|"
                             , (userName comment),                "|"
                             , (htmlToString $ content comment), "\n"
                             ]
            liftIO $ appendFile f str

        formatTime'  = formatTime defaultTimeLocale "%s"
        htmlToString = L.unpack . renderHtml

        loadComments' id = do
            contents <- liftIO $ readFile f
            return $ mapMaybe (readComment id) (lines contents)

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
commentFromForm :: String -> CommentForm -> GHandler s m Comment
commentFromForm thread cf = do
    timeNow <- liftIO getCurrentTime
    ip      <- return . B.unpack . remoteHost =<< waiRequest

    -- return the comment
    if formIsHtml cf
        then return $ Comment thread timeNow ip (formUser cf) (htmlToHtml $ formComment cf)
        else return $ Comment thread timeNow ip (formUser cf) (textToHtml $ formComment cf)
    where
        -- the user entered html source directly
        htmlToHtml :: Textarea -> Html
        htmlToHtml = preEscapedString . sanitizeXSS . stripNewLines . unTextarea
        
        -- the user intends plaintext
        textToHtml :: Textarea -> Html
        textToHtml = toHtml . liftT stripReturn

        -- with html, \r\n and \n should always become space
        stripNewLines []               = []
        stripNewLines ('\r':'\n':rest) = ' ' : stripNewLines rest
        stripNewLines ('\n':rest)      = ' ' : stripNewLines rest
        stripNewLines (x:rest)         = x   : stripNewLines rest

        -- with plaintext, \n will become <br>, \r should just be discarded
        stripReturn []          = []
        stripReturn ('\r':rest) =     stripReturn rest
        stripReturn (x:rest)    = x : stripReturn rest

-- | lift a String function into Textara
liftT :: (String -> String) -> (Textarea -> Textarea)
liftT f = Textarea . f . unTextarea

-- | lift a String function into Html
--liftH :: (String -> String) -> (Html -> Html)
--liftH f = preEscapedString . f . L.unpack . renderHtml

-- | The input form itself
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
            comment <- commentFromForm thread cf
            storeComment db $ comment
            -- redirect to prevent accidental reposts and to clear the
            -- form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary $ r

    -- load existing comments
    comments <- loadComments db $ thread

    -- return it as a widget
    --return $ commentsTemplate comments form enctype
    
    -- return it as hamlet; todo: this is a _hack_
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
