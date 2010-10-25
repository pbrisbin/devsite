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
-- A generic Comments interface for a Yesod application.
--
-------------------------------------------------------------------------------
module Comments (commentsForm) where

import Comments.Core
import Comments.Templates
import Comments.Storage

import Yesod
import Control.Applicative        ((<$>), (<*>))
import Data.Time.Clock            (getCurrentTime)
import Network.Wai                (remoteHost)
import Text.Hamlet                (toHtml)
import Text.HTML.SanitizeXSS      (sanitizeXSS)

import Yesod.Form
import Yesod.Form.Core
import Control.Monad   (mplus)
import Data.Maybe      (fromMaybe)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- | Cleans form input and create a comment type to be stored
commentFromForm :: String -> CommentForm -> GHandler s m Comment
commentFromForm thread cf = do
    timeNow <- liftIO getCurrentTime
    ip      <- return . B.unpack . remoteHost =<< waiRequest

    if formIsHtml cf
        then return $ Comment thread timeNow ip (formUser cf) (htmlToHtml $ formComment cf)
        else return $ Comment thread timeNow ip (formUser cf) (textToHtml $ formComment cf)
    where
        -- the user entered html source directly
        htmlToHtml :: Textarea -> Html
        htmlToHtml = preEscapedString . sanitizeXSS . stripCRLF . unTextarea
        
        -- the user intends plaintext
        textToHtml :: Textarea -> Html
        textToHtml = toHtml . liftT stripCR

        -- with html, \r\n and \n should always become space
        stripCRLF []               = []
        stripCRLF ('\r':'\n':rest) = ' ' : stripCRLF rest
        stripCRLF ('\n':rest)      = ' ' : stripCRLF rest
        stripCRLF (x:rest)         = x   : stripCRLF rest

        -- with plaintext, \n will become <br>, \r should just be discarded
        stripCR []          = []
        stripCR ('\r':rest) =     stripCR rest
        stripCR (x:rest)    = x : stripCR rest

-- | lift a String function into Textara
liftT :: (String -> String) -> Textarea -> Textarea
liftT f = Textarea . f . unTextarea

-- | The input form itself
commentForm :: Maybe CommentForm -> Form s m CommentForm
commentForm cf = fieldsToTable $ CommentForm
    <$> userField    "name:"    (fmap formUser    cf)
    <*> commentField "comment:" (fmap formComment cf)
    <*> boolField    "html?"    (fmap formIsHtml  cf)

-- | todo: A copy of stringField but with custom validation
userField :: String -> FormletField sub y String
userField label initial = GForm $ do
    userId   <- newFormIdent
    userName <- newFormIdent
    env      <- askParams

    let res = case env of
                [] -> FormMissing
                _  ->
                    case lookup userName env of
                        Just userString -> 
                            if isValid userString
                                then FormSuccess userString
                                else FormFailure ["[a-zA-Z-_. ]"]
                        _               -> FormFailure ["Value is required"]

    let userValue = fromMaybe "" $ lookup userName env `mplus` initial
    let fi = FieldInfo { fiLabel   = string label
                       , fiTooltip = string ""
                       , fiIdent = userId
                       , fiInput = [$hamlet|
%input#userId!name=$userName$!type=text!value=$userValue$!size="22"
|]
                       , fiErrors =
                           case res of
                               FormFailure [x] -> Just $ string x
                               _               -> Nothing
                       }

    return (res, [fi], UrlEncoded)
    where
        isValid s  = (s /= []) && all (`elem` validChars) s
        validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_. " 

-- | A copy of textareaField but with a larger entry box
commentField :: FormFieldSettings -> FormletField sub y Textarea
commentField = requiredFieldHelper textareaFieldProfile'

textareaFieldProfile' :: FieldProfile sub y Textarea
textareaFieldProfile' = FieldProfile
    { fpParse  = Right . Textarea
    , fpRender = unTextarea
    , fpWidget = \theId name val _isReq -> addBody [$hamlet|
%textarea#$theId$!name=$name$!cols="100%"!rows="10" $val$
|]
    }

-- | Provides a single call to retrieve the html for the comments
--   section of a page
commentsForm :: (Yesod m)
             => ([Comment]
             -> GWidget s m ()
             -> Enctype
             -> GWidget s m ()) -- ^ the overall template
             -> CommentStorage  -- ^ how you store your comments
             -> String          -- ^ the id for the thread you're requesting
             -> Route m         -- ^ a route to redirect to after a POST
             -> GHandler s m (Hamlet (Route m))
commentsForm template db thread r = do
    -- POST if needed
    (res, form, enctype) <- runFormPost $ commentForm Nothing
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> do
            comment <- commentFromForm thread cf
            storeComment db comment
            -- redirect to prevent accidental reposts and to clear the
            -- form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary r

    -- load existing comments
    comments <- loadComments db thread

    -- return it as a widget
    --return $ template comments form enctype
    
    -- return it as hamlet; todo: this is a _hack_
    pc <- widgetToPageContent $ template comments form enctype
    return $ pageBody pc
