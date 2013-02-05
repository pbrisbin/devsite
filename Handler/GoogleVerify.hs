module Handler.GoogleVerify where

import Import

getGoogleVerifyR :: Handler RepHtml
getGoogleVerifyR = sendFile "text/html" "config/googled672395359cbd0e8.html"
