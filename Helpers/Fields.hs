module Helpers.Fields (checkBoxField) where

import Prelude
import Yesod.Widget
import Yesod.Message (RenderMessage)
import Yesod.Form.Types
import qualified Data.Text as T

checkBoxField :: RenderMessage m FormMessage => Field s m Bool
checkBoxField = Field
    { fieldParse = return . checkBoxParser
    , fieldView  = \theId name theClass val _ -> [whamlet|
        <input id=#{theId} :not (null theClass):class="#{T.intercalate " " theClass}" type=checkbox name=#{name} value=yes :showVal id val:checked>
        |]
    }

    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)
