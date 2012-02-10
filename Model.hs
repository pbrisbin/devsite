module Model where

import Prelude
import Yesod

import Data.Text      (Text)
import Data.Time      (UTCTime)
import Yesod.Markdown (Markdown)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFile "config/models")
