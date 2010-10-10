--
-- pbrisbin 2010
--
module DataTypes where

import Yesod
import Yesod.Helpers.Static

-- | The main site data type
data DevSite = DevSite { getStatic :: Static }

-- | A convenience synonymn
type Handler = GHandler DevSite DevSite

-- | A convenience synonymn
type HtmlTemplate = Hamlet (Route DevSite)

-- | The datatype of a Post
data Post = Post
    { postTitle   :: String
    , postSlug    :: String
    , postDate    :: String
    , postDescr   :: String
    , postContent :: HtmlTemplate
    , postTags    :: [String]
    }

