module Handler.Posts 
    ( getPostR
    , postPostR
    , getManagePostsR
    , postManagePostsR
    , getNewPostR
    , postNewPostR
    , getEditPostR
    , postEditPostR
    , getDelPostR
    ) where

import Import
import Prelude (init, last)
import Helpers.Post
import Yesod.Comments (addComments)
import Yesod.Links
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (takeBaseName)
import qualified Data.Text as T

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    (post,tags,mprev,mnext) <- runDB $ do
        (post',tags') <- getPost404 slug
        mprev'        <- getPreviousPost post'
        mnext'        <- getNextPost     post'

        return $ (post',tags',mprev',mnext')

    (published,content) <- liftIO $ do
        published' <- humanReadableTime $ postDate post
        content'   <- postContent post

        return (published',content')

    defaultLayout $ do
        setTitle $ toHtml slug
        addKeywords $ map tagName tags
        $(widgetFile "post/show")

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    posts <- runDB $ selectList [] [Desc PostDate]
    tags  <- fmap (map entityVal) $ runDB $ selectList [] [Asc TagName]

    (now,unknowns) <- liftIO $ do
        now'      <- getCurrentTime
        unknowns' <- getUnknowns $ map (postSlug . entityVal) posts

        return (now',unknowns')

    defaultLayout $ do
        setTitle "Manage posts"
        $(widgetFile "post/index")

    where
        -- look for pandoc files with no associated post and present
        -- them for easy creation
        getUnknowns :: [Text] -> IO [Text]
        getUnknowns knowns = do
            files <- getDirectoryContents "/home/patrick/Site/pandoc"

            return . filter (`notElem` knowns)
                   . map (T.pack . takeBaseName)
                   $ filter (not . hidden) files

            where
                hidden :: FilePath -> Bool
                hidden ('.':_) = True
                hidden _       = False

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getNewPostR :: Handler RepHtml
getNewPostR = do
    -- we can pass a slug via GET param to prepopulate the form when
    -- we've got something on the filesystem to start from
    mslug <- runInputGet $ iopt textField "slug"

    ((res,form), enctype) <- runFormPost $ postForm (fmap mkStub mslug)

    case res of
        FormSuccess pf -> upsertPost pf
        _              -> return ()

    defaultLayout $ do
        setTitle "New post"
        $(widgetFile "post/new")

    where
        mkStub :: Text -> (Post,[Tag])
        mkStub slug = (Post
            { postSlug  = slug
            , postDate  = undefined -- FIXME called now, since fields are strict
            , postTitle = titleize slug
            , postDescr = Nothing
            , postDraft = True
            }, [])

        titleize :: Text -> Text
        titleize = T.unwords . map capitalize . T.words . T.map score

        capitalize :: Text -> Text
        capitalize = (\(x,xs) -> T.toUpper x `T.append` xs) . T.splitAt 1

        score :: Char -> Char
        score '_' = ' '
        score  x  =  x

postNewPostR :: Handler RepHtml
postNewPostR = getNewPostR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    record <- runDB $ getPost404 slug

    ((res,form), enctype) <- runFormPost $ postForm $ Just record

    case res of
        FormSuccess pf -> upsertPost pf
        _              -> return ()

    defaultLayout $ do
        setTitle "Edit post"
        $(widgetFile "post/edit")

postEditPostR :: Text -> Handler RepHtml
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler RepHtml
getDelPostR slug = do
    msg <- runDB $ do
        mentity <- getBy $ UniquePost slug

        case mentity of
            Just (Entity key _) -> do
                deleteWhere [TagPost ==. key]
                delete key
                return "post deleted!"

            _ -> return "post not found!"

    setMessage msg
    redirect ManagePostsR
