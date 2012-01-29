module Handler.Feed 
    ( getFeedR
    , getFeedTagR
    ) where

import Import
import Helpers.Post
import Yesod.RssFeed
import Text.Blaze (preEscapedText)

getFeedR :: Handler RepRss
getFeedR = do
    posts' <- runDB $ selectList [PostDraft !=. True] [Desc PostDate, LimitTo 10]
    case posts' of
        []    -> notFound
        posts -> feedFromPosts $ map entityVal posts

-- | Limited to a tag
getFeedTagR :: Text -> Handler RepRss
getFeedTagR tag = do
    posts' <- runDB $ do
        tags <- selectList [TagName ==. tag] []

        let pids = map (tagPost . entityVal) tags
        posts <- selectList [PostDraft !=. True, PostId <-. pids] [Desc PostDate]

        return $ map entityVal posts

    case posts' of
        []    -> notFound
        posts -> feedFromPosts posts

feedFromPosts :: [Post] -> Handler RepRss
feedFromPosts posts = do
    entries <- mapM postToRssEntry posts

    rssFeed Feed
        { feedTitle       = "pbrisbin dot com"
        , feedDescription = "New posts on pbrisbin dot com"
        , feedLanguage    = "en-us"
        , feedLinkSelf    = FeedR
        , feedLinkHome    = RootR
        , feedUpdated     = postDate $ head posts
        , feedEntries     = entries
        }

-- | Note: does not gracefully handle a post with no pandoc or in-db
--   content
postToRssEntry :: Post -> Handler (FeedEntry (Route DevSite))
postToRssEntry post = do
    markdown <- liftIO $ postMarkdown post

    return FeedEntry
        { feedEntryLink    = PostR $ postSlug post
        , feedEntryUpdated = postDate  post
        , feedEntryTitle   = postTitle post
        , feedEntryContent = cdata markdown
        }

        where
            -- Should appear as formatted HTML in readers that support
            -- that. Rss validation errors on script tag used by
            -- markdown conversion to obfuscate an email. Looks pretty
            -- bad in cli clients (but readable).
            cdata :: Markdown -> Html
            cdata mkd = mconcat [ preEscapedText "<![CDATA["
                                , markdownToHtml mkd
                                , preEscapedText "]]>"
                                ]
