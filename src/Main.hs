{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List as L
import           Data.Monoid
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Hakyll as H

main :: IO ()
main = hakyll $ do
    match (H.fromList ["humans.txt", "robots.txt"]) $ do
        route   idRoute
        compile $ getResourceBody >>= applyAsTemplate defContext

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString
              >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
              >>= return . fmap compressCss

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    tagsRules tags $ \ tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            makeItem ""
              >>= loadAndApplyTemplate "templates/tags.html"    (tagsCtx posts tag tags)
              >>= loadAndApplyTemplate "templates/default.html" (tagsCtx posts tag tags)
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" "Archives"                   `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match "sitemap.xml" $ do
        route   idRoute
        compile $ do
            posts <- loadAll "posts/*"
            let sitemapCtx = listField "posts" (postCtx tags) (return posts) <>
                             defContext
            getResourceBody >>= applyAsTemplate sitemapCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
          renderAtom feedConfiguration (postCtx tags) posts

author, email, root :: String
author = "Sam Truzjan"
email  = "pxqr.sta@gmail.com"
root   = "https://pxqr.github.io"


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "pxqr's blog"
  , feedDescription = "TODO"
  , feedAuthorName  = author
  , feedAuthorEmail = email
  , feedRoot        = root
  }

headings :: Pandoc -> [String]
headings = query extractHeader
  where
    extractHeader (Header _ _ inlines) = [unwords $ query extractStr inlines]
    extractHeader _                    = []

    extractStr    (Str    str        ) = [str]
    extractStr    _                    = []

headingsField :: Context String
headingsField = listField "headings" headingField getHeadings
  where
    headingField =
        field "caption"  (return . itemBody) `mappend`
        field "fragment" (return . hToId . itemBody)
      where
        hToId = intercalate "-" . words . L.map toLower

    getHeadings = do
      body <- getResourceBody
      mapM makeItem $ headings $ itemBody $ readPandoc body

defContext :: Context String
defContext =
    constField "root"   root `mappend`
    defaultContext

formatField :: String -> Context String
formatField name = field name (return . fileFormat)
  where
    fileFormat = show . fileType . toFilePath . itemIdentifier

prettyTagsField :: String -> Tags -> Context String
prettyTagsField name tags
    = tagsFieldWith getTags renderLink (mconcat . intersperse ",") name tags
  where
    renderLink :: String -> (Maybe FilePath) -> Maybe H.Html
    renderLink tag Nothing         = Just $ H.a $ toHtml tag
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) ! A.title (toValue linkTitle) $ toHtml tag
      where
        linkTitle = show postCount ++ " post" ++ suff ++ " tagged with " ++ tag
          where
            suff = if postCount == 1 then "" else "s"
        Just postCount = fmap L.length $ L.lookup tag $ tagsMap tags

postCtx :: Tags -> Context String
postCtx tags =
    formatField "format"             `mappend`
    dateField "date"     "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d"  `mappend` -- used by sitemap template
    prettyTagsField "taglist" tags   `mappend`
    headingsField                    `mappend`
    defContext

relatedCtx :: String -> Tags -> Context String
relatedCtx tag tags
  | L.null related = mempty
  | otherwise      = mconcat
  [ listField  "related" (field "tag" (return . itemBody)) (mapM makeItem related)
  , constField "related-count" (show (L.length related))
  ]
  where
    -- FIX: O(tag_count * tag_count * post_count)
    -- may take a long time in the future
    related = L.map fst $ L.filter isRelated $ tagsMap tags
    isRelated (oTag, oPosts)
        = oTag /= tag && not (L.null (L.intersect oPosts posts))
      where
        Just posts = L.lookup tag $ tagsMap tags

tagsCtx :: [Item String] -> String -> Tags -> Context String
tagsCtx posts tag tags =
    constField "title"   ("Posts tagged with " ++ tag)     `mappend`
    constField "topic"    tag                              `mappend`
    listField  "posts"   (postCtx tags)  (return posts)    `mappend`
    relatedCtx tag tags                                    `mappend`
    defaultContext
