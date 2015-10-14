{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List as L
import           Data.Monoid
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Walk
import           Hakyll as H
import           Data.Configurator as Config
import qualified Data.Configurator.Types as Config
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           System.IO.Unsafe (unsafePerformIO)

import Pager

articlesGlob :: Pattern
articlesGlob = "content/articles/*"

configPaths :: [Worth FilePath]
configPaths = [Required "config"]

main :: IO ()
main = hakyll $ do
    -- Assets
    match "assets/favicon.ico" $ do
        route $ constRoute "favicon.ico"
        compile copyFileCompiler

    match "assets/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/css/*.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString
              >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
              >>= return . fmap compressCss

    -- Content Pages
    match "content/images/*" $ do
        route $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    tags <- buildTags articlesGlob (fromCapture "tags/*.html")
    match "templates/*" $ compile templateCompiler

    match articlesGlob $ do
        route   $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
        let isArticleField = constField "isArticle" "yes"
        compile $ pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate "templates/article.html"    (articleCtx tags)
            >>= loadAndApplyTemplate "templates/base.html" (articleCtx tags <> isArticleField)
            >>= relativizeUrls

    -- Index Pages
    match "pages/index.html" $ do
        route $ constRoute "index.html"
        compile $ do
            articles <- recentFirst =<< loadAll articlesGlob
            let indexCtx =
                    listField "articles" (articleCtx tags) (return articles) `mappend`
                    defContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll articlesGlob
            let archiveCtx =
                    listField "articles" (articleCtx tags) (return articles) `mappend`
                    constField "title" "Archives"                   `mappend`
                    defContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/base.html" archiveCtx
                >>= relativizeUrls

    tagsRules tags $ \ tag pattern -> do
        route $ idRoute
        compile $ do
            articles <- recentFirst =<< loadAll pattern
            makeItem ""
              >>= loadAndApplyTemplate "templates/tags.html"    (tagsCtx articles tag tags)
              >>= loadAndApplyTemplate "templates/base.html" (tagsCtx articles tag tags)
              >>= relativizeUrls

    -- Error pages
    match "pages/404.html" $ do
        route $ constRoute "404.html"
        compile $ getResourceBody >>= applyAsTemplate defContext

    -- Metadata
    match (H.fromList ["meta/humans.txt", "meta/robots.txt", "meta/opensearch.xml"]) $ do
        route $ gsubRoute "meta/" (const "")
        compile $ getResourceBody >>= applyAsTemplate defContext

    match "meta/sitemap.xml" $ do
        route $ constRoute "sitemap.xml"
        compile $ do
            articles <- loadAll articlesGlob
            let sitemapCtx = listField "articles" (articleCtx tags) (return articles) <>
                             defContext
            getResourceBody >>= applyAsTemplate sitemapCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          articles <- fmap (take 10) . recentFirst =<< loadAll articlesGlob
          renderAtom feedConfiguration (articleCtx tags) articles

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions
  { readerSmart = True
  }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

feedConfiguration :: FeedConfiguration
feedConfiguration = unsafePerformIO $ do
  config <- Config.load configPaths
  name <- require config "name"
  author <- require config "author"
  email <- require config "email"
  root  <- require config "root"
  return $ FeedConfiguration
    { feedTitle       = name
    , feedDescription = "Articles feed"
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
defContext = mappend defaultContext $ unsafePerformIO $ do
    config <- Config.load configPaths
    hmap <- getMap config
    return $ mconcat $ L.map (uncurry configField) $ HM.toList hmap
  where
    configField key (Config.String val) = constField (T.unpack key)  (T.unpack val)
    configField _ _ = mempty

formatField :: String -> Context String
formatField name = field name (return . fileFormat)
  where
    fileFormat = show . fileType . toFilePath . itemIdentifier

prettyTagsField :: String -> Tags -> Context String
prettyTagsField name tags
    = tagsFieldWith getTags renderLink (mconcat . intersperse ", ") name tags
  where
    renderLink :: String -> (Maybe FilePath) -> Maybe H.Html
    renderLink tag Nothing         = Just $ H.a $ toHtml tag
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) ! A.title (toValue linkTitle) $ toHtml tag
      where
        linkTitle = show articleCount ++ " article" ++ suff ++ " tagged with " ++ tag
          where
            suff = if articleCount == 1 then "" else "s"
        Just articleCount = fmap L.length $ L.lookup tag $ tagsMap tags

-- | Average number of words the expected reader can read for a minute.
wordsPerMinute :: Int
wordsPerMinute = 250

readingTimeField :: String -> Context String
readingTimeField name = field name (return . readingTime)
  where
    readingTime = ppTime . L.length . words . itemBody
    ppTime secs = show (ceiling
                        (fromIntegral secs / fromIntegral wordsPerMinute :: Double)
                        :: Integer)

articleCtx :: Tags -> Context String
articleCtx tags =
    formatField "format"             `mappend`
    dateField "date"     "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d"  `mappend` -- used by sitemap template
    prettyTagsField "taglist" tags   `mappend`
    readingTimeField "readingtime"   `mappend`
    headingsField                    `mappend`
    pagerCtx                         `mappend`
    defContext

relatedCtx :: String -> Tags -> Context String
relatedCtx tag tags
  | L.null related = mempty
  | otherwise      = mconcat
  [ listField  "related" (field "tag" (return . itemBody)) (mapM makeItem related)
  , constField "related-count" (show (L.length related))
  ]
  where
    -- FIX: O(tag_count * tag_count * article_count)
    -- may take a long time in the future
    related = L.map fst $ L.filter isRelated $ tagsMap tags
    isRelated (oTag, oArticles)
        = oTag /= tag && not (L.null (L.intersect oArticles articles))
      where
        Just articles = L.lookup tag $ tagsMap tags

tagsCtx :: [Item String] -> String -> Tags -> Context String
tagsCtx articles tag tags =
    constField "title"   ("Articles tagged with " ++ tag)  `mappend`
    constField "topic"    tag                              `mappend`
    listField  "articles"   (articleCtx tags)  (return articles) `mappend`
    relatedCtx tag tags                                    `mappend`
    defContext
