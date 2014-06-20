{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List as L
import           Data.Monoid (mappend, (<>))
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
            let title   = "Posts tagged with " ++ tag
            let tagsCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" title                        `mappend`
                    constField "topic" tag                          `mappend`
                    defaultContext

            makeItem ""
              >>= loadAndApplyTemplate "templates/tags.html"    tagsCtx
              >>= loadAndApplyTemplate "templates/default.html" tagsCtx
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

postCtx :: Tags -> Context String
postCtx tags =
    dateField "date"     "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d"  `mappend` -- used by sitemap template
    tagsField "taglist"  tags        `mappend`
    headingsField                    `mappend`
    defContext
