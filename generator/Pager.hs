{-# LANGUAGE OverloadedStrings #-}
module Pager
    ( pagerCtx
    , newerField, olderField
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Function (on)
import Data.List as L

import Hakyll

articlesGlob :: Pattern
articlesGlob = "content/articles/*"

-- Adopted from
-- https://github.com/rgoulter/my-hakyll-blog/commit/a4dd0513553a77f3b819a392078e59f461d884f9
-- Thanks to @rgoulter!

pagerCtx :: Context String
pagerCtx = newerCtx <> olderCtx

newerField, newerTitleField, newerCtx :: Context String
newerField = field "newer" $ (newerId . itemIdentifier) >=> getUrl
newerTitleField = field "newerTitle" $ (newerId . itemIdentifier) >=> getTitle
newerCtx = newerField <> newerTitleField

olderField, olderTitleField, olderCtx :: Context String
olderField = field "older" $ (olderId . itemIdentifier) >=> getUrl
olderTitleField = field "olderTitle" $ (olderId . itemIdentifier) >=> getTitle
olderCtx = olderField <> olderTitleField

newerId :: Identifier -> Compiler Identifier
newerId article = do
  articles <- getMatches articlesGlob >>= sortChronological
  maybe empty return $ articles `after` article

olderId :: Identifier -> Compiler Identifier
olderId article = do
  articles <- getMatches articlesGlob >>= sortChronological
  maybe empty return $ articles `before` article

getTitle :: Identifier -> Compiler String
getTitle = (`getMetadataField'` "title")

getUrl :: Identifier -> Compiler String
getUrl = fmap (maybe empty toUrl) . getRoute

after :: Eq a => [a] -> a -> Maybe a
after xs x = lookup x $ zip xs (tail xs)

before :: Eq a => [a] -> a -> Maybe a
before xs x = lookup x $ zip (tail xs) xs
