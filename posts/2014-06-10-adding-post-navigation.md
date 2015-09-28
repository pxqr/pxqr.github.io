---
title:       Adding Table of Contents
description: How to generate table of contents for Hakyll blog posts.
tags:        Hakyll, Pandoc
toc:         include
---

Sometimes blog posts might grow so they won't fit in two or more
screen pages. A good practice is to break post or article into parts
using paragraphs and headings. Hovewer, it is often vital to get page
overview before starting reading. The most straightforward and natural
way to generate table of contents is to list blog post headings in
original order. If hyperlinked, table of contents can be used for
navigation within a post scope.

Extracting headings
------

Although [pandoc](http://johnmacfarlane.net/pandoc/README.html)
already have an option for table of contents generation we will not
use it. For educational purposes we will do it manually. Internally,
pandoc have generic representation for every format of document it can
parse or render. Not suprisingly, it's called
[Pandoc][pandoc-type]. Our first task is to extract list of headings
from the `Pandoc` datatype. For this purpose pandoc provides
[Text.Pandoc.Walk][text-pandoc-walk] module; more specifically, it
export the very useful `query` function simplifying document tree
traversal.

The following new function goes to `site.hs`:

```haskell
import Text.Pandoc
import Text.Pandoc.Walk (query)

headings :: Pandoc -> [String]
headings = query extractHeader
  where
    extractHeader (Header _ _ inlines) = [unwords $ query extractStr inlines]
    extractHeader _                    = []

    extractStr    (Str    str        ) = [str]
    extractStr    _                    = []
```

Core pandoc types are located into `pandoc-types` package, so you
might need to add this package to `build-depends` cabal field.

[text-pandoc-walk]: http://hackage.haskell.org/package/pandoc-types-1.12.3.3/docs/Text-Pandoc-Walk.html
[pandoc-type]: http://hackage.haskell.org/package/pandoc-types-1.12.3.3/docs/Text-Pandoc-Definition.html#t:Pandoc

Composing context
-------

To render extracted headings we need to fill in template
context. [Hakyll](http://jaspervdj.be/hakyll/) has
[Context][hakyll-context] machinery for this. Any Hakyll context is
consisting of a set of fields and there are a lot of different kinds
of fields. We will need just this two:

* the two string `field`s containing heading string and heading id
  attribute;

* the `listField` is used to keep a list of previous (heading and its
  id) two fields.

Again, we'll add the following field "description" to the `site.hs`:

```haskell
import Data.Char (toLower)
import Data.List as L

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
```

Scaffold generated `site.hs` module have `postCtx` context. To be
able to query the `headingsField` from `post.html` template we must
include it in the `postCtx`:

```haskell
postCtx :: Context String
postCtx =
    dateField "date"     "%B %e, %Y" `mappend`
    headingsField                    `mappend` -- for TOC rendering
    defaultContext
```

[hakyll-context]: http://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html

HTML rendering
--------

Now we should generate an HTML chunk representing table of
contents. It should be included in post if only "toc" field is
present, so we use pandoc's `$if$` conditional. Pandoc
[template][pandoc-templates] system also have `$for$` operator
which is used to generate list of table entries.

The following goes to `/templates/post.html`.

```html
$if(toc)$
<aside>
  <nav id="toc">
    <p>Contents</p>
    <ul>
      $for(headings)$
      <li>
        <a href="#$fragment$">$caption$</a>
      </li>
      $endfor$
    </ul>
  </nav>
<aside>
$endif$
```

Notice that for every `$headings$` item the `$fragment$` and
`$caption$` fields lives in the different scopes. Thus, Hakyll
contexts have tree-like structure. For instance (a bit simplified)
context of this document will be:

* postCtx
    - title: Adding Table Of Contents
    - description: ....
    - toc: on
    - headings
        - heading1
            - caption: Extracting headings
            - fragment: extracting-headings
        - heading2
            - caption: Composing context
            - fragment: composing-context
        - etc...

[pandoc-templates]: http://johnmacfarlane.net/pandoc/README.html#templates

Basic styling
-------

Both [wikipedia][wiki-toc] and
[haddock documentation tool][haddock-toc] provides an example of how
table of contents might look like. But let's go towards simplicity --
formatting and nothing else. The quick & dirty [SASS][sass-guide]
snippet from `css/default.scss` will do:

```css
aside nav#toc {
    margin: 1em;
    padding: 1em;

    border: $decoration solid 1px;
    background-color: $background;
    float: right;

    p {
        text-align: center;
    }

    ul {
        list-style-type: none;
        padding-left: 0;
    }
}
```

Hakyll's scaffolded site does not include SASS preprocessing, but
[Hakyll.Core.UnixFilter][sass-hakyll] provide a
[Compiler][hakyll-compiler] capable to filter files using an external
process.

[sass-guide]: http://sass-lang.com/guide
[sass-hakyll]: http://jaspervdj.be/hakyll/reference/Hakyll-Core-UnixFilter.html
[hakyll-compiler]: http://jaspervdj.be/hakyll/reference/Hakyll-Core-Compiler.html
[haddock-toc]: http://hackage.haskell.org/package/async/docs/Control-Concurrent-Async.html#table-of-contents
[wiki-toc]: http://en.wikipedia.org/wiki/ICC_profile#toc
