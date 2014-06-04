Sources for pxqr's blog.

For development use run:

```
cabal install && blog watch && xdg-open http://localhost:8000
```

To add a post:

    * add `YEAR-MONTH-DAY-post-topic.{md,lhs,...}` file to the `posts` directory;

    * optionally add `posts/$FULL_TITLE` to the cabal file;

    * run `./deploy` script. (TODO how to deploy?)
