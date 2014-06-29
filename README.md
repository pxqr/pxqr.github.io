Sources for pxqr's blog. Rendered HTML version is available at
https://github.com/pxqr/pxqr.github.io

Prerequisites:

    * install haskell platform

    * install ruby and rubygems

    * install sass gem

For development use run:

    cabal install && blog watch && xdg-open http://localhost:8000

To add a post:

    * add `YEAR-MONTH-DAY-post-topic.{md,lhs,...}` file to the `posts` directory;

    * optionally add `posts/$FULL_TITLE` to the cabal file;

    * run `./deploy.sh` script.
