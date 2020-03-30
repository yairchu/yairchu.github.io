# yairchu.github.io

This is the source code for yairchu's website, based on [slick](https://github.com/ChrisPenner/slick)'s template site.

Editing site locally:

    # Initially, or when the code changes
    stack build
    # After rebuilds, cleanup to work-around Shake issues
    rm -r .shake

    # Install "serve"
    npm install -g serve

    # Serve the static site
    serve docs

    # And in another shell -
    # For editing content / live-reloading. Restart manually when adding pages.
    find site | entr stack exec build-site
