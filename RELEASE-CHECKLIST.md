# Releasing new versions of Repa packages

1. Update the versions in Cabal files and update the `CHANGELOG.md` files to describe changes associated with these version;
2. Commit changes;
3. Tag the commit and push to GitHub:
    
    ```bash
    $ git tag -a "vW.X.Y.Z" && git push origin "vW.X.Y.Z"
    ```

4. Create source distributions:

    ```bash
    $ cabal sdist all
    Wrote tarball sdist to
    (...)/dist-newstyle/sdist/<pkg>-W.X.Y.Z.tar.gz
    ```

5. Upload to Hackage:

    ```bash
    $ cabal upload --publish (...)/dist-newstyle/sdist/*
    ```

That's it!