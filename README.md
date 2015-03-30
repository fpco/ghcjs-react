react-ghcjs
===========

React bindings for GHCJS

## Compile examples

    ghcjs -iexamples/ -isrc/ examples/Main.hs
    cp examples/index.html examples/Main.jsexe/

## Haddock

    $ haddock React --optghc=-isrc -h

## Alternate means of building

If you have trouble with `cabal install --ghcjs`, you may instead try:

    $ rm -r dist/
    $ ghcjs Setup.hs
    $ node Setup.jsexe/all.js clean
    $ node Setup.jsexe/all.js configure --user --ghcjs
    $ node Setup.jsexe/all.js build
    $ node Setup.jsexe/all.js copy
    $ node Setup.jsexe/all.js register
    $ node Setup.jsexe/all.js haddock
