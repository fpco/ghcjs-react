react-ghcjs
===========

React bindings for GHCJS

## Compile examples

    fpbuild exec -- ghcjs -iexamples/ -isrc/ examples/Main.hs; cp examples/index.html examples/Main.jsexe/

## Haddock

    fpbuild exec -- haddock React React.Builder React.Events --optghc=-isrc -h

## Miscellanous wizardry

node Setup.jsexe/all.js clean
node Setup.jsexe/all.js configure --user --ghcjs
node Setup.jsexe/all.js build
node Setup.jsexe/all.js copy
node Setup.jsexe/all.js register
