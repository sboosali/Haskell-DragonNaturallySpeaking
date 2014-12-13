#!/bin/bash
set -e
# ./map.sh ./depends.sh commands criterion

cabal exec -- ghc -O2  Map.hs  -o ./Map.exe  >  /dev/null
cabal exec -- ./Map.exe $@
