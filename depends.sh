#!/bin/bash
set -e
# ./depends.sh commands

cabal exec -- ghc -O2  Depends.hs  -o ./Depends.exe  >  /dev/null
cabal exec -- ./Depends.exe $@
