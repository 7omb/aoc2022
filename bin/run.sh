#!/usr/bin/env sh

# $1: file to recompile when saved
# $2: input file to read after compilation

echo "$1" | entr -sc "ghc $1 && ./${1%.hs} < $2 && echo"
