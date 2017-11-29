#!/bin/sh

set -e

opt="0 1 2 3"
align="1 2 4 8 16"
type="float double longdouble"

for T in $type; do
  for A in $align; do
    for O in $opt; do
      echo ">>> TYPE=$T ALIGN=$A opt=$O"
      opencc -O$O -DALIGN=$A -DTYPE=$T -o misaligned misaligned.c >& /dev/null
      ./misaligned
    done
  done
done

rm misaligned
