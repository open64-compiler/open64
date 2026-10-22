#!/bin/sh
set -eu

artifact=$1
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$here/../../../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

objcopy --globalize-symbol=ai_matmul_m0 \
    "$artifact/binary/matmul.o" "$tmp/matmul.link.o"
${CC:-cc} -std=c11 -O2 -Wall -Wextra -Werror -fno-fast-math \
    -DAI_MATMUL_M2_RUNTIME -I "$repo/osprey/include" \
    -I "$repo/osprey/be/vho/tests/ai_matmul_m0" \
    "$repo/osprey/be/vho/tests/ai_matmul_m0/runtime.c" \
    "$here/compiled_pu_smoke.c" "$tmp/matmul.link.o" \
    -no-pie -o "$tmp/matmul-compiled-pu-smoke"
"$tmp/matmul-compiled-pu-smoke"
