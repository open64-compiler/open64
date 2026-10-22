#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$here/../../../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

${CC:-cc} -std=c11 -O2 -Wall -Wextra -Werror -fno-fast-math \
    -DAI_MATMUL_M2_RUNTIME -I "$repo/osprey/include" \
    "$here/runtime.c" "$here/generated_runtime_test.c" \
    -o "$tmp/ai_matmul_m2_runtime_test"
"$tmp/ai_matmul_m2_runtime_test"
echo 'AI matmul m2 generated runtime contract passed'
