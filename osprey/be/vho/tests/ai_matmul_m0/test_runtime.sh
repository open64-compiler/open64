#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$here/../../../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

${CC:-cc} -std=c11 -O2 -Wall -Wextra -Werror -fno-fast-math \
    -I "$repo/osprey/include" \
    "$here/runtime.c" "$here/benchmark.c" "$here/runtime_smoke_entry.c" \
    -o "$tmp/ai_matmul_m0_smoke"
"$tmp/ai_matmul_m0_smoke" > "$tmp/raw.csv"
[ "$(wc -l < "$tmp/raw.csv")" -eq 31 ]
[ "$(head -1 "$tmp/raw.csv")" = 'sample,elapsed_ns' ]
echo 'AI matmul m0 host runtime smoke passed'
