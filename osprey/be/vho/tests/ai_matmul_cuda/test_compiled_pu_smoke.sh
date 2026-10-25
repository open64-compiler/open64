#!/bin/sh
set -eu

artifact=$1
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$here/../../../../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
source_file="$artifact/source/matmul.generated.cu"
plan=$(awk '$1 == "#define" && $2 == "AI_TILE_PLAN_ID" { print $3; exit }' "$source_file")
shared=$(awk '$1 == "//" && $2 ~ /^shared_ab_bytes_per_stage=/ {
    split($2, parts, "="); print parts[2]; exit
}' "$source_file")
case $plan in
    ''|*[!0-9]*) echo "invalid generated plan ID" >&2; exit 1 ;;
esac
case $shared in
    ''|*[!0-9]*) echo "invalid generated shared allocation" >&2; exit 1 ;;
esac

objcopy --globalize-symbol=ai_matmul_m0 \
    "$artifact/binary/matmul.o" "$tmp/matmul.link.o"
${CC:-cc} -std=c11 -O2 -Wall -Wextra -Werror -fno-fast-math \
    -DAI_MATMUL_M2_RUNTIME -DAI_EXPECT_PLAN="$plan" \
    -DAI_EXPECT_SHARED="$shared" -I "$repo/osprey/include" \
    -I "$repo/osprey/be/vho/tests/ai_matmul_m0" \
    "$repo/osprey/be/vho/tests/ai_matmul_m0/runtime.c" \
    "$here/compiled_pu_smoke.c" "$tmp/matmul.link.o" \
    -no-pie -o "$tmp/matmul-compiled-pu-smoke"
"$tmp/matmul-compiled-pu-smoke"
