#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 3 ]]; then
    echo "usage: $0 LEGACY_CU WHIRL_W2C_C ARTIFACT_DIR" >&2
    exit 2
fi
legacy=$1
whirl=$2
artifact_dir=$3
test_dir=$(cd "$(dirname "$0")" && pwd)
cxx=${CXX:-c++}
for definition in 'M 128' 'N 128' 'K 128' 'BM 128' 'BN 128' \
                  'BK 32' 'TM 8' 'TN 8'; do
    grep -Fxq "#define $definition" "$legacy"
done
for tile in as bs; do
    if ! grep -Eq "^[[:space:]]*__shared__[[:space:]]+_IEEE32[[:space:]]+${tile}\\[4096LL\\];$" "$whirl"; then
        echo "whirl2c -cuda did not place ${tile} in CUDA shared memory" >&2
        exit 1
    fi
done
if grep -Eq '^[[:space:]]*__shared__[[:space:]]+_IEEE32[[:space:]]+acc\[' "$whirl"; then
    echo "whirl2c -cuda placed the per-thread accumulator in shared memory" >&2
    exit 1
fi
mkdir -p "$artifact_dir"
"$cxx" -x c++ -std=c++11 -O0 -pthread -I "$test_dir/cuda_stub" \
    -Dai_matmul_tile_f32=ai_matmul_whirl -c "$whirl" \
    -o "$artifact_dir/matmul.whirl.o"
"$cxx" -x c++ -std=c++11 -O0 -pthread -I "$test_dir/cuda_stub" \
    -Dai_matmul_tile_f32=ai_matmul_legacy -c "$legacy" \
    -o "$artifact_dir/matmul.legacy.o"
"$cxx" -std=c++11 -O0 -pthread -I "$test_dir/cuda_stub" \
    "$test_dir/whirl_cuda_parity.cpp" \
    "$artifact_dir/matmul.whirl.o" "$artifact_dir/matmul.legacy.o" \
    -o "$artifact_dir/whirl_cuda_parity"
"$artifact_dir/whirl_cuda_parity" | tee "$artifact_dir/parity.log"
