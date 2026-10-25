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
definitions=()
for name in M N K BM BN BK TM TN; do
    value=$(awk -v symbol="$name" '$1 == "#define" && $2 == symbol { print $3; exit }' "$legacy")
    if [[ ! $value =~ ^[0-9]+$ ]]; then
        echo "legacy CUDA source lacks numeric $name" >&2
        exit 1
    fi
    definitions+=("-DAI_TEST_${name}=$value")
    printf -v "AI_TEST_${name}" '%s' "$value"
done
as_elements=$(awk '$1 == "#define" && $2 == "AI_A_SHARED_ELEMS" { print $3; exit }' "$legacy")
if [[ -z $as_elements ]]; then as_elements=$((AI_TEST_BM * AI_TEST_BK)); fi
if [[ ! $as_elements =~ ^[0-9]+$ ]]; then
    echo "legacy CUDA source has invalid A shared allocation" >&2
    exit 1
fi
bs_elements=$((AI_TEST_BK * AI_TEST_BN))
for tile in as bs; do
    elements=$as_elements
    if [[ $tile == bs ]]; then elements=$bs_elements; fi
    if ! grep -Eq "^[[:space:]]*__shared__[[:space:]]+_IEEE32[[:space:]]+${tile}\\[${elements}LL\\];$" "$whirl"; then
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
    "${definitions[@]}" \
    "$test_dir/whirl_cuda_parity.cpp" \
    "$artifact_dir/matmul.whirl.o" "$artifact_dir/matmul.legacy.o" \
    -o "$artifact_dir/whirl_cuda_parity"
"$artifact_dir/whirl_cuda_parity" | tee "$artifact_dir/parity.log"
