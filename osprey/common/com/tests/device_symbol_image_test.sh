#!/usr/bin/env bash

set -euo pipefail

contract_test="${OPEN64_DSL_BUILDER_TEST:?set OPEN64_DSL_BUILDER_TEST to the rebuilt contract test}"
ir_b2a="${OPEN64_IR_B2A:?set OPEN64_IR_B2A to the rebuilt ir_b2a}"
artifact_dir="${OPEN64_DEVICE_SYMBOL_ARTIFACT_DIR:?set OPEN64_DEVICE_SYMBOL_ARTIFACT_DIR}"

mkdir -p "$artifact_dir"
image="$artifact_dir/device-symbol.B"
host_image="$artifact_dir/host-only.B"
text="$artifact_dir/device-symbol.ir.T"

OPEN64_DEVICE_SYMBOL_ONLY=1 OPEN64_DEVICE_SYMBOL_ARTIFACT="$image" \
    "$contract_test"
OPEN64_DEVICE_SYMBOL_ONLY=1 OPEN64_DEVICE_HOST_ONLY=1 \
    OPEN64_DEVICE_SYMBOL_ARTIFACT="$host_image" "$contract_test"
"$ir_b2a" -st -src "$image" "$text"
test -s "$text"

grep -Fq 'device_symbol_contract) --> (DEVICE_SYMBOL) 1' "$text"
grep -Fq 'device_global_contract) --> (DEVICE_SYMBOL) 3' "$text"
grep -Fq 'device_shared_contract) --> (DEVICE_SYMBOL) 4' "$text"
grep -Eq '^FUNC_ENTRY <[^,]+,[^,]+,host_symbol_contract>' "$text"
grep -Fq 'DO_LOOP {line:' "$text"
grep -Fq 'I4INTCONST 16 (0x10)' "$text"
grep -Fq 'I4INTCONST 4 (0x4)' "$text"
grep -Eq 'I4STID 0 <[^>]+,device_shared_contract>' "$text"

if [[ -n "${OPEN64_WHIRL2C_CUDA:-}" ]]; then
    if (cd "$artifact_dir" && ulimit -c 0 &&
        "$OPEN64_WHIRL2C_CUDA" -cuda -fB,"$host_image" host-only-cuda.c \
            > host-only.reject.log 2>&1); then
        echo "whirl2c -cuda accepted an image without a device PU" >&2
        exit 1
    fi
    grep -Fq 'found no device-attributed PU' \
        "$artifact_dir/host-only.reject.log"
    (cd "$artifact_dir" &&
        "$OPEN64_WHIRL2C_CUDA" -cuda -fB,"$image" device-symbol-cuda.c)
    cuda_source="$artifact_dir/device-symbol-cuda.w2c.c"
    grep -Fq '#include <cuda_runtime.h>' "$cuda_source"
    grep -Fq 'extern "C" __global__ void device_symbol_contract()' "$cuda_source"
    grep -Fq '__shared__ _INT32 device_shared_contract;' "$cuda_source"
    grep -Fq '_w2c_device_tile_base < 16' "$cuda_source"
    grep -Fq '_w2c_device_tile_base + 4' "$cuda_source"
    ! grep -Fq 'host_symbol_contract' "$cuda_source"
    cp "$cuda_source" "$artifact_dir/device-symbol-cuda.cu"
    if [[ -n "${OPEN64_CUDA_HOST_CXX:-}" ]]; then
        "$OPEN64_CUDA_HOST_CXX" -x c++ -std=c++11 -fsyntax-only \
            -I "$(dirname "$0")/cuda_stub" \
            "$artifact_dir/device-symbol-cuda.cu"
    fi
fi

(cd "$artifact_dir" && sha256sum host-only.B device-symbol.B \
    device-symbol.ir.T \
    ${OPEN64_WHIRL2C_CUDA:+device-symbol-cuda.cu} > SHA256SUMS)
printf 'device symbol mapped-image contract passed: %s\n' "$text"
