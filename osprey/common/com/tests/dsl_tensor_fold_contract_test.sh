#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
cxx="${CXX:-g++}"
output="${TMPDIR:-/tmp}/dsl_tensor_fold_contract_test"

"$cxx" -std=gnu++98 \
  -DDSL_TENSOR_FOLD_TEST_STUB \
  -I"$repo_root/osprey/linux/include" \
  -I"$repo_root/osprey/common/com" \
  -I"$repo_root/osprey/common/com/x8664" \
  -I"$repo_root/osprey/common/util" \
  -I"$repo_root/osprey/include" \
  "$repo_root/osprey/common/com/dsl_domain.cxx" \
  "$repo_root/osprey/common/com/dsl_opcode.cxx" \
  "$repo_root/osprey/common/com/dsl_tensor_fold.cxx" \
  "$repo_root/osprey/common/com/tests/dsl_tensor_fold_strtab_stub.cxx" \
  "$repo_root/osprey/common/com/tests/dsl_tensor_fold_contract_test.cxx" \
  -o "$output"

"$output"
