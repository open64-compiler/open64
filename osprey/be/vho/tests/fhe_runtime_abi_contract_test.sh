#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../../../.." && pwd)"
build_dir="${TMPDIR:-/tmp}/open64-fhe-runtime-abi-test"

mkdir -p "$build_dir"

cc -std=c11 -Wall -Wextra -Werror \
  -I"$repo_root/osprey/include" \
  "$repo_root/osprey/be/vho/tests/fhe_runtime_abi_c_test.c" \
  -o "$build_dir/fhe_runtime_abi_c_test"

c++ -std=c++11 -Wall -Wextra -Werror \
  -I"$repo_root/osprey/include" \
  "$repo_root/osprey/be/vho/tests/fhe_runtime_abi_cxx_test.cxx" \
  -o "$build_dir/fhe_runtime_abi_cxx_test"

"$build_dir/fhe_runtime_abi_c_test"
"$build_dir/fhe_runtime_abi_cxx_test"

echo "FHE runtime ABI C/C++ contract test passed"
