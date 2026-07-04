#!/usr/bin/env bash
#
# Lightweight syntax regression fixture for the DSL native builder surface.
# This keeps Phase 2/3 checks runnable before a full Open64 toolchain build is
# available in the current worktree.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"

if [[ "$(uname -s)" == "Darwin" &&
      -z "${OPEN64_DSL_TEST_ALLOW_DARWIN:-}" ]]; then
  echo "skip: this native builder syntax fixture targets the Linux/Open64 "
  echo "GNU libstdc++ environment; run it in the Open64 Docker image."
  echo "Set OPEN64_DSL_TEST_ALLOW_DARWIN=1 to force a local macOS attempt."
  exit 0
fi

cxx="${CXX:-g++}"
cxxstd="${OPEN64_DSL_TEST_CXXSTD:--std=gnu++98}"

cxxflags=(
  "$cxxstd"
  -DKEY
  -fsyntax-only
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/ir_tools"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/include"
)

sources=(
  "osprey/common/com/dsl_builder.cxx"
  "osprey/common/com/tests/dsl_builder_contract_test.cxx"
  "osprey/common/com/tests/dsl_common_add_print_test.cxx"
  "osprey/common/com/tests/dsl_common_matmul_print_test.cxx"
)

for source in "${sources[@]}"; do
  "$cxx" "${cxxflags[@]}" "$repo_root/$source"
  echo "syntax ok: $source"
done

echo "DSL native syntax fixture passed"
