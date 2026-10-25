#!/usr/bin/env bash
# Compile and run the focused two-PU CKKS2C image collector fixture.
# Design: doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks2c-collect}
mkdir -p "$out_dir"

common_flags=(
  -DKEY -D__MIPS_AND_IA64_ELF_H
  -Wno-unknown-pragmas -Wno-reorder -Wno-unused-parameter
  -Wno-class-memaccess
  -I"$repo_root/osprey"
  -I"$repo_root/osprey/be/vho"
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/common/fhe"
  -I"$repo_root/osprey/include"
)

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror "${common_flags[@]}" \
    "$repo_root/osprey/be/vho/fhe_sha256.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks_plan_bytes.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks2c_collect.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks2c_collect_test.cxx" \
    -o "$out_dir/fhe_ckks2c_collect_test" >"$out_dir/build.log" 2>&1

"$out_dir/fhe_ckks2c_collect_test" >"$out_dir/run.log" 2>&1

printf 'FHE CKKS2C two-PU collection passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
