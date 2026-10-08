#!/usr/bin/env bash
# Build the explicit ResNet-20 pooling/classifier CKKS plan fixture.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C4.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-tail-plan}
mkdir -p "$out_dir"

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror -DKEY \
    -D__MIPS_AND_IA64_ELF_H \
    -I"$repo_root/osprey/be/vho" \
    -I"$repo_root/osprey/linux/include" \
    -I"$repo_root/osprey/common/com" \
    -I"$repo_root/osprey/common/util" \
    -I"$repo_root/osprey/include" \
    "$repo_root/osprey/be/vho/fhe_ckks_plan_bytes.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks_tail_plan.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_tail_plan_test.cxx" \
    -o "$out_dir/fhe_ckks_tail_plan_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_tail_plan_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS ResNet tail operation plans passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
