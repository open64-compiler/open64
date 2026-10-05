#!/usr/bin/env bash
# Test canonical CKKS event-plan bytes without mutating WHIRL.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-plan-bytes}
mkdir -p "$out_dir"

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/be/vho" \
    "$repo_root/osprey/be/vho/fhe_ckks_plan_bytes.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_plan_bytes_test.cxx" \
    -o "$out_dir/fhe_ckks_plan_bytes_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_plan_bytes_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS canonical plan bytes passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
