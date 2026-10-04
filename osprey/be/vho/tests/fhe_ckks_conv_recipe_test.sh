#!/usr/bin/env bash
# Test the bounded column-first packed Conv recipe without mutating WHIRL.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-conv-recipe}
mkdir -p "$out_dir"

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/be/vho" \
    "$repo_root/osprey/be/vho/fhe_ckks_conv_recipe.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_recipe_test.cxx" \
    -o "$out_dir/fhe_ckks_conv_recipe_test" \
    >"$out_dir/build.log" 2>&1
if [[ $# -ge 3 ]]; then
    python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_payload_fixture.py" \
        --payload "$2" --replay "$3" --output "$out_dir" \
        >"$out_dir/extraction.log" 2>&1
    "$out_dir/fhe_ckks_conv_recipe_test" \
        "$out_dir/stem_folded_oihw_f32.bin" >"$out_dir/run.log" 2>&1
else
    "$out_dir/fhe_ckks_conv_recipe_test" >"$out_dir/run.log" 2>&1
fi
printf 'FHE CKKS fixed Conv recipe passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
