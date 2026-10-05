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
    python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_payload_fixture.py" \
        --payload "$2" --replay "$3" --output "$out_dir" \
        --tensor-prefix call4_cnn_basic_block_conv2_folded_ \
        --input-channels 32 --output-channels 32 --name call4_conv2 \
        >>"$out_dir/extraction.log" 2>&1
    "$out_dir/fhe_ckks_conv_recipe_test" \
        "$out_dir/call4_conv2_folded_oihw_f32.bin" call4_conv2 32 32 16 \
        >>"$out_dir/run.log" 2>&1
    python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_payload_fixture.py" \
        --payload "$2" --replay "$3" --output "$out_dir" \
        --tensor-prefix call7_cnn_basic_block_conv2_folded_ \
        --input-channels 64 --output-channels 64 --name call7_conv2 \
        >>"$out_dir/extraction.log" 2>&1
    "$out_dir/fhe_ckks_conv_recipe_test" \
        "$out_dir/call7_conv2_folded_oihw_f32.bin" call7_conv2 64 64 8 \
        >>"$out_dir/run.log" 2>&1
else
    "$out_dir/fhe_ckks_conv_recipe_test" >"$out_dir/run.log" 2>&1
fi
printf 'FHE CKKS fixed Conv recipe passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
