#!/usr/bin/env bash
# Authenticate and deterministically budget the retained full-model masks.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:?artifact directory required}
trace=${2:?replay-authenticated ir_b2a trace required}
payload=${3:?replay-authenticated folded SafeTensors required}
replay=${4:?replay report required}
mkdir -p "$out_dir/tampered"

python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_mask_budget.py" \
    --trace "$trace" --payload "$payload" --replay "$replay" \
    --output "$out_dir/full-model-mask-budget.json" \
    >"$out_dir/run.log" 2>&1
python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_mask_budget.py" \
    --trace "$trace" --payload "$payload" --replay "$replay" \
    --output "$out_dir/repeated-mask-budget.json" \
    >"$out_dir/repeated.log" 2>&1
cmp "$out_dir/full-model-mask-budget.json" \
    "$out_dir/repeated-mask-budget.json"

tampered="$out_dir/tampered/$(basename "$payload")"
cp "$payload" "$tampered"
printf 'x' >>"$tampered"
if python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_mask_budget.py" \
        --trace "$trace" --payload "$tampered" --replay "$replay" \
        --output "$out_dir/tampered-budget.json" \
        >"$out_dir/tampered.log" 2>&1; then
    echo "tampered folded payload unexpectedly accepted" >&2
    exit 1
fi
test ! -e "$out_dir/tampered-budget.json"
printf 'Authenticated full-model mask budget passed.\n'
printf 'Evidence: %s\n' "$out_dir/full-model-mask-budget.json"
