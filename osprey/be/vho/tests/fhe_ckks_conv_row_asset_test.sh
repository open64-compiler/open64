#!/usr/bin/env bash
# Retain deterministic ACE-style raw F32 Conv rows without emitting WHIRL.
# Design: doc/FHE-SYNC6-CONV-MASK-ASSET-OPTIONS.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:?artifact directory required}
trace=${2:?replay-authenticated ir_b2a trace required}
payload=${3:?replay-authenticated folded SafeTensors required}
replay=${4:?replay report required}
producer="$repo_root/osprey/be/vho/tests/fhe_ckks_conv_mask_budget.py"
verifier="$repo_root/osprey/be/vho/tests/fhe_ckks_conv_row_asset_verify.py"

# Replace only this test's retained evidence at the start of a new run.
mkdir -p "$out_dir/first" "$out_dir/repeated" "$out_dir/injected" \
    "$out_dir/tampered"
for name in first repeated injected tampered; do
    rm -f "$out_dir/$name"/ace-conv-rows.f32 \
        "$out_dir/$name"/ace-conv-rows.index.json \
        "$out_dir/$name"/conv-plaintext-budget.json
done

for name in first repeated; do
    python3 "$producer" --trace "$trace" --payload "$payload" \
        --replay "$replay" \
        --output "$out_dir/$name/conv-plaintext-budget.json" \
        --asset-output "$out_dir/$name/ace-conv-rows.f32" \
        --index-output "$out_dir/$name/ace-conv-rows.index.json" \
        >"$out_dir/$name/run.log" 2>&1
    python3 "$verifier" --asset "$out_dir/$name/ace-conv-rows.f32" \
        --index "$out_dir/$name/ace-conv-rows.index.json" \
        --report "$out_dir/$name/conv-plaintext-budget.json" \
        >"$out_dir/$name/verify.log" 2>&1
done
cmp "$out_dir/first/ace-conv-rows.f32" \
    "$out_dir/repeated/ace-conv-rows.f32"
cmp "$out_dir/first/ace-conv-rows.index.json" \
    "$out_dir/repeated/ace-conv-rows.index.json"
cmp "$out_dir/first/conv-plaintext-budget.json" \
    "$out_dir/repeated/conv-plaintext-budget.json"

if python3 "$producer" --trace "$trace" --payload "$payload" \
        --replay "$replay" \
        --output "$out_dir/injected/conv-plaintext-budget.json" \
        --asset-output "$out_dir/injected/ace-conv-rows.f32" \
        --index-output "$out_dir/injected/ace-conv-rows.index.json" \
        --fail-after-rows 10 >"$out_dir/injected/run.log" 2>&1; then
    echo "injected row failure unexpectedly succeeded" >&2
    exit 1
fi
test ! -e "$out_dir/injected/ace-conv-rows.f32"
test ! -e "$out_dir/injected/ace-conv-rows.index.json"
test ! -e "$out_dir/injected/conv-plaintext-budget.json"
if find "$out_dir/injected" -name '*.tmp.*' -print -quit | grep -q .; then
    echo "injected row failure left a temporary file" >&2
    exit 1
fi

cp "$out_dir/first/ace-conv-rows.f32" \
    "$out_dir/tampered/ace-conv-rows.f32"
cp "$out_dir/first/ace-conv-rows.index.json" \
    "$out_dir/tampered/ace-conv-rows.index.json"
cp "$out_dir/first/conv-plaintext-budget.json" \
    "$out_dir/tampered/conv-plaintext-budget.json"
printf 'x' >>"$out_dir/tampered/ace-conv-rows.f32"
if python3 "$verifier" --asset "$out_dir/tampered/ace-conv-rows.f32" \
        --index "$out_dir/tampered/ace-conv-rows.index.json" \
        --report "$out_dir/tampered/conv-plaintext-budget.json" \
        >"$out_dir/tampered/verify.log" 2>&1; then
    echo "tampered ACE row asset unexpectedly verified" >&2
    exit 1
fi

python3 - "$out_dir/first/ace-conv-rows.index.json" \
    "$out_dir/tampered/ace-conv-rows.index.json" <<'PY'
import json
import sys
from pathlib import Path

index = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
index["rows"][1]["feature_row"] = index["rows"][0]["feature_row"]
Path(sys.argv[2]).write_text(json.dumps(index) + "\n", encoding="utf-8")
PY
if python3 "$verifier" --asset "$out_dir/first/ace-conv-rows.f32" \
        --index "$out_dir/tampered/ace-conv-rows.index.json" \
        --report "$out_dir/first/conv-plaintext-budget.json" \
        >"$out_dir/tampered/duplicate-row.log" 2>&1; then
    echo "duplicate ACE row identity unexpectedly verified" >&2
    exit 1
fi

printf 'ACE-style diagnostic Conv row asset passed.\n'
printf 'Evidence: %s\n' "$out_dir/first/ace-conv-rows.index.json"
