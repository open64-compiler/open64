#!/bin/sh
# Certify deterministic, complete, and atomic SYNC-6 Conv plaintext assets.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.

set -eu

if [ "$#" -ne 5 ]; then
  echo "usage: $0 budget.json folded.safetensors replay.json artifact-dir python" >&2
  exit 2
fi

budget=$1
payload=$2
replay=$3
artifact_dir=$4
python=$5
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
producer=$script_dir/fhe_ckks_conv_all_asset.py
verifier=$script_dir/fhe_ckks_conv_all_asset_verify.py

rm -rf "$artifact_dir"
mkdir -p "$artifact_dir/first" "$artifact_dir/repeated" \
  "$artifact_dir/injected" "$artifact_dir/tampered"

# Produce one complete family and verify every indexed range independently.
"$python" "$producer" --budget "$budget" --payload "$payload" \
  --replay "$replay" \
  --asset-output "$artifact_dir/first/secure_resnet20.conv-plaintexts.f32" \
  --index-output "$artifact_dir/first/secure_resnet20.conv-plaintexts.index.json" \
  >"$artifact_dir/first/producer.log" 2>&1
"$python" "$verifier" \
  --asset "$artifact_dir/first/secure_resnet20.conv-plaintexts.f32" \
  --index "$artifact_dir/first/secure_resnet20.conv-plaintexts.index.json" \
  >"$artifact_dir/first/verifier.log" 2>&1

# A second production must be byte-for-byte identical, including the index.
"$python" "$producer" --budget "$budget" --payload "$payload" \
  --replay "$replay" \
  --asset-output "$artifact_dir/repeated/secure_resnet20.conv-plaintexts.f32" \
  --index-output "$artifact_dir/repeated/secure_resnet20.conv-plaintexts.index.json" \
  >"$artifact_dir/repeated/producer.log" 2>&1
cmp "$artifact_dir/first/secure_resnet20.conv-plaintexts.f32" \
  "$artifact_dir/repeated/secure_resnet20.conv-plaintexts.f32"
cmp "$artifact_dir/first/secure_resnet20.conv-plaintexts.index.json" \
  "$artifact_dir/repeated/secure_resnet20.conv-plaintexts.index.json"

# A mid-production failure must leave no final endpoint or temporary file.
if "$python" "$producer" --budget "$budget" --payload "$payload" \
    --replay "$replay" \
    --asset-output "$artifact_dir/injected/secure_resnet20.conv-plaintexts.f32" \
    --index-output "$artifact_dir/injected/secure_resnet20.conv-plaintexts.index.json" \
    --fail-after-records 100 >"$artifact_dir/injected/failure.log" 2>&1; then
  echo "injected Conv asset failure unexpectedly succeeded" >&2
  exit 1
fi
if find "$artifact_dir/injected" -type f ! -name failure.log | grep -q .; then
  echo "injected Conv asset failure left a final or temporary endpoint" >&2
  exit 1
fi

# Whole-file and per-range authentication must reject modified asset bytes.
cp "$artifact_dir/first/secure_resnet20.conv-plaintexts.f32" \
  "$artifact_dir/tampered/secure_resnet20.conv-plaintexts.f32"
cp "$artifact_dir/first/secure_resnet20.conv-plaintexts.index.json" \
  "$artifact_dir/tampered/secure_resnet20.conv-plaintexts.index.json"
printf x >>"$artifact_dir/tampered/secure_resnet20.conv-plaintexts.f32"
if "$python" "$verifier" \
    --asset "$artifact_dir/tampered/secure_resnet20.conv-plaintexts.f32" \
    --index "$artifact_dir/tampered/secure_resnet20.conv-plaintexts.index.json" \
    >"$artifact_dir/tampered/failure.log" 2>&1; then
  echo "tampered Conv plaintext family unexpectedly verified" >&2
  exit 1
fi

echo "FHE complete Conv plaintext family test passed"
