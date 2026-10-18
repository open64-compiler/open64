#!/usr/bin/env bash
# Reopen the accepted S6-0c binary and certify D2 collection and C emission.
# Design: doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2/S6D-T4.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:?usage: $0 output-dir cached-input.B linked-collector}
input=${2:?usage: $0 output-dir cached-input.B linked-collector}
collector=${3:?usage: $0 output-dir cached-input.B linked-collector}
expected=d7bccc537c422b2266add7437040cac14ef7b33af93c9f5e65eb7ebb5f89876f

mkdir -p "$out_dir"
rm -f "$out_dir/secure_resnet20.ckks2c.c" \
      "$out_dir/secure_resnet20.ckks2c.o" \
      "$out_dir/secure_resnet20.ckks2c-report.txt" \
      "$out_dir/complete-audit.json" \
      "$out_dir/complete-audit.txt" \
      "$out_dir/run.log"

actual=$(sha256sum "$input" | awk '{print $1}')
if [[ "$actual" != "$expected" ]]; then
  printf 'cached S6-0c hash mismatch: expected=%s actual=%s\n' \
      "$expected" "$actual" >&2
  exit 1
fi

"$collector" "$input" \
    "$out_dir/secure_resnet20.ckks2c.c" \
    "$out_dir/secure_resnet20.ckks2c-report.txt" \
    >"$out_dir/run.log" 2>&1

"${CC:-cc}" -std=c11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/include" \
    -c "$out_dir/secure_resnet20.ckks2c.c" \
    -o "$out_dir/secure_resnet20.ckks2c.o" \
    >>"$out_dir/run.log" 2>&1

grep -qx 'pu_count=10' "$out_dir/secure_resnet20.ckks2c-report.txt"
grep -qx 'event_count=33367' "$out_dir/secure_resnet20.ckks2c-report.txt"
grep -qx 'emitted_operation_count=33367' \
    "$out_dir/secure_resnet20.ckks2c-report.txt"

artifact_dir=$(dirname "$input")
python3 "$repo_root/osprey/be/vho/tests/fhe_ckks_complete_checkpoint_audit.py" \
    --text-output "$out_dir/complete-audit.txt" \
    "$artifact_dir/secure_resnet20.ckks_ops.T" "$artifact_dir" \
    >"$out_dir/complete-audit.json"

printf 'FHE CKKS2C cached-image collection passed.\n'
printf 'Generated C: %s\n' "$out_dir/secure_resnet20.ckks2c.c"
printf 'Census: %s\n' "$out_dir/secure_resnet20.ckks2c-report.txt"
printf 'Readable audit: %s\n' "$out_dir/complete-audit.txt"
printf 'Evidence: %s\n' "$out_dir/run.log"
