#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
old_ir_b2a="${OPEN64_PREVIOUS_IR_B2A:-}"
artifact_dir="${OPEN64_DSL_VIEW_RETIRE_ARTIFACT_DIR:-$repo_root/artifacts/dsl/view-retirement}"
image="$artifact_dir/view_retirement.B"
trace="$artifact_dir/view_retirement.T"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

printf '%s\n' \
  "OPEN64_DSL_VIEW_RETIRE_ONLY=1 OPEN64_DSL_VIEW_RETIRE_ARTIFACT=$image $producer" \
  "OPEN64_DSL_VIEW_REGION_ONLY=1 $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"
OPEN64_DSL_VIEW_RETIRE_ONLY=1 \
OPEN64_DSL_VIEW_RETIRE_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
OPEN64_DSL_VIEW_REGION_ONLY=1 \
  "$producer" >"$artifact_dir/region-rollback.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1
if [[ -n "$old_ir_b2a" ]]; then
  if "$old_ir_b2a" -st -src "$image" \
      "$artifact_dir/view_retirement.previous-reader.T" \
      >"$artifact_dir/previous-reader.log" 2>&1; then
    echo "previous DSL reader unexpectedly accepted cross-TY view" >&2
    exit 1
  fi
  grep -Fq 'DSL image error: invalid node' \
    "$artifact_dir/previous-reader.log"
fi

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
for evidence in \
  'operator=OPR_DSLFLATTEN version=2' \
  'status=retired redirected_to=value2 relation=representation_view' \
  'status=retired_view' \
  'shape=[1,64,1,1]' \
  'shape=[1,64]' \
  'dsl_builder_contract_test.cxx'; do
  grep -Fq "$evidence" "$trace"
done
if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi
test ! -e "$image.tmp"
if command -v readelf >/dev/null 2>&1; then
  readelf -SW "$image" >"$artifact_dir/section-headers.txt"
  grep -Fq '.WHIRL.dsl_runtime' "$artifact_dir/section-headers.txt"
fi
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum view_retirement.B view_retirement.T \
    producer.log region-rollback.log ir_b2a.log commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 view_retirement.B view_retirement.T \
    producer.log region-rollback.log ir_b2a.log commands.txt >SHA256SUMS)
fi
if [[ -n "$old_ir_b2a" ]]; then
  if command -v sha256sum >/dev/null 2>&1; then
    (cd "$artifact_dir" && sha256sum previous-reader.log >>SHA256SUMS)
  else
    (cd "$artifact_dir" && shasum -a 256 previous-reader.log >>SHA256SUMS)
  fi
fi

echo "DSL cross-TY view retirement passed"
echo "review trace: $trace"
if [[ -n "$old_ir_b2a" ]]; then
  echo "previous-reader rejection: $artifact_dir/previous-reader.log"
fi
