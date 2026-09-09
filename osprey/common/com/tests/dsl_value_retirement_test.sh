#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
old_ir_b2a="${OPEN64_PREVIOUS_IR_B2A:-}"
artifact_dir="${OPEN64_DSL_VALUE_RETIRE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/value-retirement}"
image="$artifact_dir/value_retirement.B"
trace="$artifact_dir/value_retirement.T"
old_trace="$artifact_dir/value_retirement.previous-reader.T"
validation_log="$artifact_dir/validation.log"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_VALUE_RETIRE_ONLY=1 \
OPEN64_DSL_VALUE_RETIRE_ARTIFACT="$image" \
  "$contract_test" >"$validation_log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" >>"$validation_log" 2>&1

for evidence in \
  'operator=OPR_DSLRELU version=2' \
  'status=retired redirected_to=value' \
  'status=redirected redirected_to=value' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing retirement evidence '$evidence' in $trace" >&2
    exit 1
  fi
done
if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

if [[ -n "$old_ir_b2a" ]]; then
  "$old_ir_b2a" -st -src "$image" "$old_trace" >>"$validation_log" 2>&1
  grep -Fq 'operator=OPR_DSLRELU version=2' "$old_trace"
fi

echo "DSL value retirement fixture passed"
echo "review image: $image"
echo "review trace: $trace"
if [[ -n "$old_ir_b2a" ]]; then
  echo "previous-reader trace: $old_trace"
fi
