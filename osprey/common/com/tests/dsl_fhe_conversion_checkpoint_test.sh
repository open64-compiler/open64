#!/usr/bin/env bash
#
# Exercise the backend-owned all-PU FHE conversion checkpoint and retain the
# binary WHIRL, ir_b2a -st -src trace, commands, and diagnostics for review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
be="${OPEN64_FHE_CHECKPOINT_BE:-$repo_root/build/osprey/targdir/be/be}"
ir_b2a="${OPEN64_FHE_CHECKPOINT_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
input="${OPEN64_FHE_CHECKPOINT_INPUT:-}"
source_file="${OPEN64_FHE_CHECKPOINT_SOURCE:-}"
reject_input="${OPEN64_FHE_CHECKPOINT_REJECT_INPUT:-}"
expected_pus="${OPEN64_FHE_CHECKPOINT_EXPECTED_PUS:-2}"
expected_regions="${OPEN64_FHE_CHECKPOINT_EXPECTED_REGIONS:-}"
expected_region_contract="${OPEN64_FHE_CHECKPOINT_EXPECTED_REGION_CONTRACT:-}"
artifact_dir="${OPEN64_FHE_CHECKPOINT_ARTIFACT_DIR:-$repo_root/artifacts/fhe/conversion-checkpoint-driver}"

if [[ ! -x "$be" ]]; then
  echo "missing backend executable: $be" >&2
  exit 1
fi
if [[ ! -x "$ir_b2a" ]]; then
  echo "missing ir_b2a executable: $ir_b2a" >&2
  exit 1
fi
if [[ -z "$input" || ! -f "$input" ]]; then
  echo "set OPEN64_FHE_CHECKPOINT_INPUT to a binary WHIRL fixture" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

input_name="$(basename "$input")"
input_stem="${input_name%.B}"
output="$artifact_dir/$input_stem.fhe.B"
trace="$artifact_dir/$input_stem.fhe.T"
validation_log="$artifact_dir/validation.log"
failure_log="$artifact_dir/failure.log"
command_log="$artifact_dir/commands.txt"

printf '%s\n' \
  "$be -FHE:checkpoint=$output $input" \
  "$ir_b2a -st -src $output $trace" >"$command_log"

if [[ -n "$source_file" ]]; then
  if [[ ! -f "$source_file" ]]; then
    echo "missing checkpoint source file: $source_file" >&2
    exit 1
  fi
  cp "$source_file" "$artifact_dir/$(basename "$source_file")"
fi

if ! "$be" -FHE:checkpoint="$output" "$input" \
    >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

(
  cd "$artifact_dir"
  "$ir_b2a" -st -src "$(basename "$output")" "$(basename "$trace")"
)

actual_pus="$(grep -c '^FUNC_ENTRY' "$trace")"
if [[ "$actual_pus" != "$expected_pus" ]]; then
  echo "expected $expected_pus PUs, found $actual_pus in $trace" >&2
  exit 1
fi
if [[ -n "$expected_regions" ]]; then
  actual_regions="$(grep -c '^ REGION [0-9]' "$trace")"
  if [[ "$actual_regions" != "$expected_regions" ]]; then
    echo "expected $expected_regions REGIONs, found $actual_regions in $trace" >&2
    exit 1
  fi
fi
if [[ -n "$expected_region_contract" ]] &&
   ! grep -Fq "contract=$expected_region_contract" "$trace"; then
  echo "missing REGION contract $expected_region_contract in $trace" >&2
  exit 1
fi
if ! grep -Fq 'FHE conversion checkpoint: output=' "$validation_log"; then
  echo "missing successful checkpoint summary in $validation_log" >&2
  exit 1
fi

if [[ -n "$reject_input" ]]; then
  rejected_output="$artifact_dir/must_not_exist.fhe.B"
  if "$be" -FHE:checkpoint="$rejected_output" "$reject_input" \
      >"$failure_log" 2>&1; then
    echo "unsupported FHE conversion unexpectedly succeeded" >&2
    exit 1
  fi
  if [[ -e "$rejected_output" || -e "$rejected_output.tmp" ]]; then
    echo "failed conversion published a checkpoint artifact" >&2
    exit 1
  fi
  if ! grep -Fq 'CFHE-CHECKPOINT-002' "$failure_log"; then
    echo "missing stable checkpoint failure diagnostic" >&2
    exit 1
  fi
fi

echo "FHE all-PU conversion checkpoint fixture passed"
echo "review binary: $output"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
if [[ -f "$failure_log" ]]; then
  echo "review rejected-run diagnostics: $failure_log"
fi
