#!/usr/bin/env bash
#
# Preserve human-reviewable SYNC-5 evidence for semantic FHE lowering from one
# native common.relu value to checked standard-WHIRL ACE runtime calls.
#
# Design references:
#   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md
#   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
#   doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md
#   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_FHE_RUNTIME_LOWER_TEST:-$repo_root/build/osprey/targdir/ir_tools/fhe_runtime_lower_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_FHE_RUNTIME_LOWER_ARTIFACT_DIR:-$repo_root/artifacts/fhe/runtime-call-resolution}"
binary="$artifact_dir/runtime_call_resolution.B"
trace="$artifact_dir/runtime_call_resolution.T"
pu_trace="$artifact_dir/fhe_runtime_binding_contract.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"
hash_log="$artifact_dir/SHA256SUMS"
schedule_input="${OPEN64_FHE_RUNTIME_SCHEDULE_INPUT:-}"
schedule_log="$artifact_dir/schedule-census.log"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%q %q\n' \
  "env -u OPEN64_FHE_RUNTIME_SCHEDULE_INPUT OPEN64_FHE_RUNTIME_LOWER_ARTIFACT=$binary" \
  "$contract_test" \
  >"$command_log"
printf '%q -st -src %q %q\n' "$ir_b2a" "$binary" "$trace" \
  >>"$command_log"

if ! env -u OPEN64_FHE_RUNTIME_SCHEDULE_INPUT \
    OPEN64_FHE_RUNTIME_LOWER_ARTIFACT="$binary" \
    "$contract_test" >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

"$ir_b2a" -st -src "$binary" "$trace" >>"$validation_log" 2>&1

if [[ -n "$schedule_input" ]]; then
  printf '%q=%q %q\n' OPEN64_FHE_RUNTIME_SCHEDULE_INPUT \
    "$schedule_input" "$contract_test" >>"$command_log"
  OPEN64_FHE_RUNTIME_SCHEDULE_INPUT="$schedule_input" \
    "$contract_test" >"$schedule_log" 2>&1
  if ! grep -Fq \
      'FHE runtime schedule census: pu=6 records=32 static=87 dynamic=147' \
      "$schedule_log"; then
    cat "$schedule_log" >&2
    exit 1
  fi
fi

awk '
  /FUNC_ENTRY .*fhe_runtime_binding_contract/ { in_pu = 1 }
  in_pu && /SYMTAB for fhe_runtime_binding_contract:/ { exit }
  in_pu { print }
' "$trace" >"$pu_trace"

if [[ "$(grep -c 'open64_fhe_operation_desc_select_v1' "$pu_trace")" -ne 6 ]] ||
   [[ "$(grep -c 'open64_fhe_bootstrap_v1' "$pu_trace")" -ne 1 ]] ||
   [[ "$(grep -c 'open64_fhe_relu_normalize_v1' "$pu_trace")" -ne 1 ]] ||
   [[ "$(grep -c 'open64_fhe_relu_poly_stage_v1' "$pu_trace")" -ne 3 ]] ||
   [[ "$(grep -c 'open64_fhe_relu_reconstruct_v1' "$pu_trace")" -ne 1 ]]; then
  echo "complete ReLU runtime-call census changed in $pu_trace" >&2
  exit 1
fi

for role in \
  'role=fhe.model' \
  'role=fhe.relu.coefficient.stage0' \
  'role=fhe.relu.coefficient.stage1' \
  'role=fhe.relu.coefficient.stage2'; do
  if ! grep -Fq "$role" "$trace"; then
    echo "missing runtime-input evidence '$role' in $trace" >&2
    exit 1
  fi
done

if ! grep -Eq \
    'operator=OPR_DSLRELU .*status=lowered relation=runtime_value_projection' \
    "$trace"; then
  echo "missing lowered common.relu provenance in $trace" >&2
  exit 1
fi
if grep -q 'OPR_DSLRELU' "$pu_trace"; then
  echo "executable common.relu survived in $pu_trace" >&2
  exit 1
fi

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    runtime_call_resolution.B runtime_call_resolution.T \
    fhe_runtime_binding_contract.T >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    runtime_call_resolution.B runtime_call_resolution.T \
    fhe_runtime_binding_contract.T >SHA256SUMS)
fi

echo "FHE semantic runtime lowering fixture passed"
echo "review binary: $binary"
echo "review trace: $trace"
echo "review PU trace: $pu_trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
echo "review hashes: $hash_log"
if [[ -n "$schedule_input" ]]; then
  echo "review full-model census: $schedule_log"
fi
