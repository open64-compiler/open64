#!/usr/bin/env bash
#
# Preserve and inspect the provider-independent SYNC-4 ReLU schedule image.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
old_ir_b2a="${OPEN64_PREVIOUS_IR_B2A:-}"
artifact_dir="${OPEN64_DSL_FHE_SYNC4_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync4-materialization}"
image="$artifact_dir/fhe_sync4_materialization_contract.B"
trace="$artifact_dir/fhe_sync4_materialization_contract.T"
old_trace="$artifact_dir/fhe_sync4_materialization_contract.previous-reader.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done
if [[ -n "$old_ir_b2a" && ! -x "$old_ir_b2a" ]]; then
  echo "missing previous reader: $old_ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 OPEN64_DSL_FHE_SYNC4_MATERIALIZATION=1 OPEN64_DSL_FHE_SYNC3_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"
if [[ -n "$old_ir_b2a" ]]; then
  printf '%s\n' "$old_ir_b2a -st -src $image $old_trace" >>"$command_log"
fi

if ! (cd "$repo_root" && \
  OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 \
  OPEN64_DSL_FHE_SYNC4_MATERIALIZATION=1 \
  OPEN64_DSL_FHE_SYNC3_ARTIFACT="$image" \
    "$contract_test") >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! (cd "$repo_root" && \
  "$ir_b2a" -st -src "$image" "$trace") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

for evidence in \
  'FHE ReLU Materialization Image: version=1 capabilities=0x00000001 contexts=2 operations=12' \
  'FHE ReLU Context Operation Table:' \
  'callsite=0 ordinal=0 kind=refresh' \
  'callsite=0 ordinal=1 kind=normalize' \
  'callsite=0 ordinal=2 kind=approx_stage' \
  'callsite=0 ordinal=5 kind=reconstruct_relu' \
  'callsite=1 ordinal=0 kind=refresh' \
  'callsite=1 ordinal=5 kind=reconstruct_relu' \
  'role=post_operation state_version=4' \
  'role=result state_version=1' \
  'level=4 scale_bits=56 components=2 precision_bits=30' \
  'level=7 scale_bits=56 components=2 precision_bits=30' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing FHE SYNC-4 evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

if [[ -n "$old_ir_b2a" ]]; then
  "$old_ir_b2a" -st -src "$image" "$old_trace" \
    >>"$validation_log" 2>&1
  if grep -Fq 'FHE ReLU Materialization Image:' "$old_trace"; then
    echo "previous reader unexpectedly interpreted the optional section" >&2
    exit 1
  fi
  if [[ "$(grep -c 'FUNC_ENTRY' "$old_trace")" != "2" ]]; then
    echo "previous reader did not preserve the two baseline PUs" >&2
    exit 1
  fi
fi

if command -v readelf >/dev/null 2>&1; then
  section_line="$(readelf -SW "$image" | grep -F '.WHIRL.dsl_fhe_materialization')"
  if [[ -z "$section_line" ]]; then
    echo "missing .WHIRL.dsl_fhe_materialization in $image" >&2
    exit 1
  fi
  printf '%s\n' "$section_line" >>"$validation_log"
  section_size="$(printf '%s\n' "$section_line" | awk '{ print $6 }')"
  section_alignment="$(printf '%s\n' "$section_line" | awk '{ print $NF }')"
  if [[ "$section_size" != "000340" || "$section_alignment" != "8" ]]; then
    echo "unexpected materialization section size/alignment: $section_line" >&2
    exit 1
  fi
fi

(cd "$artifact_dir" && sha256sum \
  fhe_sync4_materialization_contract.B \
  fhe_sync4_materialization_contract.T \
  commands.txt validation.log \
  ${old_ir_b2a:+fhe_sync4_materialization_contract.previous-reader.T} \
  >SHA256SUMS)

echo "FHE SYNC-4 materialization-image fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
echo "review hashes: $artifact_dir/SHA256SUMS"
if [[ -n "$old_ir_b2a" ]]; then
  echo "previous-reader trace: $old_trace"
fi
