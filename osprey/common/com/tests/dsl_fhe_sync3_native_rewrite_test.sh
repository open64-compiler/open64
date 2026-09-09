#!/usr/bin/env bash
#
# Preserve and inspect the SYNC-3 owner-aware native rewrite contract.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_FHE_SYNC3_REWRITE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync3-rewrite}"
image="$artifact_dir/fhe_sync3_native_rewrite.B"
trace="$artifact_dir/fhe_sync3_native_rewrite.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_FHE_SYNC3_REWRITE_ONLY=1 OPEN64_DSL_FHE_SYNC3_REWRITE_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_FHE_SYNC3_REWRITE_ONLY=1 \
  OPEN64_DSL_FHE_SYNC3_REWRITE_ARTIFACT="$image" \
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
  'fhe_rewrite_first' \
  'fhe_rewrite_second' \
  'OPR_DSLADD # OPR_DSLADD version=1' \
  'OPR_DSLMUL # OPR_DSLMUL version=1' \
  'name=shared_result' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing FHE SYNC-3 rewrite evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

echo "FHE SYNC-3 native rewrite fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
