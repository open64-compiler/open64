#!/usr/bin/env bash
#
# Preserve the SYNC-3 FHE VHO phase-ordering trace for human review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_FHE_CONVERT_TEST:-$repo_root/build/osprey/targdir/ir_tools/fhe_convert_contract_test}"
artifact_dir="${OPEN64_DSL_FHE_SYNC3_VHO_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync3-vho}"
trace="$artifact_dir/fhe_sync3_vho_conversion.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"

if [[ ! -x "$contract_test" ]]; then
  echo "missing executable: $contract_test" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_FHE_CONVERT_TRACE=$trace $contract_test" >"$command_log"

if ! OPEN64_FHE_CONVERT_TRACE="$trace" \
    "$contract_test" >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

for evidence in \
  'WHIRL before VHO FHE Conversion' \
  'FUNC_ENTRY' \
  'WHIRL after VHO FHE Conversion'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing FHE VHO evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

echo "FHE SYNC-3 VHO conversion phase fixture passed"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
