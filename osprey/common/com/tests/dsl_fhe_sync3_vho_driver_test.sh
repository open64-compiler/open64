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
driver_source="$repo_root/osprey/be/be/driver.cxx"
config_source="$repo_root/osprey/common/com/config_fhe.cxx"

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

for evidence in \
  'VHO_FHE_Convert_Driver_With_Result' \
  'VHO_FHE_Convert_Checkpoint_Begin' \
  'VHO_FHE_Convert_Checkpoint_Validate' \
  'VHO_FHE_Convert_Checkpoint_Finalize' \
  'VHO_FHE_Convert_Checkpoint_Publish_Artifacts' \
  'VHO_FHE_Convert_Checkpoint_Complete' \
  'VHO_FHE_Convert_Checkpoint_Abort' \
  'Write_PU_Info(current_pu)' \
  'Write_Global_Info(pu_tree)' \
  'Publish_FHE_Conversion_Checkpoint'; do
  if ! grep -Fq "$evidence" "$driver_source"; then
    echo "missing all-PU checkpoint evidence '$evidence' in $driver_source" >&2
    exit 1
  fi
done

finalize_line="$(grep -n 'VHO_FHE_Convert_Checkpoint_Finalize' "$driver_source" | tail -1 | cut -d: -f1)"
global_line="$(grep -n 'Write_Global_Info(pu_tree)' "$driver_source" | tail -1 | cut -d: -f1)"
publish_line="$(grep -n 'VHO_FHE_Convert_Checkpoint_Publish_Artifacts' "$driver_source" | tail -1 | cut -d: -f1)"
binary_line="$(grep -n 'if (!Publish_FHE_Conversion_Checkpoint())' "$driver_source" | tail -1 | cut -d: -f1)"
complete_line="$(grep -n 'VHO_FHE_Convert_Checkpoint_Complete' "$driver_source" | tail -1 | cut -d: -f1)"
if (( finalize_line >= global_line || global_line >= publish_line ||
      publish_line >= binary_line || binary_line >= complete_line )); then
  echo "FHE checkpoint publication ordering changed in $driver_source" >&2
  exit 1
fi

if ! grep -Fq '"checkpoint", "checkpoint"' "$config_source"; then
  echo "missing -FHE:checkpoint option in $config_source" >&2
  exit 1
fi

echo "FHE SYNC-3 VHO conversion phase fixture passed"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
