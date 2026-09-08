#!/usr/bin/env bash
#
# Preserve and inspect the minimal SYNC-1 FHE mapped-image contract.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_FHE_SYNC1_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync1}"
image="$artifact_dir/fhe_sync1_contract.B"
trace="$artifact_dir/fhe_sync1_contract.T"
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
  "OPEN64_DSL_FHE_SYNC1_ONLY=1 OPEN64_DSL_FHE_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_FHE_SYNC1_ONLY=1 \
  OPEN64_DSL_FHE_ARTIFACT="$image" \
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
  'OPR_DSLADD # OPR_DSLADD version=1' \
  'FHE Image: version=1 capabilities=0x0000003f' \
  'FHE Compilation Configuration Table:' \
  'scheme=ckks security=1 ring_dimension=65536' \
  'FHE Entry Contract Table:' \
  'pu=fhe_sync1_add config=1' \
  'FHE Entry Value Table:' \
  'role=input ordinal=0 class=ciphertext' \
  'role=parameter ordinal=0 class=encoded_plaintext' \
  'role=output ordinal=0 class=ciphertext' \
  'FHE Encryption Descriptor Table:' \
  'FHE Tensor Binding Table:' \
  'FHE Key Requirement Table:' \
  'class=public' \
  'kid0=encrypted_input;kid1=encrypted_input' \
  'type_name=fhe_sync1_weight_f32_2x2 encryption=2' \
  'Tensor storage: placement = side_file (fhe_sync1_weights.safetensors)' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing FHE SYNC-1 evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

echo "FHE SYNC-1 mapped-image fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
