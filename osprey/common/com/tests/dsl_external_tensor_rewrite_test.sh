#!/usr/bin/env bash
#
# Preserve and inspect the typed external tensor and atomic rewrite contract.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_EXTERNAL_TENSOR_REWRITE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/external-tensor-rewrite}"
image="$artifact_dir/external_tensor_rewrite.B"
trace="$artifact_dir/external_tensor_rewrite.T"
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
  "OPEN64_DSL_EXTERNAL_TENSOR_REWRITE_ONLY=1 OPEN64_DSL_EXTERNAL_TENSOR_REWRITE_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_EXTERNAL_TENSOR_REWRITE_ONLY=1 \
  OPEN64_DSL_EXTERNAL_TENSOR_REWRITE_ARTIFACT="$image" \
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
  'name=layer1_0_source_weight;dtype=float32;rank=4' \
  'name=layer1_0_source_bias;dtype=float32;rank=1' \
  'name=layer1_0_folded_weight;dtype=float32;rank=4' \
  'name=layer1_1_folded_weight;dtype=float32;rank=4' \
  'name=stem_folded_bias;dtype=float32;rank=1' \
  'source.safetensors#layer1.0.conv1.weight' \
  'converted.safetensors#layer1.0.conv1.folded_weight' \
  'converted.safetensors#layer1.1.conv1.folded_weight' \
  'converted.safetensors#stem.conv.folded_bias' \
  'side_file=converted.safetensors' \
  'dsl.converted_from_value_id' \
  'DSL Call ABI Argument Table: version=1 entries=4' \
  'DSL PU Interface Formal Table: version=1 entries=3' \
  'formal=0 value=' \
  'formal=1 value=' \
  'formal=2 value=' \
  'role=cnn.basic_block.conv1.weight' \
  'role=cnn.basic_block.conv1.bias' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing external tensor rewrite evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

for evidence in \
  'FUNC_ENTRY .*external_tensor_callee' \
  'FUNC_ENTRY .*external_tensor_caller' \
  'U8LDA .*layer1_0_folded_weight' \
  'U8LDA .*layer1_0_folded_bias' \
  'U8LDA .*layer1_1_folded_weight' \
  'U8LDA .*layer1_1_folded_bias'; do
  if ! grep -Eq "$evidence" "$trace"; then
    echo "missing external tensor rewrite evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if [[ "$(grep -Ec '^FUNC_ENTRY ' "$trace")" -ne 2 ]] ||
   [[ "$(grep -Ec 'VCALL .*external_tensor_callee' "$trace")" -ne 2 ]]; then
  echo "shared-callee PU/call census changed in $trace" >&2
  exit 1
fi

if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

echo "external tensor rewrite fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
