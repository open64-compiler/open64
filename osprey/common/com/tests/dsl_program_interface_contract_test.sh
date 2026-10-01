#!/usr/bin/env bash
#
# Preserve ABI-pruning and rooted-resource program-interface evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_PROGRAM_INTERFACE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/program-interface}"
image="$artifact_dir/program_interface_contract.B"
permuted_image="$artifact_dir/program_interface_contract_permuted.B"
trace="$artifact_dir/program_interface_contract.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"
previous_reader_log="$artifact_dir/previous_reader.log"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_PROGRAM_INTERFACE_CYCLE_ONLY=1 $contract_test" \
  "OPEN64_DSL_PROGRAM_INTERFACE_ONLY=1 OPEN64_DSL_PROGRAM_INTERFACE_ARTIFACT=$image $contract_test" \
  "OPEN64_DSL_PROGRAM_INTERFACE_ONLY=1 OPEN64_DSL_PROGRAM_INTERFACE_PERMUTE=1 OPEN64_DSL_PROGRAM_INTERFACE_ARTIFACT=$permuted_image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_PROGRAM_INTERFACE_CYCLE_ONLY=1 \
    "$contract_test") >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! (cd "$repo_root" && \
  OPEN64_DSL_PROGRAM_INTERFACE_ONLY=1 \
  OPEN64_DSL_PROGRAM_INTERFACE_ARTIFACT="$image" \
    "$contract_test") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! (cd "$repo_root" && \
  OPEN64_DSL_PROGRAM_INTERFACE_ONLY=1 \
  OPEN64_DSL_PROGRAM_INTERFACE_PERMUTE=1 \
  OPEN64_DSL_PROGRAM_INTERFACE_ARTIFACT="$permuted_image" \
    "$contract_test") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! cmp -s "$image" "$permuted_image"; then
  echo "permuted program-interface plans changed binary WHIRL bytes" >&2
  exit 1
fi
if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if "$OPEN64_PREVIOUS_IR_B2A" -st -src "$image" \
       "$artifact_dir/previous_reader.T" >"$previous_reader_log" 2>&1; then
    echo "previous ir_b2a unexpectedly accepted evolved program interface" >&2
    exit 1
  fi
  if ! grep -Eq 'DSL (PU|runtime) interface error' "$previous_reader_log"; then
    echo "previous ir_b2a did not fail closed at a DSL interface gate" >&2
    exit 1
  fi
fi
if ! (cd "$repo_root" && \
  "$ir_b2a" -st -src "$image" "$trace") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

for evidence in \
  'DSL Program Interface Image: version=1 retired_formals=2 retired_call_arguments=4 runtime_inputs=6 runtime_bindings=12 runtime_calls=12' \
  'DSL Retired Formal Table:' \
  'reason=verified_dead_input role=fhe.dead_bn_input' \
  'DSL Retired Call Argument Table:' \
  'DSL Runtime Input Table:' \
  'kind=opaque_resource role=fhe.model' \
  'kind=source_external_tensor role=fhe.promoted.weight.context0' \
  'kind=source_external_tensor role=fhe.promoted.bias.context0' \
  'tensor_descriptor={kind=tensor,dtype=float32,rank=4,shape=[1,1,2,2]' \
  'tensor_descriptor={kind=tensor,dtype=float32,rank=1,shape=[4]' \
  'kind=tensor_tcon_resource role=fhe.relu.coefficient.stage0' \
  'kind=tensor_tcon_resource role=fhe.relu.coefficient.stage1' \
  'kind=tensor_tcon_resource role=fhe.relu.coefficient.stage2' \
  'DSL Runtime Input Binding Table:' \
  'kind=root_resource role=fhe.model' \
  'kind=threaded_formal role=fhe.model' \
  'kind=threaded_formal role=fhe.promoted.weight' \
  'kind=threaded_formal role=fhe.promoted.bias' \
  'DSL Runtime Input Call Table:' \
  'contract=fhe.retirement.prune.v1' \
  'VALUE ordinal=1 st=<2,1> roles=0x1 flags=0x0' \
  'program_interface_callee' \
  'program_interface_caller' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing program-interface evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq 'VALUE ordinal=0' "$trace"; then
  echo "retired REGION input remains in $trace" >&2
  exit 1
fi

if [[ "$(grep -Ec '^FUNC_ENTRY ' "$trace")" -ne 2 ]] ||
   [[ "$(grep -Ec 'VCALL .*program_interface_callee' "$trace")" -ne 2 ]]; then
  echo "program-interface PU/call census changed in $trace" >&2
  exit 1
fi

echo "DSL program interface contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
