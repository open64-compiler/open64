#!/usr/bin/env bash
#
# Preserve the canonical-tensor to standard-WHIRL runtime-interface evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_RUNTIME_INTERFACE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/runtime-interface}"
image="$artifact_dir/runtime_interface_contract.B"
trace="$artifact_dir/runtime_interface_contract.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"
previous_ir_b2a="${OPEN64_PREVIOUS_IR_B2A:-}"
previous_log="$artifact_dir/previous-reader.log"
previous_trace="$artifact_dir/previous_reader_runtime_interface.T"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_RUNTIME_INTERFACE_ONLY=1 OPEN64_DSL_RUNTIME_INTERFACE_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"
if [[ -n "$previous_ir_b2a" ]]; then
  printf '%s\n' \
    "$previous_ir_b2a -st -src $image $previous_trace (expected reject)" \
    >>"$command_log"
fi

if ! (cd "$repo_root" && \
  OPEN64_DSL_RUNTIME_INTERFACE_ONLY=1 \
  OPEN64_DSL_RUNTIME_INTERFACE_ARTIFACT="$image" \
    "$contract_test") >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! (cd "$repo_root" && \
  "$ir_b2a" -st -src "$image" "$trace") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

if [[ -n "$previous_ir_b2a" ]]; then
  if [[ ! -x "$previous_ir_b2a" ]]; then
    echo "missing previous ir_b2a: $previous_ir_b2a" >&2
    exit 1
  fi
  set +e
  (cd "$repo_root" &&
    "$previous_ir_b2a" -st -src "$image" "$previous_trace") \
      >"$previous_log" 2>&1
  previous_status=$?
  set -e
  printf 'exit_status=%d\n' "$previous_status" >>"$previous_log"
  if [[ "$previous_status" -eq 0 ]]; then
    echo "previous ir_b2a unexpectedly accepted projected ABI" >&2
    exit 1
  fi
fi

for evidence in \
  'DSL Runtime Interface Image: version=1 values=7 calls=3' \
  'DSL Runtime Value Projection Table:' \
  'kind=input_formal formal=0' \
  'kind=input_formal formal=1' \
  'kind=result_formal formal=2' \
  'kind=local_value formal=<none>' \
  'DSL Runtime Call Projection Table:' \
  'direction=input' \
  'direction=result' \
  'runtime_interface_callee' \
  'runtime_interface_caller' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing runtime-interface evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if [[ "$(grep -Ec '^FUNC_ENTRY ' "$trace")" -ne 2 ]] ||
   [[ "$(grep -Ec 'VCALL .*runtime_interface_callee' "$trace")" -ne 1 ]]; then
  echo "runtime-interface PU/call census changed in $trace" >&2
  exit 1
fi

echo "DSL runtime interface contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
if [[ -n "$previous_ir_b2a" ]]; then
  echo "previous-reader evidence: $previous_log"
fi
