#!/usr/bin/env bash
#
# Preserve native-DSL to standard-WHIRL transaction and inspection evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_STANDARD_LOWER_ARTIFACT_DIR:-$repo_root/artifacts/fhe/standard-whirl-lowering}"
image="$artifact_dir/standard_lowering_contract.B"
trace="$artifact_dir/standard_lowering_contract.T"
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
  "OPEN64_DSL_STANDARD_LOWER_ONLY=1 OPEN64_DSL_STANDARD_LOWER_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_STANDARD_LOWER_ONLY=1 \
  OPEN64_DSL_STANDARD_LOWER_ARTIFACT="$image" \
    "$contract_test") >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if "$OPEN64_PREVIOUS_IR_B2A" -st -src "$image" \
       "$artifact_dir/previous_reader.T" >"$previous_reader_log" 2>&1; then
    echo "previous ir_b2a unexpectedly accepted lowered DSL flags" >&2
    exit 1
  fi
  if ! grep -Eq 'DSL image error|invalid (node|value)' \
       "$previous_reader_log"; then
    echo "previous ir_b2a did not fail closed at the DSL image gate" >&2
    exit 1
  fi
fi

if ! (cd "$repo_root" && \
  "$ir_b2a" -st -src "$image" "$trace") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

for evidence in \
  'operator=OPR_DSLADD version=1' \
  'status=lowered relation=runtime_value_projection projection=' \
  'status=lowered relation=root_promoted_input' \
  'status=lowered relation=root_promoted_input projection=' \
  'status=dead_elided relation=none' \
  'name=standard_weight' \
  'name=standard_bias' \
  'rank=4,shape=[1,1,2,2]' \
  'rank=1,shape=[4]' \
  '__dsl_runtime_dsl_result_1_4' \
  '__dsl_runtime_dsl_result_2_5' \
  '__dsl_input_fhe_standard_weight' \
  '__dsl_input_fhe_standard_bias' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing standard-lowering evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if [[ "$(grep -Fc 'status=lowered relation=runtime_value_projection' "$trace")" -ne 4 ]] ||
   [[ "$(grep -Fc 'status=lowered relation=root_promoted_input' "$trace")" -ne 4 ]] ||
   [[ "$(grep -Fc 'status=dead_elided relation=none' "$trace")" -ne 2 ]] ||
   [[ "$(grep -Ec '^  OPR_DSLADD ' "$trace")" -ne 0 ]] ||
   [[ "$(grep -Ec '^  OPR_DSLTENSORCONST .*standard_(weight|bias|seed)' "$trace")" -ne 0 ]]; then
  echo "standard-lowering relation or executable-node census changed" >&2
  exit 1
fi

echo "DSL native-to-standard lowering contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
