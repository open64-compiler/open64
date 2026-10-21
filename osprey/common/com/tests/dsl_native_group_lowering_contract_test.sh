#!/usr/bin/env bash
#
# Preserve grouped native-to-standard WHIRL transaction evidence for review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_GROUP_LOWER_ARTIFACT_DIR:-$repo_root/artifacts/fhe/grouped-whirl-lowering}"
image="$artifact_dir/group_lowering_contract.B"
trace="$artifact_dir/group_lowering_contract.T"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
printf '%s\n' \
  "OPEN64_DSL_GROUP_LOWER_ONLY=1 OPEN64_DSL_GROUP_LOWER_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

if ! (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" "$contract_test") \
    >"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi
if ! (cd "$repo_root" && "$ir_b2a" -st -src "$image" "$trace") \
    >>"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi

for evidence in \
  'DSL Value Table:' \
  'status=lowered relation=private_group_member' \
  'status=lowered relation=runtime_value_projection projection=' \
  'group_step_0' \
  'group_step_5' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing grouped-lowering evidence '$evidence' in $trace" >&2
    exit 1
  fi
done
if [[ "$(grep -Fc 'status=lowered relation=private_group_member' "$trace")" -ne 8 ]] ||
   [[ "$(grep -Fc 'status=lowered relation=runtime_value_projection' "$trace")" -ne 4 ]] ||
   [[ "$(grep -Ec '^  OPR_DSLADD ' "$trace")" -ne 0 ]]; then
  echo "grouped lowering census changed" >&2
  exit 1
fi

for negative_case in private_cross_group return_escape call_escape \
                     region_escape interface_escape; do
  printf '%s\n' "OPEN64_DSL_GROUP_LOWER_NEGATIVE_CASE=$negative_case $contract_test" \
    >>"$artifact_dir/commands.txt"
  if ! (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
    OPEN64_DSL_GROUP_LOWER_NEGATIVE_CASE="$negative_case" \
    OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" "$contract_test") \
      >"$artifact_dir/negative_${negative_case}.log" 2>&1; then
    cat "$artifact_dir/negative_${negative_case}.log" >&2
    exit 1
  fi
  if ! grep -Fq "negative $negative_case preserved tree and image" \
      "$artifact_dir/negative_${negative_case}.log"; then
    echo "missing rollback evidence for $negative_case" >&2
    exit 1
  fi
done

if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if "$OPEN64_PREVIOUS_IR_B2A" -st -src "$image" \
       "$artifact_dir/previous_reader.T" \
       >"$artifact_dir/previous_reader.log" 2>&1; then
    echo "previous ir_b2a unexpectedly accepted private lowered rows" >&2
    exit 1
  fi
  if ! grep -Eq 'DSL image error|invalid (node|value)' \
      "$artifact_dir/previous_reader.log"; then
    echo "previous reader did not fail closed at the DSL image gate" >&2
    exit 1
  fi
fi

echo "DSL grouped native lowering contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review diagnostics: $artifact_dir/validation.log"
