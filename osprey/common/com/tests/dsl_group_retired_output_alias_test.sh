#!/usr/bin/env bash
# Retain same-TY retired output alias closure and rollback evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_GROUP_ALIAS_ARTIFACT_DIR:-$repo_root/artifacts/fhe/group-retired-output-alias}"
image="$artifact_dir/root_alias.B"
trace="$artifact_dir/root_alias.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done
mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
printf '%s\n' \
  "OPEN64_DSL_GROUP_LOWER_ONLY=1 OPEN64_DSL_GROUP_LOWER_ROOT_ALIAS=1 OPEN64_DSL_GROUP_LOWER_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

OPEN64_DSL_GROUP_LOWER_ONLY=1 \
OPEN64_DSL_GROUP_LOWER_ROOT_ALIAS=1 \
OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1

if ! grep -Fq 'operator=OPR_DSLOUTPUTLOGITS version=2' "$trace" ||
   ! grep -Fq 'status=retired redirected_to=' "$trace" ||
   ! grep -Eq 'U8U8LDID .*__dsl_runtime_group_step_5_' "$trace" ||
   grep -Eq 'U8U8LDID .*__dsl_runtime_group_output_logits_' "$trace" ||
   ! grep -Fq 'group_model_result' "$trace"; then
  echo "retired output alias closure is missing from $trace" >&2
  exit 1
fi

for case_name in address predom standard wrong_type; do
  negative="$artifact_dir/rejected_${case_name}.B"
  OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ROOT_ALIAS=1 \
  OPEN64_DSL_GROUP_LOWER_ROOT_ALIAS_NEGATIVE="$case_name" \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$negative" \
    "$producer" >"$artifact_dir/${case_name}.log" 2>&1
  if ! grep -Fq "retired alias $case_name rejected atomically" \
       "$artifact_dir/${case_name}.log" ||
     [[ -e "$negative" || -e "$negative.tmp" ]]; then
    echo "retired alias $case_name rejection was not atomic" >&2
    exit 1
  fi
done

echo "DSL grouped retired output alias contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review diagnostics: $artifact_dir"
