#!/usr/bin/env bash
#
# Preserve and inspect a mapped WHIRL image containing two independent PUs.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_MULTI_PU_DIR:-$repo_root/artifacts/torch2whirl/multi-pu}"
image="$artifact_dir/llama2_multi_pu.B"
trace="$artifact_dir/llama2_multi_pu.T"

mkdir -p "$artifact_dir"
rm -f "$image" "$trace"

OPEN64_DSL_MULTI_PU_ONLY=1 \
OPEN64_DSL_MULTI_PU_ARTIFACT="$image" \
  "$contract_test"
"$ir_b2a" -st -src "$image" > "$trace"

if [[ "$(grep -Fc 'FUNC_ENTRY' "$trace")" -ne 2 ]]; then
  echo "expected exactly two FUNC_ENTRY nodes in $trace" >&2
  exit 1
fi
for evidence in \
  "TinyRMSNorm" \
  "TinyLlama2ForCausalLM" \
  "hidden_states" \
  "normalized_result" \
  "model_hidden" \
  "model_result" \
  "normalized_hidden" \
  "U8LDA 0 <2,3,dsl_result_2>" \
  "Sclass: FORMAL" \
  "Sclass: FORMAL_REF" \
  "by_reference  read_only passed_not_saved" \
  "by_reference  out passed_not_saved" \
  "__WHIRL_DSL_CALL__:callee=TinyRMSNorm" \
  "DSL PU Source Identity Table: version=1 entries=1" \
  "definition=TinyRMSNorm.forward module=llama2_model" \
  "DSL Callsite Metadata Table: version=1 entries=1" \
  "class=TinyRMSNorm instance=model.norm" \
  "metadata=owner_pu=TinyRMSNorm" \
  "metadata=owner_pu=TinyLlama2ForCausalLM" \
  "location: file llama2_model.py, line 18" \
  "location: file llama2_model.py, line 72"; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing multiple-PU evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

callee_definition_line="$(grep -nF 'MSTID 0 <2,3,dsl_result_1>' "$trace" |
  head -1 | cut -d: -f1)"
callee_result_line="$(grep -nF 'MSTID 0 <2,2,normalized_result>' "$trace" |
  head -1 | cut -d: -f1)"
caller_definition_line="$(grep -nF 'MSTID 0 <2,3,dsl_result_2>' "$trace" |
  head -1 | cut -d: -f1)"
call_line="$(grep -nF 'VCALL 126 <1,50,TinyRMSNorm>' "$trace" |
  head -1 | cut -d: -f1)"
if [[ -z "$callee_definition_line" || -z "$callee_result_line" ||
      -z "$caller_definition_line" || -z "$call_line" ||
      "$callee_definition_line" -ge "$callee_result_line" ||
      "$caller_definition_line" -ge "$call_line" ]]; then
  echo "operator result materialization order changed in $trace" >&2
  exit 1
fi

echo "multiple-PU mapped-image fixture passed"
echo "review trace: $trace"
