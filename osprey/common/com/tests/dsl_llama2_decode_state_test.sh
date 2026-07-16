#!/usr/bin/env bash
#
# Certify decoder-layer K/V state identity and ordering through mapped WHIRL.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_DECODE_STATE_ARTIFACT_DIR:-${TMPDIR:-/tmp}/open64-dsl-decode-state.$$}"
source_file="osprey/common/com/tests/llama2_decode_state.py"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done
if [[ ! -f "$repo_root/$source_file" ]]; then
  echo "missing source fixture: $repo_root/$source_file" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
state_image="$artifact_dir/decode_state.B"
state_trace="$artifact_dir/decode_state.T"

(cd "$repo_root" && \
  OPEN64_DSL_DECODE_STATE_ONLY=1 \
  OPEN64_DSL_DECODE_STATE_ARTIFACT="$state_image" \
  OPEN64_DSL_DECODE_STATE_SOURCE="$source_file" \
    "$contract_test")
(cd "$repo_root" && \
  "$ir_b2a" -st -src "$state_image") > "$state_trace"

for evidence in \
  'source files:' \
  'llama2_decode_state.py' \
  'query = builder.model_input' \
  'positioned_query = builder.rotary_embedding_v2' \
  'cached_attention = builder.attention_v2' \
  'OPR_DSLROTARYEMBEDDING # OPR_DSLROTARYEMBEDDING version=2' \
  'OPR_DSLATTENTION # OPR_DSLATTENTION version=2' \
  'contract=transformer.decoder_layer.v2' \
  'VALUE ordinal=0' \
  'roles=0x4 flags=0x1b' \
  'DSL Abstract State Table: version=1 states=2 effects=2' \
  'name=layer0.key_cache kind=mutable_buffer' \
  'name=layer0.value_cache kind=mutable_buffer' \
  'stable_name=transformer.rotary_embedding version=2 operands=4' \
  'stable_name=transformer.attention version=2 operands=3' \
  'kind=modify'; do
  if ! grep -Fq "$evidence" "$state_trace"; then
    echo "missing decode-state evidence '$evidence'" >&2
    exit 1
  fi
done

if [[ "$(grep -Fc '  EFFECT [' "$state_trace")" -ne 2 ]]; then
  echo "expected two verified decode-state effect rows" >&2
  exit 1
fi

echo "DSL Llama 2 decode-state fixture passed"
echo "review artifacts: $artifact_dir"
