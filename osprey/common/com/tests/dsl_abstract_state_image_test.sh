#!/usr/bin/env bash
#
# Certify the additive abstract-state image before stateful op allocation.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
lower_test="${OPEN64_DSL_LOWER_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_lower_contract_test}"
artifact_dir="${OPEN64_DSL_STATE_ARTIFACT_DIR:-${TMPDIR:-/tmp}/open64-dsl-state.$$}"

for executable in "$contract_test" "$ir_b2a" "$lower_test"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
state_image="$artifact_dir/abstract_state.B"
state_trace="$artifact_dir/abstract_state.T"
lower_trace="$artifact_dir/abstract_state_lowered.T"

(cd "$(dirname "$contract_test")" && \
  OPEN64_DSL_STATE_EFFECT_ONLY=1 \
  OPEN64_DSL_STATE_EFFECT_ARTIFACT="$state_image" \
    "./$(basename "$contract_test")")
(cd "$(dirname "$ir_b2a")" && \
  "./$(basename "$ir_b2a")" -st -src "$state_image") > "$state_trace"
(cd "$(dirname "$lower_test")" && \
  OPEN64_DSL_STATE_LOWER_ONLY=1 \
  OPEN64_DSL_STATE_LOWER_TRACE="$lower_trace" \
    "./$(basename "$lower_test")")

for evidence in \
  "DSL Abstract State Table: version=1 states=3 effects=3" \
  "name=runtime_status kind=runtime_status" \
  "name=random_state kind=random" \
  "name=mutable_buffer kind=mutable_buffer" \
  "kind=read" \
  "kind=modify"; do
  if ! grep -Fq "$evidence" "$state_trace"; then
    echo "missing abstract-state evidence '$evidence'" >&2
    exit 1
  fi
done

if [[ "$(grep -Fc "  EFFECT [" "$state_trace")" -ne 3 ]]; then
  echo "expected three verified state-effect rows" >&2
  exit 1
fi

for evidence in \
  "WHIRL after abstract-state VHO DSL lowering" \
  "state_reads=1 state_modifies=2" \
  "__open64_dsl_scatter_v1" \
  "runtime_status" \
  "random_state" \
  "mutable_buffer"; do
  if ! grep -Fq "$evidence" "$lower_trace"; then
    echo "missing lowered state evidence '$evidence'" >&2
    exit 1
  fi
done

legacy_image="${OPEN64_DSL_LEGACY_IMAGE:-}"
if [[ -n "$legacy_image" ]]; then
  legacy_trace="$artifact_dir/legacy_without_effect_section.T"
  (cd "$(dirname "$ir_b2a")" && \
    "./$(basename "$ir_b2a")" -st -src "$legacy_image") > "$legacy_trace"
  grep -Fq "DSL Abstract State Table: version=1 states=0 effects=0" \
    "$legacy_trace"
fi

echo "DSL abstract-state image fixture passed"
echo "review artifacts: $artifact_dir"
