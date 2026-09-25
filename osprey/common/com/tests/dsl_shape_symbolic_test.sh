#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP8_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp8-symbolic}"
image="$artifact_dir/shape_symbolic.B"
trace="$artifact_dir/shape_symbolic.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_SHAPE_WP6_ADMISSION_ONLY=1 \
  "$producer" >"$artifact_dir/wp6-admission.log" 2>&1
for order in AB BA; do
  OPEN64_DSL_SHAPE_WP6_ORDER_ONLY="$order" \
    "$producer" >"$artifact_dir/wp6-order-$order.log" 2>&1
  sed -E \
    -e 's/fresh order (AB|BA)/fresh order ORDER/' \
    -e 's/@pu[0-9a-f]{8}/@puLOCATOR/g' \
    "$artifact_dir/wp6-order-$order.log" \
    >"$artifact_dir/wp6-order-$order.normalized"
done
cmp -s "$artifact_dir/wp6-order-AB.normalized" \
  "$artifact_dir/wp6-order-BA.normalized"
OPEN64_DSL_SHAPE_SP8_ONLY=1 \
  "$producer" >"$artifact_dir/contract.log" 2>&1
OPEN64_DSL_SHAPE_SP8_ONLY=1 \
OPEN64_DSL_SHAPE_SP8_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace"

grep -q "SP8 symbolic shape contract passed" "$artifact_dir/contract.log"
grep -q "SP8 symbolic shape contract passed" "$artifact_dir/producer.log"
grep -q "WP6 symbolic admission contract passed" \
  "$artifact_dir/wp6-admission.log"
grep -q "exact=1/1/1 mismatch=0/0/0" \
  "$artifact_dir/wp6-order-AB.normalized"
grep -Eq "logical_shape = \[1,4,L@pu[0-9a-f]{8},8\]" "$trace"
grep -Eq "logical_shape = \[1,4,L@pu[0-9a-f]{8}\+1,8\]" "$trace"
grep -Fq "logical_shape = [1,4,?,8]" "$trace"
grep -Fq "logical_shape = [1,4,<pending>,8]" "$trace"
grep -q "transformer.attention.v2" "$trace"
grep -q "common.relu.v2" "$trace"

cat >"$artifact_dir/certification.txt" <<EOF
per_pu_symbol_identity=passed
symbol_plus_constant_expression=passed
anonymous_runtime_dynamic=passed
anonymous_multi_operand_equality=fail_closed
symbolic_tensor_type_uniquing=passed
pending_strict_gate=passed
llama_decode_attention_v2=passed
ir_b2a_st_src=passed
binary_layout_change=none
EOF

echo "SP8 symbolic shape fixture passed"
echo "review artifacts: $artifact_dir"
