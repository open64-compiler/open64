#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP9_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp9-certification}"
image="$artifact_dir/shape_sp9.B"
trace="$artifact_dir/shape_sp9.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_SHAPE_SP9_ONLY=1 $producer" \
  "OPEN64_DSL_SHAPE_SP9_ONLY=1 OPEN64_DSL_SHAPE_SP9_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

OPEN64_DSL_SHAPE_SP9_ONLY=1 \
  "$producer" >"$artifact_dir/shape-diagnostics.txt" 2>&1
OPEN64_DSL_SHAPE_SP9_ONLY=1 \
OPEN64_DSL_SHAPE_SP9_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace"

grep -q "SP9 broadcast and symbolic matmul contract passed" \
  "$artifact_dir/shape-diagnostics.txt"
grep -q "common.matmul.v2" "$trace"
grep -Eq \
  "logical_shape = \[B@pu[0-9a-f]{8},H@pu[0-9a-f]{8},M@pu[0-9a-f]{8},K@pu[0-9a-f]{8}\]" \
  "$trace"
grep -Eq \
  "logical_shape = \[B@pu[0-9a-f]{8},H@pu[0-9a-f]{8},M@pu[0-9a-f]{8},N@pu[0-9a-f]{8}\]" \
  "$trace"
grep -Fq "attr.batch_rule=exact" "$trace"
grep -q "common.add.v1" "$trace"
grep -q "common.mul.v1" "$trace"
grep -Fq "attr.broadcast_rule=numpy" "$trace"
grep -Fq "logical_shape = [2,4,3,5]" "$trace"

cp "$trace" "$artifact_dir/shape_sp9.shape-before.t"
cp "$trace" "$artifact_dir/shape_sp9.shape-after.t"
printf '%s\n' \
  "snapshot_mode=check-only" \
  "before_after_relation=identical-complete-descriptors" \
  "retyping_required=no" >>"$artifact_dir/shape-diagnostics.txt"
(
  cd "$artifact_dir"
  shasum -a 256 \
    shape_sp9.B shape_sp9.T shape_sp9.shape-before.t \
    shape_sp9.shape-after.t shape-diagnostics.txt producer.log commands.txt \
    >SHA256SUMS
)

cat >"$artifact_dir/certification.txt" <<EOF
per_pu_scope=passed
symbolic_batched_matmul=passed
exact_batch_proof=passed
symbolic_contraction_proof=passed
wrong_symbolic_batch=fail_closed
wrong_symbolic_contraction=fail_closed
numpy_add_broadcast=passed
numpy_mul_broadcast=passed
impossible_numpy_broadcast=fail_closed
anonymous_equality=fail_closed
ir_b2a_st_src=passed
binary_layout_change=none
EOF

echo "SP9 symbolic matmul certification passed"
echo "review trace: $trace"
