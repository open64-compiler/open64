#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP2_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp2-interner}"
image="$artifact_dir/tensor_interner.B"
trace="$artifact_dir/tensor_interner.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_SHAPE_SP2_ONLY=1 \
OPEN64_DSL_SHAPE_SP2_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace"

grep -q "canonical tensor interner mapped-image contract passed" \
  "$artifact_dir/producer.log"
grep -q "tensor_interner_f32_2x2" "$trace"
grep -q "logical_shape = \[2,2\]" "$trace"

cat >"$artifact_dir/certification.txt" <<EOF
lookup_before_create=passed
immutable_refinement=passed
mapped_index_rebuild=passed
mapped_ty_reuse_without_growth=passed
ir_b2a_st_src=passed
binary_layout_change=none
EOF

echo "SP2 canonical tensor interner fixture passed"
echo "review artifacts: $artifact_dir"
