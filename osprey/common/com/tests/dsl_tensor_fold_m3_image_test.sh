#!/usr/bin/env bash
#
# Preserve construction-time tensor folding evidence for human review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_dir="${OPEN64_BUILD_DIR:-$repo_root/build}"
producer_default="$build_dir/osprey/targdir/ir_tools/dsl_builder_contract_test"
producer="${OPEN64_DSL_CONTRACT_TEST:-$producer_default}"
ir_b2a="${OPEN64_IR_B2A:-$build_dir/osprey/targdir/ir_tools/ir_b2a}"
artifact_default="$repo_root/artifacts/m3-tensor-fold"
artifact_dir="${OPEN64_DSL_TENSOR_FOLD_M3_ARTIFACT_DIR:-$artifact_default}"
image="$artifact_dir/tensor_fold_m3.B"
trace="$artifact_dir/tensor_fold_m3.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_TENSOR_FOLD_M3_ONLY=1 \
OPEN64_DSL_TENSOR_FOLD_M3_ARTIFACT="$image" \
  "$producer" > "$artifact_dir/producer.log" 2>&1

"$ir_b2a" -st -src "$image" "$trace"

for evidence in \
  "operator=OPR_DSLADD version=1 operands=[value1,value2]" \
  "name=folded_add;dtype=int32;rank=2;shape=[2,2];value_kind=splat;value=1" \
  "name=folded_mul;dtype=int32;rank=2;shape=[2,2];value_kind=splat;value=12" \
  "name=folded_parent;dtype=int32;rank=2;shape=[2,2];"\
"value_kind=splat;value=13" \
  "name=rejected_add" \
  "name=master_disabled_add" \
  "tensor_fold.origin = OPR_DSLADD" \
  "tensor_fold.origin = OPR_DSLMUL" \
  "source.expression = zero + one" \
  "tensor_tcon storage=splat"; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing M3 tensor-fold evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq "operator=OPR_DSLMUL version=1 operands=[" "$trace"; then
  echo "folded common.mul remained in the DSL node table" >&2
  exit 1
fi

cat > "$artifact_dir/certification.txt" <<EOF
compact_add_fold=passed
compact_mul_fold=passed
bottom_up_parent_fold=passed
stage_disabled_preserves_operator=passed
master_disabled_preserves_operator=passed
non_constant_rejection_preserves_operator=passed
result_symbol_source_metadata=passed
mapped_image_reopen=passed
ir_b2a_st_src=passed
EOF

echo "DSL tensor fold M3 mapped-image fixture passed"
echo "review artifacts: $artifact_dir"
