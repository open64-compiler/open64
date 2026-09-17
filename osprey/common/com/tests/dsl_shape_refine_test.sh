#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_SHAPE_REFINE_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_shape_refine_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP5_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp5-refinement}"
image="$artifact_dir/shape_refine.B"
trace="$artifact_dir/shape_refine.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_SHAPE_SP5_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

(cd "$repo_root" && OPEN64_DSL_SHAPE_SP5_ARTIFACT="$image" \
  "$producer") >"$artifact_dir/validation.log" 2>&1
(cd "$repo_root" && "$ir_b2a" -st -src "$image" "$trace") \
  >>"$artifact_dir/validation.log" 2>&1

grep -Fq "SP5 shape refinement contract passed" \
  "$artifact_dir/validation.log"
grep -Fq "shape_add" "$trace"
grep -Fq "shape_relu" "$trace"
grep -Fq "logical_shape = [2,3]" "$trace"
grep -Fq "shape.refine.v1" "$trace"
if grep -Fq "OPR_DSL " "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

cat >"$artifact_dir/certification.txt" <<EOF
admission_then_strict=passed
disabled_check_only_no_mutation=passed
immutable_uniqued_tensor_type=passed
atomic_late_failure_rollback=passed
wn_st_value_retype=passed
region_interface_preserved=passed
ir_b2a_st_src=passed
binary_layout_change=none
EOF

echo "SP5 shape refinement fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review diagnostics: $artifact_dir/validation.log"
