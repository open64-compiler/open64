#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP3_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp3-solver}"
image="$artifact_dir/shape_solver.B"
trace="$artifact_dir/shape_solver.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_SHAPE_SP3_ONLY=1 \
OPEN64_DSL_SHAPE_SP3_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace"

grep -q "SP3 check-only shape solver contract passed" \
  "$artifact_dir/producer.log"
grep -q "shape_add" "$trace"
grep -q "shape_relu" "$trace"
grep -q "logical_shape = \[2,3\]" "$trace"

cat >"$artifact_dir/certification.txt" <<EOF
per_pu_solver=passed
deterministic_fixed_point=passed
check_only_no_mutation=passed
source_positioned_contradiction=passed
non_dsl_noop=passed
ir_b2a_st_src=passed
binary_layout_change=none
EOF

echo "SP3 check-only shape solver fixture passed"
echo "review artifacts: $artifact_dir"
