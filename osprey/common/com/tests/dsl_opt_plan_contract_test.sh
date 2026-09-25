#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO2_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO2_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio2_opt_plan}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO2_TEST must name the linked AIO-2 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/common_matmul.before.B"
after_b="$artifact_dir/common_matmul.after.B"
repeat_b="$artifact_dir/common_matmul.repeat.B"
before_t="$artifact_dir/common_matmul.before.T"
after_t="$artifact_dir/common_matmul.after.T"
repeat_t="$artifact_dir/common_matmul.repeat.T"
graph="$artifact_dir/tensor_evolution.txt"
repeat_graph="$artifact_dir/tensor_evolution.repeat.txt"
plan="$artifact_dir/optimization_plan.txt"
repeat_plan="$artifact_dir/optimization_plan.repeat.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO2_MODE=before OPEN64_AIO2_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO2_MODE=after OPEN64_AIO2_ARTIFACT="$after_b" \
  OPEN64_AIO2_GRAPH="$graph" OPEN64_AIO2_PLAN="$plan" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO2_MODE=after OPEN64_AIO2_ARTIFACT="$repeat_b" \
  OPEN64_AIO2_GRAPH="$repeat_graph" OPEN64_AIO2_PLAN="$repeat_plan" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO2_MODE=contract "$producer" >> "$validation" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$graph" "$repeat_graph"
cmp "$plan" "$repeat_plan"

grep -q "nodes=3 edges=0" "$graph"
grep -q "candidate\[1\] kind=baseline" "$plan"
grep -q "candidate\[2\] kind=tile" "$plan"
grep -q "cost\[2\].*complete=false total=<unknown>" "$plan"
grep -q "plan\[1\].*state=selected" "$plan"
grep -q "plan\[2\].*state=cost_incomplete" "$plan"
grep -q "selected_plan=1.*complete=1 incomplete=1" "$plan"
grep -q "OPR_DSLMATMUL" "$after_t"
if grep -q "OptimizationPlanIR" "$after_t"; then
  echo "error: runtime-only AIO-2 plan leaked into binary WHIRL" >&2
  exit 1
fi

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO2_MODE=before OPEN64_AIO2_ARTIFACT=$before_b $producer
OPEN64_AIO2_MODE=after OPEN64_AIO2_ARTIFACT=$after_b OPEN64_AIO2_GRAPH=$graph OPEN64_AIO2_PLAN=$plan $producer
OPEN64_AIO2_MODE=after OPEN64_AIO2_ARTIFACT=$repeat_b OPEN64_AIO2_GRAPH=$repeat_graph OPEN64_AIO2_PLAN=$repeat_plan $producer
OPEN64_AIO2_MODE=contract $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-2 optimization plan contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review graph: $graph"
echo "review plan: $plan"
echo "review diagnostics: $validation"
