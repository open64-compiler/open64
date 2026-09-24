#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_dir="${OPEN64_BUILD_DIR:-$repo_root/build}"
producer="${OPEN64_AIO1_TEST:-$build_dir/osprey/targdir/ir_tools/dsl_tensor_evolution_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$build_dir/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_AIO1_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio1_tensor_evolution}"
before_image="$artifact_dir/common_matmul.before.B"
after_image="$artifact_dir/common_matmul.after.B"
before_trace="$artifact_dir/common_matmul.before.T"
after_trace="$artifact_dir/common_matmul.after.T"
graph_trace="$artifact_dir/tensor_evolution.txt"
validation_log="$artifact_dir/validation.log"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

{
  echo "OPEN64_AIO1_MODE=before OPEN64_AIO1_ARTIFACT=$before_image $producer"
  echo "OPEN64_AIO1_MODE=after OPEN64_AIO1_ARTIFACT=$after_image OPEN64_AIO1_GRAPH=$graph_trace $producer"
  echo "OPEN64_AIO1_MODE=contract $producer"
  echo "$ir_b2a -st -src $before_image $before_trace"
  echo "$ir_b2a -st -src $after_image $after_trace"
} > "$artifact_dir/commands.txt"

OPEN64_AIO1_MODE=before \
OPEN64_AIO1_ARTIFACT="$before_image" \
  "$producer" > "$validation_log" 2>&1
OPEN64_AIO1_MODE=after \
OPEN64_AIO1_ARTIFACT="$after_image" \
OPEN64_AIO1_GRAPH="$graph_trace" \
  "$producer" >> "$validation_log" 2>&1
OPEN64_AIO1_MODE=contract \
  "$producer" >> "$validation_log" 2>&1

"$ir_b2a" -st -src "$before_image" "$before_trace"
"$ir_b2a" -st -src "$after_image" "$after_trace"

cmp "$before_image" "$after_image"
cmp "$before_trace" "$after_trace"

for evidence in \
  'TensorEvolutionGraph: owner=' \
  'nodes=3 edges=0' \
  'kind=semantic value=1 name=kid0' \
  'dtype=float32 shape=[2,3]' \
  'kind=semantic value=2 name=kid1' \
  'dtype=float32 shape=[3,4]' \
  'kind=semantic value=3 name=matmul_result' \
  'dtype=float32 shape=[2,4]'; do
  if ! grep -Fq "$evidence" "$graph_trace"; then
    echo "missing AIO-1 evidence '$evidence' in $graph_trace" >&2
    exit 1
  fi
done

if ! grep -Fq 'OPR_DSLMATMUL # OPR_DSLMATMUL version=1' "$after_trace"; then
  echo "missing common.matmul evidence in $after_trace" >&2
  exit 1
fi
if grep -Fq 'TensorEvolutionGraph' "$after_trace"; then
  echo "in-memory AIO-1 graph leaked into binary WHIRL" >&2
  exit 1
fi

echo "AIO-1 TensorEvolutionGraph contract passed" | tee -a "$validation_log"
echo "review before trace: $before_trace"
echo "review after trace: $after_trace"
echo "review graph: $graph_trace"
echo "review diagnostics: $validation_log"
