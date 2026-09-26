#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO10_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO10_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio10_pipeline}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO10_TEST must name the linked AIO-10 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/matmul.before.B"
before_t="$artifact_dir/matmul.before.T"
stage_b="$artifact_dir/matmul.G12.B"
stage_t="$artifact_dir/matmul.G12.T"
stage_analysis="$artifact_dir/matmul.G12.analysis.txt"
repeat_b="$artifact_dir/matmul.G12.repeat.B"
repeat_t="$artifact_dir/matmul.G12.repeat.T"
repeat_analysis="$artifact_dir/matmul.G12.repeat.analysis.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO10_MODE=before OPEN64_AIO10_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
"$ir_b2a" -st -src "$before_b" "$before_t"

OPEN64_AIO10_MODE=pipeline OPEN64_AIO10_ARTIFACT="$stage_b" \
  OPEN64_AIO10_ANALYSIS="$stage_analysis" \
  "$producer" >> "$validation" 2>&1
"$ir_b2a" -st -src "$stage_b" "$stage_t"
cmp "$before_b" "$stage_b"
cmp "$before_t" "$stage_t"

OPEN64_AIO10_MODE=pipeline OPEN64_AIO10_ARTIFACT="$repeat_b" \
  OPEN64_AIO10_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"
cmp "$before_b" "$repeat_b"
cmp "$before_t" "$repeat_t"
cmp "$stage_analysis" "$repeat_analysis"

hopper="$artifact_dir/hopper.G12.analysis.txt"
blackwell="$artifact_dir/blackwell.G12.analysis.txt"
cpu="$artifact_dir/cpu.G12.analysis.txt"
unsafe="$artifact_dir/unsafe-distance.G12.analysis.txt"
OPEN64_AIO10_MODE=hopper OPEN64_AIO10_ANALYSIS="$hopper" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO10_MODE=blackwell OPEN64_AIO10_ANALYSIS="$blackwell" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO10_MODE=cpu OPEN64_AIO10_ANALYSIS="$cpu" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO10_MODE=unsafe OPEN64_AIO10_ANALYSIS="$unsafe" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO10_MODE=control "$producer" >> "$validation" 2>&1

grep -q "CommonFetchPlanIR/CommonPipelineIR:.*target=nvidia_hopper.*stage=G12.*sites=1.*plans=4.*fetches=8.*stages=7.*apply=no" "$stage_analysis"
grep -q "engine=demand.*buffers=1" "$stage_analysis"
grep -q "engine=vector.*buffers=1" "$stage_analysis"
grep -q "engine=async_copy.*buffers=2.*distance=1" "$stage_analysis"
grep -q "engine=tma_like.*buffers=3.*distance=1" "$stage_analysis"
grep -q "movement raw=.*hidden=.*unhidden=" "$stage_analysis"
grep -q "kind=staged_buffer" "$stage_analysis"
grep -q "target=nvidia_blackwell.*plans=4" "$blackwell"
grep -q "movement\[4\].*engine=tma_like.*transaction=256.*stages=4" "$blackwell"
grep -q "target=cpu_baseline.*plans=1.*fetches=2.*stages=1" "$cpu"
if grep -q "engine=async_copy\|engine=tma_like" "$cpu"; then
  echo "error: CPU fallback contains unsupported async movement" >&2
  exit 1
fi
grep -q "engine=async_copy.*distance=4.*legality=rejected reason=resource" "$unsafe"
grep -q "engine=tma_like.*distance=4.*legality=rejected reason=resource" "$unsafe"
grep -q "engine=vector.*legality=proven" "$unsafe"
grep -q "OPR_DSLMATMUL" "$stage_t"
grep -q "OPR_DSLRELU" "$stage_t"
if grep -q "CommonFetchPlanIR\|CommonPipelineIR" "$stage_t"; then
  echo "error: runtime-only AIO-10 plan leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$artifact_dir"/*.B "$artifact_dir"/*.T \
  "$artifact_dir"/*.analysis.txt > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO10_MODE=before OPEN64_AIO10_ARTIFACT=$before_b $producer
OPEN64_AIO10_MODE=pipeline OPEN64_AIO10_ARTIFACT=$stage_b OPEN64_AIO10_ANALYSIS=$stage_analysis $producer
$ir_b2a -st -src $stage_b $stage_t
OPEN64_AIO10_MODE=hopper OPEN64_AIO10_ANALYSIS=$hopper $producer
OPEN64_AIO10_MODE=blackwell OPEN64_AIO10_ANALYSIS=$blackwell $producer
OPEN64_AIO10_MODE=unsafe OPEN64_AIO10_ANALYSIS=$unsafe $producer
EOF

echo "AIO-10 fetch/pipeline contract passed"
echo "review WHIRL trace: $stage_t"
echo "review Hopper analysis: $hopper"
echo "review Blackwell analysis: $blackwell"
echo "review unsafe-distance analysis: $unsafe"
echo "review diagnostics: $validation"
