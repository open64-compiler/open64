#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO5_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO5_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio5_fusion_candidates}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO5_TEST must name the linked AIO-5 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/fusion_candidates.before.B"
after_b="$artifact_dir/fusion_candidates.after.B"
repeat_b="$artifact_dir/fusion_candidates.repeat.B"
before_t="$artifact_dir/fusion_candidates.before.T"
after_t="$artifact_dir/fusion_candidates.after.T"
repeat_t="$artifact_dir/fusion_candidates.repeat.T"
analysis="$artifact_dir/fusion_candidates.txt"
repeat_analysis="$artifact_dir/fusion_candidates.repeat.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO5_MODE=before OPEN64_AIO5_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO5_MODE=after OPEN64_AIO5_ARTIFACT="$after_b" \
  OPEN64_AIO5_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=after OPEN64_AIO5_ARTIFACT="$repeat_b" \
  OPEN64_AIO5_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=candidate "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=traits "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=generic_budget "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=generic_descriptor "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=generic_effect "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=generic_fanout "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=generic_region "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=descriptor "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=effect "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=semantic "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=resource "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=unknown "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=region "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=control "$producer" >> "$validation" 2>&1
OPEN64_AIO5_MODE=scope "$producer" >> "$validation" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "DSLFusionCandidates:.*sites=2.*semantic=yes.*generic=no.*select=yes.*apply=no" "$analysis"
grep -q "DSLFusionCandidates:.*sites=1.*semantic=no.*generic=yes.*select=yes.*apply=no" "$analysis"
grep -q "pattern=matmul_bias_activation.*legality=proven" "$analysis"
grep -q "pattern=matmul_bias_activation.*materializations=2 bytes=32" "$analysis"
grep -q "pattern=residual_activation.*materializations=1 bytes=16" "$analysis"
grep -q "pattern=residual_activation.*selected=2" "$analysis"
grep -q "pattern=generic_cluster.*legality=proven.*layout=unknown" "$analysis"
grep -q "role=generic_contraction" "$analysis"
grep -q "role=generic_pointwise" "$analysis"
grep -q "DSLLogicalLayouts:" "$analysis"
grep -q "kind=alternative_cut" "$analysis"
grep -q "OPR_DSLMATMUL" "$after_t"
grep -q "OPR_DSLRESIDUALADD" "$after_t"
grep -q "OPR_DSLRELU" "$after_t"
if grep -q "DSLFusionCandidates" "$after_t"; then
  echo "error: runtime-only AIO-5 candidates leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$after_b" "$repeat_b" \
  "$before_t" "$after_t" "$repeat_t" \
  "$analysis" "$repeat_analysis" > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO5_MODE=before OPEN64_AIO5_ARTIFACT=$before_b $producer
OPEN64_AIO5_MODE=after OPEN64_AIO5_ARTIFACT=$after_b OPEN64_AIO5_ANALYSIS=$analysis $producer
OPEN64_AIO5_MODE=after OPEN64_AIO5_ARTIFACT=$repeat_b OPEN64_AIO5_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO5_MODE=candidate $producer
OPEN64_AIO5_MODE=traits $producer
OPEN64_AIO5_MODE=generic_budget $producer
OPEN64_AIO5_MODE=generic_descriptor $producer
OPEN64_AIO5_MODE=generic_effect $producer
OPEN64_AIO5_MODE=generic_fanout $producer
OPEN64_AIO5_MODE=generic_region $producer
OPEN64_AIO5_MODE=descriptor $producer
OPEN64_AIO5_MODE=effect $producer
OPEN64_AIO5_MODE=semantic $producer
OPEN64_AIO5_MODE=resource $producer
OPEN64_AIO5_MODE=unknown $producer
OPEN64_AIO5_MODE=region $producer
OPEN64_AIO5_MODE=control $producer
OPEN64_AIO5_MODE=scope $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-5 high-level fusion candidate contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review analysis: $analysis"
echo "review diagnostics: $validation"
