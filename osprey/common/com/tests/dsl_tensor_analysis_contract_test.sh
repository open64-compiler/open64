#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO3_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO3_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio3_semantic_tensor}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO3_TEST must name the linked AIO-3 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/semantic_tensor.before.B"
after_b="$artifact_dir/semantic_tensor.after.B"
repeat_b="$artifact_dir/semantic_tensor.repeat.B"
before_t="$artifact_dir/semantic_tensor.before.T"
after_t="$artifact_dir/semantic_tensor.after.T"
repeat_t="$artifact_dir/semantic_tensor.repeat.T"
analysis="$artifact_dir/semantic_tensor_analysis.txt"
repeat_analysis="$artifact_dir/semantic_tensor_analysis.repeat.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO3_MODE=before OPEN64_AIO3_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO3_MODE=after OPEN64_AIO3_ARTIFACT="$after_b" \
  OPEN64_AIO3_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO3_MODE=after OPEN64_AIO3_ARTIFACT="$repeat_b" \
  OPEN64_AIO3_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO3_MODE=metadata "$producer" >> "$validation" 2>&1
OPEN64_AIO3_MODE=contract "$producer" >> "$validation" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "owner=<.*aio3_common_matmul.*facts=3 uses=2 complete=yes" "$analysis"
grep -q "consumer=common.matmul.v1.*kid0 role=contraction_kid0" "$analysis"
grep -q "consumer=common.matmul.v1.*kid1 role=contraction_kid1" "$analysis"
grep -q "owner=<.*aio3_resnet_conv2d.*facts=4 uses=3 complete=yes" "$analysis"
grep -q "consumer=cnn.conv2d.v2.*kid1 role=weight" "$analysis"
grep -q "owner=<.*aio3_llama_rms_norm.*facts=3 uses=2 complete=yes" "$analysis"
grep -q "consumer=transformer.rms_norm.v1.*kid0 role=activation" "$analysis"
grep -q "OPR_DSLMATMUL" "$after_t"
grep -q "OPR_DSLCONV2D" "$after_t"
grep -q "OPR_DSLRMSNORM" "$after_t"
if grep -q "DSLTensorAnalysis" "$after_t"; then
  echo "error: runtime-only AIO-3 analysis leaked into binary WHIRL" >&2
  exit 1
fi

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO3_MODE=before OPEN64_AIO3_ARTIFACT=$before_b $producer
OPEN64_AIO3_MODE=after OPEN64_AIO3_ARTIFACT=$after_b OPEN64_AIO3_ANALYSIS=$analysis $producer
OPEN64_AIO3_MODE=after OPEN64_AIO3_ARTIFACT=$repeat_b OPEN64_AIO3_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO3_MODE=metadata $producer
OPEN64_AIO3_MODE=contract $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-3 semantic tensor analysis contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review analysis: $analysis"
echo "review diagnostics: $validation"
