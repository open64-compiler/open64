#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO4_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO4_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio4_lifetime_locality}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO4_TEST must name the linked AIO-4 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/lifetime_locality.before.B"
after_b="$artifact_dir/lifetime_locality.after.B"
repeat_b="$artifact_dir/lifetime_locality.repeat.B"
before_t="$artifact_dir/lifetime_locality.before.T"
after_t="$artifact_dir/lifetime_locality.after.T"
repeat_t="$artifact_dir/lifetime_locality.repeat.T"
analysis="$artifact_dir/lifetime_locality.txt"
repeat_analysis="$artifact_dir/lifetime_locality.repeat.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO4_MODE=before OPEN64_AIO4_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO4_MODE=after OPEN64_AIO4_ARTIFACT="$after_b" \
  OPEN64_AIO4_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=after OPEN64_AIO4_ARTIFACT="$repeat_b" \
  OPEN64_AIO4_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=branch "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=loop "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=region "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=effect "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=alias "$producer" >> "$validation" 2>&1
OPEN64_AIO4_MODE=scope "$producer" >> "$validation" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "DSLTensorControlSnapshot:.*aio4_lifetime_locality" "$analysis"
grep -q "DSLTensorLocality:.*aio4_lifetime_locality.*facts=5 uses=6" "$analysis"
grep -q "value=3.*size=static bytes=32 lifetime=exact_block" "$analysis"
grep -q "value=3.*distance=exact:1.*access=elementwise residency=high" "$analysis"
grep -q "value=3.*critical=on alias=proven_unique" "$analysis"
grep -q "OPR_DSLADD" "$after_t"
grep -q "OPR_DSLMUL" "$after_t"
if grep -q "DSLTensorLocality" "$after_t"; then
  echo "error: runtime-only AIO-4 analysis leaked into binary WHIRL" >&2
  exit 1
fi

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO4_MODE=before OPEN64_AIO4_ARTIFACT=$before_b $producer
OPEN64_AIO4_MODE=after OPEN64_AIO4_ARTIFACT=$after_b OPEN64_AIO4_ANALYSIS=$analysis $producer
OPEN64_AIO4_MODE=after OPEN64_AIO4_ARTIFACT=$repeat_b OPEN64_AIO4_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO4_MODE=branch $producer
OPEN64_AIO4_MODE=loop $producer
OPEN64_AIO4_MODE=region $producer
OPEN64_AIO4_MODE=effect $producer
OPEN64_AIO4_MODE=alias $producer
OPEN64_AIO4_MODE=scope $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-4 tensor lifetime and locality contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review analysis: $analysis"
echo "review diagnostics: $validation"
