#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO6_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO6_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio6_logical_layout}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO6_TEST must name the linked AIO-6 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/logical_layout.before.B"
after_b="$artifact_dir/logical_layout.after.B"
repeat_b="$artifact_dir/logical_layout.repeat.B"
before_t="$artifact_dir/logical_layout.before.T"
after_t="$artifact_dir/logical_layout.after.T"
repeat_t="$artifact_dir/logical_layout.repeat.T"
analysis="$artifact_dir/logical_layout.txt"
repeat_analysis="$artifact_dir/logical_layout.repeat.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO6_MODE=before OPEN64_AIO6_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO6_MODE=after OPEN64_AIO6_ARTIFACT="$after_b" \
  OPEN64_AIO6_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=after OPEN64_AIO6_ARTIFACT="$repeat_b" \
  OPEN64_AIO6_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=candidate "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=effect "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=region "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=unknown "$producer" >> "$validation" 2>&1
OPEN64_AIO6_MODE=control "$producer" >> "$validation" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "DSLLogicalLayouts:.*descriptors=2.*sites=1.*alternatives=2.*select=yes.*apply=no" "$analysis"
grep -q "descriptor 1 kind=permuted.*axes=\[1,0\]" "$analysis"
grep -q "descriptor 2 kind=blocked.*blocks=\[0:8,1:8\]" "$analysis"
grep -q "compatibility=proven conversion=known bytes=2048" "$analysis"
grep -q "kind=logical_layout" "$analysis"
grep -q "OPR_DSLMATMUL" "$after_t"
grep -q "OPR_DSLRELU" "$after_t"
if grep -q "DSLLogicalLayouts" "$after_t"; then
  echo "error: runtime-only AIO-6 layouts leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$after_b" "$repeat_b" \
  "$before_t" "$after_t" "$repeat_t" \
  "$analysis" "$repeat_analysis" > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO6_MODE=before OPEN64_AIO6_ARTIFACT=$before_b $producer
OPEN64_AIO6_MODE=after OPEN64_AIO6_ARTIFACT=$after_b OPEN64_AIO6_ANALYSIS=$analysis $producer
OPEN64_AIO6_MODE=after OPEN64_AIO6_ARTIFACT=$repeat_b OPEN64_AIO6_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO6_MODE=candidate $producer
OPEN64_AIO6_MODE=effect $producer
OPEN64_AIO6_MODE=region $producer
OPEN64_AIO6_MODE=unknown $producer
OPEN64_AIO6_MODE=control $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-6 logical-layout candidate contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review analysis: $analysis"
echo "review diagnostics: $validation"
