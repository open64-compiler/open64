#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO8_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO8_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio8_residency}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO8_TEST must name the linked AIO-8 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/residency.before.B"
after_b="$artifact_dir/residency.after.B"
repeat_b="$artifact_dir/residency.repeat.B"
before_t="$artifact_dir/residency.before.T"
after_t="$artifact_dir/residency.after.T"
repeat_t="$artifact_dir/residency.repeat.T"
analysis="$artifact_dir/residency.txt"
repeat_analysis="$artifact_dir/residency.repeat.txt"
validation="$artifact_dir/validation.log"
hopper_capacity="$artifact_dir/hopper-capacity.txt"
blackwell_capacity="$artifact_dir/blackwell-capacity.txt"
region_lifetime="$artifact_dir/region-lifetime.txt"

OPEN64_AIO8_MODE=before OPEN64_AIO8_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO8_MODE=after OPEN64_AIO8_ARTIFACT="$after_b" \
  OPEN64_AIO8_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=after OPEN64_AIO8_ARTIFACT="$repeat_b" \
  OPEN64_AIO8_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=cpu "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=hopper_capacity OPEN64_AIO8_ANALYSIS="$hopper_capacity" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=blackwell_capacity \
  OPEN64_AIO8_ANALYSIS="$blackwell_capacity" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=effect "$producer" >> "$validation" 2>&1
OPEN64_AIO8_MODE=region OPEN64_AIO8_ANALYSIS="$region_lifetime" \
  "$producer" >> "$validation" 2>&1
for mode in symbolic control; do
  OPEN64_AIO8_MODE="$mode" "$producer" >> "$validation" 2>&1
done

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$after_b" "$after_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$after_b"
cmp "$before_t" "$after_t"
cmp "$after_b" "$repeat_b"
cmp "$after_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "CommonMemoryResidencyIR:.*profile=nvidia_hopper.*sites=1.*descriptors=6.*alternatives=6.*select=yes.*apply=no" "$analysis"
grep -q "MemoryHierarchyDescriptorIR: profile=2 name=nvidia_hopper tiers=6" "$analysis"
grep -q "tier=hbm.*legality=proven reason=none" "$analysis"
grep -q "tier=shared.*legality=proven reason=none" "$analysis"
grep -q "tier=register.*legality=proven reason=none" "$analysis"
grep -q "tier=system.*legality=unknown reason=incomplete_analysis" "$analysis"
grep -q "selected=7" "$analysis"
grep -q "kind=local_physical" "$analysis"
grep -q "transform=local_layout" "$analysis"
grep -q "profile=nvidia_hopper" "$hopper_capacity"
grep -q "tier=l2.*required=67108864.*capacity=52428800.*legality=rejected reason=resource" "$hopper_capacity"
grep -q "profile=nvidia_blackwell" "$blackwell_capacity"
grep -q "tier=l2.*required=67108864.*capacity=132120576.*legality=proven reason=none" "$blackwell_capacity"
grep -q "tier=register.*legality=rejected reason=descriptor" "$region_lifetime"
grep -q "OPR_DSLMATMUL" "$after_t"
grep -q "OPR_DSLRELU" "$after_t"
if grep -q "CommonMemoryResidencyIR" "$after_t"; then
  echo "error: runtime-only AIO-8 analysis leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$after_b" "$repeat_b" \
  "$before_t" "$after_t" "$repeat_t" \
  "$analysis" "$repeat_analysis" > "$artifact_dir/SHA256SUMS"
sha256sum "$hopper_capacity" "$blackwell_capacity" "$region_lifetime" \
  >> "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO8_MODE=before OPEN64_AIO8_ARTIFACT=$before_b $producer
OPEN64_AIO8_MODE=after OPEN64_AIO8_ARTIFACT=$after_b OPEN64_AIO8_ANALYSIS=$analysis $producer
OPEN64_AIO8_MODE=after OPEN64_AIO8_ARTIFACT=$repeat_b OPEN64_AIO8_ANALYSIS=$repeat_analysis $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $after_b $after_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-8 residency candidate contract passed"
echo "review before trace: $before_t"
echo "review after trace: $after_t"
echo "review analysis: $analysis"
echo "review diagnostics: $validation"
