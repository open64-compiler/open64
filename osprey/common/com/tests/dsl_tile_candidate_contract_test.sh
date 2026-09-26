#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO9_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO9_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio9_tiling}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO9_TEST must name the linked AIO-9 producer" >&2
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
validation="$artifact_dir/validation.log"

OPEN64_AIO9_MODE=before OPEN64_AIO9_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
"$ir_b2a" -st -src "$before_b" "$before_t"

for phase in $(seq 0 11); do
  stage_b="$artifact_dir/matmul.G${phase}.B"
  stage_t="$artifact_dir/matmul.G${phase}.T"
  stage_analysis="$artifact_dir/matmul.G${phase}.analysis.txt"
  OPEN64_AIO9_MODE=stage OPEN64_AIO9_PHASE="$phase" \
    OPEN64_AIO9_ARTIFACT="$stage_b" \
    OPEN64_AIO9_ANALYSIS="$stage_analysis" \
    "$producer" >> "$validation" 2>&1
  "$ir_b2a" -st -src "$stage_b" "$stage_t"
  cmp "$before_b" "$stage_b"
  cmp "$before_t" "$stage_t"
  grep -q "CommonTilePlanIR:.*target=nvidia_hopper.*stage=G${phase}.*sites=1.*apply=no" \
    "$stage_analysis"
  grep -q "tile-stage.*stage=G0.*phase=P7.0" "$stage_analysis"
done

repeat_b="$artifact_dir/matmul.G11.repeat.B"
repeat_t="$artifact_dir/matmul.G11.repeat.T"
repeat_analysis="$artifact_dir/matmul.G11.repeat.analysis.txt"
OPEN64_AIO9_MODE=stage OPEN64_AIO9_PHASE=11 \
  OPEN64_AIO9_ARTIFACT="$repeat_b" \
  OPEN64_AIO9_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"
cmp "$before_b" "$repeat_b"
cmp "$before_t" "$repeat_t"
cmp "$artifact_dir/matmul.G11.analysis.txt" "$repeat_analysis"

hopper="$artifact_dir/hopper.G11.analysis.txt"
blackwell="$artifact_dir/blackwell.G11.analysis.txt"
effect="$artifact_dir/effect.G11.analysis.txt"
OPEN64_AIO9_MODE=hopper OPEN64_AIO9_ANALYSIS="$hopper" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO9_MODE=blackwell OPEN64_AIO9_ANALYSIS="$blackwell" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO9_MODE=effect OPEN64_AIO9_ANALYSIS="$effect" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO9_MODE=control "$producer" >> "$validation" 2>&1

grep -q "target=nvidia_hopper.*plans=3.*stages=25" "$hopper"
grep -q "family=cuda_64" "$hopper"
grep -q "family=cuda_128" "$hopper"
if grep -q "family=blackwell_wide" "$hopper"; then
  echo "error: Hopper plan contains Blackwell-only tile family" >&2
  exit 1
fi
grep -q "target=nvidia_blackwell.*plans=4.*stages=37" "$blackwell"
grep -q "family=blackwell_wide" "$blackwell"
grep -q "cta=\[128,256,32\]" "$blackwell"
grep -q "legality=rejected reason=effect" "$effect"
grep -q "tile-stage.*stage=G11.*phase=P7.11.*level=instruction" "$hopper"
grep -q "OPR_DSLMATMUL" "$before_t"
grep -q "OPR_DSLRELU" "$before_t"
if grep -q "CommonTilePlanIR" "$before_t"; then
  echo "error: runtime-only AIO-9 plan leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$artifact_dir"/*.B "$artifact_dir"/*.T \
  "$artifact_dir"/*.analysis.txt > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO9_MODE=before OPEN64_AIO9_ARTIFACT=$before_b $producer
OPEN64_AIO9_MODE=stage OPEN64_AIO9_PHASE=<0..11> OPEN64_AIO9_ARTIFACT=matmul.G*.B OPEN64_AIO9_ANALYSIS=matmul.G*.analysis.txt $producer
$ir_b2a -st -src matmul.G*.B matmul.G*.T
OPEN64_AIO9_MODE=hopper OPEN64_AIO9_ANALYSIS=$hopper $producer
OPEN64_AIO9_MODE=blackwell OPEN64_AIO9_ANALYSIS=$blackwell $producer
EOF

echo "AIO-9 hierarchical tile-plan contract passed"
echo "review WHIRL trace: $artifact_dir/matmul.G11.T"
echo "review Hopper analysis: $hopper"
echo "review Blackwell analysis: $blackwell"
echo "review diagnostics: $validation"
