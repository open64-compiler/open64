#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO12_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO12_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio12_runtime_variant}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO12_TEST must name the linked AIO-12 producer" >&2
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
variant_b="$artifact_dir/matmul.G15.B"
variant_t="$artifact_dir/matmul.G15.T"
repeat_b="$artifact_dir/matmul.G15.repeat.B"
repeat_t="$artifact_dir/matmul.G15.repeat.T"
analysis="$artifact_dir/matmul.G15.runtime-variants.txt"
repeat_analysis="$artifact_dir/matmul.G15.repeat.runtime-variants.txt"
control="$artifact_dir/runtime-variant-control.G15.log"
validation="$artifact_dir/validation.log"

OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO11_MODE=runtime-variant OPEN64_AIO12_ARTIFACT="$variant_b" \
  OPEN64_AIO12_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=runtime-variant OPEN64_AIO12_ARTIFACT="$repeat_b" \
  OPEN64_AIO12_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=runtime-variant-control \
  "$producer" > "$control" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$variant_b" "$variant_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$variant_b"
cmp "$before_t" "$variant_t"
cmp "$variant_b" "$repeat_b"
cmp "$variant_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"

grep -q "RuntimeVariantIR:.*sites=1.*variants=2.*guards=2" "$analysis"
grep -q "VHORuntimeVariantDecision:.*stage=G15.*sites=1.*variants=2.*guards=2.*apply=no" "$analysis"
grep -q "provider=open64_direct.*guards=0.*state=fallback" "$analysis"
grep -q "provider=nvidia_cublaslt.*guards=2.*guard_cost=4.*state=selected" "$analysis"
grep -q "kind=operand_alignment.*operand=kid0.*required=16.*cost=2" "$analysis"
grep -q "kind=operand_alignment.*operand=kid1.*required=16.*cost=2" "$analysis"
grep -q "runtime_selection=4" "$analysis"
grep -q "label=alignment_guard_true.*guard=true.*fallback=no" "$analysis"
grep -q "label=alignment_guard_false.*guard=false.*fallback=yes" "$analysis"
grep -q "AIO-12 runtime variant control contract passed" "$control"
if grep -Eq "RuntimeVariant|runtime-variant|nvidia_cublaslt|operand_alignment" "$variant_t"; then
  echo "error: AIO-12 runtime-only analysis leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$before_t" "$variant_b" "$variant_t" \
  "$repeat_b" "$repeat_t" "$analysis" "$repeat_analysis" \
  "$control" > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT=$before_b $producer
OPEN64_AIO11_MODE=runtime-variant OPEN64_AIO12_ARTIFACT=$variant_b OPEN64_AIO12_ANALYSIS=$analysis $producer
OPEN64_AIO11_MODE=runtime-variant OPEN64_AIO12_ARTIFACT=$repeat_b OPEN64_AIO12_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO11_MODE=runtime-variant-control $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $variant_b $variant_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-12 runtime variant contract passed"
echo "review unchanged WHIRL trace: $variant_t"
echo "review runtime variants and true/false paths: $analysis"
echo "review control diagnostics: $control"
echo "review validation: $validation"
