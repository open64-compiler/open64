#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO11_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO11_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio11_physical_plan}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO11_TEST must name the linked AIO-11 producer" >&2
  exit 1
fi
if [[ -z "$ir_b2a" || ! -x "$ir_b2a" ]]; then
  echo "error: OPEN64_IR_B2A must name ir_b2a" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

before_b="$artifact_dir/matmul.before.B"
stage_b="$artifact_dir/matmul.G13.B"
repeat_b="$artifact_dir/matmul.G13.repeat.B"
before_t="$artifact_dir/matmul.before.T"
stage_t="$artifact_dir/matmul.G13.T"
repeat_t="$artifact_dir/matmul.G13.repeat.T"
analysis="$artifact_dir/matmul.G13.analysis.txt"
repeat_analysis="$artifact_dir/matmul.G13.repeat.analysis.txt"
lower_b="$artifact_dir/matmul.G14.B"
lower_repeat_b="$artifact_dir/matmul.G14.repeat.B"
fallback_b="$artifact_dir/matmul.G14.fallback.B"
lower_t="$artifact_dir/matmul.G14.T"
lower_repeat_t="$artifact_dir/matmul.G14.repeat.T"
fallback_t="$artifact_dir/matmul.G14.fallback.T"
lower_diff="$artifact_dir/matmul.before-to-G14.diff"
reject="$artifact_dir/unsupported-provider.G14.log"
rollback="$artifact_dir/late-failure-rollback.G14.log"
o0="$artifact_dir/o0-baseline.G13.analysis.txt"
generated="$artifact_dir/generated-kernel.G13.analysis.txt"
unavailable="$artifact_dir/unavailable-provider.G13.analysis.txt"
mismatch="$artifact_dir/provider-mismatch.G13.analysis.txt"
validation="$artifact_dir/validation.log"

OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO11_MODE=select OPEN64_AIO11_ARTIFACT="$stage_b" \
  OPEN64_AIO11_ANALYSIS="$analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=select OPEN64_AIO11_ARTIFACT="$repeat_b" \
  OPEN64_AIO11_ANALYSIS="$repeat_analysis" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=o0 OPEN64_AIO11_ANALYSIS="$o0" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=generated OPEN64_AIO11_ANALYSIS="$generated" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=unavailable OPEN64_AIO11_ANALYSIS="$unavailable" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=mismatch OPEN64_AIO11_ANALYSIS="$mismatch" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=control "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=lower-provider OPEN64_AIO11_ARTIFACT="$lower_b" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=lower-provider OPEN64_AIO11_ARTIFACT="$lower_repeat_b" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=lower-fallback OPEN64_AIO11_ARTIFACT="$fallback_b" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=lower-reject "$producer" > "$reject" 2>&1
OPEN64_AIO11_MODE=lower-rollback "$producer" > "$rollback" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$stage_b" "$stage_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"
"$ir_b2a" -st -src "$lower_b" "$lower_t"
"$ir_b2a" -st -src "$lower_repeat_b" "$lower_repeat_t"
"$ir_b2a" -st -src "$fallback_b" "$fallback_t"

cmp "$before_b" "$stage_b"
cmp "$before_t" "$stage_t"
cmp "$stage_b" "$repeat_b"
cmp "$stage_t" "$repeat_t"
cmp "$analysis" "$repeat_analysis"
cmp "$lower_b" "$lower_repeat_b"
cmp "$lower_t" "$lower_repeat_t"
diff -u "$before_t" "$lower_t" > "$lower_diff" || true

grep -q "CommonPhysicalPlanIR:.*stage=G13.*implementations=4.*opt_level=3.*apply=no" "$analysis"
grep -q "provider=nvidia_cublaslt.*state=selected" "$analysis"
grep -q "provider=open64_direct.*fallback=0" "$analysis"
grep -q "provider=open64_generated.*schedule=tiled_pipeline" "$analysis"
grep -q "OPR_DSLMATMUL" "$stage_t"
grep -q "opt_level=0.*apply=no" "$o0"
grep -q "provider=open64_direct.*state=selected" "$o0"
grep -q "provider=open64_generated.*state=selected" "$generated"
grep -q "provider=nvidia_cublaslt.*reason=provider_unavailable" "$unavailable"
grep -q "provider=open64_direct.*state=selected" "$unavailable"
grep -q "provider=nvidia_cudnn.*reason=provider_mismatch" "$mismatch"
grep -q "provider=open64_direct.*state=selected" "$mismatch"
grep -q "__open64_dsl_matmul_physical_v1" "$lower_t"
grep -q ".dsl_physical_plan" "$lower_t"
grep -A8 ".dsl_physical_plan" "$lower_t" | grep -q "Alignment: 8 bytes"
grep -q "__open64_dsl_matmul_v1" "$fallback_t"
if grep -q "__open64_dsl_matmul_physical_v1" "$fallback_t"; then
  echo "error: unavailable provider did not use direct fallback" >&2
  exit 1
fi
grep -q "unsupported provider rejected before WN mutation" "$reject"
grep -q "late lowering failure rolled back WN mutation" "$rollback"
if grep -Eq "nvidia_cublaslt|triton|existing_ptx|OPR_CALL" "$stage_t"; then
  echo "error: AIO-11 provider selection leaked into source WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$stage_b" "$repeat_b" \
  "$before_t" "$stage_t" "$repeat_t" \
  "$analysis" "$repeat_analysis" "$o0" "$generated" \
  "$unavailable" "$mismatch" "$lower_b" "$lower_repeat_b" \
  "$fallback_b" "$lower_t" "$lower_repeat_t" "$fallback_t" \
  "$lower_diff" "$reject" "$rollback" > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT=$before_b $producer
OPEN64_AIO11_MODE=select OPEN64_AIO11_ARTIFACT=$stage_b OPEN64_AIO11_ANALYSIS=$analysis $producer
OPEN64_AIO11_MODE=select OPEN64_AIO11_ARTIFACT=$repeat_b OPEN64_AIO11_ANALYSIS=$repeat_analysis $producer
OPEN64_AIO11_MODE=o0 OPEN64_AIO11_ANALYSIS=$o0 $producer
OPEN64_AIO11_MODE=generated OPEN64_AIO11_ANALYSIS=$generated $producer
OPEN64_AIO11_MODE=unavailable OPEN64_AIO11_ANALYSIS=$unavailable $producer
OPEN64_AIO11_MODE=mismatch OPEN64_AIO11_ANALYSIS=$mismatch $producer
OPEN64_AIO11_MODE=control $producer
OPEN64_AIO11_MODE=lower-provider OPEN64_AIO11_ARTIFACT=$lower_b $producer
OPEN64_AIO11_MODE=lower-provider OPEN64_AIO11_ARTIFACT=$lower_repeat_b $producer
OPEN64_AIO11_MODE=lower-fallback OPEN64_AIO11_ARTIFACT=$fallback_b $producer
OPEN64_AIO11_MODE=lower-reject $producer
OPEN64_AIO11_MODE=lower-rollback $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $stage_b $stage_t
$ir_b2a -st -src $repeat_b $repeat_t
$ir_b2a -st -src $lower_b $lower_t
$ir_b2a -st -src $lower_repeat_b $lower_repeat_t
$ir_b2a -st -src $fallback_b $fallback_t
EOF

echo "AIO-11 physical-plan selector contract passed"
echo "review WHIRL trace: $stage_t"
echo "review selected plan: $analysis"
echo "review O0 baseline: $o0"
echo "review unavailable provider: $unavailable"
echo "review provider mismatch: $mismatch"
echo "review lowered WHIRL trace: $lower_t"
echo "review direct fallback trace: $fallback_t"
echo "review before/after diff: $lower_diff"
echo "review unsupported-provider rejection: $reject"
echo "review late-failure rollback: $rollback"
echo "review diagnostics: $validation"
