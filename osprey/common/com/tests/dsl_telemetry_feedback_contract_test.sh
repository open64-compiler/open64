#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_AIO13_TEST:-}"
ir_b2a="${OPEN64_IR_B2A:-}"
artifact_dir="${OPEN64_AIO13_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/aio13_telemetry_feedback}"

if [[ -z "$producer" || ! -x "$producer" ]]; then
  echo "error: OPEN64_AIO13_TEST must name the linked AIO-13 producer" >&2
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
feedback_b="$artifact_dir/matmul.G16.B"
feedback_t="$artifact_dir/matmul.G16.T"
repeat_b="$artifact_dir/matmul.G16.repeat.B"
repeat_t="$artifact_dir/matmul.G16.repeat.T"
feedback="$artifact_dir/matmul.G16.telemetry-feedback.txt"
repeat_feedback="$artifact_dir/matmul.G16.repeat.telemetry-feedback.txt"
no_profile="$artifact_dir/no-profile.G16.txt"
negative="$artifact_dir/stale-profile.G16.log"
validation="$artifact_dir/validation.log"

OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT="$before_b" \
  "$producer" > "$validation" 2>&1
OPEN64_AIO11_MODE=telemetry-feedback OPEN64_AIO13_ARTIFACT="$feedback_b" \
  OPEN64_AIO13_ANALYSIS="$feedback" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=telemetry-feedback OPEN64_AIO13_ARTIFACT="$repeat_b" \
  OPEN64_AIO13_ANALYSIS="$repeat_feedback" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=telemetry-no-profile OPEN64_AIO13_ANALYSIS="$no_profile" \
  "$producer" >> "$validation" 2>&1
OPEN64_AIO11_MODE=telemetry-negative \
  "$producer" > "$negative" 2>&1

"$ir_b2a" -st -src "$before_b" "$before_t"
"$ir_b2a" -st -src "$feedback_b" "$feedback_t"
"$ir_b2a" -st -src "$repeat_b" "$repeat_t"

cmp "$before_b" "$feedback_b"
cmp "$before_t" "$feedback_t"
cmp "$feedback_b" "$repeat_b"
cmp "$feedback_t" "$repeat_t"
cmp "$feedback" "$repeat_feedback"

grep -q "TelemetryProfileIR: schema=1 producer=benchmark generation=7" \
  "$feedback"
grep -q "latency_ns=70000.*occupancy_ppm=520000.*memory_read=3276800" \
  "$feedback"
grep -q "guard=70/100.*latency_ns=100000.*communication_ns=10000" \
  "$feedback"
grep -q "cost\[1\].*complete=true total=700.*confidence=high" "$feedback"
grep -q "compute=700 nanoseconds.*evidence=telemetry" "$feedback"
grep -q "cost\[2\].*complete=true total=1000.*confidence=high" "$feedback"
grep -q "compute=1000 nanoseconds.*evidence=telemetry" "$feedback"
grep -q "average_latency_ns=700.*recommended=yes" "$feedback"
grep -q "average_latency_ns=1000.*guard_hit_ppm=700000.*original=yes" \
  "$feedback"
grep -q "original_variant=2.*recommended_variant=1.*profitability_changed=yes.*legality_changed=no" \
  "$feedback"
grep -q "status=no_profile.*sites=0.*variants=0" "$no_profile"
grep -q "malformed, stale, and incomplete profile rejection passed" \
  "$negative"
if grep -Eq "TelemetryProfile|TelemetryFeedback|feedback-variant" "$feedback_t"; then
  echo "error: runtime-only AIO-13 feedback leaked into binary WHIRL" >&2
  exit 1
fi

sha256sum "$before_b" "$before_t" "$feedback_b" "$feedback_t" \
  "$repeat_b" "$repeat_t" "$feedback" "$repeat_feedback" \
  "$no_profile" "$negative" > "$artifact_dir/SHA256SUMS"

cat > "$artifact_dir/commands.txt" <<EOF
OPEN64_AIO11_MODE=before OPEN64_AIO11_ARTIFACT=$before_b $producer
OPEN64_AIO11_MODE=telemetry-feedback OPEN64_AIO13_ARTIFACT=$feedback_b OPEN64_AIO13_ANALYSIS=$feedback $producer
OPEN64_AIO11_MODE=telemetry-feedback OPEN64_AIO13_ARTIFACT=$repeat_b OPEN64_AIO13_ANALYSIS=$repeat_feedback $producer
OPEN64_AIO11_MODE=telemetry-no-profile OPEN64_AIO13_ANALYSIS=$no_profile $producer
OPEN64_AIO11_MODE=telemetry-negative $producer
$ir_b2a -st -src $before_b $before_t
$ir_b2a -st -src $feedback_b $feedback_t
$ir_b2a -st -src $repeat_b $repeat_t
EOF

echo "AIO-13 telemetry feedback contract passed"
echo "review unchanged WHIRL trace: $feedback_t"
echo "review telemetry and feedback: $feedback"
echo "review deterministic no-profile behavior: $no_profile"
echo "review stale-profile diagnostics: $negative"
echo "review validation: $validation"
