#!/usr/bin/env bash
#
# Certify the complete six-PU SYNC-5 operation plan without publishing lowered
# WHIRL. This diagnostic lane is separate from the atomic S5-F apply gate.
#
# Design references:
#   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-F
#   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md

set -euo pipefail

input="${OPEN64_FHE_OPERATION_PLAN_INPUT:?set the six-PU S5-E .B path}"
producer="${OPEN64_FHE_RUNTIME_LOWER_TEST:?set the linked FHE test path}"
ir_b2a="${OPEN64_IR_B2A:?set the ir_b2a path}"
artifact_dir="${OPEN64_FHE_OPERATION_PLAN_ARTIFACT_DIR:?set a host-mounted artifact directory}"
stem="secure_resnet20.operation-plan"
binary="$artifact_dir/$stem.B"
trace="$artifact_dir/$stem.T"
log="$artifact_dir/validation.log"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$input" "$binary"
printf 'OPEN64_FHE_OPERATION_PLAN_INPUT=%q %q\n' \
  "$binary" "$producer" >"$artifact_dir/commands.txt"
printf '%q -st -src %q %q\n' "$ir_b2a" "$binary" "$trace" \
  >>"$artifact_dir/commands.txt"

OPEN64_FHE_OPERATION_PLAN_INPUT="$binary" "$producer" >"$log" 2>&1
"$ir_b2a" -st -src "$binary" "$trace" >>"$log" 2>&1

grep -Fq \
  'FHE operation plan total: pu=6 computed=33 promoted=44 unpromoted=126 live_unpromoted=0 selectors=87 evaluations=87 calls=174 valid=1' \
  "$log"
test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 6
test "$(grep -c '^ VCALL' "$trace")" -eq 9
test "$(grep -c '^REGION id=.*cnn.basic_block.v1' "$trace")" -eq 5
grep -Fq 'def forward(self, value):' "$trace"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum "$stem.B" "$stem.T" >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 "$stem.B" "$stem.T" >SHA256SUMS)
fi

echo "FHE six-PU operation planning passed"
echo "review binary: $binary"
echo "review trace: $trace"
echo "review commands: $artifact_dir/commands.txt"
echo "review diagnostics: $log"
echo "review hashes: $artifact_dir/SHA256SUMS"
