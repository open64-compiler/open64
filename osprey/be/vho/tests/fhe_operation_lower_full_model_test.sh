#!/usr/bin/env bash
#
# Certify six-PU S5-F standard-WHIRL lowering and rejected-request cleanup.
#
# Design references:
#   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-F
#   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md

set -euo pipefail

input="${OPEN64_FHE_OPERATION_APPLY_INPUT:?set the six-PU S5-E .B path}"
source="${OPEN64_FHE_OPERATION_SOURCE:?set the captured model source path}"
producer="${OPEN64_FHE_RUNTIME_LOWER_TEST:?set the linked FHE test path}"
ir_b2a="${OPEN64_IR_B2A:?set the ir_b2a path}"
artifact_dir="${OPEN64_FHE_OPERATION_ARTIFACT_DIR:?set a host-mounted artifact directory}"
stem="secure_resnet20.operation-lowered"
binary="$artifact_dir/$stem.B"
trace="$artifact_dir/$stem.T"
negative="$artifact_dir/secure_resnet20.rejected.B"
log="$artifact_dir/validation.log"
negative_log="$artifact_dir/rejected-request.log"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$source" "$artifact_dir/secure_resnet20.py"
printf 'cp %q %q\n' "$source" "$artifact_dir/secure_resnet20.py" \
  >"$artifact_dir/commands.txt"
printf 'OPEN64_FHE_OPERATION_APPLY_INPUT=%q OPEN64_FHE_OPERATION_APPLY_OUTPUT=%q %q\n' \
  "$input" "$binary" "$producer" >>"$artifact_dir/commands.txt"
printf '%q -st -src %q %q\n' "$ir_b2a" "$binary" "$trace" \
  >>"$artifact_dir/commands.txt"
printf 'OPEN64_FHE_OPERATION_REJECT_PU=6 OPEN64_FHE_OPERATION_APPLY_INPUT=%q OPEN64_FHE_OPERATION_APPLY_OUTPUT=%q %q\n' \
  "$input" "$negative" "$producer" >>"$artifact_dir/commands.txt"

OPEN64_FHE_OPERATION_APPLY_INPUT="$input" \
OPEN64_FHE_OPERATION_APPLY_OUTPUT="$binary" \
  "$producer" >"$log" 2>&1
"$ir_b2a" -st -src "$binary" "$trace" >>"$log" 2>&1

grep -Fq \
  'FHE operation total: pu=6 computed=33 promoted=44 unpromoted=126 live_unpromoted=0 selectors=87 evaluations=87 calls=174 valid=1' \
  "$log"
test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 6
test "$(grep -c 'operator=OPR_DSLCONV2D .*status=lowered' "$trace")" -eq 13
test "$(grep -c 'operator=OPR_DSLRESIDUALADD .*status=lowered' "$trace")" -eq 5
test "$(grep -c 'operator=OPR_DSLRELU .*status=lowered' "$trace")" -eq 11
test "$(grep -c 'operator=OPR_DSLTENSORCONST .*status=dead_elided' "$trace")" -eq 126
test "$(grep -c 'U4CALL .*open64_fhe_operation_desc_select_v1' "$trace")" -eq 87
test "$(grep -c 'VALUE ordinal=.*roles=0xa' "$trace")" -eq 5
grep -Fq 'def forward(self, value):' "$trace"

if OPEN64_FHE_OPERATION_REJECT_PU=6 \
   OPEN64_FHE_OPERATION_APPLY_INPUT="$input" \
   OPEN64_FHE_OPERATION_APPLY_OUTPUT="$negative" \
   "$producer" >"$negative_log" 2>&1; then
  echo "invalid final-PU request unexpectedly succeeded" >&2
  exit 1
fi
grep -Fq 'FHE operation negative: pu=6 last_request_rollback=1' \
  "$negative_log"
test ! -e "$negative"
test ! -e "$negative.tmp"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    "$stem.B" "$stem.T" secure_resnet20.py >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    "$stem.B" "$stem.T" secure_resnet20.py >SHA256SUMS)
fi

echo "FHE six-PU operation lowering passed"
echo "review binary: $binary"
echo "review trace: $trace"
echo "review diagnostics: $log"
echo "review negative: $negative_log"
echo "review commands: $artifact_dir/commands.txt"
echo "review hashes: $artifact_dir/SHA256SUMS"
