#!/usr/bin/env bash
#
# Certify the six-PU SYNC-5 interface transaction and retained mapped image.
# The caller must mount the source path recorded by the input binary so -src
# can interleave the original model text.
#
# Design references:
#   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-D and S5-E
#   doc/FHE-SYNC5-PROGRAM-INTERFACE-CONTRACT.md

set -euo pipefail

input="${OPEN64_FHE_INTERFACE_APPLY_INPUT:?set the materialized six-PU .B path}"
producer="${OPEN64_FHE_RUNTIME_LOWER_TEST:?set the linked FHE contract test path}"
ir_b2a="${OPEN64_IR_B2A:?set the ir_b2a path}"
artifact_dir="${OPEN64_FHE_INTERFACE_ARTIFACT_DIR:?set a host-mounted artifact directory}"
stem="secure_resnet20.interfaced-initialized"
binary="$artifact_dir/$stem.B"
trace="$artifact_dir/$stem.T"
log="$artifact_dir/validation.log"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf 'OPEN64_FHE_INTERFACE_APPLY_INPUT=%q OPEN64_FHE_INTERFACE_APPLY_OUTPUT=%q %q\n' \
  "$input" "$binary" "$producer" >"$artifact_dir/commands.txt"
printf '%q -st -src %q %q\n' "$ir_b2a" "$binary" "$trace" \
  >>"$artifact_dir/commands.txt"

if ! OPEN64_FHE_INTERFACE_APPLY_INPUT="$input" \
     OPEN64_FHE_INTERFACE_APPLY_OUTPUT="$binary" \
     "$producer" >"$log" 2>&1; then
  test ! -e "$binary" && test ! -e "$binary.tmp"
  cat "$log" >&2
  exit 1
fi

"$ir_b2a" -st -src "$binary" "$trace" >>"$log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 6
test "$(grep -c '^ VCALL' "$trace")" -eq 9
test "$(grep -c '^REGION id=.*cnn.basic_block.v1' "$trace")" -eq 5
test "$(grep -c '^  U8INTCONST 0 (0x0)$' "$trace")" -eq 9
test "$(grep -c '^ U8STID 0 <2,.*__dsl_runtime_.*output.*{line:' "$trace")" -eq 9
grep -Fq 'DSL Runtime Interface Image: version=1 values=248 calls=58' "$trace"
grep -Fq 'DSL Program Interface Image: version=1 retired_formals=48 retired_call_arguments=80 runtime_inputs=48 runtime_bindings=92 runtime_calls=76' "$trace"
grep -Fq 'def forward(self, value):' "$trace"
grep -Fq 'FHE interface apply: pu=6 retired_formals=48 retired_actuals=80 bindings=92 calls=76 projections=248 rewritten_calls=9 returns=6 valid=1' "$log"
test ! -e "$binary.tmp"

negative_binary="$artifact_dir/$stem.negative.B"
printf 'OPEN64_FHE_INTERFACE_REJECT_PU=5 OPEN64_FHE_INTERFACE_APPLY_INPUT=%q OPEN64_FHE_INTERFACE_APPLY_OUTPUT=%q %q\n' \
  "$input" "$negative_binary" "$producer" >>"$artifact_dir/commands.txt"
if OPEN64_FHE_INTERFACE_REJECT_PU=5 \
   OPEN64_FHE_INTERFACE_APPLY_INPUT="$input" \
   OPEN64_FHE_INTERFACE_APPLY_OUTPUT="$negative_binary" \
   "$producer" >"$artifact_dir/negative.log" 2>&1; then
  echo "PU-5 failure injection unexpectedly succeeded" >&2
  exit 1
fi
test ! -e "$negative_binary" && test ! -e "$negative_binary.tmp"
grep -Fq 'FHE interface apply: pu=4' "$artifact_dir/negative.log"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum "$stem.B" "$stem.T" >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 "$stem.B" "$stem.T" >SHA256SUMS)
fi

echo "FHE six-PU interface application passed"
echo "review binary: $binary"
echo "review trace: $trace"
echo "review commands: $artifact_dir/commands.txt"
echo "review diagnostics: $log"
echo "review hashes: $artifact_dir/SHA256SUMS"
