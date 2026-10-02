#!/usr/bin/env bash
# Certify the logical CKKS registry through the unchanged mapped WHIRL path.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_CKKS_OPCODE_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-opcode}"
image="$artifact_dir/ckks_opcode_roundtrip.B"
trace="$artifact_dir/ckks_opcode_roundtrip.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$repo_root/osprey/common/com/tests/dsl_builder_contract_test.cxx" \
  "$artifact_dir/dsl_builder_contract_test.cxx"
printf '%s\n' \
  "OPEN64_DSL_CKKS_OPCODE_ONLY=1 OPEN64_DSL_CKKS_OPCODE_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

OPEN64_DSL_CKKS_OPCODE_ONLY=1 \
OPEN64_DSL_CKKS_OPCODE_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
for operator in ADD SUB MUL ENCODE ROTATE RESCALE MODSWITCH RELIN BOOTSTRAP; do
  grep -Fq "operator=OPR_DSLCKKS$operator version=1" "$trace"
done
grep -Fq "stable_name=ckks.bootstrap" "$trace"
grep -Fq "attr.signed_steps" "$trace"
grep -Fq "attr.reason" "$trace"
grep -Fq 'dsl_comment_projection=OPR_COMMENT' "$trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into CKKS trace" >&2
  exit 1
fi
test ! -e "$image.tmp"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    ckks_opcode_roundtrip.B ckks_opcode_roundtrip.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    ckks_opcode_roundtrip.B ckks_opcode_roundtrip.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    commands.txt >SHA256SUMS)
fi

echo "CKKS logical opcode mapped-image contract passed"
echo "review trace: $trace"
