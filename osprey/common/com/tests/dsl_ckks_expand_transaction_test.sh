#!/usr/bin/env bash
# Retain grouped native CKKS expansion, rollback, and mapped-reopen evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_CKKS_EXPAND_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-expand}"
image="$artifact_dir/ckks_expand.B"
trace="$artifact_dir/ckks_expand.T"
region_image="$artifact_dir/ckks_region.B"
region_trace="$artifact_dir/ckks_region.T"

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
  "OPEN64_DSL_CKKS_EXPAND_ONLY=1 OPEN64_DSL_CKKS_EXPAND_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" \
  "OPEN64_DSL_CKKS_REGION_ONLY=1 OPEN64_DSL_CKKS_REGION_ARTIFACT=$region_image $producer" \
  "$ir_b2a -st -src $region_image $region_trace" \
  >"$artifact_dir/commands.txt"

OPEN64_DSL_CKKS_EXPAND_ONLY=1 \
OPEN64_DSL_CKKS_EXPAND_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1
OPEN64_DSL_CKKS_REGION_ONLY=1 \
OPEN64_DSL_CKKS_REGION_ARTIFACT="$region_image" \
  "$producer" >"$artifact_dir/region-producer.log" 2>&1
"$ir_b2a" -st -src "$region_image" "$region_trace" \
  >"$artifact_dir/region-ir_b2a.log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
grep -Fq 'CKKS Event Image: version=1 records=6' "$trace"
grep -Fq 'status=lowered relation=ckks_expansion' "$trace"
grep -Fq 'OPR_DSLCKKSENCODE' "$trace"
grep -Fq 'OPR_DSLCKKSBOOTSTRAP' "$trace"
grep -Fq 'static_ordinal=1 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'static_ordinal=2 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'static_ordinal=6 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'final=true' "$trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into CKKS expansion trace" >&2
  exit 1
fi
test ! -e "$image.tmp"
test "$(grep -c '^FUNC_ENTRY' "$region_trace")" -eq 2
grep -Fq 'REGION id=1 parent=0 depth=1 kind=0 contract=cnn.basic_block.v1' \
  "$region_trace"
grep -Fq 'VCALL' "$region_trace"
grep -Fq 'DSL Call ABI Argument Table: version=1 entries=1' "$region_trace"
grep -Fq 'CKKS Event Image: version=1 records=1' "$region_trace"
grep -Fq 'status=lowered relation=ckks_expansion' "$region_trace"
test ! -e "$region_image.tmp"

if command -v readelf >/dev/null 2>&1; then
  readelf -SW "$image" >"$artifact_dir/section-headers.txt"
  grep -Fq '.WHIRL.dsl_ckks_event' "$artifact_dir/section-headers.txt"
  readelf -SW "$region_image" \
    >"$artifact_dir/region-section-headers.txt"
  grep -Fq '.WHIRL.dsl_ckks_event' \
    "$artifact_dir/region-section-headers.txt"
fi

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    ckks_expand.B ckks_expand.T ckks_region.B ckks_region.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    region-producer.log region-ir_b2a.log commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    ckks_expand.B ckks_expand.T ckks_region.B ckks_region.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    region-producer.log region-ir_b2a.log commands.txt >SHA256SUMS)
fi

echo "CKKS grouped expansion transaction passed"
echo "review trace: $trace"
echo "call/REGION trace: $region_trace"
