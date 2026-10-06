#!/usr/bin/env bash
# Retain mapped evidence for FHE-admitted typed rows and generated masks.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
build_root=${OPEN64_BUILD_ROOT:-$repo_root/build}
artifact_dir=${OPEN64_FHE_CONV_ASSET_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-conv-assets}
tool_dir="$build_root/osprey/targdir/ir_tools"
fragment="$repo_root/osprey/be/vho/tests/fhe_ckks_conv_assets_test.mk"
image="$artifact_dir/conv_assets.B"
trace="$artifact_dir/conv_assets.T"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$repo_root/osprey/be/vho/tests/fhe_ckks_conv_assets_test.cxx" \
  "$artifact_dir/fhe_ckks_conv_assets_test.cxx"
make -C "$tool_dir" -f Makefile -f "$fragment" \
  fhe_ckks_conv_assets_test ir_b2a >"$artifact_dir/build.log" 2>&1
"$tool_dir/fhe_ckks_conv_assets_test" "$image" \
  >"$artifact_dir/producer.log" 2>&1
"$tool_dir/ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1

test "$(grep -Fc 'dsl.typed_external_row=1' "$trace")" -eq 9
test "$(grep -Fc 'dsl.generated_external=1' "$trace")" -eq 2
grep -Fq 'dsl.transformation_name=fhe.conv_feature_row' "$trace"
grep -Fq 'dsl.transformation_ordinal=8' "$trace"
grep -Fq 'dsl.generation_name=fhe.ckks.stride_compaction.mask' "$trace"
grep -Fq 'dsl.geometry_manifest_sha256=0123456789abcdef' "$trace"
grep -Fq 'dsl.variant_signature_sha256=1111111111111111' "$trace"
grep -Fq 'storage_byte_offset=1280' "$trace"
grep -Fq 'materialized 9 typed rows and 2 generated masks' \
  "$artifact_dir/producer.log"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into $trace" >&2
  exit 1
fi
test ! -e "$image.tmp"

printf '%s\n' \
  "make -C $tool_dir -f Makefile -f $fragment fhe_ckks_conv_assets_test ir_b2a" \
  "$tool_dir/fhe_ckks_conv_assets_test $image" \
  "$tool_dir/ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum conv_assets.B conv_assets.T \
    fhe_ckks_conv_assets_test.cxx *.log commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 conv_assets.B conv_assets.T \
    fhe_ckks_conv_assets_test.cxx *.log commands.txt >SHA256SUMS)
fi
echo "CKKS Conv asset WHIRL roundtrip passed: $artifact_dir"
