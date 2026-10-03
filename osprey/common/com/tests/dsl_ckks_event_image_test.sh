#!/usr/bin/env bash
# Preserve the mapped CKKS event relation and its logical ASCII evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_CKKS_EVENT_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-event}"
image="$artifact_dir/ckks_event_roundtrip.B"
trace="$artifact_dir/ckks_event_roundtrip.T"

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
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_EVENT_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_EVENT_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
grep -Fq 'CKKS Event Image: version=1 records=2' "$trace"
grep -Fq 'static_ordinal=1 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'static_ordinal=1 context_identity=1 callsite=0 step=1' "$trace"
grep -Fq 'final=true' "$trace"
grep -Fq 'operator=OPR_DSLRELU version=2' "$trace"
grep -Fq 'operator=OPR_DSLCKKSBOOTSTRAP version=1' "$trace"
grep -Fq 'status=lowered relation=ckks_expansion' "$trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
if command -v readelf >/dev/null 2>&1; then
  readelf -SW "$image" >"$artifact_dir/section-headers.txt"
  grep -Eq '\.WHIRL\.dsl_ckks_event .* 0000a0 .* 8$' \
    "$artifact_dir/section-headers.txt"
fi
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into CKKS event trace" >&2
  exit 1
fi
test ! -e "$image.tmp"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    ckks_event_roundtrip.B ckks_event_roundtrip.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    ckks_event_roundtrip.B ckks_event_roundtrip.T \
    dsl_builder_contract_test.cxx producer.log ir_b2a.log \
    commands.txt >SHA256SUMS)
fi
if [[ -f "$artifact_dir/section-headers.txt" ]]; then
  if command -v sha256sum >/dev/null 2>&1; then
    (cd "$artifact_dir" && sha256sum section-headers.txt >>SHA256SUMS)
  else
    (cd "$artifact_dir" && shasum -a 256 section-headers.txt >>SHA256SUMS)
  fi
fi

echo "CKKS event mapped-image contract passed"
echo "review trace: $trace"
