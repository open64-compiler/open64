#!/usr/bin/env bash
# Certify one source-context union spanning linked CKKS static subgroups.
set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_CKKS_COMPLETE_GROUP_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-complete-group}"
image="$artifact_dir/ckks_complete_group.B"
trace="$artifact_dir/ckks_complete_group.T"

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
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=complete OPEN64_DSL_CKKS_EVENT_ARTIFACT=$image $producer" \
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=private_projection $producer" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=complete \
OPEN64_DSL_CKKS_EVENT_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=private_projection \
  "$producer" >"$artifact_dir/private-projection-rejection.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
grep -Fq 'CKKS Event Image: version=1 records=2' "$trace"
grep -Fq 'static_ordinal=1 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'static_ordinal=2 context_identity=1 callsite=0 step=0' "$trace"
grep -Eq 'name=event_encoded .*status=lowered relation=private_group_member' "$trace"
grep -Eq 'name=event_refreshed .*status=lowered relation=runtime_value_projection' "$trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into complete-group trace" >&2
  exit 1
fi
test ! -e "$image.tmp"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    ckks_complete_group.B ckks_complete_group.T \
    dsl_builder_contract_test.cxx producer.log \
    private-projection-rejection.log ir_b2a.log commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    ckks_complete_group.B ckks_complete_group.T \
    dsl_builder_contract_test.cxx producer.log \
    private-projection-rejection.log ir_b2a.log commands.txt >SHA256SUMS)
fi

echo "CKKS complete-group lowering passed"
echo "review trace: $trace"
