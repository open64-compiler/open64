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
call_image="$artifact_dir/ckks_complete_group_call.B"
call_trace="$artifact_dir/ckks_complete_group_call.T"
terminal_image="$artifact_dir/ckks_complete_group_terminal.B"
terminal_trace="$artifact_dir/ckks_complete_group_terminal.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done
if ! command -v readelf >/dev/null 2>&1; then
  echo "readelf is required for terminal ELF section validation" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$repo_root/osprey/common/com/tests/dsl_builder_contract_test.cxx" \
  "$artifact_dir/dsl_builder_contract_test.cxx"
printf '%s\n' \
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=complete OPEN64_DSL_CKKS_EVENT_ARTIFACT=$image $producer" \
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=private_projection $producer" \
  "$ir_b2a -st -src $image $trace" \
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=call OPEN64_DSL_CKKS_EVENT_ARTIFACT=$call_image $producer" \
  "$ir_b2a -st -src $call_image $call_trace" \
  "OPEN64_DSL_CKKS_EVENT_ONLY=1 OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=terminal OPEN64_DSL_CKKS_COMPLETE_GROUP_TERMINAL_ARTIFACT=$terminal_image $producer" \
  "$ir_b2a -st -src $terminal_image $terminal_trace" \
  >"$artifact_dir/commands.txt"

OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=complete \
OPEN64_DSL_CKKS_EVENT_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=private_projection \
  "$producer" >"$artifact_dir/private-projection-rejection.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1
OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=call \
OPEN64_DSL_CKKS_EVENT_ARTIFACT="$call_image" \
  "$producer" >"$artifact_dir/call-producer.log" 2>&1
"$ir_b2a" -st -src "$call_image" "$call_trace" \
  >"$artifact_dir/call-ir_b2a.log" 2>&1
for case_name in call_bad_actual call_bad_projection; do
  OPEN64_DSL_CKKS_EVENT_ONLY=1 \
  OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY="$case_name" \
    "$producer" >"$artifact_dir/$case_name.log" 2>&1
done
OPEN64_DSL_CKKS_EVENT_ONLY=1 \
OPEN64_DSL_CKKS_COMPLETE_GROUP_ONLY=terminal \
OPEN64_DSL_CKKS_COMPLETE_GROUP_TERMINAL_ARTIFACT="$terminal_image" \
  "$producer" >"$artifact_dir/terminal-producer.log" 2>&1
"$ir_b2a" -st -src "$terminal_image" "$terminal_trace" \
  >"$artifact_dir/terminal-ir_b2a.log" 2>&1
readelf -SW "$terminal_image" >"$artifact_dir/terminal-sections.txt"
readelf -p .comment "$terminal_image" \
  >"$artifact_dir/terminal-revision.txt"

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
grep -Fq 'CKKS Event Image: version=1 records=2' "$trace"
grep -Fq 'static_ordinal=1 context_identity=1 callsite=0 step=0' "$trace"
grep -Fq 'static_ordinal=2 context_identity=1 callsite=0 step=0' "$trace"
grep -Eq 'name=event_encoded .*status=lowered relation=private_group_member' "$trace"
grep -Eq 'name=event_refreshed .*status=lowered relation=runtime_value_projection' "$trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
test "$(grep -c '^FUNC_ENTRY' "$call_trace")" -eq 2
test "$(grep -c ' VCALL ' "$call_trace")" -eq 1
grep -Fq 'CKKS Event Image: version=1 records=2' "$call_trace"
grep -Eq 'name=event_refreshed .*status=lowered relation=runtime_value_projection' "$call_trace"
grep -Eq 'source_value=[0-9]+ projection=[0-9]+ actual=0 formal=0 direction=input' "$call_trace"
grep -Fq 'retired_formals=1 retired_call_arguments=1' "$call_trace"
grep -Eq 'old_actual=1 old_formal=1 value=[0-9]+ reason=verified_dead_input role=ckks.retired_original' "$call_trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$call_trace"
test "$(grep -c '^FUNC_ENTRY' "$terminal_trace")" -eq 1
grep -Fq '__dsl_runtime_event_refreshed_' "$terminal_trace"
grep -Eq '^ LOC 1 [1-9][0-9]* ' "$terminal_trace"
grep -Fq 'WHIRL::0.33:' "$artifact_dir/terminal-revision.txt"
if grep -Fq '.WHIRL.dsl' "$artifact_dir/terminal-sections.txt" ||
   grep -Fq 'CKKS Event Image:' "$terminal_trace" ||
   grep -Fq 'OPR_DSL' "$terminal_trace"; then
  echo "terminal CKKS image retained private DSL evidence" >&2
  exit 1
fi
if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  "$OPEN64_PREVIOUS_IR_B2A" -st -src "$terminal_image" \
    "$artifact_dir/previous_terminal.T" \
    >"$artifact_dir/previous-terminal-ir_b2a.log" 2>&1
  cmp "$terminal_trace" "$artifact_dir/previous_terminal.T"
fi
if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
  echo "physical DSL escape leaked into complete-group trace" >&2
  exit 1
fi
test ! -e "$image.tmp"
test ! -e "$call_image.tmp"
test ! -e "$terminal_image.tmp"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    ckks_complete_group.B ckks_complete_group.T \
    ckks_complete_group_call.B ckks_complete_group_call.T \
    ckks_complete_group_terminal.B ckks_complete_group_terminal.T \
    dsl_builder_contract_test.cxx producer.log \
    private-projection-rejection.log ir_b2a.log \
    call-producer.log call-ir_b2a.log call_bad_actual.log \
    call_bad_projection.log terminal-producer.log terminal-ir_b2a.log \
    terminal-sections.txt terminal-revision.txt commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    ckks_complete_group.B ckks_complete_group.T \
    ckks_complete_group_call.B ckks_complete_group_call.T \
    ckks_complete_group_terminal.B ckks_complete_group_terminal.T \
    dsl_builder_contract_test.cxx producer.log \
    private-projection-rejection.log ir_b2a.log \
    call-producer.log call-ir_b2a.log call_bad_actual.log \
    call_bad_projection.log terminal-producer.log terminal-ir_b2a.log \
    terminal-sections.txt terminal-revision.txt commands.txt >SHA256SUMS)
fi
if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if command -v sha256sum >/dev/null 2>&1; then
    (cd "$artifact_dir" && sha256sum previous_terminal.T \
      previous-terminal-ir_b2a.log >>SHA256SUMS)
  else
    (cd "$artifact_dir" && shasum -a 256 previous_terminal.T \
      previous-terminal-ir_b2a.log >>SHA256SUMS)
  fi
fi

echo "CKKS complete-group lowering passed"
echo "review trace: $trace"
echo "review call trace: $call_trace"
echo "review terminal trace: $terminal_trace"
