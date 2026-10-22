#!/usr/bin/env bash
# Retain batch dead-constant elision, rollback, and terminal-writer evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_DEAD_CONSTANT_ARTIFACT_DIR:-$repo_root/artifacts/fhe/dead-native-constants}"
image="$artifact_dir/dead_constants.B"
trace="$artifact_dir/dead_constants.T"
terminal_image="$artifact_dir/dead_constants_terminal.B"
terminal_trace="$artifact_dir/dead_constants_terminal.T"

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
  "OPEN64_DSL_DEAD_CONSTANT_ONLY=1 OPEN64_DSL_DEAD_CONSTANT_ARTIFACT=$image $producer" \
  "$ir_b2a -st -src $image $trace" \
  "OPEN64_DSL_DEAD_CONSTANT_ONLY=1 OPEN64_DSL_DEAD_CONSTANT_DISCARD=1 OPEN64_DSL_DEAD_CONSTANT_ARTIFACT=$terminal_image $producer" \
  "$ir_b2a -st -src $terminal_image $terminal_trace" \
  >"$artifact_dir/commands.txt"

OPEN64_DSL_DEAD_CONSTANT_ONLY=1 \
OPEN64_DSL_DEAD_CONSTANT_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/ir_b2a.log" 2>&1
for case_name in wrong_identity duplicate live region call; do
  rejected="$artifact_dir/rejected_${case_name}.B"
  OPEN64_DSL_DEAD_CONSTANT_ONLY=1 \
  OPEN64_DSL_DEAD_CONSTANT_NEGATIVE="$case_name" \
  OPEN64_DSL_DEAD_CONSTANT_ARTIFACT="$rejected" \
    "$producer" >"$artifact_dir/${case_name}.log" 2>&1
  test ! -e "$rejected"
  test ! -e "$rejected.tmp"
done
OPEN64_DSL_DEAD_CONSTANT_ONLY=1 \
OPEN64_DSL_DEAD_CONSTANT_DISCARD=1 \
OPEN64_DSL_DEAD_CONSTANT_ARTIFACT="$terminal_image" \
  "$producer" >"$artifact_dir/discard-producer.log" 2>&1
"$ir_b2a" -st -src "$terminal_image" "$terminal_trace" \
  >"$artifact_dir/terminal-ir_b2a.log" 2>&1

test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 1
test "$(grep -Ec 'operator=OPR_DSLTENSORCONST .*status=dead_elided' "$trace")" -eq 2
grep -Fq 'name=dead_zero' "$trace"
grep -Fq 'name=dead_one' "$trace"
grep -Fq 'source files:' "$trace"
test "$(grep -c '^FUNC_ENTRY' "$terminal_trace")" -eq 1
grep -Eq 'DSL IR Image: .*nodes=0 .*values=0 ' "$terminal_trace"
if grep -Fq 'operator=OPR_DSL' "$terminal_trace"; then
  echo "standard-only output retained native DSL evidence" >&2
  exit 1
fi
grep -Fq 'request=1 dead-constant identity mismatch' \
  "$artifact_dir/wrong_identity.log"
grep -Fq 'request=1 duplicate transaction member' \
  "$artifact_dir/duplicate.log"
grep -Fq 'request=1 source value has an unlowered or escaping use' \
  "$artifact_dir/live.log"
grep -Fq 'request=1 source REGION interface cannot be redirected' \
  "$artifact_dir/region.log"
grep -Fq 'request=1 runtime relation mismatch' \
  "$artifact_dir/call.log"
test ! -e "$terminal_image.tmp"

if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  "$OPEN64_PREVIOUS_IR_B2A" -st -src "$terminal_image" \
    "$artifact_dir/dead_constants_terminal.previous.T" \
    >"$artifact_dir/previous-reader.log" 2>&1
  cmp "$terminal_trace" "$artifact_dir/dead_constants_terminal.previous.T"
fi
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum dead_constants.B dead_constants.T \
    dead_constants_terminal.B dead_constants_terminal.T \
    dsl_builder_contract_test.cxx commands.txt producer.log ir_b2a.log \
    wrong_identity.log duplicate.log live.log region.log call.log \
    discard-producer.log terminal-ir_b2a.log >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 dead_constants.B dead_constants.T \
    dead_constants_terminal.B dead_constants_terminal.T \
    dsl_builder_contract_test.cxx commands.txt producer.log ir_b2a.log \
    wrong_identity.log duplicate.log live.log region.log call.log \
    discard-producer.log terminal-ir_b2a.log >SHA256SUMS)
fi
if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if command -v sha256sum >/dev/null 2>&1; then
    (cd "$artifact_dir" && sha256sum dead_constants_terminal.previous.T \
      previous-reader.log >>SHA256SUMS)
  else
    (cd "$artifact_dir" && shasum -a 256 dead_constants_terminal.previous.T \
      previous-reader.log >>SHA256SUMS)
  fi
fi

echo "DSL dead native constant batch passed"
echo "review provenance trace: $trace"
echo "review terminal trace: $terminal_trace"
