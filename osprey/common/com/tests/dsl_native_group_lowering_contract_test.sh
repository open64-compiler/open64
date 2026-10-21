#!/usr/bin/env bash
#
# Preserve grouped native-to-standard WHIRL transaction evidence for review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_GROUP_LOWER_ARTIFACT_DIR:-$repo_root/artifacts/fhe/grouped-whirl-lowering}"
image="$artifact_dir/group_lowering_contract.B"
trace="$artifact_dir/group_lowering_contract.T"
terminal_image="$artifact_dir/group_lowering_terminal.B"
terminal_trace="$artifact_dir/group_lowering_terminal.T"

for executable in "$contract_test" "$ir_b2a"; do
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
  "OPEN64_DSL_GROUP_LOWER_ONLY=1 OPEN64_DSL_GROUP_LOWER_ARTIFACT=$image $contract_test" \
  "OPEN64_DSL_GROUP_LOWER_ONLY=1 OPEN64_DSL_GROUP_LOWER_ARTIFACT=$image OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT=$terminal_image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$artifact_dir/commands.txt"

if ! (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" "$contract_test") \
    >"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi
if ! (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT="$terminal_image" \
  "$contract_test") >>"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi
grep -Fq 'DSL terminal preflight rejected live tensor seed and accepted standard control tree' \
  "$artifact_dir/validation.log"
grep -Fq 'DSL-TERMINAL-001: executable native DSL node OPR_DSLTENSORCONST remains' \
  "$artifact_dir/validation.log"
if ! (cd "$repo_root" && "$ir_b2a" -st -src "$image" "$trace") \
    >>"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi
printf '%s\n' "$ir_b2a -st -src $terminal_image $terminal_trace" \
  >>"$artifact_dir/commands.txt"
if ! (cd "$repo_root" && "$ir_b2a" -st -src \
       "$terminal_image" "$terminal_trace") \
    >>"$artifact_dir/validation.log" 2>&1; then
  cat "$artifact_dir/validation.log" >&2
  exit 1
fi
readelf -SW "$terminal_image" >"$artifact_dir/terminal-sections.txt"
readelf -p .comment "$terminal_image" \
  >"$artifact_dir/terminal-revision.txt"
if grep -Fq '.WHIRL.dsl' "$artifact_dir/terminal-sections.txt" ||
   grep -Eq 'DSL IR Image:.*nodes=[1-9]' "$terminal_trace" ||
   grep -Fq 'status=lowered' "$terminal_trace" ||
   grep -Fq 'OPR_DSL' "$terminal_trace"; then
  echo "standard-only terminal image retained private DSL evidence" >&2
  exit 1
fi
if [[ "$(grep -Ec '^FUNC_ENTRY ' "$terminal_trace")" -ne 2 ]]; then
  echo "standard-only terminal image lacks two complete PUs" >&2
  exit 1
fi
if ! grep -Eq '^ LOC 1 [1-9][0-9]* ' "$terminal_trace"; then
  echo "standard-only terminal trace lost source cross-reference" >&2
  exit 1
fi
if ! grep -Fq 'WHIRL::0.33:' \
     "$artifact_dir/terminal-revision.txt"; then
  echo "standard-only terminal image lacks the baseline WHIRL revision" >&2
  exit 1
fi

for evidence in \
  'DSL Value Table:' \
  'status=lowered relation=private_group_member' \
  'status=lowered relation=runtime_value_projection projection=' \
  'group_step_0' \
  'group_step_5' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing grouped-lowering evidence '$evidence' in $trace" >&2
    exit 1
  fi
done
if [[ "$(grep -Fc 'status=lowered relation=private_group_member' "$trace")" -ne 8 ]] ||
   [[ "$(grep -Fc 'status=lowered relation=runtime_value_projection' "$trace")" -ne 4 ]] ||
   [[ "$(grep -Ec '^  OPR_DSLADD ' "$trace")" -ne 0 ]]; then
  echo "grouped lowering census changed" >&2
  exit 1
fi

for negative_case in private_cross_group return_escape call_escape \
                     region_escape interface_escape; do
  printf '%s\n' "OPEN64_DSL_GROUP_LOWER_NEGATIVE_CASE=$negative_case $contract_test" \
    >>"$artifact_dir/commands.txt"
  if ! (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
    OPEN64_DSL_GROUP_LOWER_NEGATIVE_CASE="$negative_case" \
    OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" "$contract_test") \
      >"$artifact_dir/negative_${negative_case}.log" 2>&1; then
    cat "$artifact_dir/negative_${negative_case}.log" >&2
    exit 1
  fi
  if ! grep -Fq "negative $negative_case preserved tree and image" \
      "$artifact_dir/negative_${negative_case}.log"; then
    echo "missing rollback evidence for $negative_case" >&2
    exit 1
  fi
done

if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
  if "$OPEN64_PREVIOUS_IR_B2A" -st -src "$image" \
       "$artifact_dir/previous_reader.T" \
       >"$artifact_dir/previous_reader.log" 2>&1; then
    echo "previous ir_b2a unexpectedly accepted private lowered rows" >&2
    exit 1
  fi
  if ! grep -Eq 'DSL image error|invalid (node|value)' \
      "$artifact_dir/previous_reader.log"; then
    echo "previous reader did not fail closed at the DSL image gate" >&2
    exit 1
  fi
  if ! "$OPEN64_PREVIOUS_IR_B2A" -st -src "$terminal_image" \
       "$artifact_dir/previous_terminal_reader.T" \
       >"$artifact_dir/previous_terminal_reader.log" 2>&1; then
    echo "previous ir_b2a rejected standard-only terminal WHIRL" >&2
    cat "$artifact_dir/previous_terminal_reader.log" >&2
    exit 1
  fi
  if [[ "$(grep -Ec '^FUNC_ENTRY ' \
             "$artifact_dir/previous_terminal_reader.T")" -ne 2 ]]; then
    echo "previous reader lost a terminal PU" >&2
    exit 1
  fi
fi

incomplete_temp="$artifact_dir/incomplete_terminal.B.tmp"
native_temp="$artifact_dir/native_terminal.B.tmp"
printf '%s\n' \
  "OPEN64_DSL_GROUP_LOWER_TERMINAL_NATIVE=1 OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT=$native_temp $contract_test" \
  "OPEN64_DSL_GROUP_LOWER_TERMINAL_INCOMPLETE=1 OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT=$incomplete_temp $contract_test" \
  >>"$artifact_dir/commands.txt"
if (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT="$native_temp" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_NATIVE=1 \
  "$contract_test") >"$artifact_dir/native_terminal.log" 2>&1; then
  echo "native terminal PU was accepted" >&2
  exit 1
fi
if ! grep -Fq 'standard-only PU section' \
     "$artifact_dir/native_terminal.log" ||
   [[ -e "$artifact_dir/native_terminal.B" ]]; then
  echo "native terminal output failed outside the physical-tree gate" >&2
  exit 1
fi
if (cd "$repo_root" && OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$image" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT="$incomplete_temp" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_INCOMPLETE=1 \
  "$contract_test") >"$artifact_dir/incomplete_terminal.log" 2>&1; then
  echo "incomplete terminal PU coverage was accepted" >&2
  exit 1
fi
if ! grep -Fq 'standard-only PU coverage' \
     "$artifact_dir/incomplete_terminal.log" ||
   [[ -e "$artifact_dir/incomplete_terminal.B" ]]; then
  echo "incomplete terminal output failed outside the coverage gate" >&2
  exit 1
fi

(cd "$artifact_dir" && sha256sum *.B *.T *.log \
  dsl_builder_contract_test.cxx commands.txt terminal-*.txt >SHA256SUMS)

echo "DSL grouped native lowering contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review standard-only trace: $terminal_trace"
echo "review diagnostics: $artifact_dir/validation.log"
