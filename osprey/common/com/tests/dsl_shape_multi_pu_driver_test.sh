#!/usr/bin/env bash
#
# Exercise backend-owned per-PU shape refinement in one backend process and
# retain the binary WHIRL, traces, diagnostics, and provenance for review.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_SHAPE_WP5_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_shape_refine_contract_test}"
be="${OPEN64_DSL_SHAPE_WP5_BE:-$repo_root/build/osprey/targdir/be/be}"
ir_b2a="${OPEN64_DSL_SHAPE_WP5_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_WP5_ARTIFACT_DIR:-$repo_root/artifacts/shape/wp5-multi-pu-driver}"
validation_lane="${OPEN64_DSL_SHAPE_WP5_LANE:-unspecified}"

fail()
{
  echo "WP5 multi-PU driver test failed: $*" >&2
  exit 1
}

require_text()
{
  local file="$1"
  local text="$2"
  grep -Fq "$text" "$file" || fail "missing '$text' in $file"
}

require_count()
{
  local file="$1"
  local text="$2"
  local expected="$3"
  local actual
  actual="$(grep -Fc "$text" "$file")"
  [[ "$actual" == "$expected" ]] ||
    fail "expected $expected occurrences of '$text' in $file, found $actual"
}

require_colliding_full_st_idx()
{
  local file="$1"
  local line
  line="$(grep -F 'WP5 producer mode=' "$file")"
  if [[ ! "$line" =~ first_st=([0-9]+):([0-9]+)[[:space:]]second_st=([0-9]+):([0-9]+)[[:space:]]collision=1 ]]; then
    fail "could not parse complete colliding ST_IDX values in $file"
  fi
  [[ "${BASH_REMATCH[1]}:${BASH_REMATCH[2]}" == \
     "${BASH_REMATCH[3]}:${BASH_REMATCH[4]}" ]] ||
    fail "producer ST_IDX values do not collide in $file"
}

for executable in "$producer" "$be" "$ir_b2a"; do
  [[ -x "$executable" ]] || fail "missing executable: $executable"
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -delete

command_log="$artifact_dir/commands.txt"
cat >"$command_log" <<EOF
OPEN64_DSL_SHAPE_WP5_UNIT=MODE $producer
OPEN64_DSL_SHAPE_WP5_PRODUCER=MODE OPEN64_DSL_SHAPE_WP5_ARTIFACT=INPUT.B $producer
$ir_b2a -st -src INPUT.B INPUT.T
LD_LIBRARY_PATH=$(dirname "$be"):${LD_LIBRARY_PATH:-} $be -show -O0 -DSL:dump_after_shape_refine=on -ft,TRACE -FHE:checkpoint=OUTPUT.B INPUT.B
$ir_b2a -st -src OUTPUT.B OUTPUT.T
diff -u INPUT.T OUTPUT.T
EOF

unit_dir="$artifact_dir/unit"
mkdir -p "$unit_dir"
for mode in \
  ab-success-success ba-success-success \
  ab-complete-refinable ba-complete-refinable \
  ab-success-failure ba-success-failure; do
  final_artifact="$unit_dir/$mode.must-not-exist.B"
  OPEN64_DSL_SHAPE_WP5_UNIT="$mode" \
  OPEN64_DSL_SHAPE_WP5_ARTIFACT="$final_artifact" \
    "$producer" >"$unit_dir/$mode.log" 2>&1
  require_text "$unit_dir/$mode.log" "collision=1"
  require_text "$unit_dir/$mode.log" "first_isolated=1"
  require_text "$unit_dir/$mode.log" "second_valid=1"
  require_text "$unit_dir/$mode.log" "artifact_absent=1 valid=1"
  [[ ! -e "$final_artifact" ]] ||
    fail "supplemental unit case published $final_artifact"
  if [[ "$mode" == *success-failure ]]; then
    require_text "$unit_dir/$mode.log" "rollback=1"
    require_text "$unit_dir/$mode.log" "diagnostic_008=1"
    if grep -Fq "DSL-SHAPE-RETYPE-007:" "$unit_dir/$mode.log"; then
      fail "unit rollback used preflight diagnostic 007"
    fi
  fi
done

run_success_case()
{
  local mode="$1"
  local first="$2"
  local second="$3"
  local first_refinable="$4"
  local second_refinable="$5"
  local case_dir="$artifact_dir/success-$mode"
  local input="$case_dir/input.B"
  local input_text="$case_dir/input.T"
  local output="$case_dir/output.final.B"
  local output_text="$case_dir/output.final.T"
  local backend_trace="$case_dir/backend.trc"
  local log="$case_dir/backend.log"
  local raw_diff="$case_dir/input-output.diff"
  mkdir -p "$case_dir"

  OPEN64_DSL_SHAPE_WP5_PRODUCER="$mode" \
  OPEN64_DSL_SHAPE_WP5_ARTIFACT="$input" \
    "$producer" >"$case_dir/producer.log" 2>&1
  require_colliding_full_st_idx "$case_dir/producer.log"
  "$ir_b2a" -st -src "$input" "$input_text"

  if ! LD_LIBRARY_PATH="$(dirname "$be"):${LD_LIBRARY_PATH:-}" \
      "$be" -show -O0 "-DSL:dump_after_shape_refine=on" \
      "-ft,$backend_trace" "-FHE:checkpoint=$output" "$input" \
      >"$log" 2>&1; then
    cat "$log" >&2
    fail "backend rejected success case $mode"
  fi
  [[ -f "$output" ]] || fail "success case did not publish $output"
  [[ ! -e "$output.tmp" ]] || fail "success case retained $output.tmp"
  "$ir_b2a" -st -src "$output" "$output_text"
  if diff -u "$input_text" "$output_text" >"$raw_diff"; then
    fail "shape refinement did not change $mode"
  else
    local diff_status=$?
    [[ "$diff_status" == "1" ]] || fail "could not diff $mode traces"
  fi

  require_count "$log" "Compiling $first(" 1
  require_count "$log" "Compiling $second(" 1
  require_count "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$first" 1
  require_count "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$second" 1
  require_text "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$first nodes=1 values=2 refinable=$first_refinable"
  require_text "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$second nodes=1 values=2 refinable=$second_refinable"
  local first_compile_line
  local first_summary_line
  local second_compile_line
  local second_summary_line
  first_compile_line="$(grep -nF "Compiling $first(" "$log" | cut -d: -f1)"
  first_summary_line="$(grep -nF "DSL-SHAPE-REFINE-SUMMARY: pu=$first" "$log" | cut -d: -f1)"
  second_compile_line="$(grep -nF "Compiling $second(" "$log" | cut -d: -f1)"
  second_summary_line="$(grep -nF "DSL-SHAPE-REFINE-SUMMARY: pu=$second" "$log" | cut -d: -f1)"
  (( first_compile_line < first_summary_line &&
     first_summary_line < second_compile_line &&
     second_compile_line < second_summary_line )) ||
    fail "backend PU order changed for $mode"
  require_count "$backend_trace" "WHIRL after VHO DSL Shape Refinement" 2
  require_text "$backend_trace" "$first"
  require_text "$backend_trace" "$second"
  require_text "$log" "FHE conversion checkpoint: output=$output pu=2 semantic_gates=0 passes=0"
  [[ "$(grep -c '^FUNC_ENTRY' "$input_text")" == "2" ]] ||
    fail "input $mode does not contain two PUs"
  [[ "$(grep -c '^FUNC_ENTRY' "$output_text")" == "2" ]] ||
    fail "output $mode does not contain two PUs"
  grep -E "name=shape_wp5_a_result.*shape=\[2,3\]" "$output_text" >/dev/null ||
    fail "shape_wp5_a result was not refined in $mode"
  grep -E "name=shape_wp5_b_result.*shape=\[2,5\]" "$output_text" >/dev/null ||
    fail "shape_wp5_b result was not refined in $mode"
}

run_failure_case()
{
  local mode="$1"
  local first="$2"
  local second="$3"
  local case_dir="$artifact_dir/failure-$mode"
  local input="$case_dir/input.B"
  local input_text="$case_dir/input.T"
  local output="$case_dir/output.final.B"
  local backend_trace="$case_dir/backend.partial.trc"
  local log="$case_dir/backend.failure.log"
  mkdir -p "$case_dir"

  OPEN64_DSL_SHAPE_WP5_PRODUCER="$mode" \
  OPEN64_DSL_SHAPE_WP5_ARTIFACT="$input" \
    "$producer" >"$case_dir/producer.log" 2>&1
  require_colliding_full_st_idx "$case_dir/producer.log"
  "$ir_b2a" -st -src "$input" "$input_text"
  sha256sum "$input" >"$case_dir/input.before.sha256"

  set +e
  LD_LIBRARY_PATH="$(dirname "$be"):${LD_LIBRARY_PATH:-}" \
  OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_AFTER_WRITE=1 \
    "$be" -show -O0 "-DSL:dump_after_shape_refine=on" \
    "-ft,$backend_trace" "-FHE:checkpoint=$output" "$input" \
    >"$log" 2>&1
  local status=$?
  set -e
  printf 'exit_status=%s\n' "$status" >"$case_dir/exit-status.txt"
  (( status != 0 )) || fail "backend accepted failure case $mode"

  require_count "$log" "Compiling $first(" 1
  require_count "$log" "Compiling $second(" 1
  require_count "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$first" 1
  require_count "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$second" 1
  require_text "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$first nodes=1 values=2 refinable=0"
  require_text "$log" "DSL-SHAPE-REFINE-SUMMARY: pu=$second nodes=1 values=2 refinable=1"
  local first_compile_line
  local first_summary_line
  local second_compile_line
  local second_summary_line
  first_compile_line="$(grep -nF "Compiling $first(" "$log" | cut -d: -f1)"
  first_summary_line="$(grep -nF "DSL-SHAPE-REFINE-SUMMARY: pu=$first" "$log" | cut -d: -f1)"
  second_compile_line="$(grep -nF "Compiling $second(" "$log" | cut -d: -f1)"
  second_summary_line="$(grep -nF "DSL-SHAPE-REFINE-SUMMARY: pu=$second" "$log" | cut -d: -f1)"
  (( first_compile_line < first_summary_line &&
     first_summary_line < second_compile_line &&
     second_compile_line < second_summary_line )) ||
    fail "backend PU order changed for failing case $mode"
  require_text "$log" "DSL-SHAPE-RETYPE-INJECT: after_write=1 action=fail"
  require_text "$log" "DSL-SHAPE-RETYPE-008:"
  if grep -Fq "DSL-SHAPE-RETYPE-007:" "$log"; then
    fail "backend rollback used preflight diagnostic 007"
  fi
  require_count "$backend_trace" \
    "WHIRL after VHO DSL Shape Refinement" 1
  grep -Eq "^FUNC_ENTRY <[0-9]+,[0-9]+,$first>" "$backend_trace" ||
    fail "successful trace dump is not bound to first PU $first"
  if grep -Fq "$second" "$backend_trace"; then
    fail "failed later PU published a successful dump for $second"
  fi
  [[ ! -e "$output" && ! -e "$output.tmp" ]] ||
    fail "failed backend published a final or temporary checkpoint"
  mapfile -t checkpoint_remnants < <(
    find "$case_dir" -mindepth 1 -maxdepth 1 \
      -name "$(basename "$output")*" -print
  )
  (( ${#checkpoint_remnants[@]} == 0 )) ||
    fail "failed backend retained checkpoint output or auxiliary files"
  mapfile -t unexpected_files < <(
    find "$case_dir" -mindepth 1 -maxdepth 1 \
      ! -name input.B ! -name input.T ! -name input.before.sha256 \
      ! -name producer.log ! -name backend.failure.log \
      ! -name backend.partial.trc ! -name exit-status.txt -print
  )
  (( ${#unexpected_files[@]} == 0 )) ||
    fail "failed backend retained an unexpected auxiliary artifact"
  sha256sum -c "$case_dir/input.before.sha256" >/dev/null
  {
    printf 'final_artifact_absent=%s\n' "$([[ ! -e "$output" ]] && echo 1 || echo 0)"
    printf 'temporary_artifact_absent=%s\n' "$([[ ! -e "$output.tmp" ]] && echo 1 || echo 0)"
    printf 'checkpoint_auxiliary_artifacts_absent=1\n'
    printf 'unexpected_directory_artifacts_absent=1\n'
    printf 'input_unchanged=1\n'
  } >"$case_dir/absence-report.txt"
}

run_success_case ab-both-refinable shape_wp5_a shape_wp5_b 1 1
run_success_case ba-both-refinable shape_wp5_b shape_wp5_a 1 1
run_success_case ab-complete-refinable shape_wp5_a shape_wp5_b 0 1
run_success_case ba-complete-refinable shape_wp5_b shape_wp5_a 0 1
run_failure_case ab-complete-refinable shape_wp5_a shape_wp5_b
run_failure_case ba-complete-refinable shape_wp5_b shape_wp5_a

cat >"$artifact_dir/certification.txt" <<EOF
scope=PR137_WP5_shape_subsystem_only
backend_owned_single_process_two_pu=passed
colliding_complete_local_st_idx=passed
success_success_ab_ba=passed
complete_refinable_ab_ba=passed
later_pu_failure_ab_ba=passed
per_pu_exactly_once=passed
first_pu_snapshot_isolated=passed
failed_checkpoint_final_absent=passed
failed_checkpoint_temporary_absent=passed
subprocess_per_pu=not_used
shape_owned_pu_traversal=not_added
all_pu_rollback=not_added
fhe_milestone_certification=not_claimed
validation_lane=$validation_lane
EOF

{
  printf 'source_commit=%s\n' "$(git -C "$repo_root" rev-parse HEAD)"
  printf 'source_status_begin\n'
  git -C "$repo_root" status --short
  printf 'source_status_end\n'
  printf 'fixture_source_sha256=%s\n' "$(sha256sum "$repo_root/osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx" | cut -d' ' -f1)"
  printf 'driver_test_source_sha256=%s\n' "$(sha256sum "$repo_root/osprey/common/com/tests/dsl_shape_multi_pu_driver_test.sh" | cut -d' ' -f1)"
  printf 'producer=%s\n' "$producer"
  printf 'producer_sha256=%s\n' "$(sha256sum "$producer" | cut -d' ' -f1)"
  printf 'be=%s\n' "$be"
  printf 'be_sha256=%s\n' "$(sha256sum "$be" | cut -d' ' -f1)"
  printf 'ir_b2a=%s\n' "$ir_b2a"
  printf 'ir_b2a_sha256=%s\n' "$(sha256sum "$ir_b2a" | cut -d' ' -f1)"
  printf 'uname=%s\n' "$(uname -a)"
  printf 'os_release=%s\n' "$(tr '\n' ' ' </etc/os-release)"
  printf 'cc=%s\n' "$(cc --version | head -1)"
  printf 'cxx=%s\n' "$(c++ --version | head -1)"
  printf 'validation_lane=%s\n' "$validation_lane"
  printf 'ld_library_path=%s\n' "$(dirname "$be"):${LD_LIBRARY_PATH:-}"
} >"$artifact_dir/MANIFEST.txt"
find "$artifact_dir" -type f ! -name SHA256SUMS -print0 | \
  sort -z | xargs -0 sha256sum >"$artifact_dir/SHA256SUMS"

echo "WP5 backend-owned multi-PU shape refinement passed"
echo "review evidence: $artifact_dir"
