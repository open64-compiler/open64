#!/usr/bin/env bash
# Check SP10 isolation in a native producer and backend process.
set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
targets="$repo_root/build/osprey/targdir"
producer="${OPEN64_DSL_SHAPE_REFINE_TEST:-$targets/ir_tools/dsl_shape_refine_contract_test}"
backend="${OPEN64_DSL_SHAPE_MULTI_PU_BE:-$targets/be/be}"
ir_b2a="${OPEN64_IR_B2A:-$targets/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_MULTI_PU_ARTIFACT_DIR:-$repo_root/artifacts/shape/multi-pu-driver}"

for executable in "$producer" "$backend" "$ir_b2a"; do
  [[ -x "$executable" ]] || { echo "missing executable: $executable" >&2; exit 1; }
done

for order in ab ba; do
  case_dir="$artifact_dir/$order"
  mkdir -p "$case_dir"
  find "$case_dir" -mindepth 1 -maxdepth 1 -type f -delete

  for stage in before after; do
    image="$case_dir/$order.$stage.B"
    OPEN64_DSL_SHAPE_MULTI_PU_MODE="$order-$stage" \
      OPEN64_DSL_SHAPE_MULTI_PU_ARTIFACT="$image" \
      "$producer" >"$case_dir/producer.$stage.log" 2>&1
    "$ir_b2a" -st -src "$image" "${image%.B}.T"
  done
  if diff -u "$case_dir/$order.before.T" "$case_dir/$order.after.T" \
      >"$case_dir/$order.before-after.diff"; then
    echo "shape refinement produced an empty diff for $order" >&2
    exit 1
  else
    [[ $? -eq 1 ]]
  fi

  output="$case_dir/$order.checkpoint.B"
  LD_LIBRARY_PATH="$(dirname "$backend"):${LD_LIBRARY_PATH:-}" \
    "$backend" -show -O0 -DSL:dump_after_shape_refine=on \
    "-ft,$case_dir/$order.backend.trc" "-FHE:checkpoint=$output" \
    "$case_dir/$order.before.B" >"$case_dir/backend.log" 2>&1
  "$ir_b2a" -st -src "$output" "${output%.B}.T"
  first="shape_wp5_${order:0:1}"
  second="shape_wp5_${order:1:1}"
  awk -v first="$first" -v second="$second" '
    /DSL-SHAPE-REFINE-SUMMARY:/ {
      if (index($0, "pu=" first " ")) { ++seen_first; first_line = NR }
      if (index($0, "pu=" second " ")) { ++seen_second; second_line = NR }
    }
    END { exit !(seen_first == 1 && seen_second == 1 && first_line < second_line) }
  ' "$case_dir/backend.log"
done
printf 'SP10 multi-PU artifacts: %s\n' "$artifact_dir"
