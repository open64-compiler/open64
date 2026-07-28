#!/usr/bin/env bash
#
# Exercise the backend DSL WOPT hook with the same mapped WHIRL image.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_root="${OPEN64_BUILD_ROOT:-$repo_root/build/osprey/targdir}"
producer="${OPEN64_DSL_WOPT_PRODUCER:-$build_root/ir_tools/dsl_wopt_bridge_test}"
ir_b2a="${OPEN64_IR_B2A:-$build_root/ir_tools/ir_b2a}"
backend="${OPEN64_BE:-$build_root/be/be}"
wopt_dir="${OPEN64_WOPT_DIR:-$build_root/wopt}"
artifact_dir="${OPEN64_DSL_WOPT_ARTIFACT_DIR:-${TMPDIR:-/tmp}/open64-dsl-wopt-driver}"
source_file="$repo_root/osprey/be/opt/tests/dsl_wopt_bridge_test.cxx"

require_executable()
{
  if [[ ! -x "$1" ]]; then
    echo "missing executable: $1" >&2
    exit 1
  fi
}

require_text()
{
  local file="$1"
  local pattern="$2"

  if ! grep -Fq "$pattern" "$file"; then
    echo "missing DSL WOPT evidence '$pattern' in $file" >&2
    exit 1
  fi
}

require_executable "$producer"
require_executable "$ir_b2a"
require_executable "$backend"
if [[ ! -f "$wopt_dir/wopt.so" ]]; then
  echo "missing WOPT shared library: $wopt_dir/wopt.so" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

input_image="$artifact_dir/dsl_wopt_input.B"
input_text="$artifact_dir/dsl_wopt_input.T"
off_object="$artifact_dir/dsl_wopt_off.o"
off_image="$artifact_dir/dsl_wopt_off.O"
off_text="$artifact_dir/dsl_wopt_off.T"
off_trace="$artifact_dir/dsl_wopt_off.trc"
on_object="$artifact_dir/dsl_wopt_on.o"
on_image="$artifact_dir/dsl_wopt_on.O"
on_text="$artifact_dir/dsl_wopt_on.T"
on_trace="$artifact_dir/dsl_wopt_on.trc"
factor_input_image="$artifact_dir/dsl_wopt_factor_input.B"
factor_input_text="$artifact_dir/dsl_wopt_factor_input.T"
factor_on_object="$artifact_dir/.dsl_wopt_factor_on.tmp.o"
factor_on_trace="$artifact_dir/dsl_wopt_factor_on.trc"
factor_on_stderr="$artifact_dir/dsl_wopt_factor_on.stderr"
factor_simp_off_object="$artifact_dir/.dsl_wopt_factor_simp_off.tmp.o"
factor_simp_off_trace="$artifact_dir/dsl_wopt_factor_simp_off.trc"
factor_simp_off_stderr="$artifact_dir/dsl_wopt_factor_simp_off.stderr"
no_factor_input_image="$artifact_dir/dsl_wopt_no_factor_input.B"
no_factor_input_text="$artifact_dir/dsl_wopt_no_factor_input.T"
no_factor_on_object="$artifact_dir/.dsl_wopt_no_factor_on.tmp.o"
no_factor_on_trace="$artifact_dir/dsl_wopt_no_factor_on.trc"
no_factor_on_stderr="$artifact_dir/dsl_wopt_no_factor_on.stderr"
divrem_input_image="$artifact_dir/dsl_wopt_divrem_input.B"
divrem_input_text="$artifact_dir/dsl_wopt_divrem_input.T"
divrem_on_object="$artifact_dir/.dsl_wopt_divrem_on.tmp.o"
divrem_on_trace="$artifact_dir/dsl_wopt_divrem_on.trc"
divrem_on_stderr="$artifact_dir/dsl_wopt_divrem_on.stderr"
divrem_off_object="$artifact_dir/.dsl_wopt_divrem_off.tmp.o"
divrem_off_trace="$artifact_dir/dsl_wopt_divrem_off.trc"
divrem_off_stderr="$artifact_dir/dsl_wopt_divrem_off.stderr"

"$producer" "$input_image"
"$ir_b2a" -st -src "$input_image" "$input_text"

run_backend()
{
  local input="$1"
  local enabled="$2"
  local object="$3"
  local trace="$4"
  local cr_simp="${5:-on}"
  local divrem="${6:-on}"

  LD_LIBRARY_PATH="$(dirname "$backend"):$wopt_dir:${LD_LIBRARY_PATH:-}" \
    "$backend" \
      "-fB,$input" \
      "-fo,$object" \
      "-ft,$trace" \
      "-PHASE:w=on:c=off:wpath=$wopt_dir" \
      -O2 \
      "-DSL:wopt=$enabled" \
      "-WOPT:cr_simp=$cr_simp" \
      "-WOPT:divrem=$divrem" \
      -tr25 \
      "$source_file"
}

run_backend "$input_image" off "$off_object" "$off_trace"
run_backend "$input_image" on "$on_object" "$on_trace"

"$ir_b2a" -st -src "$off_image" "$off_text"
"$ir_b2a" -st -src "$on_image" "$on_text"

require_text "$input_text" "OPR_DSLADD"
require_text "$off_trace" "__open64_dsl_add_v1"
if grep -Fq "Driver dump after DSL_WOPT" "$off_trace"; then
  echo "disabled DSL WOPT unexpectedly executed" >&2
  exit 1
fi
require_text "$on_trace" "Driver dump after DSL_WOPT"
require_text "$on_trace" "OPR_DSLTENSORCONST"
require_text "$on_trace" "name=wopt_add"
require_text "$on_trace" "value=1"
if grep -Fq "OPR_DSLADD" "$on_trace"; then
  echo "enabled DSL WOPT did not fold common.add" >&2
  exit 1
fi

"$producer" "$factor_input_image" factor
"$ir_b2a" -st -src "$factor_input_image" "$factor_input_text"
if run_backend "$factor_input_image" on \
     "$factor_on_object" "$factor_on_trace" 2>"$factor_on_stderr"; then
  echo "factorization fixture unexpectedly passed marker-only lowering" >&2
  exit 1
fi
rm -f "$factor_on_object" "${factor_on_object%.o}.O"

require_text "$factor_input_text" \
  "payload=kid0=wopt_xy;kid1=wopt_xz;attr.broadcast_rule=none"
require_text "$factor_on_trace" \
  "payload=kid0=wopt_y;kid1=wopt_z;attr.broadcast_rule=none"
require_text "$factor_on_trace" \
  "payload=kid0=wopt_x;kid1=wopt_xy;attr.broadcast_rule=none"
require_text "$factor_on_stderr" \
  "OPR_DSLMUL.v1 has no executable VHO lowering route"

if run_backend "$factor_input_image" on \
     "$factor_simp_off_object" "$factor_simp_off_trace" off \
     2>"$factor_simp_off_stderr"; then
  echo "simplifier-disabled fixture unexpectedly passed lowering" >&2
  exit 1
fi
rm -f "$factor_simp_off_object" "${factor_simp_off_object%.o}.O"
require_text "$factor_simp_off_trace" \
  "payload=kid0=wopt_x;kid1=wopt_y;attr.broadcast_rule=none"
require_text "$factor_simp_off_trace" \
  "payload=kid0=wopt_x;kid1=wopt_z;attr.broadcast_rule=none"
require_text "$factor_simp_off_trace" \
  "payload=kid0=wopt_xy;kid1=wopt_xz;attr.broadcast_rule=none"
require_text "$factor_simp_off_stderr" \
  "OPR_DSLMUL.v1 has no executable VHO lowering route"

"$producer" "$no_factor_input_image" no-factor
"$ir_b2a" -st -src "$no_factor_input_image" "$no_factor_input_text"
if run_backend "$no_factor_input_image" on \
     "$no_factor_on_object" "$no_factor_on_trace" \
     2>"$no_factor_on_stderr"; then
  echo "no-factor fixture unexpectedly passed marker-only lowering" >&2
  exit 1
fi
rm -f "$no_factor_on_object" "${no_factor_on_object%.o}.O"

require_text "$no_factor_on_trace" \
  "payload=kid0=wopt_x;kid1=wopt_y;attr.broadcast_rule=none"
require_text "$no_factor_on_trace" \
  "payload=kid0=wopt_z;kid1=wopt_z;attr.broadcast_rule=none"
require_text "$no_factor_on_trace" \
  "payload=kid0=wopt_xy;kid1=wopt_xz;attr.broadcast_rule=none"
require_text "$no_factor_on_stderr" \
  "OPR_DSLMUL.v1 has no executable VHO lowering route"

"$producer" "$divrem_input_image" divrem
"$ir_b2a" -st -src "$divrem_input_image" "$divrem_input_text"
if run_backend "$divrem_input_image" on \
     "$divrem_on_object" "$divrem_on_trace" on on \
     2>"$divrem_on_stderr"; then
  echo "DIVREM target-declined fixture unexpectedly passed lowering" >&2
  exit 1
fi
rm -f "$divrem_on_object" "${divrem_on_object%.o}.O"
if run_backend "$divrem_input_image" on \
     "$divrem_off_object" "$divrem_off_trace" on off \
     2>"$divrem_off_stderr"; then
  echo "DIVREM option-disabled fixture unexpectedly passed lowering" >&2
  exit 1
fi
rm -f "$divrem_off_object" "${divrem_off_object%.o}.O"

require_text "$divrem_input_text" "OPR_DSLDIV"
require_text "$divrem_input_text" "OPR_DSLREM"
require_text "$divrem_on_trace" "OPR_DSLDIV"
require_text "$divrem_on_trace" "OPR_DSLREM"
require_text "$divrem_off_trace" "OPR_DSLDIV"
require_text "$divrem_off_trace" "OPR_DSLREM"
if grep -Fq "OPR_DSLDIVREM" "$divrem_on_trace" ||
   grep -Fq "OPR_DSLDIVREM" "$divrem_off_trace"; then
  echo "target-declined DSL DIVREM reached emitted WHIRL" >&2
  exit 1
fi
require_text "$divrem_on_stderr" \
  "OPR_DSLDIV.v1 has no executable VHO lowering route"
require_text "$divrem_off_stderr" \
  "OPR_DSLDIV.v1 has no executable VHO lowering route"

echo "backend DSL WOPT option and phase-order fixture passed"
echo "review artifacts: $artifact_dir"
