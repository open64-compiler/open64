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

"$producer" "$input_image"
"$ir_b2a" -st -src "$input_image" "$input_text"

run_backend()
{
  local enabled="$1"
  local object="$2"
  local trace="$3"

  LD_LIBRARY_PATH="$(dirname "$backend"):$wopt_dir:${LD_LIBRARY_PATH:-}" \
    "$backend" \
      "-fB,$input_image" \
      "-fo,$object" \
      "-ft,$trace" \
      "-PHASE:w=on:c=off:wpath=$wopt_dir" \
      -O2 \
      "-DSL:wopt=$enabled" \
      -tr25 \
      "$source_file"
}

run_backend off "$off_object" "$off_trace"
run_backend on "$on_object" "$on_trace"

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

echo "backend DSL WOPT option and phase-order fixture passed"
echo "review artifacts: $artifact_dir"
