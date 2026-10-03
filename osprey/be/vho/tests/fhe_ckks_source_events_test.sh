#!/usr/bin/env bash
# Link FHE source-event collection to read-only schedule/call-image substitutes.
# Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-source-events}
mkdir -p "$out_dir"

if command -v g++-15 >/dev/null 2>&1; then
  cxx=${CXX:-g++-15}
else
  cxx=${CXX:-g++}
fi
extra_includes=()
if [[ $(uname -s) == Darwin ]]; then
  extra_includes=(-I"$repo_root/osprey/jfe/libb2w/osprey-macos/macos/include")
fi
# Legacy Open64 headers trigger these four GCC diagnostics before this unit.
"$cxx" -std=gnu++11 -Wall -Wextra -Werror \
    -Wno-unknown-pragmas -Wno-reorder -Wno-unused-parameter \
    -Wno-class-memaccess \
    -DKEY -D__MIPS_AND_IA64_ELF_H \
    -I"$repo_root/osprey/linux/include" \
    -I"$repo_root/osprey/ir_tools" \
    -I"$repo_root/osprey/be/vho" \
    -I"$repo_root/osprey/common/com" \
    -I"$repo_root/osprey/common/fhe" \
    -I"$repo_root/osprey/common/com/x8664" \
    -I"$repo_root/osprey/common/util" \
    -I"$repo_root/osprey/include" \
    -I"$repo_root/osprey/libdwarf/libdwarf" \
    -I"$repo_root/osprey" \
    "${extra_includes[@]}" \
    "$repo_root/osprey/be/vho/fhe_ckks_event_coverage.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks_source_events.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks_relu_plan.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_source_events_test.cxx" \
    -o "$out_dir/fhe_ckks_source_events_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_source_events_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS read-only table API fixture passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
