#!/usr/bin/env bash
# Link-test the FHE-owned CKKS expansion/state boundary.
# Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-expand-state}
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
    "$repo_root/osprey/be/vho/fhe_ckks_expand.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_expand_state_test.cxx" \
    -o "$out_dir/fhe_ckks_expand_state_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_expand_state_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS expansion state adapter passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
