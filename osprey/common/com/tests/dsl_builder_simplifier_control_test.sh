#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
cxx="${CXX:-g++}"
output="${TMPDIR:-/tmp}/dsl_builder_simplifier_control_test"

"$cxx" -std=gnu++98 \
  -DKEY \
  -D__MIPS_AND_IA64_ELF_H \
  -ffunction-sections \
  -fdata-sections \
  -I"$repo_root/osprey/linux/include" \
  -I"$repo_root/osprey/ir_tools" \
  -I"$repo_root/osprey/common/com" \
  -I"$repo_root/osprey/common/com/x8664" \
  -I"$repo_root/osprey/common/util" \
  -I"$repo_root/osprey/include" \
  -I"$repo_root/osprey/libdwarf/libdwarf" \
  "$repo_root/osprey/common/com/dsl_builder.cxx" \
  "$repo_root/osprey/common/com/tests/dsl_builder_simplifier_control_test.cxx" \
  -Wl,--gc-sections \
  -o "$output"

"$output"
