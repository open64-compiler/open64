#!/usr/bin/env bash
# Build the provider-independent ANT ACE ReLU algebraic recipe fixture.
# Design: doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-relu-recipe}
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
    -I"$repo_root/osprey/be/vho" \
    -I"$repo_root/osprey/common/com" \
    -I"$repo_root/osprey/common/com/x8664" \
    -I"$repo_root/osprey/common/util" \
    -I"$repo_root/osprey/include" \
    -I"$repo_root/osprey" \
    "${extra_includes[@]}" \
    "$repo_root/osprey/be/vho/fhe_ckks_relu_recipe.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_relu_recipe_test.cxx" \
    -o "$out_dir/fhe_ckks_relu_recipe_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_relu_recipe_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS ANT ACE ReLU recipe fixture passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
