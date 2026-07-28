#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
tmp_dir="${TMPDIR:-/tmp}/open64-dsl-wopt-semantic-$$"
trap 'rm -rf "$tmp_dir"' EXIT
mkdir -p "$tmp_dir"

cxx="${CXX:-g++}"
"$cxx" -std=gnu++98 \
  -I"$repo_root/osprey/common/com" \
  -I"$repo_root/osprey/common/util" \
  -I"$repo_root/osprey/linux/include" \
  -I"$repo_root/osprey/be/opt" \
  "$repo_root/osprey/be/opt/opt_dsl.cxx" \
  "$repo_root/osprey/be/opt/tests/dsl_wopt_semantic_info_test.cxx" \
  -o "$tmp_dir/dsl_wopt_semantic_info_test"

"$tmp_dir/dsl_wopt_semantic_info_test"
