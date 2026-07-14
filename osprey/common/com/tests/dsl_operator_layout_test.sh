#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
cxx="${CXX:-g++}"
source_file="$script_dir/dsl_operator_layout_test.cxx"
operator_info_source="$repo_root/osprey/common/com/opcode_gen_core.cxx"
image_table_source="$repo_root/osprey/common/com/dsl_ir_image.cxx"

common_flags=(
  -std=gnu++98
  -fsyntax-only
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/include"
)

compile_layout() {
  local name="$1"
  shift
  "$cxx" "${common_flags[@]}" "$@" "$source_file"
  "$cxx" "${common_flags[@]}" "$@" "$operator_info_source"
  "$cxx" "${common_flags[@]}" "$@" "$image_table_source"
  echo "operator layout ok: $name"
}

compile_layout x8664 -DTARG_X8664 -DKEY
compile_layout mips -DTARG_MIPS
compile_layout mips-sl -DTARG_MIPS -DTARG_SL
compile_layout key-generic -DKEY
compile_layout loongson -DKEY -DTARG_LOONGSON
compile_layout baseline

echo "DSL physical operator compatibility matrix passed"
