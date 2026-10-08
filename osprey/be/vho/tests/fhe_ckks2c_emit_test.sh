#!/usr/bin/env bash
# Compile deterministic CKKS2C output and link every facade primitive.
# Design: doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D1.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks2c-emit}
mkdir -p "$out_dir"

common_flags=(
  -DKEY -D__MIPS_AND_IA64_ELF_H
  -I"$repo_root/osprey/be/vho"
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/include"
)

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror "${common_flags[@]}" \
    "$repo_root/osprey/be/vho/fhe_ckks_plan_bytes.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks2c_emit.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks2c_emit_test.cxx" \
    -o "$out_dir/fhe_ckks2c_emit_test" >"$out_dir/build.log" 2>&1

"$out_dir/fhe_ckks2c_emit_test" "$out_dir/fixture.ckks.c" \
    >"$out_dir/run.log" 2>&1

"${CC:-cc}" -std=c11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/include" \
    "$out_dir/fixture.ckks.c" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks2c_facade_link_stub.c" \
    -o "$out_dir/fixture.ckks.link" >"$out_dir/c-build.log" 2>&1

printf 'FHE CKKS2C emission and facade link passed.\n'
printf 'Generated C: %s\n' "$out_dir/fixture.ckks.c"
printf 'Evidence: %s\n' "$out_dir/run.log"
