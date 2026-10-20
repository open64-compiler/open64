#!/usr/bin/env bash
# Compile and run the S6-0d D3 private-output checkpoint transaction.
# Design: doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D3/S6D-T3.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks2c-checkpoint}
mkdir -p "$out_dir"

common_flags=(
  -DKEY -D__MIPS_AND_IA64_ELF_H
  -Wno-unknown-pragmas -Wno-reorder -Wno-unused-parameter
  -Wno-class-memaccess
  -I"$repo_root/osprey"
  -I"$repo_root/osprey/be/vho"
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/common/fhe"
  -I"$repo_root/osprey/include"
)

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror "${common_flags[@]}" \
    "$repo_root/osprey/be/vho/fhe_sha256.cxx" \
    "$repo_root/osprey/be/vho/fhe_checkpoint.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks_plan_bytes.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks2c_emit.cxx" \
    "$repo_root/osprey/be/vho/fhe_ckks2c_publish.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks2c_checkpoint_test.cxx" \
    -o "$out_dir/fhe_ckks2c_checkpoint_test" \
    >"$out_dir/build.log" 2>&1

"$out_dir/fhe_ckks2c_checkpoint_test" \
    "$out_dir/artifacts" "${CC:-cc}" "$repo_root/osprey/include" \
    >"$out_dir/run.log" 2>&1

grep -qx 'FHE CKKS2C checkpoint publication passed: cases=6 artifacts=4' \
    "$out_dir/run.log"
grep -qx 'format=open64.ckks2c.facade-manifest.v1' \
    "$out_dir/artifacts/success/fixture.ckks2c-facade-manifest.txt"
grep -qx 'pu_count=2' \
    "$out_dir/artifacts/success/fixture.ckks2c-report.txt"
grep -qx 'event_count=18' \
    "$out_dir/artifacts/success/fixture.ckks2c-report.txt"
grep -qx 'source_file=fixture.ckks2c.c' \
    "$out_dir/artifacts/success/fixture.ckks2c-compile-command.txt"
grep -qx 'object_file=fixture.ckks2c.o' \
    "$out_dir/artifacts/success/fixture.ckks2c-compile-command.txt"
grep -Fqx 'command=${CC:-cc} -std=c11 -Wall -Wextra -Werror -I${OPEN64_ROOT}/osprey/include -c "${CKKS2C_C}" -o "${CKKS2C_OBJECT}"' \
    "$out_dir/artifacts/success/fixture.ckks2c-compile-command.txt"
grep -q 'open64_fhe_ckks2c_bootstrap_v1' \
    "$out_dir/artifacts/success/fixture.ckks2c.c"

printf 'FHE CKKS2C checkpoint publication passed.\n'
printf 'Private C: %s\n' \
    "$out_dir/artifacts/success/fixture.ckks2c.c"
printf 'Group report: %s\n' \
    "$out_dir/artifacts/success/fixture.ckks2c-report.txt"
printf 'Evidence: %s\n' "$out_dir/run.log"
