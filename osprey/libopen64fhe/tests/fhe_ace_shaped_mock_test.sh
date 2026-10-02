#!/usr/bin/env bash
# Link the Open64 ACE evaluator adapter against the local ACE-shaped mock.
# Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ace-shaped-mock}
mkdir -p "$out_dir"

c++ -std=c++11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/include" \
    -I"$repo_root/osprey/libopen64fhe" \
    "$repo_root/osprey/libopen64fhe/open64_fhe_ace_eval_adapter.cxx" \
    "$repo_root/osprey/libopen64fhe/open64_fhe_ace_mock.cxx" \
    "$repo_root/osprey/libopen64fhe/tests/fhe_ace_shaped_mock_test.cxx" \
    -o "$out_dir/fhe_ace_shaped_mock_test" \
    >"$out_dir/build.log" 2>&1

"$out_dir/fhe_ace_shaped_mock_test" >"$out_dir/run.log" 2>&1
nm -C "$out_dir/fhe_ace_shaped_mock_test" >"$out_dir/symbols.txt"
rg -q ' [_]?Add_ciph$' "$out_dir/symbols.txt"
rg -q ' [_]?Bootstrap$' "$out_dir/symbols.txt"
! rg -q 'Prepare_context|Generate_secret_key|Decrypt|Handle_output' \
    "$out_dir/symbols.txt"
printf 'ACE-shaped adapter/mock arithmetic and ownership tests passed.\n'
printf 'Evidence: %s\n' "$out_dir/symbols.txt"
