#!/usr/bin/env bash
# Test the FHE-owned CKKS event-to-step coverage preflight without shared IR.
# Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-event-coverage}
mkdir -p "$out_dir"

c++ -std=c++11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/be/vho" \
    "$repo_root/osprey/be/vho/fhe_ckks_event_coverage.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_event_coverage_test.cxx" \
    -o "$out_dir/fhe_ckks_event_coverage_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_event_coverage_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS event-to-step coverage preflight passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
