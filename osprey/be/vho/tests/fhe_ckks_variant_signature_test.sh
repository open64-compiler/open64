#!/usr/bin/env bash
# Test the FHE complete-signature grouping policy without mutating WHIRL.
# Design: doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:-/private/tmp/open64-fhe-ckks-variant-signature}
mkdir -p "$out_dir"

"${CXX:-c++}" -std=c++11 -Wall -Wextra -Werror \
    -I"$repo_root/osprey/be/vho" \
    "$repo_root/osprey/be/vho/fhe_ckks_variant_signature.cxx" \
    "$repo_root/osprey/be/vho/tests/fhe_ckks_variant_signature_test.cxx" \
    -o "$out_dir/fhe_ckks_variant_signature_test" \
    >"$out_dir/build.log" 2>&1
"$out_dir/fhe_ckks_variant_signature_test" >"$out_dir/run.log" 2>&1
printf 'FHE CKKS variant signature grouping passed.\n'
printf 'Evidence: %s\n' "$out_dir/run.log"
