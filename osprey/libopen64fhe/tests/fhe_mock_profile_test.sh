#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../../.." && pwd)"
build_dir="${TMPDIR:-/tmp}/open64-fhe-mock-profile-test"

mkdir -p "$build_dir"
c++ -std=c++11 -Wall -Wextra -Werror \
  -I"$repo_root/osprey/include" \
  -I"$repo_root/osprey/libopen64fhe" \
  "$repo_root/osprey/libopen64fhe/open64_fhe_mock_sha256.cxx" \
  "$repo_root/osprey/libopen64fhe/open64_fhe_mock_runtime.cxx" \
  "$repo_root/osprey/libopen64fhe/tests/fhe_mock_profile_test.cxx" \
  -o "$build_dir/fhe_mock_profile_test"

"$build_dir/fhe_mock_profile_test"
echo "FHE mock 147-call profile test passed"
