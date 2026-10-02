#!/usr/bin/env bash
# Verify the S6-1 ACE source probe stays deterministic and fails closed.
# Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../.." && pwd)
ace_root=${1:?pass the pinned ACE source checkout}
out_dir=${2:-$(mktemp -d /private/tmp/open64-fhe-sync6-admission.XXXXXX)}
mkdir -p "$out_dir"
probe="$repo_root/osprey/libopen64fhe/tests/fhe_ace_ant_admission.py"

python3 "$probe" --ace-root "$ace_root" --output "$out_dir/ace-source-admission.json"
python3 "$probe" --ace-root "$ace_root" --output "$out_dir/ace-source-admission-repeat.json"
cmp "$out_dir/ace-source-admission.json" "$out_dir/ace-source-admission-repeat.json"

set +e
python3 "$probe" --ace-root "$ace_root" --require-admitted \
    --output "$out_dir/ace-source-admission-required.json" \
    >"$out_dir/admission.stdout" 2>"$out_dir/admission.stderr"
status=$?
set -e
test "$status" -eq 3
cmp "$out_dir/ace-source-admission.json" "$out_dir/ace-source-admission-required.json"

wrong_pin="$out_dir/wrong-pin"
mkdir -p "$wrong_pin"
git -C "$wrong_pin" init -q
git -C "$wrong_pin" -c user.name=Open64 -c user.email=open64@example.invalid \
    commit -q --allow-empty -m 'Wrong ACE pin fixture'
set +e
python3 "$probe" --ace-root "$wrong_pin" \
    >"$out_dir/wrong-pin.stdout" 2>"$out_dir/wrong-pin.stderr"
status=$?
set -e
test "$status" -eq 2
rg -q ACE_REVISION_MISMATCH "$out_dir/wrong-pin.stderr"

python3 - "$out_dir/ace-source-admission.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    report = json.load(stream)
assert report["admitted"] is False
assert report["rtlib_tracked_file_count"] > 0
assert all(report["source_observations"].values())
assert all(report["capabilities"][name] == "not_demonstrated" for name in (
    "evaluation_only_context_import",
    "non_secret_keyset_import",
    "ciphertext_import",
    "ciphertext_export",
))
PY

printf 'S6-1 pinned-source admission probe passed; provider remains blocked.\n'
printf 'Evidence: %s\n' "$out_dir/ace-source-admission.json"
