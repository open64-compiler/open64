#!/usr/bin/env bash
# Certify the provider-independent S6-0d closure and preserve the exact ANT
# admission boundary for S6-1. Design:
# doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D6.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../.." && pwd)
out_dir=${1:?usage: $0 output-dir d4-receipt-dir d5-route-dir ace-root}
d4_dir=${2:?usage: $0 output-dir d4-receipt-dir d5-route-dir ace-root}
d5_dir=${3:?usage: $0 output-dir d4-receipt-dir d5-route-dir ace-root}
ace_root=${4:?usage: $0 output-dir d4-receipt-dir d5-route-dir ace-root}

mkdir -p "$out_dir"
find "$out_dir" -mindepth 1 -maxdepth 1 -type f -delete
mkdir -p "$out_dir/ace-admission"

(cd "$d4_dir" && sha256sum -c SHA256SUMS) \
  >"$out_dir/d4-sha256.log"

d5_binary="$d5_dir/cached-ckks-route.ckks2c.B"
d5_trace="$d5_dir/cached-ckks-route.ckks2c.T"
d5_report="$d5_dir/cached-ckks-route.ckks2c-report.txt"
for artifact in "$d5_binary" "$d5_trace" "$d5_report" \
                "$d5_dir/cached-ckks-route.ckks2c.c" \
                "$d5_dir/cached-ckks-route.ckks2c-facade-manifest.txt" \
                "$d5_dir/cached-ckks-route.ckks2c-compile-command.txt"; do
  test -s "$artifact"
done
test ! -e "$d5_binary.tmp"
grep -Fq 'FHE-CKKS2C-ROUTE: selected=ckks2c' \
  "$d5_dir/cached-ckks-route.log"
grep -Fq 'FHE-CKKS2C-ROUTE: exit=ckks2c' \
  "$d5_dir/cached-ckks-route.log"
grep -Fq 'poly_entered=0' "$d5_dir/cached-ckks-route.log"
grep -qx 'pu_count=10' "$d5_report"
grep -qx 'event_count=33367' "$d5_report"
grep -qx 'public_schedule.static_descriptor_count=87' "$d5_report"
grep -qx 'public_schedule.dynamic_binding_count=147' "$d5_report"
if grep -Eq 'OPR_DSL|DSL CKKS Event Image:|FHE Plan Image:' "$d5_trace"; then
  echo "D5 terminal trace retained a private DSL/FHE surface" >&2
  exit 1
fi

generated_trace="$d4_dir/compile/generated_trace"
generated_mock="$d4_dir/compile/generated_mock"
"$generated_trace" >"$out_dir/generated-trace.log"
grep -Fxq 'SUMMARY selects=147 evals=147 output=set' \
  "$out_dir/generated-trace.log"
"$generated_mock" "$out_dir/generated-trace.log" \
  >"$out_dir/mock-positive.log"
grep -Fq 'MOCK positive=147 output=set' "$out_dir/mock-positive.log"
for mode in wrong-ordinal missing-resource missing-coefficient wrong-handle; do
  "$generated_mock" "$out_dir/generated-trace.log" "$mode" \
    >"$out_dir/mock-$mode.log"
  grep -Fxq "MOCK negative=$mode output=null" \
    "$out_dir/mock-$mode.log"
done

if [[ -n "${OPEN64_FHE_ACE_ADMISSION_JSON:-}" ]]; then
  test -s "$OPEN64_FHE_ACE_ADMISSION_JSON"
  cp "$OPEN64_FHE_ACE_ADMISSION_JSON" \
    "$out_dir/ace-admission/ace-source-admission.json"
  printf 'Reused separately certified exact-pin admission report: %s\n' \
    "$OPEN64_FHE_ACE_ADMISSION_JSON" >"$out_dir/ace-admission.log"
else
  bash "$repo_root/osprey/libopen64fhe/tests/fhe_ace_ant_admission_test.sh" \
    "$ace_root" "$out_dir/ace-admission" \
    >"$out_dir/ace-admission.log" 2>&1
fi
python3 - "$out_dir/ace-admission/ace-source-admission.json" \
  "$d4_dir/receipt.json" "$d5_report" "$out_dir/closure.txt" <<'PY'
import json
from pathlib import Path
import sys

admission = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
d4 = json.loads(Path(sys.argv[2]).read_text(encoding="utf-8"))
d5 = dict(
    line.split("=", 1)
    for line in Path(sys.argv[3]).read_text(encoding="utf-8").splitlines()
    if "=" in line
)
assert d4["status"] == "accepted_public_abi_lowering"
assert d4["standard_whirl_only"] is True
assert d4["census"]["dynamic_bindings"] == 147
assert d5["pu_count"] == "10"
assert d5["event_count"] == "33367"
assert d5["public_schedule.static_descriptor_count"] == "87"
assert d5["public_schedule.dynamic_binding_count"] == "147"
assert admission["admitted"] is False
assert admission["diagnostic"].startswith("CAPABILITY_MISSING:")

Path(sys.argv[4]).write_text(
    "Open64 FHE SYNC-6 S6-0d Closure\n"
    "=================================\n"
    "status=PROVIDER_INDEPENDENT_COMPILER_CLOSURE_PASSED\n"
    "d4_standard_whirl=passed\n"
    "d5_codegen_route=ckks2c\n"
    "d5_poly_entered=0\n"
    "physical_pus=10\n"
    "ckks_events=33367\n"
    "public_static_descriptors=87\n"
    "public_dynamic_bindings=147\n"
    "generated_trace=147_selects_147_evaluations_output_set\n"
    "mock_positive=passed\n"
    "mock_negatives=wrong_ordinal,missing_resource,missing_coefficient,wrong_handle\n"
    "ant_provider_admission=CAPABILITY_MISSING\n"
    "next_milestone=S6-1_PROVIDER_ADMISSION\n"
    "claim_limit=no_real_ciphertext_inference_or_provider_admission\n",
    encoding="utf-8",
)
PY

(cd "$out_dir" && \
  find . -type f ! -name SHA256SUMS -print0 | sort -z | \
  xargs -0 sha256sum >SHA256SUMS)
printf 'S6-0d provider-independent closure passed.\n'
printf 'Readable closure: %s\n' "$out_dir/closure.txt"
printf 'ANT boundary: %s\n' \
  "$out_dir/ace-admission/ace-source-admission.json"
printf 'Hashes: %s\n' "$out_dir/SHA256SUMS"
