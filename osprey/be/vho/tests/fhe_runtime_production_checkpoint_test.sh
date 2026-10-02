#!/usr/bin/env bash
#
# Certify S5-G six-PU production runtime lowering and binary-last publication.
#
# Design references:
#   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-G
#   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md

set -euo pipefail

input="${OPEN64_FHE_RUNTIME_INPUT:?set the S5-E six-PU .B path}"
source="${OPEN64_FHE_RUNTIME_SOURCE:?set the captured model source path}"
provider="${OPEN64_FHE_RUNTIME_PROVIDER:?set the approved provider manifest}"
backend="${OPEN64_BE:?set the rebuilt be executable}"
be_library="${OPEN64_BE_LIBRARY_DIR:?set the rebuilt be.so directory}"
ir_b2a="${OPEN64_IR_B2A:?set the rebuilt ir_b2a executable}"
artifact_dir="${OPEN64_FHE_RUNTIME_ARTIFACT_DIR:?set a host-mounted directory}"
provider_sha="5c7c072b90c1461cb5278b2beb38fdad1713815dc048e68cef8dd94d99731b58"
stem="secure_resnet20.mid"
binary="$artifact_dir/$stem.B"
trace="$artifact_dir/$stem.T"
schedule="$binary.schedule.json"
report="$binary.report.json"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$input" "$artifact_dir/secure_resnet20.interfaced-initialized.B"
cp "$source" "$artifact_dir/secure_resnet20.py"
cp "$provider" "$artifact_dir/ace-ant-subset-capability.json"

printf 'LD_LIBRARY_PATH=%q %q -FHE:runtime_checkpoint=%q -FHE:provider_manifest=%q:provider_sha256=%s %q\n' \
  "$be_library" "$backend" "$binary" \
  "$artifact_dir/ace-ant-subset-capability.json" "$provider_sha" \
  "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
  >"$artifact_dir/commands.txt"
printf '%q -st -src %q %q\n' "$ir_b2a" "$binary" "$trace" \
  >>"$artifact_dir/commands.txt"

LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  "$backend" -FHE:runtime_checkpoint="$binary" \
  -FHE:provider_manifest="$artifact_dir/ace-ant-subset-capability.json":provider_sha256="$provider_sha" \
  "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
  >"$artifact_dir/backend.log" 2>&1
"$ir_b2a" -st -src "$binary" "$trace" \
  >>"$artifact_dir/backend.log" 2>&1
"$ir_b2a" -st -src \
  "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
  "$artifact_dir/secure_resnet20.interfaced-initialized.T" \
  >>"$artifact_dir/backend.log" 2>&1

grep -Fq \
  'FHE-SYNC5-LOWERING: pu=6 definitions=32 computed=33 promoted=44 dead=126 static=87 dynamic=147 calls=174' \
  "$artifact_dir/backend.log"
test "$(grep -c '^FUNC_ENTRY' "$trace")" -eq 6
test "$(grep -c 'U4CALL .*open64_fhe_operation_desc_select_v1' "$trace")" -eq 87
test "$(grep -c 'operator=OPR_DSLRELU .*status=lowered' "$trace")" -eq 11
test "$(grep -c 'operator=OPR_DSLTENSORCONST .*status=dead_elided' "$trace")" -eq 126
test "$(grep -c 'VALUE ordinal=.*roles=0xa' "$trace")" -eq 5
# Logical carrier rows remain for provenance, but executable trees are clean.
awk '/^DSL Tensor Type Extensions:/ { exit }
     /OPR_DSL/ { found = 1 }
     END { exit found ? 1 : 0 }' "$trace"
grep -Fq 'def forward(self, value):' "$trace"
test ! -e "$binary.tmp"

python3 - "$schedule" "$report" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    schedule = json.load(stream)
with open(sys.argv[2], encoding="utf-8") as stream:
    report = json.load(stream)
assert schedule["schema"] == "open64.fhe.sync5.runtime-schedule.v1"
assert schedule["static_evaluations"] == 87
assert schedule["dynamic_evaluations"] == 147
assert len(schedule["records"]) == 32
ordinal = 1
for record in schedule["records"]:
    assert record["first_ordinal"] == ordinal
    ordinal += record["static_count"]
assert ordinal == 88
assert report["schema"] == "open64.fhe.sync5.runtime-lowering-report.v1"
for name, expected in {
    "pu_count": 6,
    "scheduled_definitions": 32,
    "computed_definitions": 33,
    "promoted_sources": 44,
    "dead_sources": 126,
    "static_evaluations": 87,
    "dynamic_evaluations": 147,
    "selectors": 87,
    "standard_calls": 174,
    "output_handles": 174,
    "status_checks": 174,
}.items():
    assert report[name] == expected, (name, report[name])
PY

bad="$artifact_dir/secure_resnet20.bad-provider.B"
printf 'LD_LIBRARY_PATH=%q %q -FHE:runtime_checkpoint=%q -FHE:provider_manifest=%q:provider_sha256=%064d %q\n' \
  "$be_library" "$backend" "$bad" \
  "$artifact_dir/ace-ant-subset-capability.json" 0 \
  "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
  >>"$artifact_dir/commands.txt"
if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
   "$backend" -FHE:runtime_checkpoint="$bad" \
   -FHE:provider_manifest="$artifact_dir/ace-ant-subset-capability.json":provider_sha256="$(printf '%064d' 0)" \
   "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
   >"$artifact_dir/bad-provider.log" 2>&1; then
  echo "unapproved provider unexpectedly published a checkpoint" >&2
  exit 1
fi
test ! -e "$bad"
test ! -e "$bad.tmp"

# A stale binary destination must remain the commit marker for the first run.
binary_before="$(cksum "$binary")"
schedule_before="$(cksum "$schedule")"
report_before="$(cksum "$report")"
if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
   "$backend" -FHE:runtime_checkpoint="$binary" \
   -FHE:provider_manifest="$artifact_dir/ace-ant-subset-capability.json":provider_sha256="$provider_sha" \
   "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
   >"$artifact_dir/stale-binary.log" 2>&1; then
  echo "stale runtime checkpoint destination was overwritten" >&2
  exit 1
fi
test ! -e "$binary.tmp"
test "$(cksum "$binary")" = "$binary_before"
test "$(cksum "$schedule")" = "$schedule_before"
test "$(cksum "$report")" = "$report_before"

# A rejected auxiliary destination must not leave a binary commit marker.
blocked="$artifact_dir/secure_resnet20.blocked-aux.B"
blocked_schedule="$blocked.schedule.json"
printf 'outside-owner\n' >"$blocked_schedule"
if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
   "$backend" -FHE:runtime_checkpoint="$blocked" \
   -FHE:provider_manifest="$artifact_dir/ace-ant-subset-capability.json":provider_sha256="$provider_sha" \
   "$artifact_dir/secure_resnet20.interfaced-initialized.B" \
   >"$artifact_dir/blocked-aux.log" 2>&1; then
  echo "blocked auxiliary destination published a checkpoint" >&2
  exit 1
fi
test "$(cat "$blocked_schedule")" = outside-owner
test ! -e "$blocked"
test ! -e "$blocked.tmp"
test ! -e "$blocked_schedule.tmp"
test ! -e "$blocked.report.json"
test ! -e "$blocked.report.json.tmp"

if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum \
    "$stem.B" "$stem.T" "$stem.B.schedule.json" \
    "$stem.B.report.json" secure_resnet20.py \
    secure_resnet20.interfaced-initialized.B \
    secure_resnet20.interfaced-initialized.T \
    ace-ant-subset-capability.json >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 \
    "$stem.B" "$stem.T" "$stem.B.schedule.json" \
    "$stem.B.report.json" secure_resnet20.py \
    secure_resnet20.interfaced-initialized.B \
    secure_resnet20.interfaced-initialized.T \
    ace-ant-subset-capability.json >SHA256SUMS)
fi

echo "FHE six-PU production runtime checkpoint passed"
echo "review binary: $binary"
echo "review trace: $trace"
echo "review schedule: $schedule"
echo "review report: $report"
echo "review log: $artifact_dir/backend.log"
echo "review negative: $artifact_dir/bad-provider.log"
echo "review stale binary: $artifact_dir/stale-binary.log"
echo "review blocked auxiliary: $artifact_dir/blocked-aux.log"
echo "review commands: $artifact_dir/commands.txt"
echo "review hashes: $artifact_dir/SHA256SUMS"
