#!/usr/bin/env bash
# Certify unchanged whirl2c output against the public FHE ABI and mock runtime.
# See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-H.

set -euo pipefail

source_root="${OPEN64_SOURCE_ROOT:?set the Open64 source root}"
model_source="${OPEN64_FHE_MODEL_SOURCE:?set the captured model source}"
capture_binary="${OPEN64_FHE_CAPTURE_BINARY:?set the captured model .B}"
capture_trace="${OPEN64_FHE_CAPTURE_TRACE:?set the captured -st -src .T}"
binary="${OPEN64_FHE_MID_BINARY:?set the six-PU middle WHIRL .B}"
trace="${OPEN64_FHE_MID_TRACE:?set the separate-process -st -src .T}"
schedule="${OPEN64_FHE_MID_SCHEDULE:?set the production schedule manifest}"
report="${OPEN64_FHE_MID_REPORT:?set the production conversion report}"
phase_log="${OPEN64_FHE_PHASE_LOG:?set the production backend log}"
whirl2c="${OPEN64_WHIRL2C:?set the unchanged whirl2c executable}"
library_dir="${OPEN64_BE_LIBRARY_DIR:?set the be.so directory}"
artifact_dir="${OPEN64_FHE_GENERATED_ARTIFACT_DIR:?set a host-mounted directory}"
stem=secure_resnet20.mid

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -depth -delete
cp "$model_source" "$artifact_dir/secure_resnet20.py"
cp "$capture_binary" "$artifact_dir/secure_resnet20.interfaced-initialized.B"
cp "$capture_trace" "$artifact_dir/secure_resnet20.interfaced-initialized.T"
cp "$binary" "$artifact_dir/$stem.B"
cp "$trace" "$artifact_dir/$stem.T"
cp "$schedule" "$artifact_dir/$stem.B.schedule.json"
cp "$report" "$artifact_dir/$stem.B.report.json"
cp "$phase_log" "$artifact_dir/phase-backend.log"

printf '%q -fB,%q %q\n' "$whirl2c" "$artifact_dir/$stem.B" \
  "$artifact_dir/$stem.c" >"$artifact_dir/commands.txt"
stage="$artifact_dir/.whirl2c-stage"
mkdir "$stage"
if ! (cd "$stage" && \
  LD_LIBRARY_PATH="$library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  "$whirl2c" -fB,"$artifact_dir/$stem.B" "$artifact_dir/$stem.c" \
  >"$artifact_dir/whirl2c.log" 2>&1); then
  echo "whirl2c failed; staged partial files were not published" >&2
  exit 1
fi
mv "$stage/$stem.w2c.h" "$artifact_dir/$stem.w2c.h"
mv "$stage/$stem.w2c.c" "$artifact_dir/$stem.w2c.c"
rmdir "$stage"
test -s "$artifact_dir/$stem.w2c.c"
test -s "$artifact_dir/$stem.w2c.h"
test "$(grep -c '^FUNC_ENTRY' "$artifact_dir/$stem.T")" -eq 6
test "$(grep -c 'I4U8EQ' "$artifact_dir/$stem.T")" -eq 9
grep -Fq 'def forward(self, value):' "$artifact_dir/$stem.T"

cat >>"$artifact_dir/commands.txt" <<COMMANDS
gcc -std=gnu89 -Werror=incompatible-pointer-types -include open64_fhe_runtime_abi.h -c $stem.w2c.c
gcc -std=gnu89 -Werror=incompatible-pointer-types fhe_generated_resnet_trace.c
gcc -std=gnu89 -Werror=incompatible-pointer-types fhe_generated_resnet_mock.c
g++ -std=c++11 -pthread generated_mock.o open64_fhe_mock_runtime.cxx open64_fhe_mock_sha256.cxx
COMMANDS

gcc -std=gnu89 -Wno-unused-value -Werror=incompatible-pointer-types \
  -I"$source_root/osprey/include" \
  -include open64_fhe_runtime_abi.h \
  -c "$artifact_dir/$stem.w2c.c" -o "$artifact_dir/$stem.o" \
  >"$artifact_dir/generated-c-build.log" 2>&1
if gcc -std=gnu89 -Werror=incompatible-pointer-types \
   -I"$source_root/osprey/include" \
   -c "$source_root/osprey/be/vho/tests/fhe_generated_wrong_handle.c" \
   -o "$artifact_dir/wrong-handle.o" \
   >"$artifact_dir/wrong-handle-compile.log" 2>&1; then
  echo "ciphertext passed as plaintext weight unexpectedly compiled" >&2
  exit 1
fi
grep -Fq 'incompatible pointer type' \
  "$artifact_dir/wrong-handle-compile.log"
test ! -e "$artifact_dir/wrong-handle.o"
gcc -std=gnu89 -Wno-unused-value -Werror=incompatible-pointer-types \
  -I"$source_root/osprey/include" -I"$artifact_dir" \
  -include open64_fhe_runtime_abi.h \
  "$source_root/osprey/be/vho/tests/fhe_generated_resnet_trace.c" \
  -o "$artifact_dir/generated_trace" \
  >"$artifact_dir/trace-build.log" 2>&1
"$artifact_dir/generated_trace" >"$artifact_dir/generated-trace.log"
grep -Fxq 'SUMMARY selects=147 evals=147 output=set' \
  "$artifact_dir/generated-trace.log"

python3 "$source_root/osprey/be/vho/tests/fhe_generated_resnet_schedule_check.py" \
  "$artifact_dir/generated-trace.log" \
  "$artifact_dir/$stem.B.schedule.json" \
  "$artifact_dir/$stem.w2c.c"
python3 - "$artifact_dir/$stem.B.schedule.json" \
  "$artifact_dir/malformed-schedule.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    schedule = json.load(stream)
schedule["records"][0]["first_ordinal"] = 2
with open(sys.argv[2], "w", encoding="utf-8") as stream:
    json.dump(schedule, stream, sort_keys=True)
PY
if python3 "$source_root/osprey/be/vho/tests/fhe_generated_resnet_schedule_check.py" \
   "$artifact_dir/generated-trace.log" \
   "$artifact_dir/malformed-schedule.json" \
   "$artifact_dir/$stem.w2c.c" \
   >"$artifact_dir/malformed-schedule.log" 2>&1; then
  echo "malformed schedule unexpectedly passed generated-C validation" >&2
  exit 1
fi

for mode in 's 1' 's 74' 's 147' 'e 74'; do
  label="${mode// /-}"
  # Word splitting is intentional for the two explicit mode arguments.
  "$artifact_dir/generated_trace" $mode \
    >"$artifact_dir/fail-$label.log"
  grep -Fq 'output=null' "$artifact_dir/fail-$label.log"
done
grep -Fxq 'SUMMARY selects=74 evals=73 output=null' \
  "$artifact_dir/fail-s-74.log"
grep -Fxq 'SUMMARY selects=74 evals=74 output=null' \
  "$artifact_dir/fail-e-74.log"

gcc -std=gnu89 -Wno-unused-value -Werror=incompatible-pointer-types \
  -I"$source_root/osprey/include" \
  -I"$source_root/osprey/libopen64fhe" -I"$artifact_dir" \
  -include open64_fhe_runtime_abi.h \
  -c "$source_root/osprey/be/vho/tests/fhe_generated_resnet_mock.c" \
  -o "$artifact_dir/generated_mock.o" \
  >"$artifact_dir/mock-build.log" 2>&1
g++ -std=c++11 -pthread \
  -I"$source_root/osprey/include" \
  -I"$source_root/osprey/libopen64fhe" \
  "$artifact_dir/generated_mock.o" \
  "$source_root/osprey/libopen64fhe/open64_fhe_mock_runtime.cxx" \
  "$source_root/osprey/libopen64fhe/open64_fhe_mock_sha256.cxx" \
  -o "$artifact_dir/generated_mock" \
  >>"$artifact_dir/mock-build.log" 2>&1
"$artifact_dir/generated_mock" "$artifact_dir/generated-trace.log" \
  >"$artifact_dir/mock-positive.log"
grep -Fq 'MOCK positive=147 output=set' \
  "$artifact_dir/mock-positive.log"
for mode in wrong-ordinal missing-resource missing-coefficient wrong-handle; do
  "$artifact_dir/generated_mock" "$artifact_dir/generated-trace.log" \
    "$mode" >"$artifact_dir/mock-$mode.log"
  grep -Fxq "MOCK negative=$mode output=null" \
    "$artifact_dir/mock-$mode.log"
done

# The translator may leave staged diagnostics on failure; none become a
# published generated-C artifact. The old unguarded image is optional so this
# lane can also run in a clean checkout without historical local evidence.
if [[ -n "${OPEN64_FHE_UNGUARDED_BINARY:-}" ]]; then
  mkdir "$artifact_dir/.unguarded-stage"
  if (cd "$artifact_dir/.unguarded-stage" && \
      LD_LIBRARY_PATH="$library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$whirl2c" -fB,"$OPEN64_FHE_UNGUARDED_BINARY" \
      "$artifact_dir/unguarded.c" \
      >"$artifact_dir/reject-unguarded.log" 2>&1); then
    echo "unguarded middle WHIRL unexpectedly translated" >&2
    exit 1
  fi
  grep -Fq 'CFHELOWER-CALL-005' "$artifact_dir/reject-unguarded.log"
  test ! -e "$artifact_dir/unguarded.w2c.c"
  test ! -e "$artifact_dir/unguarded.w2c.h"
fi
if [[ -n "${OPEN64_FHE_UNLOWERED_BINARY:-}" ]]; then
  mkdir "$artifact_dir/.unlowered-stage"
  if (cd "$artifact_dir/.unlowered-stage" && \
      LD_LIBRARY_PATH="$library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$whirl2c" -fB,"$OPEN64_FHE_UNLOWERED_BINARY" \
      "$artifact_dir/unlowered.c" \
      >"$artifact_dir/reject-unlowered.log" 2>&1); then
    echo "unlowered DSL carrier unexpectedly translated" >&2
    exit 1
  fi
  grep -Fq 'DSL gatekeeper error:' "$artifact_dir/reject-unlowered.log"
  test ! -e "$artifact_dir/unlowered.w2c.c"
  test ! -e "$artifact_dir/unlowered.w2c.h"
fi

hash_files=(
  secure_resnet20.py secure_resnet20.interfaced-initialized.B
  secure_resnet20.interfaced-initialized.T phase-backend.log
  "$stem.B" "$stem.T" "$stem.B.schedule.json" "$stem.B.report.json"
  "$stem.w2c.c" "$stem.w2c.h" "$stem.o"
  generated_trace generated_mock.o generated_mock
  whirl2c.log generated-c-build.log trace-build.log mock-build.log
  wrong-handle-compile.log
  generated-trace.log mock-positive.log
  fail-s-1.log fail-s-74.log fail-s-147.log fail-e-74.log
  mock-wrong-ordinal.log mock-missing-resource.log
  mock-missing-coefficient.log mock-wrong-handle.log
  malformed-schedule.json commands.txt
)
if [[ -f "$artifact_dir/reject-unguarded.log" ]]; then
  hash_files+=(reject-unguarded.log)
fi
if [[ -f "$artifact_dir/reject-unlowered.log" ]]; then
  hash_files+=(reject-unlowered.log)
fi
(cd "$artifact_dir" && sha256sum "${hash_files[@]}" >SHA256SUMS)
echo "S5-H generated C and standalone mock certification passed"
echo "review generated C: $artifact_dir/$stem.w2c.c"
echo "review mock log: $artifact_dir/mock-positive.log"
echo "review source-interleaved trace: $artifact_dir/$stem.T"
echo "review hashes: $artifact_dir/SHA256SUMS"
