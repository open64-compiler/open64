#!/usr/bin/env bash
#
# Check the staged S6-0d D5 codegen option after input-image inspection,
# without selecting an unavailable backend route. See the D5 plan.

set -euo pipefail

producer="${OPEN64_DSL_CONTRACT_TEST:?set the native DSL contract producer}"
backend="${OPEN64_BE:?set the rebuilt backend executable}"
be_library="${OPEN64_BE_LIBRARY_DIR:?set the rebuilt be.so directory}"
ir_b2a="${OPEN64_IR_B2A:?set the rebuilt ir_b2a executable}"
artifact_dir="${OPEN64_FHE_CODEGEN_OPTION_ARTIFACT_DIR:?set a host-mounted directory}"
openpy="${OPEN64_OPENPY:-}"

for executable in "$producer" "$backend" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done
if [[ -n "$openpy" && ! -x "$openpy" ]]; then
  echo "missing executable: $openpy" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
image="$artifact_dir/fhe_codegen_option.B"
trace="$artifact_dir/fhe_codegen_option.T"
plain_image="$artifact_dir/plain_terminal.B"
plain_trace="$artifact_dir/plain_terminal.T"
commands="$artifact_dir/commands.txt"

printf 'OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 OPEN64_DSL_FHE_SYNC3_ARTIFACT=%q %q\n' \
  "$image" "$producer" >"$commands"
OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 \
  OPEN64_DSL_FHE_SYNC3_ARTIFACT="$image" \
  "$producer" >"$artifact_dir/producer.log" 2>&1
printf '%q -st -src %q %q\n' "$ir_b2a" "$image" "$trace" \
  >>"$commands"
"$ir_b2a" -st -src "$image" "$trace" \
  >"$artifact_dir/reader.log" 2>&1
grep -Fq 'FHE Plan Image:' "$trace"

printf 'OPEN64_DSL_GROUP_LOWER_REGION_TERMINAL=1 OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT=%q %q\n' \
  "$plain_image" "$producer" >>"$commands"
OPEN64_DSL_GROUP_LOWER_ONLY=1 \
  OPEN64_DSL_GROUP_LOWER_REGION_OUTPUT=1 \
  OPEN64_DSL_GROUP_LOWER_REGION_TERMINAL=1 \
  OPEN64_DSL_GROUP_LOWER_ARTIFACT="$artifact_dir/plain_intermediate_unused.B" \
  OPEN64_DSL_GROUP_LOWER_TERMINAL_ARTIFACT="$plain_image" \
  "$producer" >"$artifact_dir/plain-producer.log" 2>&1
"$ir_b2a" -st -src "$plain_image" "$plain_trace" \
  >"$artifact_dir/plain-reader.log" 2>&1
grep -Fq 'contract=cnn.basic_block.v1' "$plain_trace"

for route in ckks2c poly2c invalid CKKS2C; do
  log_name="$route"
  if [[ "$route" == CKKS2C ]]; then
    log_name="uppercase-route"
  fi
  log="$artifact_dir/$log_name.log"
  printf 'LD_LIBRARY_PATH=%q %q -FHE:codegen=%s -O0 %q\n' \
    "$be_library" "$backend" "$route" "$image" >>"$commands"
  if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$backend" -FHE:codegen="$route" -O0 "$image" >"$log" 2>&1; then
    echo "unavailable FHE codegen route $route was accepted" >&2
    exit 1
  fi
  if [[ "$route" == ckks2c || "$route" == poly2c ]]; then
    grep -Fq 'CFHECKKS2C-003:' "$log"
    grep -Fq 'no fallback route was selected' "$log"
  else
    grep -Fq 'CFHECKKS2C-002:' "$log"
  fi
done

missing_image="$artifact_dir/missing.B"
printf 'LD_LIBRARY_PATH=%q %q -FHE:codegen=ckks2c -O0 %q\n' \
  "$be_library" "$backend" "$missing_image" >>"$commands"
if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$backend" -FHE:codegen=ckks2c -O0 "$missing_image" \
    >"$artifact_dir/missing-image.log" 2>&1; then
  echo "missing WHIRL input unexpectedly accepted" >&2
  exit 1
fi
if ! grep -Fq "Can't open intermediate compiler file" \
     "$artifact_dir/missing-image.log" ||
   grep -Fq 'CFHECKKS2C-003:' "$artifact_dir/missing-image.log"; then
  echo "FHE route rejected before input validation" >&2
  exit 1
fi

# An unrelated WHIRL image must take the normal backend path even when the
# driver forwarded an option owned only by FHE. The result may fail later if
# this test build intentionally has no CG; the two paths must agree.
normal_status=0
printf 'LD_LIBRARY_PATH=%q %q -O0 %q\n' \
  "$be_library" "$backend" "$plain_image" >>"$commands"
LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  "$backend" -O0 "$plain_image" \
  >"$artifact_dir/plain-normal.log" 2>&1 || normal_status=$?
for route in ckks2c poly2c; do
  route_status=0
  printf 'LD_LIBRARY_PATH=%q %q -FHE:codegen=%s -O0 %q\n' \
    "$be_library" "$backend" "$route" "$plain_image" >>"$commands"
  LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$backend" -FHE:codegen="$route" -O0 "$plain_image" \
    >"$artifact_dir/plain-with-$route.log" 2>&1 || route_status=$?
  if [[ "$normal_status" -ne "$route_status" ]] ||
     ! cmp -s "$artifact_dir/plain-normal.log" \
        "$artifact_dir/plain-with-$route.log"; then
    echo "FHE-only option $route changed an unrelated WHIRL backend path" >&2
    exit 1
  fi
done

if [[ -n "${OPEN64_FHE_CODEGEN_CKKS_IMAGE:-}" ]]; then
  ckks_image="$OPEN64_FHE_CODEGEN_CKKS_IMAGE"
  expected_sha="${OPEN64_FHE_CODEGEN_CKKS_IMAGE_SHA256:?set the CKKS image receipt SHA-256}"
  actual_sha="$(sha256sum "$ckks_image" | cut -d ' ' -f 1)"
  if [[ "$actual_sha" != "$expected_sha" ]]; then
    echo "cached CKKS image does not match its receipt" >&2
    exit 1
  fi
  printf 'LD_LIBRARY_PATH=%q %q -FHE:codegen=ckks2c -O0 %q\n' \
    "$be_library" "$backend" "$ckks_image" >>"$commands"
  if LD_LIBRARY_PATH="$be_library${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$backend" -FHE:codegen=ckks2c -O0 "$ckks_image" \
      >"$artifact_dir/cached-ckks-route.log" 2>&1; then
    echo "unavailable CKKS2C route accepted the cached CKKS image" >&2
    exit 1
  fi
  grep -Fq 'CFHECKKS2C-003:' "$artifact_dir/cached-ckks-route.log"
  if grep -Fq 'Error loading cg.so' \
       "$artifact_dir/cached-ckks-route.log"; then
    echo "CG loaded before the CKKS route gate" >&2
    exit 1
  fi
fi

if [[ -n "$openpy" ]]; then
  model="osprey/torch2whirl/python/tests/models/resnet_model.py"
  printf '%q -show0 -FHE:codegen=ckks2c -O0 %q\n' \
    "$openpy" "$model" >>"$commands"
  "$openpy" -show0 -FHE:codegen=ckks2c -O0 "$model" \
    >"$artifact_dir/openpy-route.log" 2>&1
  grep -Fq ' -FHE:codegen=ckks2c ' "$artifact_dir/openpy-route.log"
  grep -Fq '/be ' "$artifact_dir/openpy-route.log"
fi

test ! -e "$image.tmp"
(cd "$artifact_dir" && sha256sum *.B *.T *.log commands.txt >SHA256SUMS)
echo "FHE codegen route option contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review unrelated-WHIRL trace: $plain_trace"
echo "review commands: $commands"
echo "review hashes: $artifact_dir/SHA256SUMS"
echo "review diagnostics: $artifact_dir"
