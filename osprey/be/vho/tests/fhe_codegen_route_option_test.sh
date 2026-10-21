#!/usr/bin/env bash
#
# Check the staged S6-0d D5 codegen option without selecting an unavailable
# backend route. See doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md.

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
echo "FHE codegen route option contract passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $commands"
echo "review diagnostics: $artifact_dir"
