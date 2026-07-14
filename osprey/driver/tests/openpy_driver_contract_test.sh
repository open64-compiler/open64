#!/usr/bin/env bash

set -euo pipefail

build_dir="${OPEN64_BUILD_DIR:-/build}"
openpy="$build_dir/osprey/targdir/driver/openpy"

if [[ ! -x "$openpy" ]]; then
  echo "openpy driver is not executable: $openpy" >&2
  exit 1
fi

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/openpy-driver.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

printf 'def create_model():\n    pass\n' > "$work_dir/model.py"

(
  cd "$work_dir"
  "$openpy" -run-build="$build_dir" -### -keep model.py
) > "$work_dir/driver.trace" 2>&1

trace="$work_dir/driver.trace"
grep -q '/torch2whirl/torch2whirl' "$trace"
grep -q '"model.py" "--entry" "forward"' "$trace"
grep -q '"--backend" "native"' "$trace"
grep -q '"--sample-input" "shape:1,3,224,224"' "$trace"
grep -q '"--output" "model.B"' "$trace"
grep -q '"-O0"' "$trace"
grep -q '"-DSL:dump_after_lower=ON"' "$trace"
grep -q '"-ft,model.t"' "$trace"

if grep '/torch2whirl/torch2whirl' "$trace" |
     grep -Eq '"-O[0-9]|"-TARG:|"-PHASE:'; then
  echo "openpy forwarded compiler-only options to torch2whirl" >&2
  exit 1
fi

echo "openpy driver contract passed"
