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

if grep -q '/lw_inline/lw_inline' "$trace"; then
  echo "openpy scheduled canonical inlining before VHO DSL lowering" >&2
  exit 1
fi

if grep '/torch2whirl/torch2whirl' "$trace" |
     grep -Eq '"-O[0-9]|"-TARG:|"-PHASE:'; then
  echo "openpy forwarded compiler-only options to torch2whirl" >&2
  exit 1
fi

fake_build="$work_dir/fake-build"
fake_frontend="$fake_build/osprey/targdir/torch2whirl/torch2whirl"
env_capture="$work_dir/frontend-ld-library-path"
mkdir -p "$(dirname "$fake_frontend")"
printf '%s\n' '#!/usr/bin/env bash' \
  'printf "%s" "${LD_LIBRARY_PATH-}" > "$OPENPY_ENV_CAPTURE"' \
  'while [[ $# -gt 0 ]]; do' \
  '  if [[ "$1" == "--output" ]]; then shift; : > "$1"; fi' \
  '  shift' \
  'done' > "$fake_frontend"
chmod +x "$fake_frontend"

(
  cd "$work_dir"
  OPENPY_ENV_CAPTURE="$env_capture" \
    LD_LIBRARY_PATH=/openpy/user/runtime \
    "$openpy" -run-build="$fake_build" -INLINE:none model.py
) >/dev/null 2>&1 || true

if [[ ! -f "$env_capture" ]] ||
   [[ "$(cat "$env_capture")" != /openpy/user/runtime ]]; then
  echo "openpy did not preserve the frontend LD_LIBRARY_PATH" >&2
  exit 1
fi

echo "openpy driver contract passed"
