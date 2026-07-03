#!/usr/bin/env bash
#
# Smoke fixture for the ir_b2a/ir_a2b boundary used by the DSL IR plan.
# It intentionally records the current ir_a2b limitation before expanding
# binary IR image type records.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"

opencc="${OPEN64_OPENCC:-$repo_root/opt/open64/bin/opencc}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
ir_a2b="${OPEN64_IR_A2B:-$repo_root/build/osprey/targdir/ir_tools/ir_a2b}"
work_dir="${OPEN64_IR_SMOKE_DIR:-${TMPDIR:-/tmp}/open64-ir-tools-smoke.$$}"

cleanup()
{
  if [[ -z "${OPEN64_IR_SMOKE_KEEP:-}" ]]; then
    rm -rf "$work_dir"
  else
    echo "kept smoke directory: $work_dir"
  fi
}
trap cleanup EXIT

require_executable()
{
  local tool="$1"

  if [[ ! -x "$tool" ]]; then
    echo "missing executable: $tool" >&2
    exit 1
  fi
}

require_executable "$opencc"
require_executable "$ir_b2a"
require_executable "$ir_a2b"

mkdir -p "$work_dir"
cd "$work_dir"

cat > smoke.c <<'EOF'
int add(int a, int b) { return a + b; }
EOF

"$opencc" -c -keep smoke.c > opencc.stdout 2> opencc.stderr

if [[ ! -f smoke.B ]]; then
  echo "opencc did not produce smoke.B" >&2
  cat opencc.stdout >&2
  cat opencc.stderr >&2
  exit 1
fi

"$ir_b2a" smoke.B smoke.ir

if ! grep -q "FUNC_ENTRY" smoke.ir ||
   ! grep -q "I4ADD" smoke.ir ||
   ! grep -q "I4RETURN_VAL" smoke.ir; then
  echo "ir_b2a smoke output did not contain expected WHIRL text" >&2
  cat smoke.ir >&2
  exit 1
fi

set +e
"$ir_a2b" smoke.ir smoke.roundtrip.B > ir_a2b.stdout 2> ir_a2b.stderr
ir_a2b_status=$?
set -e

if [[ "$ir_a2b_status" -eq 0 ]]; then
  echo "ir_a2b unexpectedly accepted ASCII WHIRL; update this fixture" >&2
  exit 1
fi

if ! grep -q "New symbol table format not supported by ir_a2b (yet)" \
     ir_a2b.stderr; then
  echo "ir_a2b failed with an unexpected diagnostic" >&2
  cat ir_a2b.stderr >&2
  exit 1
fi

cat > dsl_marker.ir <<'EOF'
COMMENT "__WHIRL_DSL__:opcode:common.add:v1:kid0=a;kid1=b;attr.broadcast_rule=none"
EOF

set +e
"$ir_a2b" dsl_marker.ir dsl_marker.B > dsl_marker_a2b.stdout \
  2> dsl_marker_a2b.stderr
dsl_marker_a2b_status=$?
set -e

if [[ "$dsl_marker_a2b_status" -eq 0 ]]; then
  echo "ir_a2b unexpectedly accepted DSL marker ASCII; update this fixture" >&2
  exit 1
fi

if ! grep -q "New symbol table format not supported by ir_a2b (yet)" \
     dsl_marker_a2b.stderr; then
  echo "ir_a2b DSL-marker failure was not the expected compatibility gate" >&2
  cat dsl_marker_a2b.stderr >&2
  exit 1
fi

echo "ir_tools smoke fixture passed"
