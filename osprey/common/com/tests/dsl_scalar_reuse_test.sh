#!/usr/bin/env bash
# Exercise the real scalar bridge and independently reopen retained WHIRL.
# common/com owns the expression-scoped fixture; this does not certify tensor
# lowering or execution. Design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md.
set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_root="${OPEN64_BUILD_ROOT:-$repo_root/build/osprey/targdir}"
producer="${OPEN64_DSL_SCALAR_REUSE_TEST:-$build_root/ir_tools/dsl_scalar_reuse_test}"
ir_b2a="${OPEN64_IR_B2A:-$build_root/ir_tools/ir_b2a}"
artifact_root="${OPEN64_DSL_TEST_ARTIFACT_DIR:-$repo_root/artifacts/dsl}"
for executable in "$producer" "$ir_b2a"; do
    test -x "$executable" || { echo "missing executable: $executable" >&2; exit 1; }
done
mkdir -p "$artifact_root"
# A fresh family retains failed diagnostics and never overwrites an accepted run.
artifact_dir="$(mktemp -d "$artifact_root/scalar-reuse.XXXXXX")"
mkdir "$artifact_dir/source"
cp "$script_dir/dsl_scalar_reuse_test.cxx" "$artifact_dir/source/"
cp "$0" "$artifact_dir/source/dsl_scalar_reuse_test.sh"
exec > >(tee "$artifact_dir/run.log") 2>&1
set -x
OPEN64_DSL_SCALAR_REUSE_ARTIFACT="$artifact_dir/before.B" "$producer"
OPEN64_DSL_SCALAR_REUSE_APPLY=1 \
    OPEN64_DSL_SCALAR_REUSE_ARTIFACT="$artifact_dir/after.B" "$producer"
OPEN64_DSL_SCALAR_REUSE_APPLY=1 OPEN64_DSL_SCALAR_REUSE_DISABLED_IMAGE=1 \
    OPEN64_DSL_SCALAR_REUSE_ARTIFACT="$artifact_dir/disabled.B" "$producer"
for stem in before after disabled; do
    "$ir_b2a" -st -src "$artifact_dir/$stem.B" "$artifact_dir/$stem.T"
    test ! -e "$artifact_dir/$stem.B.tmp"
done
diff -u "$artifact_dir/before.T" "$artifact_dir/after.T" \
    > "$artifact_dir/before-after.diff" || test "$?" -eq 1
diff -u "$artifact_dir/before.T" "$artifact_dir/disabled.T" \
    > "$artifact_dir/master-disabled.diff"
python3 - "$artifact_dir" <<'PY'
import pathlib
import sys

root = pathlib.Path(sys.argv[1])
before = (root / "before.T").read_text()
after = (root / "after.T").read_text()
for opcode in ("I4SUB", "I4I4GT", "I4LNOT", "I4SELECT"):
    assert opcode in before and opcode not in after, opcode
# Everything after the first four integer result statements must be identical,
# including strict FP arithmetic, symbols, types, source positions and constants.
marker = "   F4F4LDID"
assert before.split(marker, 1)[1] == after.split(marker, 1)[1]
for text in (before, after):
    assert "dsl_scalar_reuse_test.cxx" in text and "USRCPOS_linenum(source)" in text
    for name in ("strict_sub", "strict_gt", "strict_zero_sub",
                 "strict_sub_negative_zero", "strict_nested_sub"):
        assert name in text, name
print("scalar rewrite, strict-FP no-op and master-disable trace checks passed")
PY
set +x
echo "review artifacts: $artifact_dir"
