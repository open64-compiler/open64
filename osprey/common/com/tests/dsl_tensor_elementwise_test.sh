#!/usr/bin/env bash
# Validate direct ordinary tensor operators and frozen mapped-image compatibility.
# Owner: common/com tests; design: doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md.
set -euo pipefail
script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build="${OPEN64_BUILD_ROOT:-$repo_root/build/osprey/targdir}"
artifact_root="${OPEN64_DSL_TEST_ARTIFACT_DIR:-$repo_root/artifacts/dsl}"
mkdir -p "$artifact_root"
out="$(mktemp -d "$artifact_root/direct-tensor.XXXXXX")"
mkdir "$out/source" "$out/current" "$out/previous"
cp "$script_dir/dsl_tensor_elementwise_fixture.h" "$out/source/"
cp "$script_dir/dsl_scalar_reuse_test.cxx" "$out/source/"
cp "$0" "$out/source/dsl_tensor_elementwise_test.sh"
exec > >(tee "$out/run.log") 2>&1
set -x
OPEN64_DSL_DIRECT_TENSOR_ONLY=1 OPEN64_DSL_DIRECT_TENSOR_ARTIFACT_DIR="$out" \
    "$build/ir_tools/dsl_scalar_reuse_test"
for stem in tensor.before tensor.after; do
    "$build/ir_tools/ir_b2a" -st -src "$out/$stem.B" "$out/current/$stem.T"
    if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
        "$OPEN64_PREVIOUS_IR_B2A" -st -src "$out/$stem.B" "$out/previous/$stem.T"
    fi
done
diff -u "$out/current/tensor.before.T" "$out/current/tensor.after.T" > "$out/tensor.before-after.diff"
if [[ -n "${OPEN64_PREVIOUS_IR_B2A:-}" ]]; then
    diff -u "$out/previous/tensor.before.T" "$out/previous/tensor.after.T" > "$out/previous/tensor.before-after.diff"
    grep -Fq 'OPR_DSLSUB' "$out/previous/tensor.after.T"
    grep -Fq 'OPR_DSLGREATER' "$out/previous/tensor.after.T"
fi
for opcode in MADD MSUB MMPY MDIV MREM MMGT MLNOT MSELECT; do
    grep -Fq "$opcode" "$out/current/tensor.after.T"
done
grep -Fq 'tensor_contract=elementwise.v1' "$out/current/tensor.after.T"
grep -Fq 'attr.broadcast_rule=numpy' "$out/current/tensor.after.T"
grep -Fq 'shape=[2,3]' "$out/current/tensor.after.T"
# Corrupt retained bytes, not live WNs, to exercise the actual mapping boundary.
# The fixture is ELF64 little-endian; locate the unique semantic-record index
# through its sections rather than baking in a WN size or compiler address.
python3 - "$out" "$build/ir_tools/ir_b2a" <<'PY'
from pathlib import Path
import struct
import subprocess
import sys

root = Path(sys.argv[1])
binary = (root / "tensor.before.B").read_bytes()
assert binary[:6] == b"\x7fELF\x02\x01"
shoff = struct.unpack_from("<Q", binary, 40)[0]
size, count, names_index = struct.unpack_from("<HHH", binary, 58)
rows = [struct.unpack_from("<IIQQQQIIQQ", binary, shoff + index * size)
        for index in range(count)]
names_row = rows[names_index]
names = binary[names_row[4]:names_row[4] + names_row[5]]
sections = {names[row[0]:].split(b"\0", 1)[0].decode(): row for row in rows}
strings = sections[".WHIRL.strtab"]
table = binary[strings[4]:strings[4] + strings[5]]
record = table.index(b"__WHIRL_DSL__:opcode:common.sub:")
tree = sections[".WHIRL.pu_section"]
image = binary[tree[4]:tree[4] + tree[5]]
pattern = struct.pack("<I", record)
hits = [index for index in range(len(image) - 3) if image[index:index + 4] == pattern]
assert len(hits) == 1, "fixture record index must identify exactly one tree node"
offset = tree[4] + hits[0]
variants = {}
for label, value in (("past-table", len(table)), ("high-bit", 0x80000000),
                     ("all-ones", 0xffffffff)):
    malformed = bytearray(binary)
    struct.pack_into("<I", malformed, offset, value)
    variants[label] = malformed
malformed = bytearray(binary)
malformed[strings[4] + record] = ord("x")
variants["bad-prefix"] = malformed
malformed = bytearray(binary)
struct.pack_into("<I", malformed, offset, len(table) - 1)
malformed[strings[4] + len(table) - 1] = ord("x")
variants["unterminated"] = malformed
for label, malformed in variants.items():
    path = root / (label + ".invalid-elf")
    path.write_bytes(malformed)
    result = subprocess.run([sys.argv[2], "-st", "-src", str(path)],
                            capture_output=True)
    (root / (label + ".stdout")).write_bytes(result.stdout)
    (root / (label + ".stderr")).write_bytes(result.stderr)
    assert result.returncode > 0, (label, result.returncode, result.stderr)
    assert b"Assertion" not in result.stderr, (label, result.stderr)
print("mapped malformed record indices/prefix/termination rejected without signal")
PY
set +x
echo "direct tensor and mapped compatibility checks passed: $out"
