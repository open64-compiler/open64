#!/usr/bin/env bash
#
# Preserve mapped-image and logical ir_b2a evidence for tensor TCON carriers.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_dir="${OPEN64_BUILD_DIR:-$repo_root/build}"
producer="${OPEN64_DSL_CONTRACT_TEST:-$build_dir/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$build_dir/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_TENSOR_TCON_ARTIFACT_DIR:-$repo_root/artifacts/m2-tensor-tcon}"
image="$artifact_dir/tensor_tcon.B"
trace="$artifact_dir/tensor_tcon.T"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

OPEN64_DSL_TENSOR_TCON_ONLY=1 \
OPEN64_DSL_TENSOR_TCON_ARTIFACT="$image" \
  "$producer" > "$artifact_dir/producer.log" 2>&1

"$ir_b2a" -st -src "$image" > "$trace"

for evidence in \
  "tensor_tcon storage=zero" \
  "element=I4 elements=4 bytes=16 alignment=16" \
  "tensor_tcon storage=side_file" \
  "side_file=weights/tensor.bin" \
  "byte_offset=0 byte_length=16"; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing tensor TCON evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq "MTYPE_STRING" "$trace"; then
  echo "physical tensor TCON carrier leaked into $trace" >&2
  exit 1
fi

cat > "$artifact_dir/certification.txt" <<EOF
mapped_image_reopen=passed
derived_cache_rebuild=passed
logical_tcon_print=passed
logical_symbol_print=passed
ir_b2a_st_src=passed
physical_carrier_hidden=passed
EOF

echo "tensor TCON mapped-image fixture passed"
echo "review artifacts: $artifact_dir"
