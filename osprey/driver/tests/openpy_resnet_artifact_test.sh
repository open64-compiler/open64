#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
src_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
build_dir=${OPEN64_BUILD_DIR:-$src_root/build}
artifact_root=${OPEN64_TORCH2WHIRL_ARTIFACT_DIR:-$src_root/artifacts/torch2whirl}
openpy=$build_dir/osprey/targdir/driver/openpy
ir_b2a=$build_dir/osprey/targdir/ir_tools/ir_b2a
source_model=$src_root/osprey/torch2whirl/python/tests/models/resnet_model.py
work_dir=$artifact_root/openpy/resnet
binary_dir=$work_dir/binary
lowered_dir=$work_dir/lowered

for executable in "$openpy" "$ir_b2a"; do
    if [ ! -x "$executable" ]; then
        echo "required executable not found: $executable" >&2
        exit 1
    fi
done

mkdir -p "$work_dir"
find "$work_dir" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
mkdir -p "$binary_dir" "$lowered_dir"
cp "$source_model" "$work_dir/resnet.py"

set +e
(
    cd "$work_dir"
    "$openpy" -run-build="$build_dir" -keep -O0 resnet.py
) > "$work_dir/openpy_driver.log" 2>&1
status=$?
set -e

driver_log=$work_dir/openpy_driver.log
driver_log_tmp=$work_dir/openpy_driver.log.tmp
{
    echo "exit_status=$status"
    cat "$driver_log"
} > "$driver_log_tmp"
mv "$driver_log_tmp" "$driver_log"
if [ "$status" -ne 0 ]; then
    cat "$driver_log" >&2
    exit "$status"
fi

mv "$work_dir/resnet.B" "$binary_dir/resnet.B"
mv "$work_dir/LocalResNet.safetensors" \
    "$binary_dir/LocalResNet.safetensors"
mv "$work_dir/resnet.I" "$lowered_dir/resnet.I"
mv "$work_dir/resnet.t" "$lowered_dir/resnet.t"
mv "$work_dir/resnet.s" "$lowered_dir/resnet.s"

"$ir_b2a" -st -src "$binary_dir/resnet.B" "$binary_dir/resnet.T"

for artifact in \
    "$work_dir/resnet.py" \
    "$driver_log" \
    "$binary_dir/resnet.B" \
    "$binary_dir/resnet.T" \
    "$binary_dir/LocalResNet.safetensors" \
    "$lowered_dir/resnet.I" \
    "$lowered_dir/resnet.t" \
    "$lowered_dir/resnet.s"; do
    if [ ! -s "$artifact" ]; then
        echo "openpy did not retain nonempty artifact: $artifact" >&2
        exit 1
    fi
done

grep -q 'source files:' "$binary_dir/resnet.T"
grep -q 'resnet.py' "$binary_dir/resnet.T"
grep -q 'cnn.conv2d' "$binary_dir/resnet.T"
grep -q 'common.residual_add' "$binary_dir/resnet.T"
grep -q 'Types:' "$binary_dir/resnet.T"
grep -q 'cnn.conv2d' "$lowered_dir/resnet.t"

echo "retained openpy ResNet artifacts: $work_dir"
echo "openpy ResNet artifact test passed"
