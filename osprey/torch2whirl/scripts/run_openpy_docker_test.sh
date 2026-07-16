#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
src_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
build_dir=${OPEN64_OPENPY_BUILD_DIR:-$src_root/build}
artifact_dir=${OPEN64_TORCH2WHIRL_ARTIFACT_DIR:-$src_root/artifacts/torch2whirl}
torch_image=${OPEN64_TORCH2WHIRL_TORCH_IMAGE:-open64:torch2whirl-torch-test}

if [ ! -x "$build_dir/osprey/targdir/driver/openpy" ]; then
    echo "full openpy build not found: $build_dir" >&2
    echo "set OPEN64_OPENPY_BUILD_DIR to a full Open64 build tree" >&2
    exit 1
fi

mkdir -p "$artifact_dir"
artifact_dir=$(CDPATH= cd -- "$artifact_dir" && pwd)
if [ "$artifact_dir" = / ] || [ "$artifact_dir" = "$src_root" ]; then
    echo "refusing unsafe artifact directory: $artifact_dir" >&2
    exit 1
fi
touch "$artifact_dir/.open64-artifact-bind"

docker run --rm --platform linux/amd64 \
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -v "$artifact_dir:/artifacts" \
    -e OPEN64_BUILD_DIR=/build \
    -e OPEN64_TORCH2WHIRL_ARTIFACT_DIR=/artifacts \
    "$torch_image" \
    /src/osprey/driver/tests/openpy_resnet_artifact_test.sh

find "$artifact_dir" -type f \
    ! -name MANIFEST.txt ! -name .open64-artifact-bind \
    | sed "s|^$artifact_dir/||" \
    | LC_ALL=C sort > "$artifact_dir/MANIFEST.txt"
echo "retained openpy artifacts: $artifact_dir/openpy"
