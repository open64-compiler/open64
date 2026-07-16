#!/bin/sh
#
# Build and run the torch-enabled torch2whirl validation lane.

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
src_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)

build_dir=${OPEN64_TORCH2WHIRL_BUILD_DIR:-/private/tmp/open64-torch2whirl-torch-test}
base_image=${OPEN64_TORCH2WHIRL_BASE_IMAGE:-open64:x86_64-apple-silicon}
torch_image=${OPEN64_TORCH2WHIRL_TORCH_IMAGE:-open64:torch2whirl-torch-test}
torch_version=${OPEN64_TORCH2WHIRL_TORCH_VERSION:-2.4.1}
docker_buildkit=${OPEN64_TORCH2WHIRL_DOCKER_BUILDKIT:-0}
rebuild_image=${OPEN64_TORCH2WHIRL_REBUILD_IMAGE:-0}
run_ir_tools=${OPEN64_TORCH2WHIRL_RUN_IR_TOOLS:-1}
artifact_dir=${OPEN64_TORCH2WHIRL_ARTIFACT_DIR:-$src_root/artifacts/torch2whirl}

mkdir -p "$build_dir"
mkdir -p "$artifact_dir"
artifact_dir=$(CDPATH= cd -- "$artifact_dir" && pwd)
if [ "$artifact_dir" = / ] || [ "$artifact_dir" = "$src_root" ]; then
    echo "refusing unsafe artifact directory: $artifact_dir" >&2
    exit 1
fi
find "$artifact_dir" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
touch "$artifact_dir/.open64-artifact-bind"

if [ "$rebuild_image" = 1 ] ||
        ! docker image inspect "$torch_image" >/dev/null 2>&1; then
    DOCKER_BUILDKIT=$docker_buildkit docker build \
        -f "$src_root/osprey/torch2whirl/docker/Dockerfile.torch-test" \
        --build-arg "BASE_IMAGE=$base_image" \
        --build-arg "TORCH_VERSION=$torch_version" \
        -t "$torch_image" \
        "$src_root/osprey/torch2whirl/docker"
else
    echo "reusing existing Docker image: $torch_image"
fi

docker run --rm \
    -v "$artifact_dir:/artifacts" \
    "$torch_image" \
    test -f /artifacts/.open64-artifact-bind

docker run --rm \
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -v "$artifact_dir:/artifacts" \
    -w /build \
    "$torch_image" \
    /src/configure --enable-torch2whirl-only

docker run --rm \
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -v "$artifact_dir:/artifacts" \
    -w /build/osprey/targdir/torch2whirl \
    "$torch_image" \
    make python_torch_test

docker run --rm \
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -v "$artifact_dir:/artifacts" \
    -w /build/osprey/targdir/torch2whirl \
    "$torch_image" \
    make driver_torch_test

if [ "$run_ir_tools" = 1 ]; then
    docker run --rm \
        -v "$src_root:/src" \
        -v "$build_dir:/build" \
        -v "$artifact_dir:/artifacts" \
        -w /build/osprey/targdir/torch2whirl \
        "$torch_image" \
        make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
            python_native_ir_tools_smoke driver_native_ir_tools_smoke \
            llama2_decode_native_ir_tools_smoke
fi

echo "retained test artifacts: $artifact_dir"
