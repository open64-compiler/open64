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

mkdir -p "$build_dir"

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
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -w /build \
    "$torch_image" \
    /src/configure --enable-torch2whirl-only

docker run --rm \
    -v "$src_root:/src" \
    -v "$build_dir:/build" \
    -w /build/osprey/targdir/torch2whirl \
    "$torch_image" \
    make python_torch_test
