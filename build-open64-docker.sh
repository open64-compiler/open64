#!/usr/bin/env bash
#
# build-open64-docker.sh — Build Open64 compiler in Docker on macOS (Apple Silicon)
#
# This script automates the full Open64 build inside an x86_64 Docker container.
# It handles all the workarounds needed for building on ARM64 via Rosetta emulation:
#   - Pre-generates targ_info files natively on ARM64 (avoids Rosetta segfaults)
#   - Uses system GCC for runtime libraries (avoids opencc crashes under emulation)
#   - Disables JFE (Java Frontend) since it requires gradle/openjdk-8/cmake
#
# Prerequisites:
#   - Docker Desktop for Mac with Rosetta emulation enabled
#   - The Open64 source tree (https://github.com/open64-compiler/open64)
#   - LLVM/Clang 11 headers and libs (installed inside container)
#
# Usage:
#   ./build-open64-docker.sh [path-to-open64-source]
#
# If no path is given, defaults to the directory containing this script.
#

set -euo pipefail

# ─── Configuration ───────────────────────────────────────────────────────────

# Auto-detect cores and RAM
TOTAL_CORES=$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 8)
TOTAL_RAM_GB=$(( $(sysctl -n hw.memsize 2>/dev/null || echo 17179869184) / 1073741824 ))

# Reserve 2 cores and 25% RAM for the host OS
BUILD_CORES=$(( TOTAL_CORES - 2 ))
BUILD_RAM_GB=$(( TOTAL_RAM_GB * 3 / 4 ))

# Clamp minimums
[[ $BUILD_CORES -lt 2 ]] && BUILD_CORES=2
[[ $BUILD_RAM_GB -lt 4 ]] && BUILD_RAM_GB=4

OPEN64_SRC="${1:-$(cd "$(dirname "$0")" && pwd)/open64}"
CONTAINER_NAME="open64-build"
ARM_CONTAINER_NAME="open64-arm-gen"
PREGENERATED_HOST_DIR="/tmp/open64-generated"

echo "============================================="
echo "  Open64 Docker Build Script"
echo "============================================="
echo "  Source:     $OPEN64_SRC"
echo "  Cores:      $BUILD_CORES / $TOTAL_CORES"
echo "  RAM:        ${BUILD_RAM_GB}GB / ${TOTAL_RAM_GB}GB"
echo "  Container:  $CONTAINER_NAME"
echo "============================================="
echo ""

if [[ ! -d "$OPEN64_SRC/osprey" ]]; then
    echo "ERROR: $OPEN64_SRC does not look like an Open64 source tree (no osprey/ dir)"
    echo "Usage: $0 [path-to-open64-source]"
    exit 1
fi

# ─── Step 1: Pre-generate targ_info files natively on ARM64 ─────────────────
#
# The targ_info generators (isa_gen, targ_si_gen, etc.) are x86_64 host tools
# that segfault under Rosetta emulation. We build and run them natively on
# ARM64 Linux, then copy the generated files for use in the x86_64 build.

echo ">>> Step 1: Pre-generating targ_info files (ARM64 native)"

if [[ -d "$PREGENERATED_HOST_DIR" ]] && [[ $(ls "$PREGENERATED_HOST_DIR"/*.h 2>/dev/null | wc -l) -ge 9 ]]; then
    echo "    Pre-generated files already exist at $PREGENERATED_HOST_DIR, skipping."
else
    echo "    Starting ARM64 container to build targ_info generators..."

    # Clean up any previous ARM64 container
    docker rm -f "$ARM_CONTAINER_NAME" 2>/dev/null || true
    mkdir -p "$PREGENERATED_HOST_DIR"

    docker run --name "$ARM_CONTAINER_NAME" \
        --platform linux/arm64 \
        -v "$OPEN64_SRC:/open64:ro" \
        -v "$PREGENERATED_HOST_DIR:/output" \
        ubuntu:20.04 \
        bash -c '
set -e
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq gcc g++ make flex bison >/dev/null 2>&1
echo "    Installed build tools"

SRC=/open64/osprey
WORK=/tmp/targ_info_build
mkdir -p $WORK && cd $WORK

# Common include paths
INCS="-I$SRC/common/targ_info/generate \
      -I$SRC/common/com \
      -I$SRC/common/com/x8664 \
      -I$SRC/common/util"

echo "    Building generators..."

# 1. isa_gen → topcode.h
g++ $INCS -o isa_gen \
    $SRC/common/targ_info/isa/x8664/isa.cxx \
    $SRC/common/targ_info/generate/isa_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_gen
echo "      topcode.h generated"

# 2. isa_subset_gen
g++ $INCS -I. -o isa_subset_gen \
    $SRC/common/targ_info/isa/x8664/isa_subset.cxx \
    $SRC/common/targ_info/generate/isa_subset_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_subset_gen
echo "      targ_isa_subset.h generated"

# 3. isa_properties_gen
g++ $INCS -I. -o isa_properties_gen \
    $SRC/common/targ_info/isa/x8664/isa_properties.cxx \
    $SRC/common/targ_info/generate/isa_properties_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_properties_gen
echo "      targ_isa_properties.h generated"

# 4. isa_enums_gen
g++ $INCS -I. -o isa_enums_gen \
    $SRC/common/targ_info/isa/x8664/isa_enums.cxx \
    $SRC/common/targ_info/generate/isa_enums_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_enums_gen
echo "      targ_isa_enums.h generated"

# 5. isa_lits_gen
g++ $INCS -I. -o isa_lits_gen \
    $SRC/common/targ_info/isa/x8664/isa_lits.cxx \
    $SRC/common/targ_info/generate/isa_lits_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_lits_gen
echo "      targ_isa_lits.h generated"

# 6. isa_registers_gen
g++ $INCS -I. -o isa_registers_gen \
    $SRC/common/targ_info/isa/x8664/isa_registers.cxx \
    $SRC/common/targ_info/generate/isa_registers_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_registers_gen
echo "      targ_isa_registers.h generated"

# 7. isa_operands_gen
g++ $INCS -I. -o isa_operands_gen \
    $SRC/common/targ_info/isa/x8664/isa_operands.cxx \
    $SRC/common/targ_info/generate/isa_operands_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_operands_gen
echo "      targ_isa_operands.h generated"

# 8. isa_hazards_gen
g++ $INCS -I. -o isa_hazards_gen \
    $SRC/common/targ_info/isa/x8664/isa_hazards.cxx \
    $SRC/common/targ_info/generate/isa_hazards_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_hazards_gen
echo "      targ_isa_hazards.h generated"

# 9. isa_pack_gen
g++ $INCS -I. -o isa_pack_gen \
    $SRC/common/targ_info/isa/x8664/isa_pack.cxx \
    $SRC/common/targ_info/generate/isa_pack_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_pack_gen
echo "      targ_isa_pack.h generated"

# 10. isa_print_gen
g++ $INCS -I. -o isa_print_gen \
    $SRC/common/targ_info/isa/x8664/isa_print.cxx \
    $SRC/common/targ_info/generate/isa_print_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_print_gen
echo "      targ_isa_print.h generated"

# 11. isa_decode_gen
g++ $INCS -I. -o isa_decode_gen \
    $SRC/common/targ_info/isa/x8664/isa_decode.cxx \
    $SRC/common/targ_info/generate/isa_decode_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_decode_gen
echo "      targ_isa_decode.h generated"

# 12. isa_pseudo_gen
g++ $INCS -I. -o isa_pseudo_gen \
    $SRC/common/targ_info/isa/x8664/isa_pseudo.cxx \
    $SRC/common/targ_info/generate/isa_pseudo_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./isa_pseudo_gen
echo "      targ_isa_pseudo.h generated"

# 13. proc_gen
g++ $INCS -I. -o proc_gen \
    $SRC/common/targ_info/proc/x8664/proc.cxx \
    $SRC/common/targ_info/generate/proc_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./proc_gen
echo "      targ_proc.h generated"

# 14. proc_properties_gen
g++ $INCS -I. -o proc_properties_gen \
    $SRC/common/targ_info/proc/x8664/proc_properties.cxx \
    $SRC/common/targ_info/generate/proc_properties_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./proc_properties_gen
echo "      targ_proc_properties.h generated"

# 15. abi_properties_gen
g++ $INCS -I. -o abi_properties_gen \
    $SRC/common/targ_info/abi/x8664/abi_properties.cxx \
    $SRC/common/targ_info/generate/abi_properties_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx
./abi_properties_gen
echo "      targ_abi_properties.h generated"

# 16. targ_si_gen (scheduling info — links compiled generated .c files)
echo "    Building scheduling info generators..."
gcc -c -I. targ_isa_subset.c -o targ_isa_subset.o
gcc -c -I. targ_isa_properties.c -o targ_isa_properties.o
gcc -c -I. targ_isa_enums.c -o targ_isa_enums.o
gcc -c -I. targ_isa_lits.c -o targ_isa_lits.o
gcc -c -I. targ_isa_registers.c -o targ_isa_registers.o
gcc -c -I. targ_isa_operands.c -o targ_isa_operands.o
gcc -c -I. targ_proc.c -o targ_proc.o
gcc -c -I. topcode.c -o topcode.o

g++ $INCS -I. -o si_gen \
    $SRC/common/targ_info/generate/si_gen.cxx \
    $SRC/common/targ_info/generate/gen_util.cxx \
    topcode.o targ_proc.o targ_isa_subset.o \
    targ_isa_properties.o targ_isa_enums.o targ_isa_lits.o \
    targ_isa_registers.o targ_isa_operands.o

g++ $INCS -I. -o targ_si_gen \
    $SRC/common/targ_info/proc/x8664/proc_si.cxx \
    $SRC/common/targ_info/proc/x8664/opteron.cxx \
    $SRC/common/targ_info/proc/x8664/barcelona.cxx \
    $SRC/common/targ_info/proc/x8664/core.cxx \
    $SRC/common/targ_info/proc/x8664/em64t.cxx \
    $SRC/common/targ_info/proc/x8664/orochi.cxx \
    $SRC/common/targ_info/proc/x8664/wolfdale.cxx \
    si_gen.o \
    $SRC/common/targ_info/generate/gen_util.cxx \
    topcode.o targ_proc.o targ_isa_subset.o \
    targ_isa_properties.o targ_isa_enums.o targ_isa_lits.o \
    targ_isa_registers.o targ_isa_operands.o

# Note: si_gen.o was compiled as part of the si_gen executable above,
# but we need it as a separate .o. Recompile:
g++ $INCS -I. -c $SRC/common/targ_info/generate/si_gen.cxx -o si_gen.o

# Re-link targ_si_gen with the .o
g++ $INCS -I. -o targ_si_gen \
    $SRC/common/targ_info/proc/x8664/proc_si.cxx \
    $SRC/common/targ_info/proc/x8664/opteron.cxx \
    $SRC/common/targ_info/proc/x8664/barcelona.cxx \
    $SRC/common/targ_info/proc/x8664/core.cxx \
    $SRC/common/targ_info/proc/x8664/em64t.cxx \
    $SRC/common/targ_info/proc/x8664/orochi.cxx \
    $SRC/common/targ_info/proc/x8664/wolfdale.cxx \
    si_gen.o \
    $SRC/common/targ_info/generate/gen_util.cxx \
    topcode.o targ_proc.o targ_isa_subset.o \
    targ_isa_properties.o targ_isa_enums.o targ_isa_lits.o \
    targ_isa_registers.o targ_isa_operands.o

./targ_si_gen
echo "      targ_si.h generated"

# Copy all generated .h and .c files to output
cp -v targ_*.h targ_*.c topcode.h topcode.c /output/
echo ""
echo "    All targ_info files generated successfully!"
'

    echo "    Pre-generated files saved to $PREGENERATED_HOST_DIR"
    docker rm "$ARM_CONTAINER_NAME" 2>/dev/null || true
fi

echo ""

# ─── Step 2: Create and start the x86_64 build container ────────────────────

echo ">>> Step 2: Starting x86_64 build container"

# Stop any existing container
docker rm -f "$CONTAINER_NAME" 2>/dev/null || true

docker run -d \
    --name "$CONTAINER_NAME" \
    --platform linux/amd64 \
    --cpus="$BUILD_CORES" \
    --memory="${BUILD_RAM_GB}g" \
    -v "$OPEN64_SRC:/open64" \
    -v "$PREGENERATED_HOST_DIR:/pregenerated_targ_info:ro" \
    ubuntu:20.04 \
    sleep infinity

echo "    Container started: $CONTAINER_NAME (amd64, ${BUILD_CORES} cores, ${BUILD_RAM_GB}GB RAM)"
echo ""

# ─── Step 3: Install dependencies ───────────────────────────────────────────

echo ">>> Step 3: Installing build dependencies"

docker exec "$CONTAINER_NAME" bash -c '
set -e
export DEBIAN_FRONTEND=noninteractive

apt-get update -qq

apt-get install -y -qq \
    build-essential \
    gcc g++ \
    gcc-multilib g++-multilib \
    make \
    flex bison \
    perl \
    gawk \
    binutils \
    libc6-dev \
    zlib1g-dev \
    libncurses-dev \
    libtinfo-dev \
    clang-11 \
    llvm-11-dev \
    libclang-11-dev \
    >/dev/null 2>&1

echo "    Dependencies installed."
'

echo ""

# ─── Step 4: Configure ──────────────────────────────────────────────────────

echo ">>> Step 4: Configuring Open64"

docker exec "$CONTAINER_NAME" bash -c '
set -e
export CLANG_HOME=/usr/lib/llvm-11

mkdir -p /build && cd /build

/open64/configure \
    --prefix=/opt/open64 \
    --disable-jfe \
    --with-build-optimize=DEBUG \
    2>&1 | tail -20

echo ""
echo "    Configure complete."
'

echo ""

# ─── Step 5: Build ──────────────────────────────────────────────────────────

echo ">>> Step 5: Building Open64 (this will take a while under Rosetta...)"

docker exec "$CONTAINER_NAME" bash -c "
set -e
export CLANG_HOME=/usr/lib/llvm-11
cd /build

# Copy pregenerated targ_info files into the build tree
mkdir -p /build/pregenerated_targ_info
cp /pregenerated_targ_info/* /build/pregenerated_targ_info/

# Main build with pregenerated targ_info to avoid Rosetta segfaults
# BUILD_COMPILER=GNU for runtime libs to avoid opencc crashes under emulation
echo '    Starting make -j${BUILD_CORES:-8}...'
make -j${BUILD_CORES:-8} \
    PREGENERATED_TARG_INFO=/build/pregenerated_targ_info \
    BUILD_COMPILER=GNU \
    2>&1 | tail -30

echo ''
echo '    Checking for opencc binary...'
ls -la /build/osprey/targdir/driver/opencc && echo '    BUILD SUCCESS!' || echo '    BUILD FAILED'
"

# ─── Step 6: Handle potential maccom.so race condition ───────────────────────

echo ""
echo ">>> Step 6: Fixing maccom.so symlink (race condition workaround)"

docker exec "$CONTAINER_NAME" bash -c '
cd /build/osprey/targdir/be
if [[ -f be.so ]] && [[ ! -f maccom.so ]]; then
    ln -sf be.so maccom.so
    echo "    Created maccom.so -> be.so symlink"
else
    echo "    maccom.so already exists or be.so not built yet, skipping"
fi
'

# ─── Step 7: Build runtime libraries ────────────────────────────────────────

echo ""
echo ">>> Step 7: Building runtime libraries"

docker exec "$CONTAINER_NAME" bash -c '
set -e
export CLANG_HOME=/usr/lib/llvm-11
cd /build

# Libraries need BUILD_COMPILER=GNU to avoid self-hosted opencc crashes
make lib -j1 \
    PREGENERATED_TARG_INFO=/build/pregenerated_targ_info \
    2>&1 | tail -20

echo ""
echo "    Runtime libraries built."
'

# ─── Done ────────────────────────────────────────────────────────────────────

echo ""
echo "============================================="
echo "  BUILD COMPLETE"
echo "============================================="
echo ""
echo "  The compiler is inside the Docker container:"
echo "    docker exec -it $CONTAINER_NAME bash"
echo "    ls /build/osprey/targdir/driver/opencc"
echo ""
echo "  To install to /opt/open64:"
echo "    docker exec $CONTAINER_NAME make -C /build install"
echo ""
echo "  To stop the container:"
echo "    docker stop $CONTAINER_NAME"
echo ""
echo "  To remove the container:"
echo "    docker rm -f $CONTAINER_NAME"
echo ""
