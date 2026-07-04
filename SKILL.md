---
name: open64-torch2whirl
description: Build, run, and smoke-test the Open64 torch2whirl standalone Python/PyTorch frontend. Use when working in the Open64 tree on osprey/torch2whirl, configure --enable-torch2whirl-only, Linux Docker validation, macOS portability, or frontend/backend isolation checks.
---

# Open64 Torch2whirl

## Overview

Use this skill to keep `torch2whirl` development focused on the standalone frontend bridge under the Open64 build umbrella. Preserve the organizational model of `clang2whirl`, but avoid dependencies on backend code such as `osprey/be/cg`.

## Source Layout

- Treat `osprey/torch2whirl` as the source directory for the standalone executable.
- Treat `osprey/targdir/torch2whirl/Makefile.in` as the configured build-tree entry point.
- Keep command-line driver code portable and frontend-only.
- Use C++17 for `torch2whirl`, while following the plain Open64 style used around `osprey/be/opt`.

## Build-Tree Smoke Test

Prefer the lightweight configure path while `torch2whirl` is under active development:

```bash
mkdir -p /private/tmp/open64-torch2whirl-macos
cd /private/tmp/open64-torch2whirl-macos
/path/to/open64/configure --enable-torch2whirl-only
make -C osprey/targdir/torch2whirl clean all
./osprey/targdir/torch2whirl/torch2whirl --help
./osprey/targdir/torch2whirl/torch2whirl --version
./osprey/targdir/torch2whirl/torch2whirl -o out.whirl model.py
```

Expected current behavior:

- `--help` exits 0 and prints usage.
- `--version` exits 0 and prints `torch2whirl 0.1`.
- `-o out.whirl model.py` exits 1 with the current "conversion is not implemented yet" placeholder until the ingestion path exists.

## Direct Source Build

Use this quick loop when only the local executable scaffold changed:

```bash
cd /path/to/open64/osprey/torch2whirl
make -f Makefile.gbase clean all
./torch2whirl --help
./torch2whirl --version
make -f Makefile.gbase clean
```

## Linux Docker Validation

Use the existing Linux Docker image to confirm `torch2whirl` builds and runs as Linux native software:

```bash
mkdir -p /private/tmp/open64-torch2whirl-linux
docker run --rm -v /path/to/open64:/src -v /private/tmp/open64-torch2whirl-linux:/build -w /build open64:x86_64-apple-silicon /src/configure --enable-torch2whirl-only
docker run --rm -v /path/to/open64:/src -v /private/tmp/open64-torch2whirl-linux:/build -w /build/osprey/targdir/torch2whirl open64:x86_64-apple-silicon make clean all
docker run --rm -v /private/tmp/open64-torch2whirl-linux:/build -w /build/osprey/targdir/torch2whirl open64:x86_64-apple-silicon ./torch2whirl --help
docker run --rm -v /private/tmp/open64-torch2whirl-linux:/build -w /build/osprey/targdir/torch2whirl open64:x86_64-apple-silicon ./torch2whirl --version
docker run --rm -v /private/tmp/open64-torch2whirl-linux:/build -w /build/osprey/targdir/torch2whirl open64:x86_64-apple-silicon ./torch2whirl -o out.whirl model.py
```

The Docker platform warning on Apple Silicon is acceptable if the commands complete; the container is still validating the Linux build.

## Isolation Checks

Before finishing frontend changes, scan for accidental backend coupling:

```bash
rg -n "be/cg|BE_CG|ercg|erauxdesc|#include .*be/" osprey/torch2whirl osprey/targdir/torch2whirl
```

README text may mention backend isolation as a warning, but source and makefiles should not depend on backend/cg headers, libraries, or object files.

## Full Build Caveat

The full Open64 configure/build may require unrelated frontend dependencies such as `CLANG_HOME`. Use `--enable-torch2whirl-only` when the task is limited to the standalone frontend scaffold or its portable smoke tests.
