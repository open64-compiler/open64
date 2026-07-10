# torch2whirl Agent Guide

This directory owns the standalone Open64 Python/PyTorch frontend driver.
Keep work scoped to `torch2whirl` source, its configured build entry under
`osprey/targdir/torch2whirl`, and the Python package under
`osprey/torch2whirl/python`.

## Direction

- Preserve the standalone frontend boundary: `torch2whirl` may use common
  WHIRL/DSL builder interfaces, but must not depend on backend code generation
  internals such as `osprey/be/cg`.
- Keep the public Python API stable unless the task explicitly asks to change
  it: `WhirlExportOptions`, `WhirlModule`, `export_to_whirl`, and
  `save_as_whirl`.
- Keep the native builder API narrow. Do not turn it into a generic
  Python-owned WHIRL construction API before the artifact path is inspectable
  through `ir_b2a -st`.
- Do not use tab characters in files touched under this directory, except
  Makefiles where tabs are required by standard make syntax.

## Verification Matrix

Run the smallest relevant set first, then broaden when the change affects a
shared path.

For any source or documentation change:

```sh
git diff --check
git diff -U0 -- ':!osprey/torch2whirl/Makefile.gbase' | \
  perl -ne 'print if /^\+[^+].*\t/'
rg -n "be/cg|BE_CG|ercg|erauxdesc|#include .*be/" \
  osprey/torch2whirl osprey/targdir/torch2whirl
```

The backend-isolation scan may find intentional README or AGENTS guidance text.
It must not find source, makefile, or link dependencies on backend/cg
internals.

For C++ driver or Makefile scaffold changes:

```sh
cd osprey/torch2whirl
make -f Makefile.gbase clean all
./torch2whirl --help
./torch2whirl --version
make -f Makefile.gbase clean
```

For Python package, graph mapping, descriptor, or mock-backend changes:

```sh
cd osprey/torch2whirl
make -f Makefile.gbase python_test
```

For PyTorch/FX capture, operator ingestion, ResNet-fragment, or parameter
metadata changes:

```sh
osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

Use `OPEN64_TORCH2WHIRL_REBUILD_IMAGE=1` when the torch Dockerfile or pinned
Python dependencies change.

For native Python bridge changes in a configured Linux build tree:

```sh
docker run --rm -v /path/to/open64:/src \
  -v /private/tmp/open64-torch2whirl-linux:/build \
  -w /build/osprey/targdir/torch2whirl \
  open64:x86_64-apple-silicon \
  sh -c 'apt-get update && apt-get install -y python3.8-dev && make python_native_test'
```

For native artifact-inspection changes:

```sh
docker run --rm -v /path/to/open64:/src \
  -v /private/tmp/open64-torch2whirl-linux:/build \
  -w /build/osprey/targdir/torch2whirl \
  open64:x86_64-apple-silicon \
  sh -c 'apt-get update && apt-get install -y python3.8-dev && make python_native_ir_tools_smoke'
docker run --rm -v /path/to/open64:/src \
  -v /private/tmp/open64-torch2whirl-linux:/build \
  -w /build/osprey/targdir/torch2whirl \
  open64:torch2whirl-torch-test \
  sh -c 'apt-get update && apt-get install -y python3.8-dev && make driver_native_ir_tools_smoke'
```

For common DSL native syntax compatibility that affects torch2whirl:

```sh
docker run --rm -v /path/to/open64:/src \
  -w /src \
  open64:x86_64-apple-silicon \
  sh -c 'apt-get update && apt-get install -y python3.8-dev && osprey/common/com/tests/dsl_native_syntax_test.sh'
```

## Change-Type Guidance

- Documentation-only changes should at least run whitespace and backend
  isolation checks.
- Driver or build changes should include the direct source build and smoke
  commands.
- Python API or mock backend changes should include `python_test`.
- FX/PyTorch changes should include the torch Docker lane so optional tests
  execute rather than skip.
- Native bridge or mapped-image changes should include Docker native tests and
  `python_native_ir_tools_smoke` when available.
- Any change intended to move beyond marker-only DSL representation must add or
  update an artifact-inspection test before broadening operator coverage.
