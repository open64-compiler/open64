# torch2whirl

`torch2whirl` is the standalone Open64 Python/PyTorch frontend driver.
Organizationally, it sits beside `clang2whirl`: a source frontend that emits
WHIRL for the rest of the Open64 pipeline.

This directory owns the current PyTorch ingestion program name and CLI.  The
native DSL builder remains the stable compiler boundary underneath it, so a
different Python ingestion implementation can be adopted later without forcing
changes into the common WHIRL/DSL infrastructure.

Keep this bridge isolated from backend code generation.  `torch2whirl` should
depend on common WHIRL/DSL construction interfaces and emit an artifact for
`opencc -x whirl`; it should not link against `osprey/be/cg` internals.

The staged boundary is:

1. capture a Python/PyTorch program,
2. construct DSL tensor WHIRL through the native builder,
3. write a binary WHIRL artifact,
4. hand the artifact to `opencc -x whirl`.

Combined driver mode is intentionally deferred:

```text
opencc -frontend=torch2whirl model.py ...
```

That mode should become only a convenience wrapper around the same binary WHIRL
artifact.  It must not bypass mapped-image finalization, `ir_b2a -st`
visibility, TensorDescriptorIR persistence, or gatekeeper validation.

This directory currently contains the buildable executable scaffold.  The next
implementation slice should replace the placeholder conversion path with calls
into the native DSL builder and mapped image finalization API.

The Python package skeleton lives under `python/open64_dsc`.  It provides the
initial public API and a mock `_whirl`-shaped backend so graph capture and CLI
integration can grow without requiring the native extension to be built first.
The first Phase 8 Python CLI slice is available as `python -m open64_dsc.cli`.
It loads a Python model file through a `create_model()` factory by default,
builds sample inputs from `--sample-input shape:d0,d1,...`, calls
`export_to_whirl`, and writes an artifact with `save_as_whirl`.  The C++
`torch2whirl` executable is wired to the same CLI, preserving the standalone
binary artifact boundary while the native WHIRL writer continues to mature.
The first native binding seam is staged under `python/native`.  It can be
syntax-checked through the Linux native fixture, and the configured Linux build
can now build and import `open64_dsc._whirl` when Python development headers are
available.
The first native API slice returns opaque integer handles for
`create_tensor_type`, `create_tensor_constant`, and `create_operator`; Python
does not own WHIRL table layout or node allocation.
`open64_dsc.builder.WhirlBuilder` wraps those raw backend calls so the
interpreter can construct tensors and operators through a stable facade.
The current interpreter skeleton records tensor type, value, and operator
manifest entries for example inputs, including a first placeholder `common.add`
when two inputs are present.
It also attaches tensor descriptors and placeholder symbol metadata through the
builder facade, including dtype, rank, logical shape, source name, and lowering
hint fields.
The first optional graph-capture hook uses `torch.fx` when PyTorch is installed:
a traced `lhs + rhs` graph maps to the first-class `common.add` operator.
Environments without PyTorch keep using the synthetic placeholder graph and skip
the FX-specific tests.
The mapping table also recognizes FX `matmul` as `common.matmul`, FX `relu` as
`common.relu`, and FX `flatten` as `common.flatten`; native validation covers
direct marker inspection, and the artifact smoke checks that `ir_b2a -st` can
print a Python-produced file containing `common.add`, `common.matmul`,
`common.relu`, `common.flatten`, and `common.output_logits`.
`make -f Makefile.gbase python_native_extension` builds that extension when
Python development headers and `OPEN64_DSC_NATIVE_OBJS` are supplied.
Configured builds set `OPEN64_DSC_NATIVE_OBJS` to the sibling `ir_tools`
common object set and check those inputs through
`make -f Makefile.gbase python_native_requirements`.
Use `make python_native_deps` from the configured `torch2whirl` build
directory to populate the Open64 common object inputs without building
backend/cg.
Use `make python_native_test` from that directory to run the optional native
Python finalization test after installing the matching Python development
headers in the test environment.
Use `make python_native_ir_tools_smoke` from the same directory to generate a
native Python WHIRL artifact and inspect it with `ir_b2a -st -src`.  In a configured
`--enable-torch2whirl-only` build, this target first builds the additional
tool-side `libjsoncpp.a` and `ir_b2a`/`ir_a2b` pieces needed for inspection.
Those reader/WSSA objects are not linked into the `_whirl` frontend extension.
The `.B`, `.safetensors`, and `.T` files are retained under
`$(OPEN64_DSL_TEST_ARTIFACT_DIR)/python-native`.  The target removes the prior
files in that artifact family before each run.
Use `make driver_native_ir_tools_smoke` from a torch-enabled configured build
to run the same native artifact inspection through the C++ `torch2whirl`
executable.  Its files are retained under
`$(OPEN64_DSL_TEST_ARTIFACT_DIR)/driver-native` and refreshed before each run.
Use `make driver_opencc_smoke` from a full-toolchain environment to run the
C++ driver with `--backend native` and verify that `opencc -x whirl -c`
consumes the generated artifact. This target skips cleanly when `opencc` is not
available.
For a full Open64 build, set `OPEN64_IR_B2A=/path/to/ir_b2a` to point at an
existing tool and `OPEN64_OPENCC=/path/to/opencc` to point at the compiler
driver.
The `--enable-torch2whirl-only` configure path emits the small helper build
files needed for that dependency target.

## Standalone build

From an out-of-tree build directory on macOS or Linux:

```sh
/path/to/open64/configure --enable-torch2whirl-only
make -C osprey/targdir/torch2whirl
./osprey/targdir/torch2whirl/torch2whirl --help
```

For quick local iteration from this source directory:

```sh
make -f Makefile.gbase
make -f Makefile.gbase python_test
make -f Makefile.gbase python_native_check
```

When PyTorch is installed, run the hard FX ingestion lane:

```sh
make -f Makefile.gbase python_torch_test
make -f Makefile.gbase driver_torch_test
```

Unlike `python_test`, these targets intentionally fail if `torch` is missing.

To exercise the Phase 8 Python CLI directly:

```sh
PYTHONPATH=/path/to/open64/osprey/torch2whirl/python \
  python3 -m open64_dsc.cli model.py \
  --sample-input shape:1,3,224,224 \
  -o model.B
```

To exercise the same path through the C++ executable:

```sh
PYTHONPATH=/path/to/open64/osprey/torch2whirl/python \
  ./torch2whirl model.py \
  --sample-input shape:1,3,224,224 \
  -o model.B
```

For a reproducible Linux Docker version of that lane:

```sh
osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

The script builds `open64:torch2whirl-torch-test` from the existing Open64
Docker image, installs CPU PyTorch, configures a `--enable-torch2whirl-only`
tree under `/private/tmp/open64-torch2whirl-torch-test`, and runs
`make python_torch_test` plus `make driver_torch_test` from the configured
`torch2whirl` directory. Override
`OPEN64_TORCH2WHIRL_TORCH_VERSION`, `OPEN64_TORCH2WHIRL_BASE_IMAGE`,
`OPEN64_TORCH2WHIRL_TORCH_IMAGE`, `OPEN64_TORCH2WHIRL_BUILD_DIR`, or
`OPEN64_TORCH2WHIRL_DOCKER_BUILDKIT` when a different local image, torch
version, build directory, or Docker builder mode is needed. Set
`OPEN64_TORCH2WHIRL_REBUILD_IMAGE=1` to refresh an existing torch image. The
script defaults `OPEN64_TORCH2WHIRL_DOCKER_BUILDKIT=0` so local-only Open64
base images are not resolved through a remote registry.
The script mounts the host directory
`$OPEN64_TORCH2WHIRL_ARTIFACT_DIR` at `/artifacts`, cleans it at startup, runs
the native `ir_b2a -st -src` model certification lanes, and leaves source,
`.B`, `.T`, side payloads, and diagnostic logs after Docker exits. It verifies
the bind mount before starting validation and
defaults to the persistent host directory
`<open64-source-root>/artifacts/torch2whirl`. For example, this checkout uses
`/Users/shinmingliu/open64/artifacts/torch2whirl`. Set
`OPEN64_TORCH2WHIRL_ARTIFACT_DIR` to choose another persistent host directory.
The completed run writes `MANIFEST.txt` at the artifact root. Model evidence is
grouped under `resnet`, `llama2-prefill`, and `llama2-decode`; native common
operator probes are grouped under `python-native`.
Llama artifact families retain local imported model modules under
`source/models`, including the `TinyRMSNorm` definition used by decode. The
model `.B` is the complete compilation artifact and contains each instantiated
logical operator; imported Python classes are source definitions and do not
produce separate `.B` files.
Set
`OPEN64_TORCH2WHIRL_RUN_IR_TOOLS=0` only when running the shorter PyTorch lane.

To certify the combined driver and retain its review artifacts, use a full
Open64 build tree:

```sh
OPEN64_OPENPY_BUILD_DIR=/path/to/open64/build \
  osprey/torch2whirl/scripts/run_openpy_docker_test.sh
```

This runs `openpy -keep -O0` on the ResNet fixture and retains:

```text
artifacts/torch2whirl/openpy/resnet/resnet.py
artifacts/torch2whirl/openpy/resnet/openpy_driver.log
artifacts/torch2whirl/openpy/resnet/binary/resnet.B
artifacts/torch2whirl/openpy/resnet/binary/resnet.T
artifacts/torch2whirl/openpy/resnet/binary/resnet.safetensors
artifacts/torch2whirl/openpy/resnet/lowered/resnet.t
artifacts/torch2whirl/openpy/resnet/lowered/resnet.s
```

Driver-specific intermediates such as `resnet.I` are retained under `lowered`
when produced, but are not required by the cross-version artifact contract.

Binary and lowered traces use separate directories because `.T` and `.t`
refer to the same filename on the default case-insensitive macOS filesystem.

For Linux Docker native extension validation from a configured build tree:

```sh
docker run --rm -v /path/to/open64:/src \
  -v /private/tmp/open64-torch2whirl-linux:/build \
  -w /build/osprey/targdir/torch2whirl \
  open64:x86_64-apple-silicon \
  sh -c 'apt-get update && apt-get install -y python3.8-dev && make python_native_test'
```

For optional `ir_b2a -st -src` inspection of the native Python artifact:

```sh
make python_native_ir_tools_smoke
```

For optional `ir_b2a -st -src` inspection of a native artifact produced through the
C++ driver:

```sh
make driver_native_ir_tools_smoke
```

For optional full-toolchain consumption of a native artifact produced through
the C++ driver:

```sh
make driver_opencc_smoke
```

The broader `osprey/common/com/tests/dsl_ir_tools_smoke_test.sh` fixture still
requires `opencc` to create its C-derived `smoke.B`.  Run it when a full Open64
toolchain is available through `OPEN64_OPENCC`, `OPEN64_IR_B2A`, and
`OPEN64_IR_A2B`.

The generated build remains under `osprey/targdir/torch2whirl`, matching the
Open64 build tree layout, but does not require the full Open64 compiler or
backend build to be configured on the host.
