# How To Build The Open64 DSL Compiler

The Open64 DSL compiler extends the normal Open64 compiler suite with a
Python/PyTorch ingestion path, first-class DSL WHIRL operators, tensor
metadata, source-aware inspection, and the `openpy` driver entry point.

The source-language boundary is:

```text
Python model / DSL
  -> torch2whirl
  -> binary very-high-level WHIRL artifact
  -> opencc -x whirl / openpy
  -> Open64 middle end and backend
```

`torch2whirl` is a standalone frontend under `osprey/torch2whirl`.  It writes a
binary WHIRL artifact and exits.  The Open64 middle end then reopens that
artifact through the normal mapped-image and ELF framework.  Python is not
linked into the middle end.

This document follows the same spirit as the top-level Open64 `README`: build
the compiler in Docker, keep the host tree mounted, and leave generated
artifacts where developers can inspect them after the container exits.

## Requirements

On macOS Apple Silicon:

1. Docker Desktop with Rosetta emulation enabled.
2. The Open64 source checkout.
3. Enough host space for a Docker build tree and retained artifacts.

On Linux/x86_64:

1. Docker.
2. The Open64 source checkout.
3. The same commands can be used without Rosetta-specific concern.

The default image names used by the repository are:

```text
open64:x86_64-apple-silicon
open64:torch2whirl-torch-test
```

The first image is the base Open64 build image.  The second image extends it
with Python development headers and CPU PyTorch for DSL frontend tests.

## Build The Base Open64 Docker Image

From the Open64 source directory:

```sh
./build-open64-docker.sh
```

The script builds the Open64 compiler as a Linux/x86_64 toolchain and creates
the reusable image:

```text
open64:x86_64-apple-silicon
```

By default, host build and install directories are placed under:

```text
~/work/open64/build
~/work/open64/opt/open64
~/work/open64/bin
```

To choose another host work root:

```sh
OPEN64_HOST_WORK_ROOT=$HOME/my-open64-work ./build-open64-docker.sh
```

To choose another image tag:

```sh
OPEN64_DOCKER_IMAGE=my-open64:latest ./build-open64-docker.sh
```

After the base build finishes, a quick compiler check is:

```sh
docker run --rm --platform linux/amd64 \
  -v "$HOME/work/open64/opt/open64:/opt/open64:ro" \
  -v "$PWD:/work" \
  open64:x86_64-apple-silicon opencc -v
```

## Build And Validate The DSL Frontend Lane

The fastest DSL compiler loop is the `--enable-torch2whirl-only` build.  It
configures the standalone frontend, the native Python bridge, and the
inspection lanes needed to certify `.B` artifacts with `ir_b2a -st -src`.

Use the repository helper:

```sh
OPEN64_TORCH2WHIRL_ARTIFACT_DIR="$PWD/artifacts/torch2whirl" \
  osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

This script:

1. Builds or reuses `open64:torch2whirl-torch-test`.
2. Mounts the Open64 checkout at `/src`.
3. Mounts the configured build tree at `/build`.
4. Mounts the host artifact directory at `/artifacts`.
5. Runs `configure --enable-torch2whirl-only`.
6. Runs dependency-light Python tests and driver tests.
7. Runs native artifact inspection lanes when enabled.
8. Leaves `.B`, `.T`, side payloads, source copies, and logs on the host.

The completed run writes:

```text
artifacts/torch2whirl/MANIFEST.txt
```

The most useful artifact families are:

```text
artifacts/torch2whirl/resnet/
artifacts/torch2whirl/llama2-prefill/
artifacts/torch2whirl/llama2-decode/
artifacts/torch2whirl/llama2-multi-pu/
artifacts/torch2whirl/python-native/
```

Each family is cleaned at the beginning of its next run, not at the end of the
current run.  A completed run therefore leaves evidence available for review.

## Reuse Docker For An Interactive Debug Session

For day-to-day development, keep the source tree, build tree, and artifact
directory mounted into one reusable shell:

```sh
mkdir -p /private/tmp/open64-torch2whirl-torch-test
mkdir -p "$PWD/artifacts/torch2whirl"

docker run --rm -it --platform linux/amd64 \
  -v "$PWD:/src" \
  -v /private/tmp/open64-torch2whirl-torch-test:/build \
  -v "$PWD/artifacts/torch2whirl:/artifacts" \
  -w /build \
  open64:torch2whirl-torch-test bash
```

Inside the container:

```sh
test -f Makefile || /src/configure --enable-torch2whirl-only
cd /build/osprey/targdir/torch2whirl

make python_test
make driver_torch_test
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  llama2_multi_pu_native_ir_tools_smoke
```

Because `/src` is the host checkout, edits made in a host editor are visible
immediately inside Docker.  Because `/artifacts` is a host directory, trace
files and diagnostics remain available after the container exits.

## Inspect Retained WHIRL Evidence

Use `ir_b2a -st -src` traces first.  They are the most direct way to review the
binary WHIRL artifact without guessing from Python state.

Examples from the host after a Docker run:

```sh
sed -n '1,180p' \
  artifacts/torch2whirl/llama2-multi-pu/llama2_multi_pu.T

rg "TinyLlama2Attention|DSL Callsite|transformer.attention" \
  artifacts/torch2whirl/llama2-multi-pu/llama2_multi_pu.T

rg "OPR_DSL|MDSL|OPC_MDSL" \
  artifacts/torch2whirl/llama2-multi-pu/llama2_multi_pu.T
```

The last command should not find private physical DSL escape spellings in
logical review output.

For Python callable and callsite names, use the frontend filter:

```sh
PYTHONPATH=osprey/torch2whirl/python \
  osprey/torch2whirl/scripts/torch2whirl-filt \
  TinyLlama2Attention.forward

PYTHONPATH=osprey/torch2whirl/python \
  osprey/torch2whirl/scripts/torch2whirl-filt \
  '__WHIRL_DSL_CALL__:callee=TinyLlama2Attention;class=TinyLlama2Attention;instance=layers.0.attention;context=TinyLlama2ForCausalLM.layers.0.attention;ordinal=2'
```

## Run The Combined `openpy` Artifact Lane

The full Python DSL driver path requires a full Open64 build tree containing
`openpy`.  If the base build used the default host root, that build tree is:

```text
~/work/open64/build
```

Run:

```sh
OPEN64_OPENPY_BUILD_DIR="$HOME/work/open64/build" \
OPEN64_TORCH2WHIRL_ARTIFACT_DIR="$PWD/artifacts/torch2whirl" \
  osprey/torch2whirl/scripts/run_openpy_docker_test.sh
```

This runs the `openpy -keep -O0` artifact test in Docker and retains:

```text
artifacts/torch2whirl/openpy/resnet/resnet.py
artifacts/torch2whirl/openpy/resnet/openpy_driver.log
artifacts/torch2whirl/openpy/resnet/binary/resnet.B
artifacts/torch2whirl/openpy/resnet/binary/resnet.T
artifacts/torch2whirl/openpy/resnet/binary/resnet.safetensors
artifacts/torch2whirl/openpy/resnet/lowered/resnet.t
artifacts/torch2whirl/openpy/resnet/lowered/resnet.s
```

`resnet.B` is the frontend binary WHIRL artifact.  `resnet.T` is the
source-aware `ir_b2a -st -src` dump of that artifact.  `resnet.t` is the
post-VHO DSL lowering trace produced before normal VHO lowering continues.

To run the same path interactively:

```sh
docker run --rm -it --platform linux/amd64 \
  -v "$PWD:/src" \
  -v "$HOME/work/open64/build:/build" \
  -v "$PWD/artifacts/torch2whirl:/artifacts" \
  -e OPEN64_BUILD_DIR=/build \
  -e OPEN64_TORCH2WHIRL_ARTIFACT_DIR=/artifacts \
  -w /src \
  open64:torch2whirl-torch-test bash
```

Inside the container:

```sh
/src/osprey/driver/tests/openpy_resnet_artifact_test.sh
```

## Triage And Script Editing Workflow

Keep triage scripts and trace artifacts in the mounted source and artifact
directories:

```text
/src/osprey/torch2whirl/scripts/
/src/osprey/torch2whirl/python/tests/
/src/osprey/driver/tests/
/artifacts/
```

A practical loop is:

1. Edit a Python test, driver test, or shell triage script in the host checkout.
2. Re-run the focused command inside the Docker shell.
3. Inspect the updated `.T`, `.t`, `.B`, side payload, or log under
   `artifacts/torch2whirl`.
4. Keep the trace file open in the host editor while iterating on the script.

Useful focused commands inside `/build/osprey/targdir/torch2whirl`:

```sh
make python_test
make python_torch_test
make driver_torch_test
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  python_native_ir_tools_smoke
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  resnet_native_ir_tools_smoke
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  llama2_prefill_native_ir_tools_smoke
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  llama2_decode_native_ir_tools_smoke
make OPEN64_DSL_TEST_ARTIFACT_DIR=/artifacts \
  llama2_multi_pu_native_ir_tools_smoke
```

Useful host-side scans before sending a change for review:

```sh
git diff --check
git diff -U0 -- ':!osprey/torch2whirl/Makefile.gbase' | \
  perl -ne 'print if /^\+[^+].*\t/'
rg -n "be/cg|BE_CG|ercg|erauxdesc|#include .*be/" \
  osprey/torch2whirl osprey/targdir/torch2whirl
```

The backend-isolation scan may find README or AGENTS guidance text.  It must
not find source, makefile, or link dependencies from `torch2whirl` into backend
code generation internals.

## Expected Success Signals

For the frontend DSL lane:

```text
make python_test
make driver_torch_test
make ... llama2_multi_pu_native_ir_tools_smoke
```

should pass, and the artifact root should contain `MANIFEST.txt`.

For the combined driver lane:

```text
openpy ResNet artifact test passed
```

should appear in the Docker output, and the retained `openpy` directory should
contain both binary and lowered evidence.

For review, report the absolute host paths to:

```text
artifacts/torch2whirl/MANIFEST.txt
artifacts/torch2whirl/<model-family>/<artifact>.B
artifacts/torch2whirl/<model-family>/<artifact>.T
artifacts/torch2whirl/openpy/<model>/lowered/<model>.t
```

Do not add generated review artifacts to Git unless a task explicitly requests
checked-in golden files.
