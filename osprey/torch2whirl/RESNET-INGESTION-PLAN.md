# ResNet Ingestion Plan

This plan tracks the torch2whirl-owned work needed to ingest ResNet-style
PyTorch models while the main common/com thread finishes native DSL WHIRL node
formalization. Keep this file updated as each batch lands.

## Current Status

- Branch: `codex/torch2whirl-python-fe`
- T0: complete in commit `4c9b056e`
- T1: complete in commit `0ef5d18a` for Python/mock paths and now certified
  through the native bridge after merging the committed common/com base.
- T2: complete. The subagent-owned slice is dependency order, operator attrs,
  source metadata, mock artifacts, and tests that do not require new native DSL
  nodes.
- T2 progress in this branch now includes operator-level source metadata in the
  Python manifest and mock WHIRL artifact: FX node op/name/target, lowering
  hint, source module path, and source module type.
- T3 native standalone certification is complete for the torch2whirl-owned
  LocalResNet path through `ir_b2a -st`. The remaining `opencc/openpy` combined
  driver gate requires the updated main-side driver build.

## Main-Agent Gates

- G0: common/com DSL node representation for non-comment WHIRL operators.
- G1: symbol-table representation for model inputs and external tensor data.
- G2: native API/IR tools support for ResNet operator emission and inspection.

G0 through G2 landed as clean native infrastructure commits on
`codex/non-comment-dsl-node`, starting with `a6e5da86`. This branch now builds
against those APIs. A follow-up common/com readback ownership fix was needed in
`osprey/common/com/dsl_ir_image.cxx`: mapped DSL image records must be copied
into owned segmented-array storage when `ir_b2a` reopens a `.B`, rather than
adopting mapped-file section pointers with `Transfer`.

Native API/files now consumed by T3:

- `osprey/common/com/dsl_builder.h` must expose
  `DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE`,
  `DSL_Builder_Create_Model_Input`, and
  `DSL_Builder_Create_External_Tensor_Constant`.
- `osprey/common/com/dsl_builder.cxx` must implement typed
  `common.model_input.v2` and external-data `common.tensor_const` values that
  feed the native DSL value/result path.
- `osprey/common/com/dsl_ir_image.{h,cxx}` and
  `osprey/common/com/dsl_ir_print.cxx` must persist and print the DSL opcode,
  node, attribute, value, and value-reference records.
- `osprey/common/com/dsl_gatekeeper.{h,cxx}` must verify native ResNet nodes,
  tensor descriptors, result values, and external tensor references.
- `osprey/common/com/ir_bread.cxx`, `ir_bwrite.cxx`, and `ir_reader.cxx`,
  plus `osprey/include/sys/elf_whirl.h`, must carry the DSL IR image section
  through mapped-image read/write and `ir_b2a -st`.
- `osprey/torch2whirl/Makefile.gbase` must link the native extension against
  `dsl_gatekeeper.o` and `dsl_ir_image.o`.
- `osprey/torch2whirl/python/native/open64_dsc_native_bridge.cxx` must bind
  the new common/com APIs to Python without exposing WHIRL node layout.

## Subagent Queue

### T0: Operator Contract Table

Status: complete.

- Centralized ResNet operator contract metadata in Python.
- Versioned `common.relu`, `common.flatten`, `common.residual_add`,
  `common.linear`, `common.output_logits`, and CNN operators as v2.
- Kept `common.add`, `common.matmul`, and `common.tensor_const` as v1.
- Routed builder and verifier checks through the contract table.
- Added deterministic native capability errors for missing v2 operators.

### T1: Model Inputs And External Tensor Sources

Status: complete for frontend/mock paths.

- Added backend protocol hooks for model inputs and external tensor constants.
- Replaced Python-built tensor constant URIs with structured external-data
  fields: storage format, side file, tensor key, byte range, checksum, and
  layout.
- Added builder validation for byte ranges, static shape size, dtype, checksum,
  and required external-data fields.
- Added verifier coverage for duplicate payload keys, overlap, unsupported
  dtype, invalid length, length mismatch, and checksum mismatch.
- Kept native execution capability-gated when `common.model_input.v2` or other
  required APIs are absent.

### T2: ResNet Operator Mapping Evidence

Status: complete.

- Confirmed exact FX dependency order for ResNet stem, projection block, basic
  block, and classifier tail.
- Preserved operator attrs needed by downstream DSL lowering.
- Preserved source provenance on graph operators: FX node op/name/target,
  lowering hint, module path, and module type where available.
- Emitted provenance in mock artifacts so ingestion regressions are visible
  without native common/com support.
- Added tests that reuse the PyTorch/FX patterns already present in
  `test_open64_dsc_fx_capture_optional.py`.
- Validated with local `python_test` and Docker Torch/driver smoke tests.

### T3: Native Capability Certification

Status: complete for standalone torch2whirl native artifact certification.

- Capability-skipped native tests now pass against the committed native APIs.
- Validates `common.model_input.v2`, external tensor source symbols, implicit
  zero conv bias operands, DSL IR image records, and all LocalResNet operator
  v2 nodes through `python_native_test` and the driver-native `ir_b2a -st`
  smoke.
- The certified standalone command shape is:
  `torch2whirl model.py --entry forward --sample-input shape:1,3,64,64
  --backend native --output model.B` for the local fixture. The final driver
  contract remains `shape:1,3,224,224`.
- The certified output family is `model.B` plus deterministic
  `model.safetensors` next to the explicit output path.
- `ir_b2a -st model.B model.st.ir` exposes `common.model_input`,
  `cnn.conv2d`, `cnn.batch_norm_infer`, `common.relu`, `cnn.max_pool2d`,
  `common.residual_add`, `cnn.global_avg_pool2d`, `common.flatten`,
  `common.linear`, `common.output_logits`, tensor descriptors, external
  payload references, implicit-zero bias values, result values, symbols, and
  types.
- Separate-process `opencc -x whirl -O0 -c model.B` could not be completed in
  this torch2whirl-only build tree because it intentionally does not build
  `opencc`; the installed host `opencc` binaries reject `-x whirl` as an
  unknown language. Main-side `147ef0b4` owns the updated `openpy/opencc`
  driver gate.

### T4: Real ResNet Fixture

Status: complete.

- Added a minimal ResNet-like fixture that avoids external network downloads.
- Confirmed exported graph operators, external payload metadata, and classifier
  logits operator.
- Covered the fixture through both FX optional tests and standalone driver
  smoke tests.

### T5: Driver Integration

Status: complete.

- Routed the standalone `torch2whirl` driver through the same Python ingestion
  path for local model scripts.
- Hardened the standalone argv contract needed by `openpy -keep model.py`:
  `torch2whirl model.py --entry forward --sample-input shape:1,3,224,224
  --backend native --output model.B`.
- Output side files are named deterministically from the explicit output path:
  `model.B` writes `model.safetensors` in the same directory.
- Artifact and side-file writes use temporary files and atomic replacement so
  failed frontend runs do not create an apparently valid partial `model.B`.
- Validated the driver path with mock WHIRL output, ResNet operator metadata,
  SafeTensors side-file records, and invalid-graph rejection.
- Kept Linux Docker and macOS build-tree validation in the standard smoke loop.

### T6: Pull Request Exit Evidence

Status: complete.

- Recorded exact commands for Python tests, Docker PyTorch tests, native optional
  tests, diff hygiene, no-tab scan, and backend-isolation scan.
- Summarized remaining native dependencies clearly because common/com work is still
  outside this branch.

Exit evidence commands:

- `PYTHONPYCACHEPREFIX=/private/tmp/open64-pycache python3 -m compileall -q
  osprey/torch2whirl/python/open64_dsc`
- `make -C osprey/torch2whirl -f Makefile.gbase python_test`
- `sh osprey/torch2whirl/scripts/run_torch_docker_test.sh`
- `make -C osprey/torch2whirl -f Makefile.gbase python_native_check`
- Docker configured build-tree native lane:
  `make -C /build/osprey/targdir/torch2whirl python_native_test`
- Docker configured build-tree ir-tools lane:
  `make -C /build/osprey/targdir/torch2whirl
  python_native_ir_tools_smoke driver_native_ir_tools_smoke`
- `git diff --check`
- no-tab scan on touched non-Makefile files
- backend-isolation scan:
  `rg -n "be/cg|BE_CG|ercg|erauxdesc|#include .*be/"
  osprey/torch2whirl osprey/targdir/torch2whirl`

## Combined Driver Contract

The final user-facing command is owned by the main Open64 driver:

```sh
openpy -keep model.py
```

`openpy` will be a symbolic link to the standard Open64 driver, analogous to
`opencc`, but basename dispatch must select the Python DSL frontend pipeline and
must never silently select the C frontend. The main agent owns `openpy`
language selection, phase orchestration, `-keep` behavior, and the `model.t`
trace hook after `VHO_DSL_Lower_Driver()` and before `VHO_Lower_Driver()`.

The standalone frontend contract provided by this branch for that driver is:

```sh
torch2whirl model.py \
  --entry forward \
  --sample-input shape:1,3,224,224 \
  --backend native \
  --output model.B
```

On success, the frontend writes `model.B` and side files named from the explicit
output path, for example `model.safetensors`. On failure, it exits nonzero,
reports whether the failure came from Python import/model load, sample-input
parsing, unsupported operator capture, gatekeeper verification, native builder,
or binary finalization, and avoids leaving a plausible partial `model.B`.

Two coordinated PRs are expected after full native certification and the
combined `openpy` gate pass:

- Native DSL infrastructure PR: must land first and provide the common/com,
  mapped-image, gatekeeper, VHO DSL lowering, `openpy`, `-keep`, and `model.t`
  driver support.
- `codex/torch2whirl-python-fe` PR: depends on the native infrastructure PR and
  supplies the standalone Python/PyTorch frontend, CLI, driver executable,
  tests, and plans.

## FHE SYNC-2 ResNet-20 Capture

Status: in progress on the FHE task branch after PR #102 merged at
`8ba9ee31`.

The FHE lane now targets complete deterministic ResNet-20/CIFAR-10 capture,
not a smaller CNN milestone. It consumes the merged `DSL_FHE_*` and
`DSL_Builder_*` APIs through opaque Python-native bridge calls, preserves every
source ReLU as `common.relu`, attaches FHE entry/encryption/key contracts only
after graph capture, and emits no bootstrap, CKKS, SIHE, or FHE-conversion
operators from Python.

Retained artifact family:
`artifacts/fhe/resnet20_capture/{secure_resnet20.py,secure_resnet20.B,secure_resnet20.T,secure_resnet20.safetensors,operator-census.txt,capture-options.txt,gatekeeper.log}`.
`secure_resnet20.T` is produced in a separate process with `ir_b2a -st -src`
after the Python capture process exits.

Current evidence:

- `operator-census.txt` records 74 graph operators: 21 `cnn.conv2d`, 21
  `cnn.batch_norm_infer`, 19 `common.relu`, 9 `common.residual_add`, 1
  `cnn.global_avg_pool2d`, 1 `common.flatten`, 1 `common.linear`, and 1
  `common.output_logits`.
- FHE tables appear in `secure_resnet20.T` with the stable SYNC-1 headings from
  `doc/FHE-SYNC1-NATIVE-CONTRACT.md`.
- The side-file payload is retained as `secure_resnet20.safetensors`, and the
  `.T` dump references it through `safetensors://secure_resnet20.safetensors`.

Open frontend item before SYNC-2 closure: true class-centric ResNet PUs are not
implemented yet. The current full-model artifact uses single-PU graph capture.
The existing ResNet class-region path reaches a duplicate native result-symbol
verification failure at full ResNet-20 scale, so class-centric ResNet PU
emission should be completed as a focused frontend refinement rather than
papered over with raw native access.
