# ResNet Ingestion Plan

This plan tracks the torch2whirl-owned work needed to ingest ResNet-style
PyTorch models while the main common/com thread finishes native DSL WHIRL node
formalization. Keep this file updated as each batch lands.

## Current Status

- Branch: `codex/torch2whirl-python-fe`
- T0: complete in commit `4c9b056e`
- T1: complete in commit `0ef5d18a` for Python/mock paths; native certification
  remains capability-gated until the common/com G1/G2 APIs land in this
  worktree.
- T2: complete. The subagent-owned slice is dependency order, operator attrs,
  source metadata, mock artifacts, and tests that do not require new native DSL
  nodes.
- T2 progress in this branch now includes operator-level source metadata in the
  Python manifest and mock WHIRL artifact: FX node op/name/target, lowering
  hint, source module path, and source module type.

## Main-Agent Gates

- G0: common/com DSL node representation for non-comment WHIRL operators.
- G1: symbol-table representation for model inputs and external tensor data.
- G2: native API/IR tools support for ResNet operator emission and inspection.

The main checkout reports G0 through G2 complete, but this worktree still lacks
those native capabilities. Native tests must therefore skip by explicit
capability checks rather than silently passing with mock-only behavior.

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

Status: blocked on G1/G2 landing in this worktree.

- Turn capability-skipped native tests into passing tests once native APIs are
  available.
- Validate `common.model_input.v2`, external tensor source symbols, and all
  ResNet operator v2 nodes through `python_native_test` and ir-tools smoke
  checks.

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
