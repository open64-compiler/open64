# Hopper CUDA Validation Bundle

## Compiler-selected source

The opt-in backend switch `-Wb,-DSL:cuda_tile_output=<new-file.cu>` runs
the active PU through tensor evolution, tensor/locality analysis, AIO-9 tile
candidates, and AIO-2 legality/cost selection before normal lowering. It
emits one selected F32 GEMM CUDA kernel for a straight-line, static rank-2 PU.
The output path must not exist. Source emission does not rewrite WHIRL or
launch a GPU kernel. The current cost is low-confidence and relative, not
target latency.

`-Wb,-DSL:cuda_legacy_emit=ON` is the default during the WHIRL-to-CUDA
transition. It preserves the current source-template emitter as a baseline
when `cuda_tile_output` requests a `.cu` file; it does not create a file on
ordinary compilations without that output option. Set
`-Wb,-DSL:cuda_legacy_emit=OFF` to suppress legacy `.cu` generation even
when a path is supplied. P7/P9 coverage then says
`disabled_by_option cuda_legacy_emit_off` when no device-WHIRL output is
requested. With `-Wb,-DSL:cuda_whirl_output=<new-file.B>`, cost selection
and the synchronous `cuda_tile_apply=ON` host rewrite can proceed even when
legacy emission is OFF. VHO stages the selected tile and constructs the device
PU at program boundary; the backend driver owns that `PU_Info`, writes its
mapped image, and publishes it after host-PU traversal. Its image is still
separate, not registered in the main compilation-unit PU tree. Multiple
selected PUs publish one device image with deterministic entries:
`ai_matmul_tile_f32` for ordinal zero and `ai_matmul_tile_f32_<ordinal>`
thereafter. A `DEVICE_BIND` trace row joins each host PU, plan/tile, device
entry, and image. Entry zero keeps the v1 provider contract; later entries
carry a 72-byte v2 generated plan with a device-entry ordinal and call
`__open64_dsl_matmul_generated_v2`. The package/runtime loader must resolve
that ordinal to the named entry in the same image. This is compiler-side
binding, not native GPU validation.
Retain the legacy source, trace, and receipt separately when comparing it
with `whirl2c -cuda` output from the same selected plan.

## Selected Device WHIRL Parity

For the selected synchronous F32 GEMM subset, `cuda_whirl_output` writes
`ai_matmul_tile_f32` as high-level device WHIRL with cooperative global-to-
shared A/B staging, two CTA barriers per K tile, register accumulation, and
output stores. Non-divisible M/N/K tails conditionally zero-fill only the A/B
accesses that can cross a boundary and guard only potentially invalid C
stores; both CTA barriers remain unconditional. The 32-bit index gate rejects
flattened or padded geometry that can overflow an I4 index. The emitted
declarations contain `__shared__` for both `as` and `bs`; `acc` remains
per-thread. To write a CUDA-suffixed output directly, run:

```sh
whirl2c -cuda -CLIST:dotc_file=matmul.whirl.cu \
  -fB,matmul.device.B matmul.cu
```

Here `matmul.cu` names the original source for translation metadata; it is
not the output path. Without `dotc_file`, the default output is a `.w2c.c`
file containing CUDA C. The review artifacts retain those bytes as
`source/matmul.whirl.cu` for future native CUDA compilation. The checked-in
`generated_runtime.cu` is a host launch adapter, not `whirl2c` kernel output;
it has no A/B tile declarations. The legacy emitter remains the default-on
transition baseline. Byte-for-byte source equality is not required: the device
PU omits bounds guards on axes where divisibility proves them redundant. The
`EDGE_PLAN` row in `.t`, `tile.log`, and `optimization-summary.log` records
A/B zero-fill, C-store guard, and uniform-barrier decisions.

For the selected 128x128x128, BM=BN=128, BK=32, TM=TN=8 case, run:

```sh
bash osprey/be/vho/tests/ai_matmul_cuda/test_whirl_cuda_parity.sh \
  path/to/matmul.generated.cu path/to/matmul.device.w2c.c \
  path/to/artifact/binary
```

The test compiles both sources with a CUDA host stub and simulates one CTA per
output tile with real barriers. It compares both outputs with reference
GEMM and each other. The first preserved zero-error evidence is
`artifacts/ai_optimization/m2-whirl-kernel/run-008/`; `run-009/` shows that
WHIRL-only selection produces no legacy `.cu` and reports
`device_whirl_sidecar` in the optimization trace. This does not establish
native CUDA compilation, PTX generation, GPU correctness, or performance.

`test_device_whirl_edges.py --build-dir /path/to/build --output-dir /new/run`
exercises 130x133x135 with all M/N/K tails in source-only and applied modes,
plus 128x128x135 with only a K tail. It reopens the device image, checks `IF`
counts and `EDGE_PLAN`, translates with `whirl2c -cuda`, runs multi-CTA host
parity against legacy source and reference GEMM, and rejects a 50000x50000x128
shape whose flattened C index cannot fit I4 before device publication. It
retains raw `.t`, phase reports, device `.B`/`.T`, `.cu`, host object, negative
compile log, receipt, and checksums. The
compiler-only checkpoint is `artifacts/ai_optimization/m2-whirl-kernel/run-014/`.

The compiler artifact runner can capture both source paths in one run:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/compile_source.py \
  --build-dir /path/to/open64/build \
  --output-dir /path/to/new/m2-whirl-kernel-run \
  --device-whirl --apply
```

`--device-whirl` is synchronous-only. It checks that the `WHIRL_KERNEL` trace
row follows the selected `TILE_ACTION` row and matches the selected plan and
actual `binary/matmul.device.B`, retains its
`ir_b2a -st -src` view, emits `source/matmul.whirl.cu`, verifies the kernel
and shared A/B attributes, and runs host CTA parity against the legacy source.
The receipt and handoff include paths and SHA256 hashes for both sources and
the device image. Compiler options use paths relative to the artifact's
`binary/` directory, so the trace remains readable after publication.
`run-012/` records a passing `--apply` run with driver-owned `PU_Info` and
mapped-image writing, plus zero host CTA error. `test_multi_device_whirl.py`
compiles the two-PU fixture, reopens the two-kernel device image, checks both
`whirl2c -cuda` entries and trace bindings, then checks v1/v2 host calls with
`cuda_tile_apply=ON`. A linked host-only smoke executes both generated PUs
through a mock provider and validates the v2 ordinal. The test renders per-PU
phase logs and an aggregate `optimization-summary.log`; its receipt and
checksums are retained in `artifacts/ai_optimization/m2-whirl-kernel/run-013/`.
The device PUs are still in a separate image; this does
not close M2.

The backend option `-Wb,-DSL:ai_opt_trace=ON` defaults to ON for this
development phase. It activates the reserved WOPT-style trace bit and opens
the compiler's `.t` file without a separate `-ttOP2` switch. Use
`-Wb,-DSL:ai_opt_trace=OFF` to suppress AI decision tracing. At AIO-9
selection, the backend writes versioned `AI_OPT`
rows for every candidate's legality, cost terms, resource estimates, and the
chosen plan. Schema v4 appends the input Python source line, FLOPs, modeled
global bytes, and three Roofline lower bounds to each candidate row. A single
`ROOF_TARGET` row records the reference ceilings. For the generic Hopper
profile, this first implementation uses published H100 SXM FP32 and HBM
peaks: 67,000 GFLOP/s and 3,350 GB/s. These are not calibrated sustainable
rates. Global load requests are used as a cold-HBM traffic proxy; L2 hits,
occupancy, launch time, and overlap are not modeled. The selector keeps the
lowest complete relative-cost plan unless another proven plan is within
`floor(best_cost/33)` and has a Roofline bound at least
`floor(current_bound/20)` lower. Blackwell has no Roofline ceiling yet and
retains the relative-cost rule. The reason is recorded as either
`lowest_complete_relative_cost` or `roofline_near_cost`; the runner checks
the full decision against candidate records. After
verified lowering it records the actual action:
`source_sidecar`, `tile_whirl_rewrite=none`, and the ordinary DSL-to-runtime
lowering count. `ai_opt_report.py` validates those rows and derives
`reports/phases/tile.log`, `reports/optimization.log`, and
`reports/optimization-summary.log`; each starts with `What it summarizes`.
Schema v7 adds explicit P0-P11 coverage for the standalone matmul source
slice. The tool checks coverage against the P7/P8 selection and emits all
twelve phase logs even when a phase is disabled or inapplicable. A phase log
with `detail=not_recorded` is an honest coverage record, not domain analysis
evidence. P0 and P1 now contain source-linked semantic tensor and locality
facts from AIO-3/AIO-4. The separate `--apply` path below records a
generated-provider rewrite; its CUDA runtime adapter remains unimplemented.
The current 128x128x128 fixture still selects `cuda_128` by relative cost;
its one-CTA grid is not priced by the relative cost or full-device Roofline
proxy, so the latter is not evidence of target throughput. New schema-v7
`TILE_GRID` rows record M/N/K, CTA grid dimensions, CTA count, and K
iterations for each candidate. The report validates the geometry and marks
the selected plan as requiring native calibration. Older v7 traces without
these additive rows remain readable.
After cost selection, the backend also constructs a canonical PU-local
`OPR_DO_LOOP` for the K-tile coordinate. The emitted synchronous CUDA loop
reads its bound and stride from this WHIRL node. A `K_LOOP` trace row and
`traces/selected-k-loop.T` preserve the selected loop for inspection; the
runner checks its site/plan/tile identity and source geometry. This node is
detached planning evidence and is not itself inserted into the device PU.
The new device image builds a complete loop nest independently from the same
selected tile. The legacy CUDA body remains source-emitted, and the host
WHIRL rewrite described below still calls a generated provider.
If an artifact run fails after staging begins, the retained staging directory
still contains an `optimization-summary.log` marked `run=failed`; it does not
certify a selection or rewrite.

`compile_source.py` creates a new durable artifact using the fixed matmul
fixture and runs `opencc` both without and with that switch. On the configured
Linux/x86-64 Open64 build:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/compile_source.py \
  --build-dir /path/to/open64/build \
  --output-dir /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/<new-run-id>
```

The artifact preserves `source/matmul.generated.cu`, the producer source,
input `.B`, separate-process `.T`, compiler outputs, option-off/on logs,
the raw decision `.t` and derived optimization reports, `receipt.json`, and
`SHA256SUMS`. The M2 handoff in `plans/m2-selected-tile.json` binds the
selected candidate, tile geometry, resources, low-confidence relative cost,
source line, fetch plan, and source/trace/WHIRL hashes. It explicitly records
`whirl_rewrite_applied=false` in the default source-only mode. The
`.cu` header states the selected plan,
relative cost, tile dimensions, and resource estimates. The current verified
fixture selects `cuda_128`, with CTA `[128,128,32]`, thread tile `[8,8]`,
256 threads, and 32 KiB of single-stage A/B shared memory. It uses cooperative
loads and `__syncthreads`, not TMA or a persistent schedule.

## Opt-in M2 generated-provider rewrite

`-Wb,-DSL:cuda_tile_apply=ON` is OFF by default. With `-O3`, a standalone
F32 matmul, `cuda_tile_output`, and synchronous movement, the backend checks
the selected tile before atomically replacing the matmul with
`__open64_dsl_matmul_generated_v1`. The call carries a read-only, versioned
64-byte plan descriptor; the original generic call remains when the option
is off. TMA source emission and tile application cannot be combined yet.

Run the compiler fixture with `--apply` to retain both option-off/on raw
`.t` files, post-DSL-lowering WHIRL dumps, selected `.cu`, backend object,
phase reports, plan handoff, and checksums. The runner uses `-noinline` at
`-O3` so the fixture's local PU survives to DSL lowering:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/compile_source.py \
  --build-dir /path/to/open64/build \
  --output-dir /path/to/new/m2-applied-run \
  --apply
```

The compiler object has an unresolved generated-provider symbol until linked
with a runtime. A fixed-shape **review adapter** now binds the emitted `.cu`
to the M0 test handles, validates plan identity and device/kernel resources,
allocates/copies device buffers, launches the selected synchronous kernel,
and copies its result back. A failed launch takes the direct CPU path only
when the plan authorizes that fallback. The host contract test exercises
success, fallback, malformed-plan rejection, and no-fallback failure. A
separate x86-64 integration smoke links the actual rewritten object to a mock
CUDA hook and checks the emitted descriptor. This is not a production tensor
provider or a GPU verification result. The focused AIO-11 contract test also
writes binary WHIRL and checks invalid-plan rejection and rollback.

To preserve the exact selected source and object for native execution:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/prepare_generated.py \
  --compiler-artifact /path/to/m2-applied/run-007 \
  --output-dir /path/to/new/m2-generated-runtime-run
```

The current prepared package is `m2-generated-runtime/run-009/`. Its input
`m2-applied/run-007/` records and validates the selected one-CTA grid against
the emitted 128x128x128 CUDA source. The grid-parallelism cost remains
unmodeled, so this package is for correctness and calibration, not a claimed
performance win.

On a native Linux/x86-64 Hopper machine with CUDA toolkit and `nvcc`, run
`python3 run_generated_gpu.py --artifact-dir /path/to/m2-generated-runtime-run`.
The runner verifies package hashes, links the real Open64 PU with the selected
CUDA source, requires the generated path on every numerical sample, retains
`nvcc`/ptxas resource output, device identity, raw full-PU timing, and a native
receipt. It fails if execution falls back to CPU. It also compiles a separate
validation-only untiled CUDA control from the same F32 problem, checks both
kernels on nonuniform inputs against a CPU reference, and records 30 paired
CUDA-event samples per kernel in `measurements/kernel-control.csv`. The pair
order alternates by sample; its ratio is **not** a comparison with the
compiler-produced M0 baseline. Full-PU timing includes allocation and
transfers, while the control timing measures resident kernels only. GPU
compilation, correctness, resource usage, and timing remain unverified locally;
same-target comparison with an accepted compiler baseline is still needed for
M2 acceptance.

For a remote Hopper host, use the generated-package transport (the older
`remote.py` handles only the source-sidecar package):

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/remote_generated.py \
  --artifact-dir /path/to/m2-generated-runtime-run \
  --host user@hopper-host \
  --remote-dir /absolute/new/m2-generated-runtime-run
```

It requires an unused remote directory, uploads only a verified prepared
package, retrieves native logs even if compilation or execution fails, and
checks the original `SHA256SUMS` again after retrieval. A successful run also
requires `native-SHA256SUMS` covering the executable, ptxas/build logs,
device identity, both raw timing files, and native receipt. The receipt's
kernel-control medians are checked against those raw paired samples. A package with any prior
native attempt cannot be reused; prepare a new run to retry. The transport
and integrity checks have host-only tests, but no remote GPU run has occurred.

## Opt-in P8 TMA source

`-Wb,-DSL:cuda_tma_prefetch=ON` adds AIO-10 fetch/pipeline analysis to the
CUDA source path. The fixed exact-tile F32 fixture selects its legal
`tma_like` movement plan with two A/B stages (64 KiB), distance one, and
explicit arrival barriers. The compiler emits `ai_matmul_tile_f32` with 2-D
tensor-map TMA loads and rotating shared-memory buffers, plus
`ai_matmul_tile_f32_sync` as a runtime fallback. The host-side
`ai_matmul_tile_f32_launch` wrapper encodes A/B tensor maps, checks device
capability/alignment/shared-memory limits, opts into dynamic shared memory,
and launches the selected kernel. It falls back to the synchronous kernel
when TMA cannot be provisioned. This option is off by default.

Create an immutable source-and-trace artifact with:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/compile_source.py \
  --build-dir /path/to/open64/build \
  --output-dir /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/<new-run-id> \
  --tma
```

Trace schema v7 records P8 movement candidates, their low-confidence relative
raw/hidden/unhidden movement scores, the selected plan, and the source action.
The runner derives `reports/phases/fetch-pipeline.log` and includes it in
`optimization.log` and `optimization-summary.log`. The `.cu` is still a source
sidecar: there is no executable WHIRL tile rewrite, native CUDA compile,
PTX/SASS check, numerical GPU result, or measured overlap yet. Compile this
source with a CUDA toolkit that provides `<cuda/ptx>` tensor-map operations,
target `sm_90` or newer, and link the CUDA driver library (`-lcuda`) for
`cuTensorMapEncodeTiled`. The direct CPU fallback remains the Open64 WHIRL
lowering output; the CUDA wrapper's fallback is separate.

The compiler-generated TMA source also exposes
`ai_matmul_tile_f32_launch_ex(..., bool *used_tma)` for validation. The flag
is true only when the TMA kernel launch is selected successfully; the regular
four-argument launcher keeps the original API.

To validate the exact compiler-emitted source on a native Hopper-or-later
machine, package an immutable compiler-source artifact without regenerating
its `.cu`:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/prepare_compiler.py \
  --compiler-artifact /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/run-017 \
  --output-dir /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-gpu-validation/<new-run-id>
```

The prepared `run-006/` package preserves the source, raw `.t`, M2 handoff,
focused reports, compiler receipt, and a nonuniform-input CUDA harness. Its native
runner compares the synchronous and selected-TMA paths numerically, refuses
to label a fallback as TMA, and records 30 wall-clock launch-to-completion
samples for each path. The timing includes host launcher setup and tensor-map
encoding, but excludes allocations and input/output transfers. Use
`remote.py` with `--artifact-dir`, `--host`, `--remote-dir`, and `--arch sm_90`
to upload and retrieve it. A native
receipt still does not close `m2` or `m3`: the source is a sidecar, not a
selected tile rewrite in executable WHIRL.

## M1 trace/report checkpoint

`ai-opt-report` reads the raw `.t`, validates versioned positional records,
and derives `optimization.log`, `optimization-summary.log`, and every P0-P11
phase log. For the standalone matmul fixture it rejects missing or duplicate
phase rows, broken plan lineage, cost mismatch, unmatched P0/P1 facts, and
coverage that contradicts the actual source action. Historical v1-v6 tile
traces remain readable as
partial evidence. The compiler records the option-off matmul compile as a
separate skipped-by-option trace; the runner retains both traces and reports.

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/ai-opt-report \
  /path/to/compile.t --reports /path/to/reports
```

Use `--phase P7` to print only `tile.log`. To publish a trace-digest-named
review checkpoint, run `compile_source.py` with `--digest-output` and pass
the `m1-trace-report` directory as `--output-dir`. The current receipt marks
the trace/report contract verified but `m1_accepted=false`: an accepted
native `m0-baseline` predecessor is still missing.

## Standalone validation prototype

This is a standalone CUDA adapter prototype for the `cuda_64` and `cuda_128`
AIO-9 tile-family shapes. It does **not** consume a selected
`CommonTilePlanIR`, lower WHIRL, use TMA, or close the `m2` generated-tile
gate. Its purpose is to obtain source-preserving correctness, resource, and
timing evidence on a real Hopper-or-later GPU while the compiler lowering
path is developed.

Prepare a **new** artifact directory in the AI review namespace:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/prepare.py \
  --family cuda_64 \
  --output-dir /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/validation-prototype/run-001
```

The artifact already contains `source/matmul.generated.cu`, `plan.json`,
`source.sha256`, and the self-contained GPU runner. The `.cu` is never
regenerated or removed by the remote run. Use a different run directory for
another family or attempt; the prepare command refuses to overwrite one.

When SSH access to a native GPU server is available:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/remote.py \
  --artifact-dir /Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/validation-prototype/run-001 \
  --host USER@HOPPER_HOST \
  --remote-dir /home/USER/ai-matmul-validation/run-001 \
  --arch sm_90
```

The remote runner compiles the preserved `.cu` with `nvcc`, performs
nonuniform F32 numerical checks at both exact and predicated-edge shapes,
and records 30 CUDA-event kernel-only timing samples after five warmups.
It retains `logs/nvcc.log` (including ptxas resources), `logs/run.log`,
`reports/device.txt`, `measurements/raw.csv`, `binary/matmul`, `receipt.json`,
and `SHA256SUMS`. The SSH wrapper retrieves the whole artifact and verifies
all returned hashes. A failed remote run still retrieves logs and source.

This bundle is not an accepted compiler optimization receipt: it lacks a
cost-selected plan, a WHIRL rewrite, the AI `.t` decision trace, and a
same-target Open64 baseline. Its `receipt.json` states those limitations.

Run host-only package tests with:

```sh
python3 -m unittest discover -s osprey/be/vho/tests/ai_matmul_cuda -p 'test_*.py'
```
