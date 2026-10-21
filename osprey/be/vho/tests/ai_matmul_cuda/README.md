# Hopper CUDA Validation Bundle

## Compiler-selected source

The opt-in backend switch `-Wb,-DSL:cuda_tile_output=<new-file.cu>` runs
the active PU through tensor evolution, tensor/locality analysis, AIO-9 tile
candidates, and AIO-2 legality/cost selection before normal lowering. It
emits one selected F32 GEMM CUDA kernel for a straight-line, static rank-2 PU.
The output path must not exist. Source emission does not rewrite WHIRL or
launch a GPU kernel. The current cost is low-confidence and relative, not
target latency.

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
Roofline corroborates that choice but does not independently change it.
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

The object still has an unresolved generated-provider symbol. The emitted
`.cu` defines the kernel, not the host runtime adapter that owns tensor
placement, launch, fallback, and errors. This verifies the compiler
transformation, not an executable CUDA optimization or accepted `m2` result.
The focused AIO-11 contract test also writes binary WHIRL for the generated
call and checks invalid-plan rejection and rollback.

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
