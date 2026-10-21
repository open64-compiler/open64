# AI Compiler Optimization Progress

Snapshot: 2026-10-21, reviewed against base commit `e17a2c23`
Scope: Open64 AI optimization, with standalone rank-2 F32 GEMM as the first
generated-kernel case. This is a running status ledger, not a replacement for
the [implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md) or
the [versioned design](AI_compiler_optimization_design_v0.3.md).

## Current position

The AIO-0 through AIO-13 planning and analysis queue is marked complete in
the implementation plan. Most slices produce check-only candidate, legality,
cost, or feedback evidence. AIO-11 also has an executable rank-2 F4
(four-byte float, not FP4) matmul
rewrite to a generic cuBLASLt provider ABI. **That provider path is not an
Open64-generated tiled GEMM.** The separate F32 path now has a selected CUDA
tile source, a trace/report tool, and an opt-in generated-provider WHIRL
rewrite. It does not yet have a linked CUDA provider, numerical GPU result,
or measured baseline-versus-generated-tile result.

**Next acceptance gate:** publish `m0-baseline` for the direct fixed-shape F32
matmul, with correctness, target identity, raw timings, source, binary `.B`,
separate-process `ir_b2a -st -src` `.T`, commands, and checksums. Do not infer
performance progress from a selected check-only plan.

An `m0` CPU execution runner now exists under
[`osprey/be/vho/tests/ai_matmul_m0`](../osprey/be/vho/tests/ai_matmul_m0/README.md).
It executes the compiler-produced `-O0` PU through a minimal test runtime.
The first Linux/x86-64 Docker run on Apple Silicon passed numerical and
artifact checks, but was explicitly labeled an emulated functional smoke;
its timings are not accepted performance evidence. Native target measurement
and publication to the immutable review namespace remain open.

A standalone CUDA validation bundle is also available under
[`osprey/be/vho/tests/ai_matmul_cuda`](../osprey/be/vho/tests/ai_matmul_cuda/README.md).
Its explicit AIO-9-shaped `cuda_64` plan and generated `.cu` are preserved at
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/validation-prototype/run-001/`.
The bundle is ready for remote compilation and measurement, but no native GPU
result is recorded yet. It is template-generated validation code, not a
cost-selected WHIRL transformation, and does not close `m2`.

An opt-in Open64 backend path now emits a **cost-selected** CUDA source
sidecar with `-Wb,-DSL:cuda_tile_output=<new-file.cu>`. The fixed F32 matmul
produced `cuda_128` under AIO-9/AIO-2 selection, and the source, input `.B`,
text `.T`, option-off/on logs, and checksums are preserved at
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/run-004/`.
This is the first readable compiler-selected kernel source, but WHIRL still
lowers the direct CPU fallback. The lowered `.I` is byte-identical with the
option off. GPU compilation, numerical verification,
resource measurement, and application of the selected tile remain open.

The next immutable run, `/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/run-005/`,
adds raw `traces/compile.t` and validated `reports/phases/tile.log`,
`reports/optimization.log`, and `reports/optimization-summary.log` beside
the same selected `.cu`. Its decision trace shows baseline, `cuda_64`, and
`cuda_128` costs before selecting plan 3. Its action row explicitly records
source-sidecar emission and **no tile WHIRL rewrite**. This closes the
source-review trace gap for P7 only, not the complete `m1` gate.
The AI decision trace option now defaults ON for this source path and may be
disabled with `-Wb,-DSL:ai_opt_trace=OFF`. The immutable `run-006/` artifact
contains the same trace and derived reports without `-ttOP2` in its compiler
command; a separate OFF check emitted the `.cu` but no `.t` file.
The subsequent immutable `run-007/` artifact uses trace schema v2: every
candidate row ends with the Python source line. The fixed fixture now gives
the matmul result its actual source position, and the artifact runner checks
that the reported line matches the `common_matmul` call (`fixture.py:28`).
The immutable `run-008/` report extends the `tile.log` header with the exact
current compute, baseline/tiled global-memory traffic, and total-score
formulas. It explicitly labels `memory_unhidden` as a traffic proxy rather
than estimated unhidden latency; no runtime calibration is implied.
Trace schema v3 records `selection_reason=lowest_complete_relative_cost` in
the raw `TILE_SELECT` row and carries it into `tile.log` and
`optimization-summary.log`. This names the observed AIO-9 decision rule; it
does not claim that the relative score predicts GPU latency.
The immutable `run-009/` compiler-source artifact validates this v3 record,
the derived reports, and a receipt carrying the same reason; its
`SHA256SUMS` check passes. The selected plan remains `cuda_128`, and the
action still reports source emission without a tile WHIRL rewrite.

The immutable `run-010/` artifact adds a first compile-time Roofline aid.
Schema v4 records the H100 SXM published FP32/HBM ceilings once, then per-plan
FLOPs, modeled global bytes, and ideal compute, cold-HBM, and maximum bounds.
The selector may prefer a legal alternative only within roughly 3% of the
lowest relative cost and with at least roughly 5% lower bound. This is an
uncalibrated tie-break heuristic, not a latency estimate or a measured gain.
For the fixed 128x128x128 GEMM, `cuda_128` remains the relative-cost winner;
its 63 ns ideal bound is below `cuda_64`'s 98 ns. Neither kernel has been
validated on a Hopper GPU, and actual HBM traffic remains unknown. Blackwell
still uses relative cost alone because no matching ceiling is supplied.
The `run-011/` artifact uses the rebuilt selector. The latest `run-012/`
also reports arithmetic intensity, bandwidth needed to reach the FP32 peak,
and the limiting Roofline resource. Its raw trace, generated `.cu`, reports,
and summary are checksum-verified.

An opt-in P8 source-emission path now consumes the AIO-10 selected fetch plan
for the exact F32 tile. The checksum-verified immutable `run-015/` artifact
selects a
two-stage `tma_like` plan (64 KiB A/B staging), emits 2-D TMA tensor-map
loads with stage barriers and a runtime-guarded launch wrapper, and records
P8 candidates, selection, and source action in trace schema v5 and
`fetch-pipeline.log`. Candidate 4 wins by `lowest_complete_relative_cost`;
its raw/hidden/unhidden movement scores are 1024/512/512 relative units,
not measured time. The Open64 backend rebuild and 16 Python report tests pass.
This is **source-only**, not GPU-compiled, not a WHIRL
rewrite, and not a measured prefetch improvement; `m3` remains open pending
native compile, correctness, instruction-path inspection, and same-target
comparison with the synchronous tile.

The digest-named `m1-trace-report/sha256-c3e7ff97ea9a49ef7b01897e1edb16cd19f9f93ca1acfaf50b9b5c93f81ae69c/`
checkpoint adds schema v7 P0-P11 coverage, source-linked AIO-3 semantic tensor
facts and AIO-4 locality facts, a separate option-off `.t`, all twelve derived
phase logs for both runs, and explicit selected/unselected tile labels. Its
`SHA256SUMS` verification passes. This verifies the trace/report implementation
but is not an accepted `m1` receipt because native `m0` is still missing.
The compiled direct PU also passes numerical checks in
`m0-baseline/emulated-smoke-001/`; its x86 execution was emulated on this host,
so its timing is not target profitability evidence or an accepted `m0` receipt.
The same executable and WHIRL evidence are retained in
`m0-baseline/native-ready-001/` with a native x86-64 runner; no native receipt
has been recorded.
The selected compiler `.cu` is retained in `compiler-source/run-018/` with an
explicit TMA-versus-fallback launch flag. The exact same source bytes and
compiler trace are packaged in `compiler-gpu-validation/run-006/` with a
nonuniform-input correctness and same-device sync/TMA wall-clock timing harness.
The source tile dimensions, thread geometry, shared-memory bytes, plan ID,
and problem shape are checked against the selected trace before publication.
The new `plans/m2-selected-tile.json` records the selected plan, cost and
resource estimates, source line, fetch plan, and source/trace/WHIRL hashes;
it is carried into the GPU package but records no executable rewrite. The
native run is pending; the local host has no `nvcc` or `nvidia-smi`.

| Outcome metric | Latest accepted value | Evidence |
| --- | --- | --- |
| Target and GEMM shape | Not recorded | `m0-baseline` receipt pending. |
| Direct F32 baseline latency | Not measured in an accepted receipt | `m0-baseline` pending. |
| Generated tiled F32 latency and speedup | Not available | `m2-multilevel-tiling` pending. |
| Selected tile, SMEM per CTA, and CTA residency | Not available for an emitted kernel | `m2` compiled-resource check pending. |
| AI decision-trace phase coverage | P0-P11 explicit coverage for the standalone matmul source slice; P7/P8 candidate, selection, and source-sidecar action | Digest-named `m1-trace-report` checkpoint; native `m0` predecessor pending. |
| Predicted-versus-measured cost error | Not available | Cost calibration follows m0-m3. |

### Status key

| State | Meaning in this ledger |
| --- | --- |
| Designed | Contract or option is written, but no implementation evidence is claimed. |
| Check-only | Analysis records exist; executable WHIRL is unchanged for that slice. |
| Provider rewrite | An executable ABI/provider selection exists, but it is not a generated kernel. |
| Generated and verified | A selected compiler-generated transformation has before/after IR, numerical, and launch/resource verification. |
| Measured | Correctness-qualified baseline and candidate have comparable target-specific timing evidence. |

## Matmul execution gates

These rows mirror the [Current Matmul Execution Queue](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#current-matmul-execution-queue).
An unchecked gate is open even if a related check-only AIO slice is complete.

| Gate | Status on 2026-10-20 | Evidence required to close | Next action |
| --- | --- | --- | --- |
| `m0-baseline` | Open; compiled-PU native package ready | Direct F32 correctness passes in emulation; source, `.B`/`.T`, executable, commands, and hashes are retained in `emulated-smoke-001/`. `native-ready-001/` preserves the exact executable for fresh native correctness and timing. Emulated timing is not target evidence. | Run on the intended native x86-64 target and review the returned receipt. |
| `m1-trace-report` | Trace/report implementation verified; acceptance open | Schema v7 emits P0-P11 coverage, P0 semantic facts, P1 locality facts, and an option-off trace. `ai-opt-report` validates decisions and produces all twelve phase logs, the complete log, and the summary. The digest-named checkpoint retains both raw `.t` files, reports, commands, and checksums; malformed/incomplete-trace tests pass. The receipt explicitly says `m1_accepted=false`. | Obtain an accepted native `m0-baseline` predecessor, then review and accept the m1 receipt; broaden other phase details as each optimizer is implemented. |
| Tile costing prerequisite | Open | Baseline plus at least two M/N/K tile families with explicit units, target resources, confidence, rejection reasons, and stable plan IDs. | Refine AIO-9 check-only plans into costable full candidates. |
| `m2-multilevel-tiling` | Open; selected-plan WHIRL rewrite verified, provider unresolved | `compiler-source/run-018/` retains M2a. `m2-applied/run-005/` shows the option-on generated-provider call and P7/P9 rewrite rows; option-off retains the generic call. `m2-whirl-g14/run-007/` retains matched before/after binary WHIRL and a full diff, and verifies ABI size, invalid-plan/resource rejection, and rollback. The generated runtime symbol is not linked to a CUDA provider. | Implement and link the CUDA runtime adapter and fallback, then verify correctness and same-target timing. |
| `m3-prefetch-pipeline` | Open; native validation package prepared | Selected P7.4/P8 staging and fallback are in the source sidecar and trace. `compiler-gpu-validation/run-006/` preserves the M2 handoff and is ready to compare selected TMA and synchronous paths with correctness and 30 wall-clock launch-to-completion samples per path, including host launcher setup. No native compile or timing has been run, and WHIRL is unchanged. | Run the package on a Hopper-or-later GPU, then integrate the selected plan into executable WHIRL after m2. |
| Cost calibration | Open | Predicted-versus-measured compute, memory, sync, launch, and occupancy terms, with provenance and model error. | Calibrate from accepted m0-m3 measurements. |

## Phase maturity

The AIO rows describe the first certified vertical slice, not full feature
completion. The authoritative phase contracts remain in the implementation
plan and their linked AIO documents.

| Phase | Current maturity | Missing executable or measured evidence |
| --- | --- | --- |
| AI-P0 semantic tensor | AIO-3 check-only; v7 trace records tensor roles, shapes, dtypes, and source lines | Broaden eligible-region and unresolved-semantic reporting beyond the standalone matmul. |
| AI-P1 lifetime and locality | AIO-4 check-only; v7 trace records lifetimes, reuse, alias, working set, and traffic estimates | Connect facts to the generated tile's legal live ranges and cost. |
| AI-P2 fusion | AIO-5 check-only | Out of scope for the first standalone matmul; report `not_applicable`. |
| AI-P3 logical layout | AIO-6 check-only | Realize the selected GMEM/SMEM access layout in generated code. |
| AI-P4 placement and sharding | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P5 communication | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P6 residency | AIO-8 check-only | Verify compiled SMEM/register usage and CTA residency against the selected plan. |
| AI-P7 multi-level tiling | AIO-9 candidate/selection plus opt-in generated-provider rewrite | Calibrate resource use and execute the selected tile on a GPU. |
| AI-P8 fetch and pipeline | AIO-10 check-only | Allocate A/B slots and emit legal copy, wait, compute, and release events. |
| AI-P9 physical plan | AIO-11 F4 provider rewrite; separate F32 generated-provider call verified | Link the generated call to the selected CUDA kernel with fallback and runtime evidence. |
| AI-P10 runtime variants | AIO-12 check-only guarded variant evidence | Defer executable variants until the kernel and fallback are certified. |
| AI-P11 feedback | AIO-13 check-only telemetry join | Add reviewed profile transport and measured feedback after executable work. |

## Options and observability

Keep registered switches distinct from proposals. The
[backend option registry](../osprey/common/com/config_dsl.cxx) is the source for registered
`-DSL` names; the Python [export options](../osprey/torch2whirl/python/open64_dsc/options.py)
are frontend settings, not backend optimization controls.

| Control | Status | Monitoring implication |
| --- | --- | --- |
| `-O0` through `-O3`, `-ipa` | Driver modes; AI scope contract in the implementation plan | `-O0` is the direct baseline; the first target tile belongs at `-O3`. Do not infer end-to-end certification from mode acceptance. |
| `-DSL:shape_refine`, `-DSL:quant`, `-DSL:fusion`, `-DSL:parallel`, `-DSL:impl_select` and related stage controls | Registered in `config_dsl.cxx` | A stage switch is not proof of an applied GPU optimization. `-DSL:quant` does not authorize GEMM precision loss. |
| `-keep`, `-DSL:dump_after_lower`, `-ft,<file>` | Driver/backend artifact controls used by the openpy lane | Useful for retained baseline `.t` and lowered output; not the proposed AI decision trace. |
| `-Wb,-ttOP2:0x00800000` | Implemented for the standalone matmul source slice | Schema v7 covers P0-P11, records P0/P1 facts, and retains P7/P8 decisions; `ai-opt-report` rejects missing or contradictory coverage. Other workload scopes still need coverage. |
| `-Wb,-DSL:ai_opt_trace=ON|OFF` | Registered; ON by default during AI optimization development | ON activates the trace bit even when CUDA source emission is off; the standalone matmul records why P7/P8 did not run. OFF suppresses AI decision rows. |
| `-Wb,-DSL:cuda_tile_output=<new-file.cu>` | Registered opt-in source sidecar | Emits one cost-selected F32 tile source for a straight-line PU. Does not apply a WHIRL rewrite or close `m2`. |
| `-Wb,-DSL:cuda_tile_apply=ON` | Registered, OFF by default; requires `-O3`, `-noinline`, one matmul, `cuda_tile_output`, and synchronous movement | Atomically rewrites the selected matmul to `__open64_dsl_matmul_generated_v1`; P7/P9 trace rows report one rewrite. The runtime provider is not implemented or linked yet. |
| `-trOP2` | Existing IR-dump selector | Separate from the proposed AI decision-trace mask. |
| `-DSL:gemm_precision=preserve|allow_narrow` | Proposed; not registered | Default preserves source precision; narrowing also needs a numerical contract and full-plan legality/profitability. |
| `-DSL:gemm_schedule=auto|conventional|persistent` | Proposed; not registered | Do not expose until a persistent kernel is implemented and verified. |

The target-specific SMEM ledger, hard resource gate, narrow-precision policy,
and P7/P8 movement sequence are in the
[hardware planning detail design](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md).

## Evidence and update rule

Accepted matmul receipts use the AI-specific immutable namespace defined by
the implementation plan:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/<target-id>/<milestone>/sha256-<digest>/`.
That is the planned publication path, not an assertion that a receipt is
present in this checkout. Legacy `artifacts/ai_optimization/` evidence stays
separate and must not be deleted or relabeled as an accepted receipt.

After each accepted milestone, update this ledger in the same change that
updates the implementation queue:

1. Record review date, repository commit, target, milestone status, and an
   exact receipt path or link. Do not close a row using only a design note or
   a check-only candidate trace.
2. Link raw `.t`, derived `optimization-summary.log` and `tile.log` when
   available, before/after `.B`/`.T` for rewrites, numerical result, and raw
   timing samples. Keep estimated cost separate from measured latency.
3. State the selected plan ID, baseline ID, decision reason, rewrite result,
   verification result, and next blocker. For skipped phases, record the
   explicit disabled/not-applicable/no-eligible reason.
4. Preserve prior receipt links and add one dated change-log entry; do not
   overwrite an accepted receipt or silently mark all phases complete.

## Change log

| Date | Change | Evidence |
| --- | --- | --- |
| 2026-10-20 | Initial ledger from implementation-plan queue and current option registry. No m0-m3 receipt is linked here. | [Implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#current-matmul-execution-queue), [registered DSL options](../osprey/common/com/config_dsl.cxx). |
| 2026-10-20 | Added an executable `-O0` F32 matmul runner and validated its compiler path under x86-64 emulation; `m0` remains open pending native timing and accepted publication. | [Runner and limitations](../osprey/be/vho/tests/ai_matmul_m0/README.md). |
| 2026-10-20 | Prepared a source-preserving Hopper CUDA validation bundle with remote compile/run/retrieval tooling. GPU execution and compiler lowering remain open. | [CUDA bundle contract](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-20 | Added opt-in compiler-selected CUDA source emission; `cuda_128` source and option-off/on evidence retained. WHIRL application and native GPU verification remain open. | [Compiler source bundle](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-21 | Added opt-in selected-tile WHIRL application and a 64-byte generated-provider plan ABI. Normal `-O3` option-off/on traces and matched binary-WHIRL before/after views verify the compiler rewrite; the CUDA runtime provider and native measurement remain open. | `m2-applied/run-005/`, `m2-whirl-g14/run-007/`, [compiler runner](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
