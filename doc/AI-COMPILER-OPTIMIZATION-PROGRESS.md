# AI Compiler Optimization Progress

Snapshot: 2026-10-20, reviewed against base commit `e17a2c23`
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
Open64-generated tiled GEMM.** No generated F32 multi-level tile, AI decision
trace/report tool, or measured baseline-versus-generated-tile result is
certified by the current matmul execution queue.

**Next acceptance gate:** publish `m0-baseline` for the direct fixed-shape F32
matmul, with correctness, target identity, raw timings, source, binary `.B`,
separate-process `ir_b2a -st -src` `.T`, commands, and checksums. Do not infer
performance progress from a selected check-only plan.

| Outcome metric | Latest accepted value | Evidence |
| --- | --- | --- |
| Target and GEMM shape | Not recorded | `m0-baseline` receipt pending. |
| Direct F32 baseline latency | Not measured in an accepted receipt | `m0-baseline` pending. |
| Generated tiled F32 latency and speedup | Not available | `m2-multilevel-tiling` pending. |
| Selected tile, SMEM per CTA, and CTA residency | Not available for an emitted kernel | `m2` compiled-resource check pending. |
| AI decision-trace phase coverage | Not available | `m1-trace-report` pending. |
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
| `m0-baseline` | Open | Direct F32 correctness, target/driver identity, raw timings, source, `.B`/`.T`, commands, hashes, accepted receipt. | Run and publish the fixed-shape baseline. |
| `m1-trace-report` | Open | WOPT-style AI trace bit, raw `.t`, `ai-opt-report`, all phase logs and coverage rows, selected/skipped cases, malformed-trace negatives, accepted receipt. | Implement trace emission and parser after m0. |
| Tile costing prerequisite | Open | Baseline plus at least two M/N/K tile families with explicit units, target resources, confidence, rejection reasons, and stable plan IDs. | Refine AIO-9 check-only plans into costable full candidates. |
| `m2-multilevel-tiling` | Open | Cost-selected generated tile, atomic rewrite, before/after `.B`/`.T`, numerical equivalence, same-target timing, accepted receipt. Provider dispatch does not close this gate. | Emit and verify one real F32 tiled kernel. |
| `m3-prefetch-pipeline` | Open | Selected P7.4/P8 staging, slot and barrier safety, movement and overlap costs, fallback, correctness, timing, accepted receipt. | Add and compare one-slot and async staged plans after m2. |
| Cost calibration | Open | Predicted-versus-measured compute, memory, sync, launch, and occupancy terms, with provenance and model error. | Calibrate from accepted m0-m3 measurements. |

## Phase maturity

The AIO rows describe the first certified vertical slice, not full feature
completion. The authoritative phase contracts remain in the implementation
plan and their linked AIO documents.

| Phase | Current maturity | Missing executable or measured evidence |
| --- | --- | --- |
| AI-P0 semantic tensor | AIO-3 check-only | Include region and eligibility coverage in the new `.t` report. |
| AI-P1 lifetime and locality | AIO-4 check-only | Connect facts to the generated tile's legal live ranges and cost. |
| AI-P2 fusion | AIO-5 check-only | Out of scope for the first standalone matmul; report `not_applicable`. |
| AI-P3 logical layout | AIO-6 check-only | Realize the selected GMEM/SMEM access layout in generated code. |
| AI-P4 placement and sharding | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P5 communication | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P6 residency | AIO-8 check-only | Verify compiled SMEM/register usage and CTA residency against the selected plan. |
| AI-P7 multi-level tiling | AIO-9 G0-G11 check-only | Cost at least two complete tile families and emit one verified tiled rewrite. |
| AI-P8 fetch and pipeline | AIO-10 check-only | Allocate A/B slots and emit legal copy, wait, compute, and release events. |
| AI-P9 physical plan | AIO-11 provider rewrite for F4 (four-byte float); generated tile absent | Apply a selected generated F32 plan with before/after evidence. |
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
| `-Wb,-ttOP2:0x00800000`, `ai-opt-report` | Proposed `m1` work; not implemented | Must produce the raw `.t`, phase logs, and summary before claiming trace coverage. |
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
