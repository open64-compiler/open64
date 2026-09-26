# AIO-9 Hierarchical Tile-Plan Contract

## Purpose

AIO-9 implements the first check-only AI-P7 vertical slice. It constructs a
typed hierarchy of GEMM tile-plan candidates from one logical
`common.matmul.v1` result and records the successive `P7.0` through `P7.11`
decisions as reviewable `G0` through `G11` evidence.

The implementation is runtime-only and PU-local. It does not rewrite loop
nests, allocate shared memory or registers, insert synchronization, select a
runtime kernel, alter a tensor type, or add a mapped-image record. The selected
plan remains analysis evidence for AIO-10 and later lowering work.

## Existing Open64 Services Reused

The implementation follows the Open64 design-continuity rule:

- `DSL_TENSOR_ANALYSIS` supplies canonical tensor and producer facts;
- `DSL_TENSOR_LOCALITY_ANALYSIS` supplies active-PU lifetime, effect, and
  unique-ownership evidence;
- `DSL_TENSOR_EVOLUTION_GRAPH` records provisional, semantics-preserving tile
  representations without mutating the canonical tensor `TY_IDX`;
- `DSL_OPT_PLAN_CONTEXT` supplies immutable candidates, legality, costs,
  fallback, deterministic selection, and budgets;
- `MemoryHierarchyDescriptorIR` supplies typed Hopper and Blackwell shared and
  register resource envelopes.

This first slice does not invoke LNO because it has not yet materialized a
canonical loop nest. Once executable loop transformation begins, AIO-9 must
reuse LNO dependence, loop canonicalization, blocking, and legality services
where their semantics apply rather than introducing a parallel loop optimizer.

## CommonTilePlanIR

`dsl_tile_candidate.{h,cxx}` owns runtime-only records for:

- one candidate site identified by owner PU, logical node/value, semantic
  tensor root, and selected input representation;
- one unchanged baseline plus bounded target-specific tile families;
- problem, CTA, warp, thread, and instruction tile shapes;
- vector width, transaction width, edge policy, and instruction family;
- estimated global-memory traffic and operation count;
- shared bytes per stage, feasible buffering depth, register bytes per thread,
  thread/warp counts, and barrier count;
- legality, rejection reason, AIO-2 candidate/plan IDs, fallback, and
  TensorEvolutionGraph node/edge IDs;
- explicit stage records for every completed `P7.*` refinement.

The initial operator contract accepts only a live, pure, rank-2
`common.matmul.v1` with two operands, static compatible shapes, matching
element types, and both transpose attributes set to `false`. Unsupported or
incomplete operators are skipped rather than guessed.

## G0 Through G11

| Evidence | Phase | First-slice record |
| --- | --- | --- |
| G0 | P7.0 baseline | Static M/N/K, naive traffic, operation count, unchanged fallback. |
| G1 | P7.1 output | CTA output tile and predicated/exact boundary policy. |
| G2 | P7.2 coalescing | Vector width and transaction-width intent. |
| G3 | P7.3 reduction | CTA K tile and accumulation scope. |
| G4 | P7.4 shared | Shared tile footprint, buffering feasibility, and TMA candidate hint. |
| G5 | P7.5 resources | Shared/register/thread/warp/barrier feasibility and legality. |
| G6 | P7.6 thread | Per-thread M/N result tile. |
| G7 | P7.7 register reuse | Register-fragment extent and K reuse intent. |
| G8 | P7.8 vector | Vector access width. |
| G9 | P7.9 family | Named bounded candidate family. |
| G10 | P7.10 warp | Warp-level M/N tile. |
| G11 | P7.11 instruction | Innermost scalar/vector-FMA instruction tile. |

The stages are evidence milestones, not eleven executable passes. A stage can
remain incomplete or be rejected while the baseline remains legal.

## Target Profiles

The first deterministic family generator uses:

- Hopper: baseline, `cuda_64`, and `cuda_128`;
- Blackwell: the Hopper families plus `blackwell_wide` with CTA shape
  `[128,256,32]`;
- CPU baseline: unchanged plan only.

These families prove that typed target capabilities can produce materially
different candidate spaces. They are not claims of peak-performance kernels.
Later analytical calibration, measurement, autotuning, instruction-family
selection, and target-specific lowering may refine or replace them.

## Legality And Cost

The unchanged baseline is always proven legal. Starting at P7.5, a generated
tile is rejected for an effect-bearing lifetime, unresolved unique ownership,
thread-count overflow, shared-memory overflow, register-pressure overflow, or
zero buffering capacity. Before P7.5, generated alternatives remain unknown
with `incomplete_analysis` because resource feasibility has not run.

Each tile is an AIO-2 candidate with the baseline as fallback. The first cost
model records low-confidence relative compute, unhidden-memory, and
synchronization terms. It exists to exercise deterministic plan composition
and selection; it is not cycles, elapsed time, occupancy, or achieved FLOP/s.

## Scope And Compatibility

All APIs require the owning PU and local symbol table to be active. The
analysis does not inspect or mutate another PU. Interprocedural tile summaries
belong to an explicit IPA design.

AIO-9 changes no logical opcode, physical WN layout, type kind, canonical
tensor type, symbol-table contract, ELF section, mapped-image row, reader,
writer, or frontend API. `OPR_DSL` remains hidden behind logical operator APIs.
Before/after `.B` files and `ir_b2a -st -src` traces must remain byte-identical.

## Certification

The focused `common.matmul -> common.relu` producer proves:

- one active-PU tile site with an unchanged baseline;
- all G0-G11 stage records and deterministic repeat output;
- distinct Hopper and Blackwell candidate spaces;
- exact and predicated boundary policies;
- typed resource evidence and exact effect rejection;
- AIO-2 costs, selection, and fallback on every candidate;
- a numerical tiled-GEMM oracle that preserves the reference K order;
- prohibited transformation application and disabled generation controls;
- unchanged DSL node/value/TY counts before binary finalization;
- byte-identical `.B` and `ir_b2a -st -src` evidence for every G stage.

Review artifacts are retained under
`artifacts/ai_optimization/aio9_tiling/` and cleaned at the start of the next
run rather than at the end of the current run.

## Deferred Work

- LNO-backed loop-nest materialization, dependence proof, and atomic rewrite;
- complete access-pattern, arithmetic-intensity, occupancy, bank-conflict, and
  launch-wave models;
- separate A/B shared layouts, padding, swizzle, and scale-tensor tiles;
- tensor-core MMA/WGMMA fragment contracts and dtype-specific accumulation;
- AIO-10 async fetch, TMA/cp.async realization, barriers, and buffering;
- bounded search, learned/measured cost evidence, and autotuning database;
- executable fallback and runtime variants;
- mapped-image publication, if later justified;
- explicit IPA summaries and cross-PU planning.
