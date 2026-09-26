# AIO-10 Fetch And Async-Pipeline Contract

## Purpose

AIO-10 implements the first check-only AI-P8 vertical slice. It consumes the
selected AIO-9 `CommonTilePlanIR` and constructs typed `CommonFetchPlanIR` and
`CommonPipelineIR` alternatives for demand movement, vector movement,
asynchronous copy, and multidimensional asynchronous transfer.

The implementation is runtime-only and PU-local. It does not emit
`OPR_PREFETCH`, shared-memory allocation, barriers, target instructions,
runtime calls, or mapped-image rows. The selected pipeline remains analysis
evidence for AIO-11 physical-plan selection and later target lowering.

## Existing Open64 Services Reused

- `CommonTilePlanIR` supplies the selected tile shape, edge policy, shared
  bytes, buffering feasibility, target profile, and TMA-candidate hint.
- `DSL_TENSOR_LOCALITY_ANALYSIS` supplies active-PU lifetime, effect, and
  unique-ownership evidence for both fetched operands.
- `DSL_DISTRIBUTED_ANALYSIS`, when available, supplies selected communication
  intent. An early fetch is rejected rather than crossing an unresolved
  communication epoch.
- `TensorEvolutionGraph` records provisional staged-buffer representations
  without changing canonical tensor values or `TY_IDX` identities.
- `OptimizationPlanIR` supplies candidates, legality, raw cost, fallback,
  deterministic selection, and budgets.
- `MemoryHierarchyDescriptorIR` supplies immutable target movement
  capabilities and source/destination memory tiers.

The existing `OPR_PREFETCH` and `WT_PREFETCH` framework remains the reference
for a later lowered CPU prefetch representation. This slice does not reuse it
as the high-level plan because it cannot express staged GPU buffers, arrival
barriers, buffering depth, or overlap cost.

## Runtime Records

Each fetch site references one selected AIO-9 tile plan. Its alternatives own:

- two operand fetch records with exact value and tensor-type identity;
- source and destination memory tiers;
- movement engine, transaction size, alignment, and bytes per tile stage;
- one, two, or three buffer-stage records;
- prefetch distance, issue point, arrival barrier, consumer wait point, and
  inherited edge-tile policy;
- provisional staged-buffer TensorEvolutionGraph nodes for legal nonbaseline
  alternatives;
- raw, hidden, and unhidden movement cost;
- legality, rejection reason, candidate, plan, selected plan, and fallback.

Records use process-lifetime C++ containers because they are analysis state,
not binary WHIRL tables.

## Initial Alternatives

| Alternative | Buffering | Issue and wait contract | Initial role |
| --- | --- | --- | --- |
| Demand | One stage | Issue at consumer; no arrival barrier. | Always-valid fallback. |
| Vector | One stage | Vector load plus staged destination; CTA wait when required. | Synchronous coalesced alternative. |
| Async copy | Two stages | Issue at previous K tile; wait at pipeline stage. | Generic cp.async-like alternative. |
| Multidimensional async | Three stages | Prologue issue; arrival barrier; pipeline-stage wait. | Generic TMA-like alternative. |

The engine names are logical capability classes, not emitted instruction
names. Target lowering must revalidate and select the concrete mechanism.

## Target Profiles

The AIO-8 runtime target profiles now expose typed movement capabilities. CPU
baseline supports demand and vector movement. Hopper and Blackwell additionally
advertise generic asynchronous-copy and multidimensional-async capabilities,
with distinct planning transaction widths and stage limits.

These records are conservative planning envelopes marked as profile
assumptions. They are not live device discovery and do not guarantee that a
particular instruction sequence is legal. AIO-11 and target lowering remain
responsible for exact architecture, dtype, layout, address-space, alignment,
barrier, and instruction checks.

## Legality

The demand fallback remains legal. A nonbaseline plan requires:

- a proven-legal selected AIO-9 tile;
- exact-block lifetime and unique ownership for both operands;
- no effect barrier on either operand;
- no selected communication intent whose visibility epoch remains unresolved;
- target support for the movement engine;
- sufficient AIO-9 and target buffering capacity;
- `prefetch_distance < stage_count` for asynchronous alternatives;
- exact tile boundaries and the AIO-9 TMA hint for the first
  multidimensional-async slice.

Missing evidence remains unknown. Effect, ownership, resource, target, and
unsafe-distance failures are explicit rejections. A rejected plan never
justifies transformation.

Each fetch record preserves the movement capability's minimum-alignment
requirement for AIO-11. This first check-only slice does not prove the eventual
address or storage alignment; physical-plan selection and target lowering must
reject any selected movement whose concrete operands do not satisfy it.

## Cost

The first model computes deterministic low-confidence relative terms:

```text
raw movement = ceil(bytes / transaction_bytes) * latency_class
hidden movement = raw * (stage_count - 1) / stage_count  [async only]
unhidden movement = raw - hidden movement
```

The plan records raw, hidden, and unhidden amounts separately. Selection
charges unhidden movement plus synchronization. These quantities exercise the
plan architecture; they are not cycles, elapsed time, achieved bandwidth, or
proof of a performance improvement. Later target modeling and telemetry may
replace them.

## Scope And Compatibility

All APIs require the owning PU and local symbol table to be active. The phase
does not inspect another PU. Interprocedural fetch summaries belong to an
explicit IPA design.

AIO-10 changes no logical opcode, WN layout, canonical tensor type, symbol
table, ELF section, mapped-image row, frontend API, or runtime ABI. Before and
after `.B` files and `ir_b2a -st -src` traces remain byte-identical.

## Certification

The focused `common.matmul -> common.relu` producer proves:

- AIO-10 consumes the selected AIO-9 tile rather than rediscovering tiling;
- one-, two-, and three-stage alternatives are represented;
- Hopper and Blackwell movement capabilities remain distinguishable;
- CPU falls back without asynchronous engines;
- an unsafe prefetch distance rejects async alternatives and selects vector
  fallback;
- raw, hidden, and unhidden movement terms are inspectable;
- staged-buffer TensorEvolutionGraph overlays are provisional and PU-local;
- generation, selection, and transformation controls fail closed;
- no logical node, value, type, executable WN, or binary image changes;
- repeated analysis is deterministic.

Review evidence is retained under
`artifacts/ai_optimization/aio10_pipeline/` and cleaned at the beginning of the
next run.

## Deferred Work

- executable prefetch and pipeline scheduling;
- exact cp.async, TMA, mbarrier, cluster, multicast, and Blackwell contracts;
- LNO-backed issue-point and loop dependence proof;
- bank-conflict, occupancy, instruction throughput, and measured latency
  models;
- predicated multidimensional edge transfers;
- scale-tensor and low-precision metadata pipelines;
- host/device, remote-HBM, KV-cache, and distributed communication pipelines;
- AIO-11 complete physical-plan selection and atomic WHIRL rewriting;
- mapped-image publication, if later justified;
- explicit IPA summaries and cross-PU planning.
