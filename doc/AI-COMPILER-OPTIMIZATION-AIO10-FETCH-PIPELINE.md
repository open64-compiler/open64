# AIO-10 Fetch And Async-Pipeline Contract

## Purpose

### P8 Enhancement Note 2026-10-27

Adopt [Common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md):
computation characterization/verified algebraic alternatives feed a joint
Work Partition, Resource Lifetime and Execution Events plan. Extend P8 with
range/generation-aware access lifetimes and physical storage assignment.
Readiness enables consumption; completion of all conflicting accesses enables
reuse. Do not reclaim a range at consumer issue or from predicted timestamps.
Record alias ranges, completion joins, scope/visibility, aligned offsets,
barrier lifetimes, progress, added synchronization and lost overlap; recheck
reservation/occupancy and complete cost at P9. Keep disjoint storage and
synchronous fallbacks. Logical release does not shrink a static CTA reservation.

This is a planned generalization, not a claim that existing AIO-10 records
are a general event graph or allocator. The executable fixed F32 slot protocol
below is the implemented case and remains unchanged. The
[four-case research milestone](research_library/building_block_planning/README.md)
supplies host/reference and synthetic analytical evidence only.

Enhanced `fetch-pipeline.log` must summarize each range/generation, issue,
ready, access-complete and reuse point, join participants, storage offsets,
bytes saved, proof/rejection, added waits and fallback. Coordinate footprints
with `residency.log` and selection/actions with `physical-plan.log`.
Keep the existing first-line description, source cross-reference and
option-controlled WOPT trace convention. New row schemas require explicit
versioning before implementation.

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

`common/com/dsl_fetch_pipeline.{h,cxx}` owns the policy-free
`CommonFetchPlanIR/CommonPipelineIR` records, bulk construction, structural
verification, accessors, stable names, and generic printing. It does not
inspect active WHIRL, generate movement plans, classify legality, estimate
overlap, or select a plan.

`be/vho/dsl_fetch_pipeline_opt.{h,cxx}` consumes the neutral
`CommonTilePlanIR` and owns active-PU movement-plan generation, overlap and
resource reasoning, costs, selection, and semantic verification. It
materializes completed decisions through the common bulk constructor.

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

The check-only description above remains the AIO-10 baseline. The executable
P8 slice below supersedes only its synchronous/two-slot TMA deferral.

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

## Executable P8 Contract

Scope: one selected fixed-shape F32 GEMM per active analysis PU. VHO owns
plan validation and schedule construction; the backend driver owns device-PU
construction/publication. No binary WHIRL layout or opcode changes are needed:
use ordinary DO_LOOP, IF, scalar address expressions, shared-attributed symbols
and conservatively effectful device CALLs. No new logical tensor operations
are introduced after semantic lowering. The repository's existing high-level
WHIRL construction/strict-tree rules remain applicable. The external
`WHIRL.pdf` baseline is not present in this checkout.

| Term | Definition |
| --- | --- |
| Slot | One A staging allocation and one B staging allocation reused together for a K tile |
| Generation | For two slots, `floor(tile_index/2)`; ready-barrier phase is generation modulo two |
| Acquire | Producer reuses an initially free or CTA-released slot and arms its ready barrier for exactly A+B payload bytes |
| Issue | One TMA copy for A and one for B, targeting that slot and generation |
| Wait | Every consumer observes the ready barrier with acquire semantics before any shared reads |
| Release | A uniform CTA barrier after all compute reads; producer may then overwrite the slot |
| Drain | Every issued generation has been waited, consumed and released before barrier invalidation/kernel exit |
| Tensor map | Immutable, host-encoded 128-byte CUDA descriptor in device-global storage, at least 64-byte aligned; describes the exact row-major unswizzled A/B tile |

Non-TMA: one slot, cooperative predicated scalar fills, CTA publication wait,
compute, CTA release, next fill. Zero-fill preserves irregular-edge behavior.
It is a synchronous schedule; it does not claim asynchronous overlap.

Two-slot TMA: compact exact tiles only, depth two, distance one, no XOR or
padded-A layout. Initialize two 8-byte ready barriers with one arrival each,
publish initialization to the async proxy, and synchronize the CTA. Thread
zero issues `min(2,K_tiles)` prologue pairs, arming expected bytes **before**
the copies. All threads wait for slot `kt%2`, phase `(kt/2)%2`, compute, then
release together. Thread zero can acquire/issue `kt+2` into the released slot
only if that future tile exists. Drain and invalidate both barriers. No
consumer-warp specialization or cluster multicast is implied.

The ordering proof uses the fixed schedule form and a per-slot induction:
initially free; issue(g) precedes wait(g); wait(g) precedes all reads(g);
release(g) follows every read(g); issue(g+1) occurs only after release(g).
The guarded prologue/refill produce every tile exactly once, and the final
release drains the outstanding generation. Barrier parity alternates on
successive uses of the same slot. Structural WHIRL checks and independent
source/slot-state tests must accompany this argument; a trace label alone is
not a proof. Unsupported geometry, resources, ordering or ABI must fail closed
before device-image publication.

`osprey/include/open64_cuda_pipeline.cuh` defines the concrete device-call ABI
for single PTX operations (init, async-proxy publish, arrive/expect-tx acquire,
2-D issue, acquire-wait, invalidate). It contains no slot selection, prologue,
refill policy or GEMM schedule. Those decisions and control flow reside in
WHIRL. `whirl2c -cuda` prints recorded user alignment and includes the operation
header when its symbols are present; it cannot select a schedule.

The TMA device entry takes `(A,B,C,A_map,B_map)`, with maps passed by pointer;
it is distinct from the legacy by-value/grid-constant template ABI. Host maps
must be prepared by `cuTensorMapEncodeTiled`, uploaded and kept alive through
completion in the same correctly ordered stream. A/B map extents, strides,
tile boxes, dtype, swizzle and base pointers must match the selected plan.
Launch only on sm_90+ with enough opt-in shared memory. The existing generated
host-provider ABI remains synchronous-only and must reject TMA application
until a reviewed launch adapter consumes this new contract.

Capacity includes both A/B slots and both ready barriers, not just payload.
Shared arrays carry explicit 128-byte alignment; ready storage is 8-byte
aligned. All seeded tile footprints are multiples of 128 bytes. Runtime
device resource checks and native compiler resource reports remain required.
This first WHIRL lowering uses static arrays and conservatively gates total
TMA storage at 48 KiB; it cannot spend the larger dynamic-SMEM opt-in budget.
Larger selected TMA plans fail closed until dynamic shared storage is lowered.
The logical vector fetch alternative lowers to cooperative scalar source
fills here; its name is not a claim of vector PTX or asynchronous execution.
Generic async-copy plans and TMA edges/swizzles are not silently lowered as
this schedule. The existing `cuda_tma_prefetch` option selects the TMA request;
device-image output must no longer reject it solely for being asynchronous.

The operation contract follows the
[NVIDIA PTX ISA](https://docs.nvidia.com/cuda/parallel-thread-execution/)
and [CUDA asynchronous-copy guide](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/async-copies.html).
Our fixed schedule, proof checks, option behavior and trace schema are Open64
design proposals. Host simulation and compiler inspection are not native PTX,
GPU correctness, race-sanitizer or performance certification.
