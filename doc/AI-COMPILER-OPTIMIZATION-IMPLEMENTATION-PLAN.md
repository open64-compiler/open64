# AI Compiler Optimization Implementation Plan

## Status

Execution plan for `AI_compiler_optimization_design_v0.3.md`. The completed
AIO-0 through AIO-13 queue below records the analysis and planning rollout;
the Current Matmul Execution Queue defines the next action-gated work.
The running [AI optimization progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md)
summarizes phase maturity, option status, and accepted execution evidence.

The architecture document is authoritative for the `AI-P0` through `AI-P11`
phase order, the TensorEvolutionGraph-centered design, common-versus-domain
ownership, and plan-level profitability. This document turns that design into
staged Open64 work with explicit ownership, compiler scope, review gates,
artifacts, and pull-request boundaries.

`OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md` is authoritative for the
larger source-to-device lifecycle. This implementation plan stops at certified
global plans, local kernel plans, package requirements, runtime variants, and
telemetry contracts. Target device compilation, AOT/late/hybrid publication,
module loading, serving integration, and provider execution consume these
outputs through the umbrella architecture and their target-specific plans.

The first implementation was deliberately narrow: one PU, one fixed-shape
`common.matmul`, check-only analysis, and no binary WHIRL layout change. Later
milestones expand the same services to CNN, Transformer, distributed
placement, target tiling, asynchronous movement, runtime variants, and
telemetry.

The next case keeps standalone matmul and multi-level tiling in scope, but
excludes fusion. Existing AIO-9 tile and AIO-10 pipeline records are
check-only; the first generated tile must produce a verified executable
rewrite and measured baseline-versus-tiled result before it is reported as
an implemented optimization.

2026-10-26 design-review increment: add a bounded, shadow-only bubble-model
experiment inside remaining FP32 order 2. The objective is minimum GEMM
completion latency with auditable operand readiness and resource waits, not
maximum tile size or occupancy. See the
[algorithm proposal](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md)
and the Bubble Model Experiment queue below. The subsequent replay slice
implements synthetic timing and diagnostic ranking only; production VHO
options/selection and native performance evidence remain open.

## Adoption Decision

Open64 adopts the following design rules from the versioned design:

1. Tensor identity is preserved through a TensorEvolutionGraph rather than
   being replaced by unrelated layout, shard, tile, buffer, and fragment
   records.
2. Fusion, layout, placement, sharding, residency, tiling, fetch, pipeline,
   and runtime choices remain candidates until a complete legal plan has an
   explicit cost record.
3. Profitability belongs to an `OptimizationPlanIR`, not to an isolated local
   rewrite.
4. Common infrastructure owns reusable lineage, representation, memory,
   parallelization, candidate, plan, cost, guard, and telemetry concepts.
   AI domains own Attention, MoE, KV-cache, serving-state, and comparable
   semantic extensions.
5. The fixed architectural phase order is `AI-P0` through `AI-P11`. Individual
   phases are independently controllable for debugging, but their order is not
   user-reorderable.
6. Runtime selection chooses only among statically certified alternatives. It
   does not repair an illegal plan or invent a new schedule.
7. Existing Open64 services remain the implementation foundation: WHIRL and
   TensorDescriptorIR, VHO orchestration, PREOPT/WOPT, LNO dependence and
   locality analysis, target information, loop multiversioning, feedback, and
   mapped-image inspection.
8. Before a phase advances from placeholder to implementation, its architecture
   section must complete the reusable phase-rationale contract in Section 3.1
   of the design document: problem, inputs, candidate space, performance
   mechanism, legality, cost uncertainty, downstream consumers, ownership
   boundary, fallback, and review evidence. A generated candidate alone is not
   a performance claim.

## Optimization Scope Contract

The implementation follows Open64 compilation scope. A phase must not perform
interprocedural analysis merely because a managed table contains identities
from multiple PUs.

| Compiler mode | Maximum optimization scope | AI optimization responsibility |
| --- | --- | --- |
| `-O0` | Legality and direct lowering | Gatekeeper, mandatory shape refinement, descriptor verification, deterministic baseline implementation selection, and straightforward lowering only. No candidate search or optional transformation. |
| `-O1` | Basic block | Local canonicalization and low-risk local cleanup only. No whole-PU graph-plan selection. |
| `-O2` | One PU and its CFG | Semantic tensor/lifetime analysis, local candidate construction, semantic fusion, logical-layout alternatives, and other target-independent graph optimization whose legality and profitability are PU-local. |
| `-O3` | Canonical loops and one-PU target plan | Memory hierarchy, parallelization, sharding intent, communication derivation, residency, hierarchical tiling, async movement, kernel scheduling, guarded runtime variants, and telemetry instrumentation. |
| `-ipa` | Multiple PUs | Explicitly requested propagation or refinement of summaries and plans across PU boundaries. Cross-PU transformation remains limited, reviewed, and separately controlled. |

The backend continues to process one active PU at a time. Per-PU analysis,
REGION handling, symbol-table access, mutation, and verification belong to the
normal backend/VHO driver lifetime. A future IPA phase consumes compact,
versioned summaries; it does not retain local `WN *`, `ST *`, or `CODEREP *`
pointers after the owning PU is inactive.

## Pipeline Placement

The planned per-PU flow is:

```text
mapped binary Very High Level WHIRL
  -> DSL gatekeeper
  -> mandatory tensor shape refinement
  -> DSL canonicalization and PREOPT preparation
  -> VHO DSL AI optimization context
       AI-P0  semantic tensor analysis
       AI-P1  lifetime, reuse, and locality analysis
       AI-P2  high-level fusion candidates
       AI-P3  logical layout alternatives
       AI-P4  placement and sharding alternatives
       AI-P5  communication derivation
       AI-P6  local physical layout and residency
       AI-P7  multi-level tile-plan construction
       AI-P8  fetch, prefetch, and async movement plans
       AI-P9  physical plan and implementation selection
       AI-P10 certified runtime variants and guards
       AI-P11 telemetry bindings
  -> selected-plan verification
  -> VHO_DSL_Lower_Driver
  -> language VHO lowering
  -> existing backend optimization and code generation
```

At `-O0`, the optional AI optimization context is absent. The baseline
implementation selector consumes the same verified logical operator and emits
the direct executable form.

## Existing Open64 Integration

### VHO orchestration

Do not create an independent pass manager. Extend the fixed-order service in:

```text
osprey/be/vho/dsl_opt.h
osprey/be/vho/dsl_opt.cxx
```

The implementation should introduce one per-PU optimization context whose
lifetime spans the registered DSL stages. Existing stage enum values and
callback APIs remain compatibility wrappers. New phase controls are append-only
or are implemented as ordered subphases inside the reviewed owning stage; do
not renumber a published enum.

Proposed new orchestration files:

```text
osprey/be/vho/dsl_ai_opt.h
osprey/be/vho/dsl_ai_opt.cxx
osprey/be/vho/dsl_runtime_variant_opt.h
osprey/be/vho/dsl_runtime_variant_opt.cxx
```

`VHO_DSL_AI_Optimize_Program_Unit()` owns active-PU traversal, phase ordering,
analysis lifetime, candidate budgets, selected-plan application, diagnostics,
tracing, and final per-PU verification. It does not own common record schemas,
operator semantics, Python capture, or target code generation.

The directory boundary is semantic, not merely organizational:

- `common/com` defines IR records and stable IDs, constructs/interns records,
  performs structural verification, and provides generic access and printing;
- `be/vho` captures PU-local facts from WHIRL, discovers candidates, evaluates
  legality and cost, selects plans, and applies VHO transformations;
- `be/opt` owns decisions requiring CFG, SSA, CODEREP, value numbering, PRE,
  or other WOPT state;
- LNO owns canonical-loop decisions and IPA owns explicitly cross-PU work.

A common IR constructor must not decide which record should be created or
selected. The owning phase computes that content and calls the common creation
API. This boundary applies retroactively to AIO services as they are revised;
new milestones must satisfy it when introduced.

### Common semantic substrate

Proposed common services, introduced only as their milestones require them:

```text
osprey/common/com/dsl_tensor_evolution.h
osprey/common/com/dsl_tensor_evolution.cxx
osprey/common/com/dsl_opt_plan.h
osprey/common/com/dsl_opt_plan.cxx
osprey/common/com/dsl_memory_plan.h
osprey/common/com/dsl_memory_plan.cxx
osprey/common/com/dsl_runtime_variant.h
osprey/common/com/dsl_runtime_variant.cxx
```

These common services define and create IR only. They use stable IDs and
fixed-layout records where persistence is required, and may provide structural
verification, generic accessors, and logical printers. They must not harvest
PU facts, discover candidates, model profitability, select an optimization,
or apply a transformation. They must not expose VHO, WOPT, LNO, backend,
Python, CUDA, or vendor-library types in public common interfaces.

### Existing analysis and transformation engines

- Tensor shape facts come from the compiler-owned shape-refinement service.
- PREOPT canonicalizes WHIRL before candidate selection.
- WOPT supplies SSA, value numbering, copy propagation, PRE, CSE, and control
  flow optimization where DSL semantics have reviewed logical CODEREP support.
- LNO supplies dependence, canonical-loop, locality, cache, tiling, and
  guarded-multiversioning design and services. New tensor planning wraps or
  extends these facilities; it does not reproduce them independently.
- Target information supplies stable architectural capabilities. Measured
  tuning data remains separate from architectural legality.
- Existing profile and feedback infrastructure is the starting point for
  `AI-P11`.

## IR Families And Invariants

### TensorEvolutionGraph

The graph starts with the canonical TensorDescriptorIR value and creates
immutable evolution nodes for semantic, logical-layout, distributed, local,
tile, staged-buffer, register-fragment, and instruction-fragment forms.

Required invariants:

1. Every node has a stable graph-local ID and one semantic source tensor.
2. An edge identifies one reviewed transformation kind and whether it
   preserves logical semantics.
3. Descriptor/type changes create or reuse immutable canonical descriptors;
   they never mutate a sealed tensor `TY_IDX` in place.
4. A provisional node is not silently projected into executable WHIRL.
5. Selected nodes retain lineage to the source value and transformation plan.
6. Cycles, orphan nodes, incompatible descriptors, and unowned PU-local values
   fail verification.

### OptimizationCandidateIR

A candidate records one alternative and its dependencies. It is immutable
after publication into the per-PU candidate arena. Refinement creates a child
candidate rather than changing the meaning of a parent already referenced by
another plan.

Every candidate records:

- candidate kind and semantic region;
- source and result evolution-node IDs;
- legality state and diagnostic reason;
- required assumptions and runtime guards;
- parent candidate and refinement relation;
- estimated search cost and deterministic ordering key; and
- owning phase and source provenance.

### OptimizationPlanIR

A plan is the unit of legality and profitability. It binds compatible fusion,
layout, placement, sharding, communication, residency, tile, fetch, pipeline,
kernel, and runtime-guard choices.

No transformation is selected merely because it is legal. A selectable plan
must have:

1. complete legality evidence;
2. a `CandidateCostIR` with known confidence and provenance;
3. compatible descriptors and evolution edges;
4. resource use within the selected target profile;
5. a deterministic fallback, including the `-O0` baseline when applicable;
6. bounded compile-time search evidence; and
7. a post-selection verifier result.

### CandidateCostIR

Store raw and unhidden costs separately:

```text
T_total = T_compute
        + T_memory_unhidden
        + T_communication_unhidden
        + T_sync
        + T_launch
        + T_runtime_selection
```

Every term carries units, confidence, source, and target-profile identity.
Unknown cost is not zero. A plan with insufficient cost confidence remains
unselected or becomes an explicitly guarded measurement candidate.

## Binary WHIRL Strategy

The first milestones keep candidate graphs in compiler memory and print them
only in phase traces. This avoids freezing immature schemas into ELF sections.

A mapped-image section is proposed only when at least one of these requirements
is demonstrated:

1. a later compiler process must consume the plan;
2. human inspection requires selected-plan evidence after process exit;
3. runtime selection needs certified variant records; or
4. IPA needs a stable summary boundary.

Any new section requires, in the same staged change:

- fixed-width row contracts and explicit alignment;
- capability and revision handling;
- complete reader/writer/mapped-load validation;
- `ir_b2a -st -src` logical printing;
- current and previous-reader behavior;
- malformed-image and no-mutation tests; and
- a documented migration path.

No private C++ object, STL container, pointer, `WN *`, `ST *`, `CODEREP *`, or
backend-local ID may enter a binary WHIRL image.

## Option And Debug Contract

The driver passes the complete option set to every phase. AI optimization
selects its own options and silently ignores unrelated options.

The proposed controls are grouped under `-AI:` and introduced with their
owning milestones:

```text
-AI:enable=on|off
-AI:p0=on|off ... -AI:p11=on|off
-AI:check_only=on|off
-AI:max_candidates=<n>
-AI:max_plans=<n>
-AI:dump_candidates=on|off
-AI:dump_selected_plan=on|off
-AI:checkpoint_phase=<0..11>
```

The optimization level supplies defaults; an explicit phase option may disable
an activity for triage but cannot reorder phases or enable a scope broader than
the active compilation mode. In particular, a phase option cannot turn an
ordinary per-PU compilation into IPA.

## Milestone Queue

### AIO-0: Contract Inventory And Baseline

Status: completed on 2026-09-24. The authoritative inventory, ownership
matrix, gap analysis, and baseline findings are recorded in
`AI-COMPILER-OPTIMIZATION-AIO0-INVENTORY.md`.

Actions:

1. Reconfirm the v0.1 design against merged `develop`.
2. Inventory current TensorDescriptorIR, shape, layout, placement, sharding,
   REGION, call-ABI, effect, ownership, and source-provenance services.
3. Inventory reusable WOPT, LNO, target-info, multiversioning, feedback, and
   VHO controls.
4. Record which proposed IR fields already have authoritative representations.
5. Record gaps without adding duplicate records.
6. Capture `-O0`, `-O2`, and `-O3` baseline behavior for one fixed-shape
   `common.matmul` PU.

Acceptance:

- no compiler behavior change;
- checked-in inventory and ownership matrix;
- retained `.B`, `.T`, source, options, and phase traces.

PR boundary: documentation and baseline tests only.

### AIO-1: TensorEvolutionGraph Core

Status: completed on 2026-09-24. The runtime-only, per-PU graph contract and
certification are recorded in
`AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md`.

Actions:

1. Define stable in-memory node and edge IDs.
2. Create one semantic root for each live tensor value in a PU.
3. Add immutable descriptor references and transformation-kind enums.
4. Add deterministic insertion, lookup, enumeration, verification, and logical
   printing.
5. Seed `common.matmul` input/result roots from existing TensorDescriptorIR.
6. Prove repeated construction is deterministic and does not duplicate roots.

Acceptance:

- check-only; no WHIRL mutation;
- one-PU ownership and reset tests;
- malformed graph tests;
- no new `be.so` dependency;
- before/after WHIRL is byte-for-byte unchanged.

PR boundary: common graph service plus focused tests.

### AIO-2: Candidate, Plan, Legality, And Cost Core

Status: completed on 2026-09-24. The runtime-only candidate, legality, cost,
plan, budget, fallback, selection, and certification contract is recorded in
`AI-COMPILER-OPTIMIZATION-AIO2-PLAN-COST.md`.

Actions:

1. Define immutable candidate and plan IDs.
2. Define legality states, rejection reasons, cost terms, confidence, evidence
   source, and fallback relationships.
3. Add deterministic candidate ordering and hard search budgets.
4. Add a baseline matmul plan and one analysis-only alternative.
5. Reject plan selection when cost or legality evidence is incomplete.

Acceptance:

- deterministic plan dump across repeated runs;
- unknown-cost and budget-exhaustion negatives;
- no executable WHIRL change.

PR boundary: candidate/plan/cost substrate.

### AIO-3: AI-P0 Semantic Tensor Analysis

Status: completed on 2026-09-24. The runtime-only per-PU semantic fact,
consumer-role, completeness, ownership, reuse, domain-preservation, and
certification contract is recorded in
`AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md`.

Actions:

1. Classify tensor roles using logical operator contracts and structured
   TensorDescriptorIR traits.
2. Attach producer, consumer, dynamic-dimension, dtype, ownership, and reuse
   role facts to evolution roots.
3. Add common facts without erasing CNN, Transformer, FHE, or other domain
   semantics.
4. Diagnose missing mandatory semantic facts; do not infer from symbol names.

Acceptance:

- matmul, ResNet, and Llama check-only traces;
- domain-visible operator identities remain unchanged;
- source metadata does not affect tensor equivalence.

PR boundary: `AI-P0` analysis and tests.

### AIO-4: AI-P1 Lifetime, Reuse, And Locality

Status: completed on 2026-09-24. The runtime-only per-PU CFG snapshot,
lifetime, ordered-use, size, reuse-distance, working-set, access-pattern,
residency-seed, critical-path, cost-seed, WOPT-adapter, and certification
contract is recorded in
`AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md`.

Actions:

1. Compute producer, ordered consumers, last use, object size, critical-path
   position, and reuse distance inside one PU.
2. Represent unknown and symbolic sizes explicitly.
3. Reuse CFG dominance/postdominance and loop information.
4. Seed access-pattern, residency-benefit, and cost records.

Acceptance:

- branch, loop, REGION, and unknown-shape tests;
- no cross-PU assumptions without `-ipa`;
- conservative results for effects and unresolved aliases.

PR boundary: PU-local lifetime and locality analysis.

### AIO-5: AI-P2 Fusion Candidates

Status: completed on 2026-09-24. The runtime-only per-PU fusion-site,
member/boundary, legality, resource, independent site-plan, control, and
certification contract is recorded in
`AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md`. The complete phase
rationale and performance contract is retrofitted in Section 3.3 of the
architecture design.

PR scope characterization: the AIO-5 implementation is intentionally the
basic skeleton of the optimization process, even though the supporting code
and tests are substantial. It proves analysis consumption, candidate
construction, legality, cost, fallback, plan selection, controls,
determinism, and non-mutation. Its two explicit patterns are a vertical slice,
not the intended long-term method for enumerating every fusible operator
sequence.

Actions:

1. Generate candidates without rewriting WHIRL.
2. Start with `matmul+bias+activation` and one residual pattern.
3. Record eliminated materialization, live-range growth, representation
   constraints, effect legality, and alternative boundaries.
4. Preserve `OPR_REGION`, source provenance, and domain gatekeeper evidence.

Future scalable-fusion actions:

1. Define versioned operator fusibility traits for iteration-space class,
   operand indexing, broadcast and reduction behavior, descriptor/layout
   requirements, effects, ownership, multi-use handling, recomputation,
   numerical constraints, and REGION boundaries.
2. Add generic producer-consumer edge discovery over the logical DSL graph.
3. Grow deterministic fusion clusters under explicit candidate/plan budgets,
   with conservative handling of fan-out, diamonds, multiple results, and
   external uses.
4. Use explicit patterns only for semantic compounds, stronger legality, or
   provider/library alternatives; route their candidates through the same
   AIO-2 plan and cost services.
5. Consume AIO-6 logical-layout compatibility during cluster refinement and
   defer target-resource and physical-kernel fusion decisions to their owning
   later phases.
6. Add comparative tests showing that generic discovery finds ordinary
   elementwise and contraction-epilogue chains without a sequence-specific
   matcher, while semantic patterns remain domain-visible.

Initial scalable-fusion slice completed on 2026-09-24:

- versioned iteration-space/indexing fusibility traits and semantic-only flags;
- deterministic, bounded producer-consumer cluster growth;
- conservative stops for fan-out/diamonds, unsupported operators, REGION and
  effect boundaries, and unsupported result structure;
- AIO-6 layout compatibility consumption without forcing a representation;
- the same AIO-2 candidate, plan, cost, fallback, and selection services for
  semantic and generic candidates; and
- comparative, deterministic, PU-scope, negative, and byte-identical binary
  evidence.

Acceptance:

- legal, effectful, descriptor-mismatch, and resource-unknown cases;
- candidate generation and plan selection independently controlled, with
  transformation application explicitly rejected in this milestone;
- selected fusion has a complete plan-level cost record.

PR boundary: candidate-generation skeleton first; generic cluster expansion
and executable mutation land in later focused PRs.

### AIO-6: AI-P3 Logical Layout Alternatives

Status: implemented as a check-only, per-PU vertical slice. Runtime-only
`CommonLayoutIR` descriptors are immutable and uniqued; provisional evolution
nodes retain the canonical semantic `TY_IDX` and carry a representation ID.
No candidate tensor type or executable WHIRL mutation occurs in this stage.

Actions:

1. Define common logical layout descriptors and explicit layout-cast evolution
   edges.
2. Generate row-major, transposed, blocked, packed-head, or domain-provided
   alternatives without selecting local memory storage.
3. Model conversion cost and layout compatibility.
4. Keep physical target layout out of frontend attributes.
5. Materialize a refined canonical `TY_IDX` only when a later selected
   transformation is applied; use `TY_Intern_Tensor_Type()` to deduplicate it
   and never mutate the source tensor type.

Acceptance:

- immutable/uniqued descriptors;
- round-trip logical printing if persistence is introduced;
- no in-place tensor type mutation.

PR boundary: layout representation and check-only alternatives.

Implemented vertical slice:

- permuted and divisible blocked alternatives for intermediate tensors;
- exact same-block/pure/layout-consumer compatibility proof;
- conservative REGION/effect/unknown-shape behavior;
- exact read-plus-write conversion volume, an incomplete latency term until a
  target bandwidth model exists, and explicit baseline fallback through AIO-2;
- byte-identical before/after/repeat `.B` and `ir_b2a -st -src` evidence.

### AIO-7: AI-P4 And AI-P5 Placement, Sharding, And Communication

Actions:

1. Define placement, ownership, tile-range alias, and communication-epoch
   contracts.
2. Generate replication and sharding alternatives.
3. Derive communication intent from selected ownership, never as an unrelated
   frontend annotation.
4. Keep collectives logical until implementation selection.
5. Add IPA summaries only after the per-PU contracts are stable.

Acceptance:

- disjoint, replicated, reduced, migrated, and unknown ownership cases;
- communication derivation is deterministic;
- ordinary compilation remains PU-scoped;
- IPA tests are separate and explicitly invoke `-ipa`.

PR boundary: common ownership first, communication derivation second, IPA
summary third.

Implemented first vertical slice:

- runtime-only immutable replication, equal-axis sharding,
  partial-reduction, and migration alternatives;
- explicit per-device ranges, distributed alias/disjointness, and one
  visibility epoch per candidate site;
- deterministic logical AllGather, Scatter, AllReduce, and peer-copy intent
  derived from ownership rather than supplied by the frontend;
- TensorEvolutionGraph `place`/`shard` overlays and AIO-2 cost, fallback, and
  selection integration;
- pure/same-block/unique-ownership/contraction/divisibility legality with
  conservative effect, REGION, symbolic-shape, and incomplete-analysis states;
- independent generation, communication-derivation, selection, and prohibited
  application controls;
- PU-local scope and byte-identical before/after/repeat binary evidence.

Deferred beyond this slice:

- executable collective insertion and implementation/provider selection;
- target topology, bandwidth, overlap, and capacity-aware cost refinement;
- mapped-image publication;
- explicit `-ipa` summaries and cross-PU placement planning.

### AIO-8: AI-P6 Residency And Memory Hierarchy

Status: completed as a check-only, runtime-only, per-PU vertical slice on
2026-09-25. `MemoryHierarchyDescriptorIR` provides typed CPU-baseline,
Hopper, and Blackwell adapters; `CommonMemoryResidencyIR` creates capacity and
lifetime checked alternatives, TensorEvolutionGraph `local_physical` overlays,
and AIO-2 plans without rewriting executable or binary WHIRL.

Actions:

1. Define target-independent memory tiers, capacity, lifetime, promotion,
   demotion, spill, and eviction concepts.
2. Import target capabilities through target-info adapters.
3. Build local HBM/L2/shared/register and host/pinned alternatives.
4. Reject capacity-violating or lifetime-incompatible plans.

Acceptance:

- CPU baseline plus distinct Hopper and Blackwell profiles;
- no free-form target capability parsing in hot queries;
- exact resource reasons in rejected-plan traces.

PR boundary: common memory hierarchy, then target adapters.

Implemented first vertical slice:

- closed target-profile, memory-tier, and scope enums with typed hot queries;
- CPU baseline plus distinct Hopper and Blackwell HBM/L2/shared/register
  capabilities;
- system, pinned-host, HBM, L2, shared, and register alternatives where the
  selected profile supports them;
- allocation-rounded capacity checks and conservative lifetime/ownership
  legality;
- explicit promotion, demotion, spill, and eviction policies;
- TensorEvolutionGraph `local_physical` nodes/`local_layout` edges and AIO-2
  cost, fallback, and deterministic selection;
- exact resource/effect/ownership/lifetime/incomplete-analysis reasons;
- PU-local scope and byte-identical before/after/repeat binary evidence.

Deferred beyond this slice:

- driver/runtime capability import and live device discovery;
- simultaneous live-set capacity accounting and cache-contention modeling;
- occupancy-aware register/shared budgets and spill costs;
- executable allocation or transfer insertion;
- mapped-image publication and explicit IPA residency summaries.

### AIO-9: AI-P7 Hierarchical Tile Plans

Status: completed as a check-only, runtime-only, per-PU vertical slice on
2026-09-25. `CommonTilePlanIR` records the `P7.0` through `P7.11` hierarchy,
bounded Hopper/Blackwell families, typed resource evidence, AIO-2
cost/selection/fallback, and provisional TensorEvolutionGraph tile overlays
without rewriting executable or binary WHIRL.

Actions:

1. Implement `P7.0` through `P7.11` as candidate refinements, not one opaque
   tiling transform.
2. Reuse LNO dependence, locality, and blocking analysis where applicable.
3. Represent output, reduction, CTA, shared-memory, warp, thread, register, and
   instruction tile levels.
4. Model coalescing, vectorization, arithmetic intensity, occupancy, register
   pressure, shared-memory footprint, barriers, edge policy, and tuning knobs.
5. Generate deterministic rule-based and bounded search-based families.

Acceptance:

- the naive GEMM fixture produces reviewable `G0` through applicable `G*`
  stage evidence;
- Hopper and Blackwell generate materially different legal candidates;
- every selected candidate has cost and legality evidence;
- numerical results match the `-O0` reference under the active FP contract.

PR boundary: one tile level or tightly related group per PR.

Implemented first vertical slice:

- one unchanged baseline and bounded `cuda_64`, `cuda_128`,
  `cuda_128_coalesced`, and `blackwell_wide` candidate families;
- problem, CTA, warp, thread, register, vector, and instruction tile evidence;
- explicit G0-G11 stage records aligned with P7.0-P7.11;
- global-memory traffic, operation count, shared/register/thread/warp/barrier,
  buffering, edge-policy, and TMA-candidate evidence;
- typed Hopper/Blackwell resource queries through AIO-8;
- AIO-2 candidates, low-confidence relative costs, legality, deterministic
  selection, and baseline fallback;
- provisional runtime-only tile nodes/edges in TensorEvolutionGraph;
- active-PU scope, effect/ownership/resource rejection, numerical GEMM oracle,
  and byte-identical before/after/repeat binary evidence.

Deferred beyond this slice:

- LNO-backed executable loop blocking and dependence legality;
- complete coalescing, occupancy, bank-conflict, wave, and arithmetic-intensity
  models;
- separate A/B shared layouts, tensor-core fragments, and low-precision scale
  tiles;
- AIO-10 fetch/prefetch/TMA/cp.async pipeline realization;
- measured/autotuned cost evidence, runtime variants, and mapped publication;
- explicit IPA summaries and cross-PU tile planning.

Delayed SMEM cache-blocking follow-up (partially implemented): the synchronous
F32 path now retains compact A/B and padded-transposed A/compact B candidates
for `BK=32`, with fixed linear GMEM copy maps. `CommonTilePlanIR` carries the
A layout, stride, and low-confidence bank-conflict work; the selected layout
changes device-WHIRL A allocation, fill, and compute indices. The legacy CUDA
sidecar mirrors that layout, and v7 `COST_MODEL 3` plus `TILE_LAYOUT` rows make
the choice reviewable. A TMA request limits selection to the supported compact
  layout. This does not model B layout alternatives or actual hardware bank
  latency.

The first contiguous-F32 GMEM planning slice is executable for the
synchronous tile. `COST_MODEL 4` counts distinct 32-byte sector requests per
source-level warp instruction for A/B loads and C stores, including edge
predicates and all 4-byte-aligned base residues; `TILE_GMEM` records aligned
and independent worst-case totals. The cost gate also charges low-confidence
shared-A load issue work. A bounded `cuda_128_coalesced` alternative changes
the thread tile from 8x8 to 16x2 while preserving the same 128x128x32 CTA
tile and linear A/B copy map. The selected thread map lowers through the same
device-WHIRL path. On 128x128x32, plan 4 selects the coalesced map; on
128x128x128, plan 5 retains padded 8x8 because of the extra A-load issues.
Both pass host CTA parity. The compact A WHIRL allocation was corrected to
`BM*BK` elements when the new selection exposed its previous under-allocation.
`m2-whirl-kernel/run-028/` retains both `.cu` outputs, raw traces, reports,
device images, parity, and checksums; `run-024/` retains irregular-edge and
I4 rejection evidence. This closes **compiler-side GMEM access-map planning
for contiguous F32 synchronous GEMM**, not PTX transaction verification,
strided/repacked GMEM layouts, cache modeling, or measured profitability.

Compiler/runtime boundary: **M2-B1 target-buffer binding**. The selected
GMEM tile is not itself an allocation. For the first synchronous F32
standalone path, the generated-provider call already emitted in the host
WHIRL PU is the plan-bound invocation point. Use a target-neutral
`TargetBufferService` with validated import, allocate, transfer, and release
operations; the fixed-shape NVIDIA review adapter supplies CUDA callbacks.
Derive A/B/C sizes from the checked selected plan and invocation dimensions.
Host-resident inputs require one allocation and transfer per invocation;
borrowed device-resident inputs must not be recopied or freed. C may be
borrowed on device or runtime-allocated and copied back only for a host
result. Guard partial allocation, transfer failure, completion, and cleanup.
Retain host-mock operation traces and the original host/device WHIRL evidence.
The full standalone host-dispatch PU generator and service adapter remain
subsequent milestones; do not label the review adapter production runtime.

The first review-adapter slice is implemented: a C ABI and target-neutral
service core bind host or borrowed-device buffers, while the NVIDIA adapter
implements its callbacks with CUDA allocation, copy, pointer validation,
and release APIs. The host-mock receipt records `AHAHADRRR` for host A/B/C
(allocate/copy each input, allocate C, copy C home, release in reverse),
`III` for three borrowed device buffers (import only), and `AHR` for a
failed first transfer with cleanup (`AR` also covers a provider that fails
after returning an allocation; `ARR` shows a failed release can be retried).
The applied selected-plan package now
includes and hashes the service source and ABI. This verifies runtime
orchestration and packaging, not native CUDA compilation or the future
generated standalone host PU.
The verified compiler receipt is `m2-buffer-binding/compiler-run-001/`;
`run-005/` retains Linux host-mock operation evidence, and `package-007/`
retains the selected-plan native-review package with the buffer service.
The V1 review adapter's public call still accepts host pointers; borrowed
device buffers are proven at the service-core boundary, not yet through
that adapter's invocation ABI.
Applied synchronous compiler review artifacts also preserve the host CUDA
adapter as `source/matmul.host.cu`, its generic buffer service dependencies,
and a plan/source/trace hash join in `plans/host-buffer-review.json`. The
matching review log describes allocation and transfer source actions; it
does not claim those actions were executed on a GPU.
`m2-buffer-binding/compiler-run-002/` retains this combined host/device source
review with verified checksums.

M2-B1 follow-on gates:

1. [x] Preserve plan identity in the selected host-WHIRL provider call and
   selected device PU; retain `.t`, `.B`, `.T`, and `.cu` evidence.
2. [x] Exercise host staging, borrowed-device import, and partial-failure
   cleanup with a target-neutral mock provider.
3. [x] Package the generic service with the NVIDIA review adapter without
   adding CUDA dependencies to `be.so`.
4. [ ] Generate the standalone host dispatch PU and its buffer-requirement
   metadata from the selected execution plan, rather than relying on the
   fixed-shape review adapter for invocation policy.
5. [ ] Compile and run the NVIDIA provider with native CUDA; verify actual
   allocation/transfer behavior, completion, numerical result, and fallback.

The remaining joint-plan design retains
matching direct/cache-served, one-slot synchronous, and target-legal
multi-slot async A/B plans through AIO-10. Generate separate A/B copy and
consumer maps, padded SMEM ledgers, bank effects, and dynamic K-tile barrier
counts. `-Wb,-DSL:gemm_plan_search=baseline|joint` is now registered for
the bounded existing synchronous/TMA subset with whole-plan costing,
selected-WHIRL lowering and `.t`/derived reports. Default remains `baseline`.
Joint requires `-O3` and device-WHIRL output; direct/cache-served kernels and
`cp.async` remain follow-on work. See
`AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md` for the exact contract.
Use the white-paper candidate
`AI-COMPILER-WHITE-PAPER-CANDIDATE-DELAYED-GEMM-PLAN-SELECTION.md` for the
equal-budget evaluation and prior-art review, not as evidence of a speedup.

### AIO-10: AI-P8 Fetch, Prefetch, And Async Pipeline

Status: implemented as a check-only, runtime-only, per-PU vertical slice on
2026-09-25. `CommonFetchPlanIR` and `CommonPipelineIR` consume the selected
AIO-9 tile and record demand, vector, async-copy, and multidimensional-async
alternatives without rewriting executable or binary WHIRL.

Actions:

1. Consume P7 prefetch hints rather than rediscovering tiles.
2. Build demand-load, vector-load, async-copy, and TMA-like alternatives.
3. Model stage count, buffering, issue point, arrival barrier, wait point,
   boundary behavior, fallback, and unhidden latency.
4. Require alias, ownership, and communication-epoch safety.

Acceptance:

- single, double, and deeper buffering feasibility tests;
- unsafe early-fetch rejection;
- target capability fallback;
- traces expose raw and hidden movement cost.

PR boundary: generic fetch/pipeline IR before target-specific movement engines.

Implemented first vertical slice:

- exact selected-tile consumption with two typed operand fetch records;
- one-, two-, and three-stage buffering alternatives;
- issue, arrival-barrier, wait, edge-policy, and fallback evidence;
- typed CPU, Hopper, and Blackwell movement capability profiles;
- raw, hidden, and unhidden low-confidence relative movement cost;
- AIO-2 candidate, plan, deterministic selection, and fallback integration;
- provisional staged-buffer TensorEvolutionGraph overlays;
- PU-local lifetime, unique-ownership, effect, communication, target,
  buffering, and unsafe-distance checks;
- byte-identical before/after/repeat binary and `ir_b2a -st -src` evidence.

Deferred beyond this slice:

- executable OPR_PREFETCH, async-copy, TMA, barrier, and wait emission;
- LNO-backed issue scheduling and dependence proof;
- target instruction, address-space, dtype, swizzle, bank, occupancy, and
  measured-latency modeling;
- host/device, remote, KV-cache, and communication pipelines;
- AIO-11 complete physical-plan selection and atomic application;
- explicit IPA summaries and mapped publication.

### AIO-11: AI-P9 Physical Plan And Implementation Selection

Status: implemented first as a selector-only, runtime-only, per-PU vertical
slice on 2026-09-25. On 2026-09-26, the first reviewed application family
connected selected `common.matmul.v1` cuBLASLt plans to normal VHO DSL
lowering. `CommonPhysicalPlanIR` remains in-memory analysis state; the source
binary WHIRL remains unchanged until that lowering boundary.

Actions:

1. Compare complete legal plans, not isolated transformations.
2. Select generated loops/kernels, runtime baseline, cuBLAS/cuDNN, Triton,
   existing PTX, or another provider only through reviewed capability records.
3. Preserve a deterministic fallback.
4. Apply the selected plan atomically and verify the resulting WHIRL before
   DSL lowering.

Acceptance:

- provider mismatch and unavailable-provider negatives;
- no early runtime-call placeholders in source `.B` files;
- selected-plan identity and fallback visible in retained traces;
- `-O0` remains executable without optional search.

PR boundary: selector first, then one provider/lowering family at a time.

Implemented first vertical slice:

- exact consumption of selected AIO-9 tile and AIO-10 fetch/pipeline plans;
- reviewed, versioned capability records for direct Open64, generated Open64,
  cuBLASLt, cuDNN, Triton, and existing PTX alternatives;
- complete AIO-2 candidate, cost, legality, fallback, and deterministic
  selection evidence for every physical implementation;
- explicit provider mismatch and provider-unavailable rejection;
- mandatory direct baseline and deterministic fallback;
- `-O0` direct-baseline-only behavior;
- PU-local lifetime, immutable tensor/operator identity, and byte-identical
  before/after/repeat binary and `ir_b2a -st -src` evidence;
- independent generation, selection, and fail-closed application controls.

Implemented first application family:

- complete plan/capability/owner/opcode/fallback preflight before WN mutation;
- copied-tree application with PU adoption only after canonical verification;
- append-only `__open64_dsl_matmul_physical_v1` runtime-provider ABI;
- fixed 48-byte read-only physical-plan descriptor with deterministic identity;
- selected cuBLASLt dispatch with runtime-owned CUDA resources and direct
  fallback policy;
- unavailable-provider lowering through the unchanged direct matmul ABI;
- unsupported generated-kernel rejection before tree mutation;
- post-lowering provider-site accounting and canonical-WHIRL verification;
- deterministic provider and fallback `.B/.T` output plus retained source-to-
  G14 diff;
- no cuBLASLt, CUDA, JsonCpp, or frontend-builder linkage added to `be.so`.

Deferred beyond this slice:

- executable generated-loop/kernel construction and atomic WN application;
- executable CUDA runtime adapter and measured cuBLASLt execution;
- cuDNN, Triton, existing-PTX, and generated-kernel lowering families;
- concrete runtime handle, stream, workspace, and error/fallback execution;
- exact physical layout, alignment, address-space, target-instruction, and
  measured-performance validation;
- AIO-12 runtime variants, mapped publication, and explicit IPA summaries.

### AIO-12: AI-P10 Runtime Variants

Status: the first generic runtime-only slice is implemented for the reviewed
rank-2 F4 `common.matmul.v1` physical site. It creates one unconditional direct
variant and one certified cuBLASLt variant guarded by `kid0`/`kid1` runtime
buffer alignment of at least 16 bytes. The two guard checks contribute an
explicit cost of 4 to AIO-2's `runtime_selection` term. Guard-true selects the
fast implementation; guard-false selects the direct fallback. The analysis is
PU-local and does not change executable or binary WHIRL. Common/com owns only
the RuntimeVariantIR records, constructor, structural verifier, accessors, and
generic printer. `be/vho/dsl_runtime_variant_opt.{h,cxx}` owns physical-plan
fact capture, target checks, candidate/cost construction, selection, semantic
verification, and runtime guard evaluation.

Actions:

1. Define certified variants, guards, fallback chains, and guard cost.
2. Reuse Open64 guarded loop-multiversioning principles.
3. Start with shape and alignment guards; add runtime workload-state guards
   only when their producer and ABI are reviewed.
4. Prohibit an unguarded assumption from leaking into a fallback plan.

Acceptance:

- fast and fallback variants both pass independent correctness tests;
- guard false always reaches the conservative variant;
- guard overhead appears in plan cost;
- persisted variants, if introduced, reopen through `ir_b2a -st -src`.

PR boundary: generic guard/variant contract before AI serving-state extensions.

### AIO-13: AI-P11 Telemetry And Feedback

Status: the first PU-local, runtime-only G16 slice is implemented for the
certified AIO-12 rank-2 F4 `common.matmul.v1` variant family. Common owns flat
immutable TelemetryProfileIR records and structural services. VHO owns the
freshness join, interpretation, measured-cost construction, and future-plan
recommendation. The slice reuses Open64's `PROFILE_PHASE_BEFORE_VHO` identity
without changing the existing `.fb` format. It is check-only and leaves source
legality, source plan selection, executable WHIRL, and binary WHIRL unchanged.

Actions:

1. Define measured latency, occupancy, memory traffic, communication, overlap,
   cache behavior, launch overhead, guard hit rate, and selected-variant facts.
2. Reuse Open64 instrumentation and feedback transport where compatible.
3. Keep measured evidence separate from architectural legality.
4. Update future cost estimates without mutating the meaning of a previously
   certified plan.

Acceptance:

- schema/version and stale-profile tests;
- deterministic no-profile behavior;
- feedback can alter profitability but never legality;
- profile-guided and non-profile builds remain independently reproducible.

PR boundary: instrumentation, profile transport, then cost-model consumption.

Implemented first-slice boundary:

- complete latency, occupancy, traffic, communication, overlap, cache, launch,
  guard-hit, and selected-variant records;
- schema, generation, target, owner, plan, and exact variant-identity checks;
- deterministic no-profile behavior;
- separate telemetry-backed OptimizationPlanIR cost view using the standard
  VHO selector;
- feedback may change future profitability but cannot change legality;
- repeated G16 traces and byte-identical `.B`/`ir_b2a -st -src` evidence.

Deferred transport work remains a separate versioned milestone because the
existing Open64 `.fb` record layout has no accelerator telemetry family.

## First Vertical Slice

The first end-to-end slice stops after `AIO-2` and proves the architecture
without transforming code:

```text
fixed-shape common.matmul in one PU
  -> verified TensorDescriptorIR seed facts
  -> three TensorEvolutionGraph roots: kid0, kid1, result
  -> baseline candidate
  -> one provisional tiled alternative
  -> legality record
  -> cost record with explicitly unknown target terms
  -> baseline selected because the alternative is incomplete
  -> unchanged WHIRL
  -> deterministic candidate/plan trace
```

This slice validates identity, ownership, deterministic planning, unknown-cost
handling, and review tooling before fusion, target data, or mutation increases
the blast radius.

## Test And Certification Matrix

Every mutating milestone must include:

1. positive legality and profitability cases;
2. malformed descriptor, effect, ownership, and resource negatives;
3. option-off behavior equal to the prior baseline;
4. repeated-run determinism;
5. failure without partial in-memory or published artifact state;
6. current reader/writer and previous-reader behavior when images change;
7. `be.so`, `lw_inline`, and affected IR-tool link closure;
8. no `DSL_Builder_*` dependency in backend shared consumers;
9. no new library dependency without approval;
10. before/after `.B` and `ir_b2a -st -src` traces for IR transformations;
11. numerical comparison with the `-O0` result; and
12. performance evidence only after correctness and semantic gates pass.

Primary model ladder:

```text
common.matmul micro fixture
  -> naive GEMM fixture
  -> ResNet convolution/fusion cases
  -> Llama prefill
  -> Llama decode and KV-cache cases
  -> distributed and runtime-adaptive cases
```

## Trace, Reports, And Review Artifacts

The compiler uses the existing WOPT trace convention: after confirming the
bit is unused, reserve `AI_OPT_TRACE_FLAG = 0x00800000` under `TP_WOPT2`,
query `Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)`, and write compact positional
`AI_OPT` records to `TFile`. The compiler switch is
`-Wb,-ttOP2:0x00800000`; `-trOP2` separately requests IR dumps. The
development option `-Wb,-DSL:ai_opt_trace=ON` now defaults ON and activates
the same bit when `cuda_tile_output` runs; `=OFF` suppresses AI decision rows.
The matmul artifact runner therefore needs no explicit `-ttOP2` switch.
AIO-9 now emits v3 tile candidate/selection rows when the choice is made,
appending the Python source line to each candidate record and
`selection_reason=lowest_complete_relative_cost` to the selection record.
The report parser also accepts archived v1/v2 rows, reporting an unrecorded
selection reason rather than inferring one. The
CUDA-source path emits its source/WHIRL action after verified lowering. The
full phase-wide trace/report gate remains open. The raw `.t` file is retained
unchanged. The planned general `ai-opt-report` parses its
versioned `AI_OPT` records and fails visibly on an unknown schema or broken
candidate/plan lineage.

From `m1-trace-report` onward, each accepted receipt derives the following
files from the same raw trace:

```text
traces/compile.t
reports/optimization.log
reports/optimization-summary.log
reports/phases/semantic.log
reports/phases/locality.log
reports/phases/fusion.log
reports/phases/layout.log
reports/phases/placement.log
reports/phases/communication.log
reports/phases/residency.log
reports/phases/tile.log
reports/phases/fetch-pipeline.log
reports/phases/physical-plan.log
reports/phases/runtime-variant.log
reports/phases/feedback.log
```

Design Section 9.7 fixes the phase-to-file mapping and exact first-line
descriptions. Every `.log` begins `# What it summarizes: ...`, before any
metadata. `optimization.log` includes the complete content of every phase
log in P0-P11 order, including rejected and non-invoked phases;
`optimization-summary.log` provides one concise row per phase. Candidate,
cost, and rewrite evidence stays in the owning phase log instead of being
duplicated into separate cross-cutting logs.

The driver emits phase coverage even when a phase is not invoked. The
summary distinguishes `disabled_by_option`, `disabled_by_policy`,
`not_applicable`, `no_eligible_region`, `analyzed_no_rewrite`, `rewritten`,
and `failed`, with reason, region/candidate counts, selected plan, rewrite
count, and next blocker. Missing coverage is a trace error, not an inferred
status. The analyzer also rejects duplicate terminal decisions, missing
baseline or cost, unknown units, mismatched IDs, and SELECT/REWRITE or
verification inconsistency. AIO-9 currently emits low-confidence relative
scores, not cycles; keep them separate from measured runtime.

Mirror the FHE effort's durable receipt convention under an AI-specific namespace:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/
  matmul-f32-v1/<target-id>/<milestone>/
    sha256-<full-primary-artifact-digest>/
      receipt.json
      SHA256SUMS
      source/ binary/ traces/ plans/ reports/ measurements/ logs/
    current -> sha256-...
```

The first milestones are `m0-baseline`, `m1-trace-report`,
`m2-multilevel-tiling`, and `m3-prefetch-pipeline`. The pre-trace `m0`
receipt retains source, input `.B`, separate-process `ir_b2a -st -src` `.T`,
correctness and timing evidence, and an explicit AI-trace-unavailable status.
From `m1` onward retain raw `.t`, every derived log, plan and cost evidence,
and transformed `.B`/`.T` where applicable. Every receipt retains exact
commands, target/driver identity, correctness results, raw timing samples
when available, and hashes. Each
receipt identifies its predecessor so baseline, trace, rewrite, and pipeline
evidence remain linked. Produce in fresh staging, complete verification,
hash every file, publish to an immutable digest directory without replacing
existing content, reopen it, then update `INDEX.md`, `index.json`, and
`current`. Never delete, overwrite, or prune accepted receipts without
explicit path-specific user approval. Existing `artifacts/ai_optimization/`
output remains legacy evidence and must not be moved or cleaned by this work.

For the staged GEMM ladder, continue using the reviewed `G*` suffix contract
from `CUDA-HOPPER-BLACKWELL-MATMUL-OPTIMIZATION-PLAN.md`. General AI phase
checkpoints use descriptive `AI-P*` names and must not overwrite the source
artifact.

## Pull-Request Strategy

1. Keep architecture, representation, analysis, selection, and transformation
   changes in reviewable commits even when one coordinated PR is appropriate.
2. Land common read-only services before backend consumers.
3. Land check-only analysis before mutation.
4. Land generic common contracts before domain or target extensions.
5. Land target-independent plans before CUDA, library, or runtime bindings.
6. Do not combine a binary image revision with an unrelated optimization.
7. Cross-reference the design, this plan, focused evidence, and downstream
   dependent PRs.

## Risks And Controls

| Risk | Required control |
| --- | --- |
| Candidate explosion | Hard per-phase and per-PU budgets, deterministic pruning, explicit budget-exhausted result. |
| Cost-model error | Confidence and provenance on every term, conservative fallback, measurement lanes. |
| Premature schema freeze | In-memory/check-only stages before mapped-image publication. |
| Duplicate Open64 infrastructure | Design review naming the WOPT/LNO/VHO/target-info service reused or the demonstrated incompatibility. |
| Scope leakage | Active-PU ownership checks; IPA-only summaries for cross-PU work. |
| Lost domain semantics | Domain gatekeeper runs before promotion or implementation selection. |
| Unsafe runtime adaptation | Only certified variants; explicit guards and fallback chains. |
| Nonreproducible autotuning | Pinned target, inputs, search space, seed, measurements, and tuning database hash. |
| Backend dependency growth | Rebuild and symbol-audit `be.so`, `lw_inline`, and standalone consumers. |
| Unreviewable transformation | Mandatory before/after `.B` and `.T` evidence with source cross-reference. |

## Active Queue

1. [x] Execute `AIO-0`: inventory existing representations and services.
2. [x] Publish the local `common.matmul` baseline artifact family for review.
3. [x] Review and implement the exact TensorEvolutionGraph in-memory schema
   for `AIO-1`.
4. [x] Certify the check-only `common.matmul` vertical slice with byte-identical
   before/after binary WHIRL and `ir_b2a -st -src` traces.
5. [x] Review candidate/plan/cost IDs and unknown-cost semantics for `AIO-2`.
6. [x] Execute `AIO-2`: add the first baseline and analysis-only alternative
   without changing executable WHIRL.
7. [x] Execute `AIO-3`: classify per-PU semantic tensor roles from logical DSL
   contracts and TensorDescriptorIR without erasing domain semantics.
8. [x] Execute `AIO-4`: compute conservative per-PU lifetime, reuse-distance,
   object-size, critical-path, and locality facts.
9. [x] Execute `AIO-5`: establish the check-only fusion candidate, legality,
   cost, fallback, and selection skeleton with two reviewed vertical-slice
   patterns.
10. [x] Execute `AIO-6`: introduce immutable logical-layout alternatives and
    layout-compatibility evidence.
11. [x] Generalize AIO-5 with operator fusibility traits, generic
    producer-consumer edge discovery, and deterministic cluster growth.
12. [x] Execute the first `AIO-7` vertical slice: define common distributed
    ownership alternatives and derive logical communication intent without
    changing executable or binary WHIRL.
13. [x] Execute the first `AIO-8` vertical slice: define typed CPU/Hopper/
    Blackwell memory hierarchies and capacity/lifetime checked residency
    alternatives without allocating storage or changing binary WHIRL.
14. [x] Execute the first `AIO-9` vertical slice: construct explicit G0-G11
    hierarchical tile-plan candidates with distinct Hopper/Blackwell families,
    AIO-2 cost/selection evidence, and no executable or binary WHIRL rewrite.
15. [x] Execute the first `AIO-10` vertical slice: construct demand, vector,
    asynchronous-copy, and multidimensional-async movement/pipeline plans from
    the selected tile without executable or binary WHIRL rewrite.
16. [x] Execute the selector-first `AIO-11` vertical slice: compare complete
    direct, generated, and reviewed-provider physical plans with deterministic
    fallback and no executable or binary WHIRL rewrite.
17. [x] Implement one reviewed AIO-11 provider or generated-kernel lowering
    family with atomic WHIRL application and retained before/after evidence.
    The first family lowers selected rank-2 F4 `common.matmul.v1` to the
    generic cuBLASLt provider ABI, preserves direct fallback, and retains G14
    provider/fallback/rejection evidence without linking CUDA into `be.so`.
18. [x] Execute the first AIO-12 runtime-variant slice with reviewed shape or
    alignment guards, a conservative direct fallback, explicit guard cost,
    and retained true/false-path evidence. The first slice uses two runtime
    operand-alignment guards around the selected cuBLASLt matmul plan, retains
    the direct implementation as an unconditional fallback, accounts for the
    guard cost through AIO-2, and leaves binary WHIRL byte-identical.
19. [x] Audit AIO-0 through AIO-13 against the common/phase ownership rule.
    AIO-0 conforms, AIO-12 conforms after its common/VHO split, AIO-13
    conforms through its common TelemetryProfileIR and PU-local VHO consumer,
    and AIO-1 through AIO-11 require staged ownership cleanup as recorded in
    `AI-COMPILER-OPTIMIZATION-PHASE-OWNERSHIP-AUDIT.md`.
20. [x] Ownership M1: split AIO-11 physical implementation planning. Keep
    provider capability and physical-plan IR in common; move candidate
    discovery, capability/legality checks, costing, and selection into VHO.
21. [x] Ownership M2: move AIO-9 and AIO-10 tile-family and fetch/pipeline
    decisions into VHO while preserving target-description and plan IR in
    common and reserving canonical-loop realization for LNO. Common now owns
    policy-free bulk-created tile/fetch/pipeline records and structural
    services; VHO owns active-PU capture, legality, costs, selection, and
    semantic verification. AIO-10 and AIO-11 consume the neutral IR handles.
22. [x] Ownership M3: move AIO-5 through AIO-8 candidate discovery, legality,
    cost, and selection into VHO while preserving record schemas and traces.
    Common now owns policy-free `FusionPlanIR`, `LogicalLayoutIR`,
    `DistributedPlanIR`, and `ResidencyPlanIR` containers plus construction,
    structural verification, access, and generic printing. VHO owns active-PU
    fact capture, candidate formation, semantic/target/resource legality,
    costs, selection, and semantic verification. AIO-9 through AIO-12 consume
    the neutral common IR handles, preserving the phase ownership boundary.
23. [x] Ownership M4: move AIO-1, AIO-3, and AIO-4 PU fact capture into VHO;
    keep graph/fact/snapshot/locality records and structural services common.
    VHO now owns active-PU semantic-root scanning, semantic tensor fact/use
    derivation, and tensor lifetime/locality derivation. Common retains graph,
    fact, control-snapshot, and locality records with construction,
    verification, access, and printing. WOPT continues to own CFG-derived
    control-snapshot capture through its existing adapter.
24. [x] Ownership M5: relocate AIO-2 selection policy to VHO after all
    production callers have moved. Common retains candidate, cost, plan,
    membership, and recorded-selection IR with policy-free construction,
    structural verification, access, and printing. VHO owns legal-plan cost
    comparison, target filtering, deterministic tie-breaking, and selection.
    Linked tests use the VHO selector and certify unchanged selected records,
    runtime traces, binary WHIRL, mapped-image behavior, runtime ABI, and
    backend shared-library dependency closure.
25. [x] Execute the first AIO-13 telemetry-feedback slice. Define complete
    immutable accelerator telemetry records in common, perform exact PU/target/
    generation/plan/variant freshness joins in VHO, construct a separate
    measured OptimizationPlanIR cost view, and recommend a future variant
    without changing legality, the current selection, executable WHIRL, or
    binary WHIRL. G16 certification covers deterministic profile/no-profile
    behavior, malformed and stale evidence, repeated traces, byte-identical
    `.B` and `ir_b2a -st -src` output, and backend link closure. Runtime
    instrumentation and reviewed durable profile transport remain a later
    versioned milestone.

## Current Matmul Execution Queue

These gates are specific to the first standalone rank-2 F32 matmul. Fusion,
distributed placement, and runtime variants are out of scope; their
phase-status rows still appear in the trace-derived summary.

The immediate development gate is **compiler-side CUDA algorithm review**,
separate from native M0/M2/M3 acceptance. For each candidate implementation,
publish the selected device `.B`, `ir_b2a` view, `whirl2c -cuda` `.cu`, raw
`.t`, `tile.log`, `optimization-summary.log`, host-CTA parity, and checksums.
The WHIRL-derived `.cu` is the manual-review target; the legacy `.cu` is a
comparison only. Review A/B addresses and layout, cooperative copy map,
SMEM allocation, edge zero-fill, uniform barriers, K-loop and accumulator,
and C stores against the selected plan before moving to GMEM alternatives.
This gate needs no GPU. Native CUDA compilation, PTX/resource inspection,
GPU correctness, and calibrated speedup remain separate later gates.

1. [ ] Certify the direct fixed-shape baseline and publish `m0-baseline`
   with source, `.B`/`.T`, correctness, measured runtime, commands, target
   identity, and checksums. The compiled-PU `emulated-smoke-001` passes
   correctness and hash checks, and `native-ready-001` preserves that exact
   executable and WHIRL for native rerun. Neither has native target timing yet.
2. [ ] Implement the WOPT-convention AI trace bit, explicit phase coverage,
   and `ai-opt-report`. Publish `m1-trace-report` with the raw `.t`, complete
   and summary logs, all P0-P11 phase logs, a selected candidate, a skipped
   candidate, and malformed/incomplete-trace negative tests. The first
   standalone-matmul implementation and schema-v7 digest-named checkpoint now
   satisfy the trace/report checks, including P0 semantic and P1 locality
   facts. The 2026-10-21 checksum audit and byte-identical replay of both
   option-on and option-off reports pass; acceptance remains open until a native
   `m0-baseline` receipt can be linked as predecessor.
3. [ ] Reuse AIO-9 G0-G11 plans. Cost the unchanged baseline and at least
   two M/N/K tile families with explicit units, target resources, confidence,
   and rejection reasons. Keep stable plan IDs through selection and rewrite.
   The current trace costs baseline, `cuda_64`, and `cuda_128`, selects plan 3,
   and checks its emitted CUDA geometry against the trace. The added
   `TILE_GRID` rows expose four CTAs for `cuda_64` versus one for selected
   `cuda_128` on the 128x128 fixture; the cost gate does not yet price grid
   underfill. Native paired timings must calibrate this before claiming a
   throughput-optimal choice. Stable identity reaches the opt-in generated
   provider WHIRL rewrite and compiled-PU mock launch; native GPU execution
   remains unverified.
4. [ ] Implement one generated matmul tile path, atomically apply the
   cost-selected plan, verify before/after IR, compare numerically with the
   direct baseline, and measure on the same target. Publish
   `m2-multilevel-tiling` only with an actual tiled rewrite; cuBLASLt
   dispatch alone does not satisfy this gate. M2a now emits a versioned
   `m2-selected-tile.json` handoff from the real compiler trace, CUDA source,
   and input/lowered WHIRL hashes (`compiler-source/run-018/`); the remote
   validation package carries it (`compiler-gpu-validation/run-006/`). The
   opt-in M2b compiler path now atomically rewrites the selected synchronous
   F32 matmul to a versioned generated-provider call at `-O3`; its plan ID,
   tile geometry, and resource contract survive in a read-only 64-byte WHIRL
   descriptor. `m2-applied/run-005/` retains the selected `.cu`, raw trace,
   option-off/on post-lowering WHIRL dumps, object, reports, and checksums;
   `m2-whirl-g14/run-007/` retains matching before/after binary WHIRL,
   independent `ir_b2a -st -src` views, their full diff, ABI size, invalid
   resource rejection, and rollback.
   The fixed CUDA kernel now has a fixed-shape review adapter that binds the
   generated symbol to the exact compiler-emitted source, checks plan identity
   and device/kernel resources, and uses the direct CPU fallback only when the
   plan permits it. Host contract tests and a compiled-PU/mock-CUDA link smoke
   pass. `m2-applied/run-007/` retains a fresh isolated-backend rewrite with
   plan ID macros and compiler-emitted `TILE_GRID` rows;
   `m2-generated-runtime/run-009/` packages its rewritten object, selected
   `.cu`, runtime adapter, raw trace, checksums, SSH transport, and a
   validation-only untiled CUDA control. The native runner will retain both
   full-PU elapsed time and paired CUDA-event kernel time on nonuniform inputs;
   the control is not an Open64 M0 baseline. Host-only integrity and result
   parsing tests, the fresh artifact's SHA256 audit, and its compiled-PU mock
   launch pass; no remote GPU run has occurred.
   The production tuning path requires an opt-in `openpy` driver performance
   harness (provisional `-perf-harness`, default OFF), not a benchmark added
   to every standalone executable. Its selected and independently certified
   baseline implementations must share target, workload, numerical contract,
   residency, and measurement scope. Correctness is a separate gate; the
   harness reports paired kernel-only and end-to-end timings, the baseline
   identity, selected plan, speedup, and model error only when a comparable
   GPU run exists. Without an accepted baseline or GPU, retain diagnostics
   and raw evidence but make no speedup claim. Preserve the raw `.t`, phase
   logs, device images, launch configuration, measurements, and checksums.
   The opt-in `whirl2c -cuda` option now recognizes a target-neutral
   `ST_ATTR_DEVICE_SYMBOL` contract: kernel/device function and
   global/shared/constant data are distinct, serialized symbol attributes.
   The translator still accepts legacy NVISA memory-space bits when the
   attribute is absent. Cloning, IPA merge, verification, and symbol printing
   recognize the new kind; IPA rejects conflicting merged values. This lets
   the x86-64 host-target build carry device intent without interpreting its
   TLS bitfield as NVISA memory space. The added native contract checks
   in-memory access, and the dedicated mapped-image test passes with rebuilt
   isolated producer and `ir_b2a` binaries, preserving all three attribute
   values across `.B`/`.T`. Older `ir_b2a` prints the new kind as unknown, so
   mixed-version toolchains are not supported.
   At this earlier checkpoint, no selected GEMM device PU had been
   materialized, and the existing `.cu` was legacy-template output. The
   subsequent `m2-whirl-kernel/run-008/` checkpoint below attaches these
   attributes to a selected device PU and its A/B staging symbols and checks
   `whirl2c -cuda` against the legacy source under the same selected plan.
   The backend's `-cuda` path now filters a mixed `.B` to device-attributed
   PUs, suppresses ordinary host global declarations, and fails if none were
   emitted. The mixed host/device mapped-image contract passes as
   `m2-device-symbol-contract/run-002/`. The expanded `run-010/` contract
   contains a nonempty high-level WHIRL `DO_LOOP` and shared-memory store.
   Isolated rebuilt backend and `whirl2c -cuda` binaries emit a device-only
   `.cu` with C-linkage kernel naming; the host PU is absent, and the source
   passes a host C++ syntax check
   with CUDA qualifiers stubbed; a separate host-only image is rejected.
   CUDA mode supplies primitive scalar aliases
   without Open64's host `whirl2c.h`. This fixture is not the selected GEMM
   body, and the syntax check is not NVCC validation. GPU-specific globals
   beyond parameter pointers and local
   staging will need declaration coverage before relying on this mode for them.
   This test-handle adapter is not the production runtime provider. M2c still
   needs CUDA compilation, numerical equivalence, resource inspection, and
   same-target timing. No current artifact is an M2 acceptance receipt.
   The opt-in `-Wb,-DSL:cuda_whirl_output=<new-file.B>` now writes a separate
   cost-selected device PU for the exact-divisible synchronous F32 GEMM case.
   Its canonical `DO_LOOP` nest contains A/B global-to-shared loads, CTA
   barriers, register accumulation, and output stores. A fresh isolated
   backend compile and `whirl2c -cuda` translation completed in
   `m2-whirl-kernel/run-008/`; the legacy source remains separately preserved.
   A 256-thread CPU CTA simulation of both emitted kernels gives zero error
   against reference GEMM and each other for the selected 128x128x128 tile.
   `run-009/` verifies the same device PU can be selected and translated with
   `cuda_legacy_emit=OFF`, no legacy source file, and an accurate
   `device_whirl_sidecar` trace/report. This closes synchronous
   source-behavior convergence for that fixture, not M2 acceptance.
   `m2-whirl-kernel/run-010/` adds a single compiler-artifact run with the
   selected host rewrite, trace-linked device `.B`, separate-process `.T`,
   `whirl2c -cuda` source, legacy source, zero-error host CTA parity, and a
   SHA256-bound receipt. The traced device-image path is relative to `binary/`
   and remains valid after publication. This establishes selected-plan/image/
   source provenance for the fixed synchronous fixture, not native CUDA
   code generation or GPU performance. In `run-011/`, VHO stages the
   selected tile and the driver publishes the device image after host-PU
   traversal. The `WHIRL_KERNEL` row follows `TILE_ACTION`; a two-PU
   negative fixture rejects a second selection before image creation.
   `run-012/` moves ownership of the constructed `PU_Info`, mapped-image
   writing, and publication trace to the driver. VHO still selects the tile
   and constructs the device PU under the driver's program-boundary pool.
   Applied and selection-only paths round-trip; host CTA parity remains zero.
   The image remains separate from the main PU tree. `run-013/` replaces the
   one-kernel compiler-side limit for exact-divisible synchronous F32 matmul
   PUs. VHO retains ordered selected plans; the driver publishes one device
   image with deterministic names and `DEVICE_BIND` trace rows. Host PU zero
   keeps the 64-byte v1 plan and fixed entry; later PUs carry a 72-byte v2
   plan with a device-entry ordinal and call the v2 provider ABI. A two-PU
   fixture verifies mapped-image round-trip, both `whirl2c -cuda` entries,
   host-object descriptor sizes and relocations, linked host-only mock
   execution, and per-PU plus aggregate `.t` reports. The package/runtime
   loader still needs to resolve v2 ordinals to device entries; the mock is
   not a production CUDA launch provider. `run-014/` extends the selected
   synchronous device WHIRL to non-divisible M/N/K tiles. It conditionally
   zero-fills only A/B loads whose axis can overrun, guards only potentially
   invalid C stores, and keeps both CTA barriers uniform. An I4 geometry
   gate rejects padded or flattened indices that can overflow. `EDGE_PLAN`
   records those decisions in the raw `.t`, `tile.log`, and summary. The
   130x133x135 all-tail fixture passes source-only and applied host-PU
   paths; 128x128x135 proves the K-only path omits the C guard. Both pass
   mapped-image reopen, `whirl2c -cuda`, and multi-CTA host simulation with
   zero error against reference and legacy source. A 50000x50000x128
   negative fixture is rejected without publishing an I4-unsafe device image.
   The artifact retains `.B`, `.T`, `.cu`, `.t`, phase reports, host object,
   negative compile log, receipt, and checksums.
   `run-015/` revises the low-confidence relative cost model: A/B global
   read bytes count only valid predicated elements, and the sync term counts
   two CTA barriers per grid CTA and K tile. The `COST_MODEL 2` row makes
   that interpretation explicit while old v7 traces keep their earlier
   formula. The all-tail 130x133x135 c128 candidate changes from relative
   cost 5267 (padded loads, static sync=2) to 4942 (valid loads, dynamic
   sync=40); it remains selected. The artifact retains both compilation
   modes, K-only and I4-negative fixtures, raw `.t`, phase reports, CUDA,
   WHIRL, host parity, receipt, and checksums. Valid element bytes still
   omit sector overfetch, cache/HBM behavior, and SM parallelism; the sync
   term counts issued barriers, not exposed latency. No measured speedup
   follows from the revised score. Next, resolve v2 ordinals in the
   package/runtime, model transactions and exposed latency, and attempt
   NVISA/PTX lowering. Native CUDA compilation,
   ptxas resource inspection, GPU correctness, and same-target timing remain
   open.
   The compiler-only manual-review runner now publishes
   `plans/cu-review.json` with the canonical WHIRL-derived `.cu` path and
   hash, selected plan/geometry/SMEM layout, trace and report pointers,
   and host-CTA parity. It rejects incomplete image/trace/parity provenance;
   it does not relabel the legacy template as the selected kernel. The
   checksum-verified `m2-whirl-kernel/run-018/` is a fixed-shape
   manual-review checkpoint. Its compiler-side gate is complete; native M2
   acceptance is still open. At that checkpoint, GMEM access-map and
   sector-aware planning was the next compiler design action.
   The subsequent contiguous-F32 GMEM slice is recorded above: `run-028/`
   selects and emits the 16x2 lane map at K=32, while keeping padded 8x8 at
   K=128. The raw `.t` and `tile.log` show A/B/C source-warp sector requests,
   shared-A issue work, complete relative costs, and both decisions. No GPU
   throughput or native transaction count is inferred from this choice.
5. [ ] Extend the selected tile with P7.4/P8 staging and a reviewed
   fetch/pipeline plan. Retain movement engine, stages, issue/wait, barriers,
   hidden versus unhidden movement, fallback, correctness, and measurement
   in `m3-prefetch-pipeline`. The opt-in `run-015/` source-sidecar checkpoint
   covers plan selection, two-stage TMA emission, fallback, and P8 trace/report
   evidence; it does not close this gate without native CUDA compilation,
   numerical correctness, and same-target timing. `compiler-source/run-017/`
   exposes which launch path was selected, and the byte-preserving
   `compiler-gpu-validation/run-006/` package is prepared to compare both
   paths on a native Hopper-or-later GPU with wall-clock launch-to-completion
   timing that includes host tensor-map setup. Neither package is a WHIRL
   rewrite.
6. [ ] Calibrate the cost model against the retained measurements. Report
   prediction error by compute, memory, synchronization, launch, and
   occupancy term without presenting relative scores as cycles.

## SMEM Package After PR 188

The next compiler-side package is synchronous F32 A/B staging planning, not
closure of native-GPU or asynchronous-P8 acceptance. The implementation:

1. Exposes target bank count/word width/warp width through the common hardware
   description adapter; preserves all WHIRL binary layouts.
2. Retains compact/padded A and compact/column-XOR B alternatives with the
   existing thread maps. `cuda_smem_layout_search=ON|OFF` controls B expansion
   (default ON). No transformation before legality and complete-cost selection.
3. Counts A/B fill stores and compute loads separately, preserving broadcasts;
   records per-operand bytes, read issues and incremental XOR address work.
   `COST_MODEL 5` explicitly charges all these modeled bank/read/address terms.
4. Lowers selected B addressing into device WHIRL, including zero-fill. Emits
   canonical CUDA through `whirl2c -cuda`; preserves legacy source for parity.
5. Requires the trace tool to recompute `TILE_SMEM`, reject corrupt records and
   summarize allocation, four bank terms, costs, selection and WHIRL actions
   in `tile.log` and the aggregate logs. Retain `.t` and option-OFF evidence.
6. Validates short/long K, irregular edges, model unit contracts, option gating,
   mapped-image inspection, host CTA parity, and AIO-9 compatibility. Publish
   artifacts under `m3-smem-planning`; no GPU speedup claim follows from this.

Next compiler work after this synchronous package: represent multi-stage
allocation, TMA-specific shared swizzles and issue/wait/reuse semantics in
WHIRL; couple stage depth and layout choices before selecting a P7/P8 plan.
Occupancy calibration, native CUDA/PTX inspection and GPU testing remain open.
The complete algorithm and report schema are in the synchronous-SMEM addendum
of `AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md`.

Compiler-side package verified at `7efdf8da`: 52 unit tests, short/long K
and search-OFF selection, corrupt trace rejection, irregular edges/I4 rejection,
Hopper/Blackwell AIO-9 compatibility, multi-device-PU binding, and compact TMA
source-sidecar regression. Host CTA simulations report zero error against
reference and legacy kernels. The durable SHA256-verified bundle is
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/smem-planning/sha256-0dbd3c72e90e8036f947036821ad9bb3f5c8191189c2cc3f04b3431a738a3534/`.
It preserves canonical and parity `.cu`, input/device `.B/.T`, raw `.t`,
phase and summary logs, search-OFF comparisons, receipts and checksums.
This closes only the synchronous SMEM compiler-review slice, not `m3` native
prefetch/pipeline acceptance.

## Executable P8 Package After PR 189

Compiler-side scope: materialize the selected P8 schedule in device WHIRL,
not just in the legacy CUDA template. Synchronous selected fetch plans use
one cooperative scalar staging slot with publication and release barriers.
`cuda_tma_prefetch=ON` plus `cuda_whirl_output` now constructs the two-slot
exact compact TMA schedule. Expected transaction bytes are armed before
the two operand issues; all consumers acquire-wait before reading and release
through a uniform CTA barrier before guarded refill. Prologue bounds,
slot/parity expressions, operation order, K-loop bounds and refill conditions
are structurally checked; per-slot induction establishes generation lifetime.
The driver emits `PIPELINE_KERNEL` only after verified image construction and
publication. Calls remain conservatively effectful; common binary formats
are unchanged. The scalar source implementation of a logical vector fetch
plan is not a claim of vector PTX.

The report tool must require selected tile/fetch/binding agreement and summarize
engine, slot count, total logical storage including barriers, operation order,
proof status, kernel ABI and actual device-WHIRL action in `fetch-pipeline.log`
and the aggregate summary. Missing or contradictory proof is a hard error.
Preserve the raw `.t` as the first debugging artifact, not just its reports.

The five-argument pointer-map TMA entry is source-reviewable but deliberately
not admitted to the existing synchronous generated-host provider ABI. Native
map setup/launch integration is the next runtime boundary. Static TMA storage
is capped conservatively at 48 KiB until dynamic shared allocation is lowered;
irregular TMA, padded/XOR TMA, cp.async, cluster multicast, GPU correctness,
resource calibration and measured overlap remain open. See the executable
contract in `AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md`.

The `dsl_lower.cxx` architectural header and all function definitions/forward
declarations now have entry contracts, enforced by `test_source_contract.py`.
Do not confuse the active P8 worktree with the older primary checkout.

Verified compiler milestone: `5fb45858`. All 53 unit tests pass. The executable
suite retains synchronous K=135 and TMA K=16/32/48/80/144 kernels (one through
multiple slot wraps), with zero host-simulated reference error and matched
issue/completion/release censuses. It rejects corrupted order/parity/refill
source, corrupted proof traces, irregular TMA and the unsupported host ABI.
Two TMA PUs share target-operation and CUDA coordinate builtin identities.
Applied synchronous, SMEM-choice, edge, multi-PU, legacy TMA and AIO-10
Hopper/Blackwell/CPU/unsafe-distance regressions pass. Accepted artifacts are:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/executable-p8/sha256-46791c6a18371b6766acf3f30d006974d1de8a9d4b18302dee8b717ecd1b903d/`.
The bundle includes exact commands, binary hashes, mapped images/dumps, `.cu`,
target-operation header, raw `.t`, reports, simulations, receipts and checksums.
This does not close native-GPU M3 acceptance or the TMA host launch adapter.

## Joint Profitability And Lowering After PR 190

- [x] Defer P7 selection and preserve generated layout/thread-map siblings
  while constructing each permitted synchronous/TMA P8 alternative.
- [x] Share executable and signed-I4 preflight between the planner/lowerer;
  reject unsupported pairs before they can win, without binary-IR changes.
- [x] Evaluate full-GEMM relative compute, exposed input/output movement,
  SMEM work, dynamic synchronization, bounded overlap, setup and provisional
  capacity pressure exactly once per pair.
- [x] Select deterministic minimum complete cost; retain and report the best
  legal synchronous compile-time fallback. Lower only the selected pair.
- [x] Register `gemm_plan_search=baseline|joint`, default baseline, with
  explicit optimization/output gates and synchronous host-provider policy.
- [x] Independently audit the pair census/costs/winner/fallback and summarize
  P9 complete candidates in `physical-plan.log` and aggregate reports.
- [ ] Native CUDA/GPU correctness, launch integration and cost calibration.
- [x] Compiler-side direct GMEM alternative within the existing proven P7
  family, with zero staging storage and distinct warp-request accounting.
- [ ] Measured cache service, `cp.async`, calibrated occupancy/grid waves,
  dynamic shared memory and runtime multiversion fallback dispatch.

The exact formulas and trace columns are in
`AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md`. These are explicit
low-confidence relative scores, not measured latency or speedup.

Compiler checkpoint `0084f7fe` passes 59 unit tests, the joint short/long/edge
and option-control suite, the executable-P8 synchronous and delayed-copy TMA
regression, and the AIO-10 Hopper/Blackwell/CPU/unsafe-distance contract.
The 128x128x144 fixture compares 16 complete pairs and selects synchronous
XOR-B tile plan 6 at relative total 9279; compact TMA remains legal but loses
at 11646. Every host-only numerical simulation reports zero reference error.
These are model decisions and compiler-review results, not GPU measurements.

Durable evidence (raw `.t`, source/device `.B/.T`, explicit before/after
scope/diff, `.cu`, reports, commands, PDF, receipts and checksums):
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/joint-gemm-plan-search/sha256-4d4174f64dcf7706275132574f6d592b49aed3f743b0c18a0e5c21a551dd9dfa/`.

## D/S1/T2 Comparison After PR 191

The current joint model is model 2, specified by
`AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md`. The model-1 Markdown/PDF
pair remains the prior checkpoint, not the current D algorithm specification.

- [x] Construct D in device WHIRL with guarded global A/B loads, A-register
  reuse across j, register accumulation and guarded C stores. No A/B shared
  symbols, barrier symbols or async operations are constructed for D.
- [x] Enumerate S1, optional D for compact layouts, and permitted T2 before
  final selection. Bound 16 tile records by 48 complete pairs.
- [x] Price D's actual warp-issued sectors and repeated loads without cache
  hit/overlap assumptions; remove all inactive SMEM charges.
- [x] Retain the best S1 fallback even when D wins; explicitly restrict the
  generated-host provider to S1 and avoid an independent D source template.
- [x] Trace D/S1/T2 kind and `DIRECT_ACCESS`; independently audit costs,
  direct traffic, selected zero allocation and actual device publication.
- [x] Cover direct/staged winners, non-square tails, multi-CTA shapes, mixed
  D/S1 PU images and malformed direct planning/evidence.
- [ ] Native CUDA/GPU correctness and calibration. No GPU result is inferred
  from host simulation or a relative model minimum.

Compiler-side checkpoint: `be85a5c0` (2026-10-25). All 64 Python unit tests,
the direct/staged integration suite, joint-search controls and executable P8
regressions pass. The rebuilt native record test rejects six malformed direct
contracts and checks two analytic sector-count oracles. Host numerical
simulations report zero reference error. The 3x5x7 tail case selects D from
19 pairs at relative cost 4, retaining S1 pair 12 as fallback; 128x128x144
selects S1 at 9279 rather than compact T2 at 11646. These are uncalibrated
model scores, not FLOP rates or measured time.

Immutable SHA256-verified evidence, including before/after WHIRL, direct
CUDA, raw traces, reports, commands and producer-source/binary fingerprints:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/d-s1-t2-comparison/sha256-e90e39c0a17ac15ac58ba41338cb4ca774d5da21a9d70bb138857bf42d845918/`.

## Remaining FP32 Multilevel-Tiling Compiler Work

Order 2 occupancy increment is controlled by
[FP32 occupancy planning](AI-COMPILER-OPTIMIZATION-OCCUPANCY.md). Implement
table-owned rounding/capacities, PU-local residency-round cost refinement,
model6 positional trace/audits and old/new ranking evidence. Architectural
maxima, configured envelope and estimated allocation/live-value assumptions
must remain distinct. Shared throughput is never multiplied by resident CTAs.
Native occupancy and GPU certification remain open regardless of this result.

Order 2 bounded occupancy acceptance: checkpoint `b19e22e0`, 109 tests,
7875 literal schedule oracles, fifteen fresh compiler/source configurations,
linked contracts and backend dependency checks pass. Fourteen readiness
configurations retain the old winner; seven nonwinning candidate costs change
in each larger-grid case. Model6 traces expose the actual unchanged result,
not an invented speedup. The 1384-file immutable M9 receipt and read-only
durable reopen/replay proof are indexed in the occupancy design's Acceptance
section. Native allocation/occupancy and timing calibration remain pending.

Order 1 compiler increment (2026-10-26): joint P7 now defers movement resource
budgets, preserving semantically safe topologies for separate D/S1/T2 checks.
P8 owns independent register-value, active SMEM (including TMA barriers), and
thread ledgers; selected lowering recomputes the ledger. Joint model 3 emits
mandatory `JOINT_RESOURCE` rows, independently audited and included in phase
and aggregate logs. Baseline search and cost formulas remain unchanged.
See [movement-resource design](AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md).
Occupancy, allocation granularity, actual registers/spills and grid waves are
not implemented by this increment. The constrained native contracts, rather
than a new production tile family, demonstrate staged rejection preserving D.
Checkpoint `ef7cebce` passes 77 unit tests, D/S1/T2 integration, joint
controls/baseline parity, executable P8 and linked AIO-10 contracts. The
immutable receipt and independent reopen/replay results are indexed in
the movement-resource design's Compiler Checkpoint section.

Current priority (2026-10-25): continue fixed-shape F32 D/S1/T2 optimization
without requiring a GPU. The selected-plan WHIRL and its `whirl2c -cuda`
output remain the review authority. Historical M0-M3 acceptance boxes above
also include native execution and must not be read as evidence that the
implemented compiler transformations are absent.

| Order | Remaining compiler work | Required review evidence |
| --- | --- | --- |
| 1 (implemented) | Movement-aware candidate/resource discovery: evaluate D, S1 and T2 resource legality before staged-only filtering can discard a useful D seed; refine register estimates independently of SMEM use. Keep enumeration bounded and deterministic. | Candidate census, per-plan rejection reasons, resource accounting, selected/fallback records and WHIRL-derived CUDA; tests in which staged resource rejection does not incorrectly reject D. |
| 2 (bounded estimate implemented; native refinement open) | Table-owned allocation rounding, resident blocks/warps, grid underfill and full/tail rounds, shared-throughput readiness costs and D-specific issued-sector/Roofline diagnostics. Read-only bubble experiment remains separate; native register allocation, partitions, cache/HBM service and calibration are not certified. | M9 immutable receipt: old/new winners, complete costs/resource bounds, 109 tests, 7875 literal oracles and fifteen compiler/source configurations. No claimed native speedup; incomplete models fail closed. |
| 3 | Expand executable movement choices as separately gated increments: actual aligned vector loads with scalar tails, non-TMA asynchronous copies, and additional legal stage/layout combinations. Dynamic SMEM beyond the current conservative static limit requires a matching launch-resource contract. | Plan-to-WHIRL address, alignment, capacity and acquire/issue/wait/release proofs; no vector/overlap credit for scalar or synchronous code. Preserve a legal synchronous fallback. |
| 4 | Complete selected-plan launch integration: generated-host application currently supports S1 only; carry D/T2 requirements, tensor-map setup, selected-entry resolution and buffer/stream ordering through the generic provider ABI and target launch adapter. | Compiler-generated host/device review sources, ABI and mock-runtime tests; distinguish compiler completion from native launch certification. |
| 5 | Close the compiler-only review matrix for the supported subset, including shapes/tails, multiple PUs, option controls and rejected plans. | Immutable raw `.t`, before/after WHIRL, `.cu`, `tile.log`, `fetch-pipeline.log`, `physical-plan.log`, `optimization.log` and `optimization-summary.log`, with costs, selection reasons and actual transformation actions. |

Orders 1 and the bounded compiler estimate in order 2 are implemented. Orders
3-5 remain separate compiler packages; native refinement/calibration stays
open. Further
movement extensions should be selected by explicit legality and cost evidence,
not enabled as unconditional rewrites. The list is a remaining-work roadmap,
not a claim that each optional movement extension is necessary for the current
bounded FP32 subset to compile correctly.

Native CUDA compilation, PTX/SASS inspection, measured resource/spill counts,
GPU correctness, baseline timing and cache/overlap calibration remain separate
acceptance gates. Host simulation and relative cost minima cannot close them.

## Bubble Model Experiment

Hardware-description follow-on (2026-10-26):
[target selection](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md) is now
implemented. Model 5 resolves exact `gpu_arch=sm_*` names through common
Hardware Description IR and consumes its per-path startup, rates and
synchronization parameters. No optimizer-owned timing default remains;
the old latency option is explicit-override only. Model 4 below is historical.
Initial tables remain estimated, not calibrated NVIDIA specifications.

| HW step | Status and acceptance |
| --- | --- |
| HW0: common description contract | Implemented policy-free immutable architecture registry and standalone performance payload; no binary WHIRL change. |
| HW1: timing/rate consumers | Evaluator and adapter consume table data; literal recurrence tests vary hardware parameters. |
| HW2: controls and target gates | Exact sm_90/sm_90a selection; sm_100/sm_100a planning descriptions; unknown/suffix and unsupported emission rejection. |
| HW3: trace/report evidence | Passed: model-5 records, independent audits, thirteen compiler/source configurations, 104 tests, native contracts and 1281-file immutable evidence. Durable WHIRL reopen and summary replay match; see target-selection Acceptance Evidence. |
| HW4: actual characterization | Open: SKU/refinement, measured timing/clock domains, calibration and native GPU certification. |

Priority update 2026-10-26: implement the lightweight
[analytical readiness selector](AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md)
before production simulator integration. This is a separate path, not a claim
that the B2/B4 event-model gates below are complete.

| Analytical step | Status and acceptance |
| --- | --- |
| AR0: contract and definitions | Defined assumed profile, units, D/S1/T2 formulas, ownership, uncertainty, trace obligations and exclusions. |
| AR1: pure schedule evaluator | Implemented max-plus two-slot recurrence; literal schedule oracle, large-K, overflow and invalid-input coverage. |
| AR2: joint selector integration | Historical model-4 compiler/source review passed for five shapes and ten configurations. Current model 5 uses resolved HW descriptions; relative model 3 retained; winner reconstruction uses identical controls. |
| AR3: trace/report verification | Independent profile/ledger/recurrence/winner audits and missing/duplicate/corrupt record rejection pass. 1119 files promoted immutably; durable WHIRL reopen and summary replay match. See analytical design Review Evidence. |
| AR4: calibration/refinement | Open: measured target profiles, exact tail classes, native occupancy, cache/queue effects, uncertainty-aware selection and GPU verification. |

Initial analytical outcome: 128x128x144 changes actual selection from S1 #12
to T2 #3, costing 31336 versus best S1 #1 at 43237 assumed reference cycles.
65x67x17 changes from D #8 to S1 #1; three tiny/tail shapes retain D #2.
102 unit tests and 2340 literal recurrence oracles pass. Low-latency T2 also
wins in the analytical model; its disagreement with the shadow simulator is
recorded for calibration rather than hidden or coefficient-fitted away.

Controlling design:
[GEMM Bubble Prediction And Candidate-Selection Experiment](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md),
with selection/event/simulation pseudocode in Sections 4-5. This queue refines
FP32 order 2; it does not reopen numerical work, add executable candidates or
require a GPU to begin. The initial B0/B1 and downstream B3 replay slice is
implemented in `tests/ai_matmul_cuda/gemm_bubble.py`; see design Section 10.
It consumes authenticated compiler traces and retains synthetic source-warp
event evidence, with independent interval audits. B2's production VHO
adapter/options, B4 selection and B5 native calibration remain pending.
A predicted ranking change is not a measured performance improvement.

Initial replay result: 96 tests pass; five compiler cases times three
synthetic profiles yield 11 changed/four unchanged hypothetical rankings.
The 128x128x144 case prefers T2 only at the higher modeled memory latencies;
the actual emitted S1 and all producer artifacts are preserved. Source-warp
assumptions and uncertainty remain review items before production integration.
Checkpoint `f352ccbd` retains all 168 independently audited schedules in a
331-file immutable review receipt, indexed in bubble design Section 11.
Production integration is not implied by completing this downstream replay.

| Step | Deliverable and owner | Acceptance evidence |
| --- | --- | --- |
| B0: representation and profile audit | VHO/target owners review LNO cache modeling, CG dependence/SWP/resource estimation, existing plan/lowering recipe and hardware descriptors. Define reusable runtime-only evidence and explain any adapter mismatch; no new backend library dependency. | Reviewed units, actors, latency versus initiation interval, shared service/queue rules, scope and provenance. Synthetic profiles cannot impersonate calibrated hardware. |
| B1: deterministic event-model oracle | Implement pure dependency/resource simulation with explicit instruction-group accuracy, fixed scheduling policy and finite capacities. No compiler selector change. | Hand-calculated load-use waits, covered/all-warp stalls, bandwidth/queue contention, initiation interval, recurrence, bank, barrier/slot-generation, fill/drain and output-completion tests. Overflow, unknown facts, budget and deadlock cases fail closed; cause accounting does not double count. |
| B2: read-only plan adapter and shadow control | VHO derives event graphs from the same supported F32 D/S1/T2 lowering contracts; integrate proposed off/shadow controls with joint/-O3/output gates. Preserve the current winner, S1 fallback and candidate census. | Compare per-plan predicted operations/actor behavior with generated WHIRL; source/plan identities and full candidate coverage. Shadow on/off yields equivalent actual WHIRL/CUDA; O0/O1/O2, invalid modes, host restrictions and multiple-PU isolation covered. |
| B3: candidate comparison and trace/report audit | Compare model-3 and cycle-model rankings on retained 3x5x7/128x128x144 cases plus bounded shapes, tails, small grids and coherent hardware/scenario sweeps. Extend positional `.t` producer and analyzer together. | Old/shadow/emitted IDs, same-model cycle margin, changed/unchanged/incomplete reason, profile/model identity, unknowns and sensitivity. Independent event/resource/winner replay; log first lines, aggregate/summary and trace-off compatibility. No forced reversal or native speedup claim. |
| B4: separately reviewed selection gate | Only after B0-B3, propose enabling select with complete comparisons, uncertainty policy and the same executable legality/fallback/publication gates. Retain current selector when evidence is incomplete or inconclusive. | Positive/negative confidence-margin tests, deterministic ties, no mutation/publication on failure, correct actual transformation trace. Synthetic selection is test-only; diagnostic cycles never mix with old relative scores. |
| B5: native refinement and calibration | When toolchain/hardware becomes available, map events to SASS, import actual registers/spills, calibrate memory/issue assumptions and compare timings with profiling evidence. | Retained native binaries/disassembly, unprofiled timings separately from counters, numerical correctness, prediction error, rank agreement and regret. No exact cycle claim from sampled stalls or instruction count alone. |

Proposed `-Wb,-DSL:gemm_bubble_model=off|shadow|select` and
`-Wb,-DSL:gemm_bubble_profile=<versioned-profile-path>` are NOT registered.
Compatibility default is off. The analyzer CLI is separate from these
proposed controls. B2 implements shadow, not select; B4 requires
explicit review before selection changes. Existing WOPT trace controls remain
the raw `.t` authority; introduce no separate ad hoc compiler log or new trace
bit for this experiment.

The analyzer obligations are phase-specific: `tile.log` explains reuse/traffic
and resource assumptions; `fetch-pipeline.log` explains readiness, queues,
slot lifetimes and fill/drain; `physical-plan.log` compares complete rankings;
new proposed `bubble.log` attributes unused compute opportunities and model
limitations. `optimization-summary.log` states the actual versus hypothetical
winner, and `optimization.log` includes completed phase bodies. Every file's
first line starts `What it summarizes:`. Disabled/skipped/unknown phases remain
visible in the summary rather than appearing successfully executed.

Preserve raw `.t`, exact profiles/scenarios and revisions, graph/timeline,
all reports, generated sources, before/after mapped `.B` plus independent
`ir_b2a -st -src` `.T`, diffs, commands and checksums in a new immutable review
receipt. The first experiment must distinguish algorithm-oracle success,
compiler ranking outcome and still-open GPU correctness/performance evidence.
Complete B0-B3 before deciding whether the cycle model is strong enough to
change selection; B5 remains separate from GPU-free compiler progress.

## Target-Specific Numerical Planning Follow-On

**Deferred (2026-10-25, user direction):** resume native partial-sum promotion
and related numerical integration only when FP8 support is explicitly started.
Retain the existing contract prototype, tests and evidence; do not make further
FP8 work a prerequisite for FP32 multilevel-tiling milestones. Blackwell FP4
remains a separate future target-profile effort.

The proposed
[numerical accumulation design](AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md)
separates Hopper FP8 WGMMA promotion from Blackwell FP4 block-scaled MMA.
This follow-on does not change the completed FP32 D/S1/T2 checkpoint or close
its native GPU gate. Minimum execution sequence:

- [ ] Policy-free numerical/accumulation records, target evidence and verifier.
- [ ] Explicit Hopper partial/reset/wait/scale/FP32-merge WHIRL; prove maximum
  reduction span independently of SMEM BK and slot count.
- [ ] Joint numerical-legality and profitability search with source-format
  fallback, complete trace/report evidence and adversarial numerical tests.
- [ ] Separate Blackwell NVFP4/MXFP4 representation, scale and instruction
  lowering; do not inherit Hopper's empirical accumulation penalty.
- [ ] Exact-target native accuracy/performance and model-level acceptance.

The first compiler-side contract slice now implements common runtime records,
an active-PU -O3 segment planner and detached scalar F32 WHIRL materialization
with postchecks. It includes an independent `AI_ACC 1` audit/report stream.
It does not close the first full target-profile gate: rounding policy,
production phase protection and actual Hopper WGMMA reset/wait/merge lowering
remain open. Native FP8 requests cannot use the scalar adapter; Blackwell
planning is rejected until its independent scaled profile exists. See
Section 7 of the numerical design for exact implemented APIs and tests.
Current AI_OPT v7 and production D/S1/T2 FP32 behavior remain unchanged.

Contract checkpoint `f606852c`: 71 Python tests, 45 scalar segment cases,
14 malformed contracts and four altered WHIRL sequences pass, along with
the final D/S1/T2 regression. Production backend build and the no-frontend-
builder symbol-boundary check pass. The standalone 257-element scalar case
emits three resets and three outer merges. Native Hopper/Blackwell lowering,
pipeline integration, optimizing-phase protection and model acceptance remain
open; scalar host evidence must not close those gates.

Immutable evidence (same-PU before/after `.B/.T`, raw diff, WHIRL-derived
`.cu`, `.t`, complete reports, commands, build logs and checksums):
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m4/accumulation-contract/sha256-ca49313ac3fa783e29803efe7137fd92b4660cdf64bfdd6671af77eac99f19e8/`.

## Related Documents

- `AI-COMPILER-OPTIMIZATION-PROGRESS.md` - running status, execution gates,
  option maturity, and evidence index.
- `OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md` - umbrella architecture
  for frontend artifacts, optimization plans, target lowering, AOT and late
  compilation, executable packages, runtime providers, serving adapters, and
  telemetry attribution.
- `AI_compiler_optimization_design_v0.3.md` - living architecture, phase
  report contract, and matmul trace/artifact convention.
- `AI-COMPILER-OPTIMIZATION-AIO0-INVENTORY.md` - merged-source inventory,
  ownership matrix, gap analysis, and fixed-shape baseline evidence.
- `AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md` - runtime-only graph
  schema, per-PU ownership, invariants, inspection, and certification.
- `AI-COMPILER-OPTIMIZATION-AIO2-PLAN-COST.md` - immutable candidate, legality,
  cost, plan, fallback, budget, selection, and inspection contract.
- `AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md` - per-PU semantic tensor
  facts, consumer roles, completeness, domain preservation, and certification.
- `AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md` - per-PU lifetime,
  locality, reuse, control-scope, and conservative-boundary evidence.
- `AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md` - runtime-only immutable
  layout descriptors, TensorEvolutionGraph overlays, compatibility, conversion
  cost, type materialization boundary, and certification.
- `AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md` - runtime-only placement,
  sharding, distributed alias/range, communication epoch/intent, AIO-2 plan,
  PU-scope, compatibility, and certification contract.
- `AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md` - typed memory hierarchy
  adapters, runtime-only residency alternatives, capacity/lifetime legality,
  AIO-2 plans, compatibility, and certification.
- `AI-COMPILER-OPTIMIZATION-AIO9-HIERARCHICAL-TILING.md` - explicit G0-G11
  tile hierarchy, typed target/resource evidence, AIO-2 plans, compatibility,
  certification, and executable-transformation boundary.
- `AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md` - selected-tile movement,
  buffering, issue/wait, overlap cost, target capability, and fallback plans.
- `AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md` - complete direct,
  generated, and reviewed-provider alternatives, capability validation,
  deterministic fallback, selection, and executable-application boundary.
- `AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md` - certified PU-local
  variants, runtime guards, guard cost, conservative fallback, runtime
  evaluation, compatibility, and G15 certification.
- `AI-COMPILER-OPTIMIZATION-AIO13-TELEMETRY-FEEDBACK.md` - PU-local telemetry
  schema, Open64 feedback continuity, stale-profile handling, measured-cost
  refinement, no-profile behavior, and G16 certification.
- `AI-COMPILER-OPTIMIZATION-PHASE-OWNERSHIP-AUDIT.md` - AIO-0 through AIO-13
  conformance matrix and staged common/VHO/WOPT/LNO/IPA ownership migration.
- `AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md` - initial fusion
  candidate skeleton, legality, cost, fallback, selection, and certification.
- `VHO-DSL-OPTIMIZATION-PLAN.md` - fixed VHO DSL optimization pipeline and
  optimization-level policy.
- `VHO-DSL-PARALLELIZATION-PLAN.md` - architecture-independent parallel work.
- `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md` - compiler-owned tensor shape facts.
- `WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md` - shape refinement
  execution and immutable tensor-type management.
- `CUDA-HOPPER-BLACKWELL-MATMUL-OPTIMIZATION-PLAN.md` - target-specific GEMM
  optimization ladder and `G*` review checkpoints.
- `NVIDIA-DSL-RUNTIME-INTEGRATION-PLAN.md` - executable NVIDIA runtime and
  provider boundary.
- `DSL-RUNTIME-VALIDATION-AND-BENCHMARK-HARNESS-PLAN.md` - correctness and
  performance harness.
- `WOPT-DSL-ADAPTATION-PLAN.md` - logical DSL CODEREP and WOPT reuse.
- `WHIRL-DSL-INFRASTRUCTURE.md` - native DSL WHIRL representation,
  compatibility, inspection, and lowering boundary.
