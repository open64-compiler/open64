# AI Compiler Optimization Implementation Plan

## Status

Execution plan for `AI_compiler_optimization_design_v0.1.md`.

The architecture document is authoritative for the `AI-P0` through `AI-P11`
phase order, the TensorEvolutionGraph-centered design, common-versus-domain
ownership, and plan-level profitability. This document turns that design into
staged Open64 work with explicit ownership, compiler scope, review gates,
artifacts, and pull-request boundaries.

The first implementation is deliberately narrow: one PU, one fixed-shape
`common.matmul`, check-only analysis, and no binary WHIRL layout change. Later
milestones expand the same services to CNN, Transformer, distributed
placement, target tiling, asynchronous movement, runtime variants, and
telemetry.

## Adoption Decision

Open64 adopts the following design rules from v0.1:

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
```

`VHO_DSL_AI_Optimize_Program_Unit()` owns active-PU traversal, phase ordering,
analysis lifetime, candidate budgets, selected-plan application, diagnostics,
tracing, and final per-PU verification. It does not own common record schemas,
operator semantics, Python capture, or target code generation.

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

These services use stable IDs and fixed-layout records where persistence is
required. They must not expose VHO, WOPT, LNO, backend, Python, CUDA, or vendor
library types in public common interfaces.

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

- one unchanged baseline and bounded `cuda_64`, `cuda_128`, and
  `blackwell_wide` candidate families;
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

## Review Artifacts

Retain each completed lane in a host-visible directory:

```text
artifacts/ai_optimization/<fixture>/<milestone>/
```

The family includes the source, input `.B`, input `.T`, selected phase
checkpoints, output `.B` when mutation occurs, output `.T`, candidate/plan
trace, diagnostics, command transcript, target profile, cost evidence, runtime
result, and checksums. Clean the directory at the beginning of the next run,
not at the end of the current run.

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
18. [ ] Execute the first AIO-12 runtime-variant slice with reviewed shape or
    alignment guards, a conservative direct fallback, explicit guard cost,
    and retained true/false-path evidence.

## Related Documents

- `AI_compiler_optimization_design_v0.1.md` - authoritative architecture.
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
