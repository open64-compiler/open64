AI compiler optimization design

Version v0.1 | Living design skeleton | Draft for continuous expansion

Scope: AI memory hierarchy optimization architecture, research opportunity mapping, common optimization IR substrate, AI-domain-specific extensions, candidate generation, profitability analysis, and implementation priorities.

Status: v0.1 consolidates the latest phase-ordering discussion and the top three AI compiler research opportunity analysis. It distinguishes research-derived design inspiration from DSC design proposals so future revisions can add paper-specific details, measurements, and implementation decisions without blurring provenance.

# 1 Executive Summary And Design Goals

The design goal is to make AI compiler optimization compositional across semantic tensors, layouts, sharding, memory residency, tiling, asynchronous movement, scheduling, and runtime adaptation. The central architectural choice is to preserve tensor identity through a TensorEvolutionGraph while optimization candidates remain provisional until enough layout, placement, tiling, resource, and runtime-state information exists to make a profitability decision.

The common compiler substrate should own reusable descriptions of objects, representations, memory hierarchy, placement, access patterns, fetch plans, pipelines, parallelization, communication intent, optimization candidates, plans, costs, and runtime guards. AI-specific layers should own workload semantics and runtime state such as batch size, sequence length, KV-cache pressure, prefix-cache hit rate, expert load, and latency service-level objectives.

The recommended implementation priority is: first TensorEvolutionGraph, then candidate and plan infrastructure, then fusion and tiling refinement, then parallelization integration, and finally runtime variants and selection policies.

## 1.1 Design Goals

- Keep high-level AI meaning visible long enough to guide memory and scheduling decisions.

- Represent tensor evolution explicitly from semantic tensors to distributed shards, local buffers, tiles, shared-memory tiles, registers, and tensor-core fragments.

- Avoid committing fusion, tiling, layout, placement, or parallelization choices too early; plan selection is gated on explicit cost modeling and legality checks.

- Make profitability a property of complete plans, not isolated transformations.

- Separate reusable compiler-control infrastructure from AI-domain-specific semantics.

- Allow static compilation to generate certified runtime variants selected by guarded runtime policy.

- Create a living skeleton that can absorb future paper reviews, measurements, and implementation notes.

# 2 Recommended AI Optimization Phase Ordering

The phases below are ordered to preserve semantic information early, introduce physical constraints gradually, and delay final profitability decisions until the optimizer can evaluate the interaction between fusion, layout, sharding, tiling, memory residency, communication, and runtime state.

| Phase | Name | Purpose |
| --- | --- | --- |
| AI-P0 | Semantic tensor analysis | Preserve Attention, GEMM, Conv, MoE, RMSNorm, residual, activation, model weight, activation, and KV-cache meaning. Attach shape, dtype, semantic role, producer-consumer relationships, dynamic dimensions, and reuse traits. |
| AI-P1 | Lifetime reuse and locality analysis | Compute producer, consumers, last use, reuse distance, object size, working-set size, critical-path position, and likely residency benefit. |
| AI-P2 | High-level fusion | Create semantic fusion candidates such as MatMul+Bias+Activation, Norm+Projection, attention subgraph fusion, residual fusion, and MoE dispatch-related fusion. Candidates remain provisional. |
| AI-P3 | Logical layout optimization | Choose logical tensor layouts such as row-major, channel-major, blocked, packed-head, sequence-major, KV-cache layout, and MoE expert-major layout without fully committing to local physical storage. |
| AI-P4 | Distributed placement and sharding | Select data parallelism, tensor parallelism, pipeline parallelism, expert parallelism, sequence/context parallelism, replication, shard ownership, and remote-HBM placement alternatives. |
| AI-P5 | Communication derivation | Derive AllReduce, ReduceScatter, AllGather, send/receive, peer copy, RDMA, and collective boundaries implied by sharding and fusion decisions. |
| AI-P6 | Local physical layout and residency | Assign local HBM, L2, shared memory, registers, pinned host memory, CPU memory, or remote memory residency. Track capacity, lifetime, reuse, spill, and eviction. |
| AI-P7 | Multi-level tiling | Build and cost hierarchical tile plans across output tile, reduction tile, CTA/threadblock tile, shared-memory tile, warp tile, thread tile, register fragment, vectorized access, and tensor-core/instruction tile. Connect each level to reuse, coalescing, arithmetic intensity, register pressure, shared-memory footprint, occupancy, synchronization, and autotuning parameters. |
| AI-P8 | Fetch prefetch and async movement | Plan cp.async/TMA-like transfers, double/triple buffering, host-to-device prefetch, remote-HBM fetch, KV-cache paging, and overlap with compute. |
| AI-P9 | Physical kernel fusion and scheduling | Select the physical kernel plan, final fusion boundaries, launch structure, stream assignment, synchronization points, and instruction-level schedule constraints. |
| AI-P10 | Runtime variants and adaptation | Generate certified variants with runtime guards for batch size, sequence length, KV-cache pressure, prefix-cache hit rate, expert load, HBM availability, network contention, latency SLO, and guarded alias or disjoint-tile predicates. |
| AI-P11 | Telemetry and profile feedback | Capture measured latency, achieved occupancy, memory traffic, communication time, overlap effectiveness, cache hit behavior, launch overhead, and runtime variant choices for future recompilation or policy tuning. |


# 3 Rationale And Ordering Constraints

The ordering is driven by the fact that AI tensors with identical numerical type may have very different optimization behavior. A KV-cache block, an activation tensor, and a model weight may all be low-precision tensors, but their lifetime, reuse distance, placement, fetch policy, sharding, and runtime sensitivity differ sharply.

## 3.1 Reusable Phase Rationale And Performance Contract

Each optimization phase needs more than a name, an IR object, and an ordering
position. Its design section must explain why the phase exists and how its
analysis can lead to performance without confusing candidate discovery with an
executed transformation. This becomes increasingly important as placement,
sharding, residency, tiling, prefetch, scheduling, and runtime variants add
interdependent alternatives.

Every phase-specific rationale should use the following structure:

| Field | Required question |
| --- | --- |
| Optimization problem | What performance limitation or lost opportunity does the phase expose? |
| Input facts | Which semantic, shape, effect, ownership, locality, target, and profile facts must already be available? |
| Candidate space | Which alternatives are represented, and what deterministic budget limits their enumeration? |
| Performance mechanism | Which traffic, reuse, parallelism, occupancy, synchronization, launch, or communication term can improve? |
| Legality contract | Which invariants must hold before an alternative can be selected? |
| Cost and uncertainty | Which quantities are known, which require a target model, and which remain unknown? |
| Downstream consumers | Which later phases refine or consume the result? |
| Non-goals and ownership boundary | What is deliberately not decided or mutated in this phase? |
| Fallback | Which executable baseline remains valid when evidence is incomplete or the candidate is rejected? |
| Review evidence | Which analysis trace, before/after IR, counters, measurements, and negative cases prove the phase contract? |

This structure should be filled in when each phase moves from architectural
placeholder to implementation. A phase must not claim performance improvement
merely because it generated a candidate. The claim becomes valid only after a
complete plan selects and applies the candidate and measured or modeled
evidence supports the expected benefit.

## 3.2 Why Semantics Come First

Semantic tensor analysis must occur before aggressive lowering because attention, MoE, KV cache, residual paths, and normalization have domain-specific lifetimes and reuse structures. Once lowered too early into generic loops or opaque kernels, the optimizer loses the reason a tensor should remain resident, be replicated, be sharded, or be prefetched.

## 3.3 AI-P2 High-Level Fusion Rationale

### 3.3.1 Optimization Problem

A high-level operator graph commonly materializes an intermediate tensor and
launches a separate implementation at every operator boundary. Those
boundaries are semantically useful but can produce avoidable memory traffic,
launch overhead, synchronization, and loss of producer-consumer locality.
AI-P2 exposes legal high-level fusion alternatives while operator semantics,
TensorDescriptorIR, REGION boundaries, and source provenance are still
available.

Fusion is not intrinsically profitable. Eliminating an intermediate can extend
live ranges, increase register and shared-memory demand, reduce occupancy,
duplicate computation at fan-out, constrain tiling, introduce layout
conversions, or cross a communication boundary. AI-P2 therefore discovers and
classifies candidates; it does not treat graph adjacency as proof of legality
or performance.

### 3.3.2 Input Facts

AI-P2 consumes:

- logical operator identity, version, shape rule, effects, attributes, and
  domain contract;
- canonical TensorDescriptorIR identity and TensorEvolutionGraph roots;
- producer-consumer value references, use counts, external boundaries, and
  result ownership;
- AI-P1 lifetime, reuse distance, materialization size, alias, and control-scope
  evidence;
- REGION membership and source-level semantic boundaries; and
- explicit numerical, recomputation, and provider constraints when applicable.

It must not infer fusibility from symbol names, Python class names, physical
`OPR_DSL` encoding, or an assumed target kernel. Missing facts remain unknown
and keep the corresponding candidate unselectable.

### 3.3.3 Candidate Space

Enumerating every useful operator sequence as a hand-written pattern is not a
scalable fusion architecture. The primary discovery mechanism should operate
on producer-consumer edges and versioned operator fusibility contracts. Each
operator publishes the properties needed to decide whether an edge may join a
cluster: iteration-space class, operand indexing relation, broadcast or
reduction behavior, descriptor and layout requirements, effects, ownership,
multi-use behavior, recomputation permission, numerical constraints, REGION
scope, and relevant resource requirements.

The compiler grows deterministic clusters under explicit candidate and plan
budgets. Fan-out, diamonds, multiple results, external uses, and alternative
cuts remain visible rather than being hidden by a greedy rewrite.

The intended architecture is hybrid:

- generic edge and cluster formation handles ordinary elementwise,
  broadcast, contraction-epilogue, view, and compatible reduction cases;
- explicit semantic patterns recognize compounds such as attention,
  normalization, residual structures, quantization sequences, and provider or
  library dispatch opportunities; and
- OptimizationPlanIR compares the baseline, generic clusters, semantic
  compounds, and alternative fusion cuts before executable WHIRL mutation.

The first AIO-5 implementation uses `matmul+bias+activation` and
`residual+activation` patterns only as vertical slices for the shared
candidate, legality, cost, fallback, selection, and inspection skeleton. They
are not the long-term enumeration strategy.

The first scalable follow-up adds versioned iteration-space and operand-indexing
traits and grows bounded clusters over ordinary producer-consumer edges. It
keeps semantic compounds on the explicit path, stops conservatively at
fan-out, diamonds, unsupported results, effects, and REGION boundaries, and
publishes both discovery paths through the same OptimizationPlanIR services.
AIO-6 layout compatibility is retained as explicit refinement evidence; it
does not silently force a physical representation during high-level fusion.

### 3.3.4 Performance Mechanisms

| Mechanism | Potential benefit | Countervailing risk |
| --- | --- | --- |
| Intermediate materialization | Avoid a complete producer write and consumer read. | A fused schedule may still spill or require a layout conversion. |
| Kernel or library launch | Amortize launch, dispatch, and synchronization overhead. | A larger kernel can reduce concurrency or prevent provider dispatch. |
| Producer-consumer locality | Forward values through registers, shared memory, or an on-chip tile. | Longer live ranges can increase register pressure and occupancy loss. |
| Cross-operator simplification | Expose constant folding, algebraic cancellation, epilogue folding, and redundant conversion removal. | Numerical, reassociation, or strict-FP rules can prohibit the rewrite. |
| Shared iteration space | Traverse one tile once for multiple compatible operations. | Broadcast, reduction, or indexing relations may require incompatible traversals. |
| Semantic compound or provider implementation | Select a reviewed attention, normalization, convolution epilogue, or library contract. | Hiding domain semantics or choosing the provider too early can block later optimization. |
| Communication boundary reduction | Keep local producer-consumer work together when ownership permits. | Fusion can obstruct sharding, collectives, overlap, or remote ownership. |

These mechanisms explain what the candidate could improve. They do not prove
the fused plan is faster. The complete plan must account for both eliminated
cost and newly introduced resource or scheduling cost.

### 3.3.5 Legality Contract

A proven high-level fusion candidate requires compatible logical iteration and
indexing contracts, canonical descriptors or an explicit representation path,
pure or explicitly composable effects, legal ownership, supported result and
use structure, preserved REGION/domain semantics, and an admissible numerical
contract. Eliminated intermediate values require sufficient use-count,
aliasing, and lifetime proof. A multi-use producer may require recomputation,
partial fusion, or retention of the materialization rather than unconditional
absorption.

Effects, incompatible descriptors, ownership violations, illegal REGION
crossing, and violated numerical rules are structured rejections. Unknown
shape, alias, resource, layout, or control evidence remains an unknown
candidate, never a zero-cost or implicitly legal one.

### 3.3.6 Cost, Uncertainty, And Fallback

AI-P2 can often calculate eliminated tensor bytes, eliminated launch count,
boundary live-range growth, and known alternative cuts. Target-independent
analysis usually cannot yet determine register allocation, occupancy,
shared-memory allocation, final tile shape, communication interaction, or the
latency of a physical fused kernel.

The initial check-only implementation may use a deterministic relative score
to exercise plan selection, but that score is not a latency prediction and
must be refined or superseded by AI-P3 through AI-P9. If a required term is
unknown, the fusion plan remains incomplete. Every fusion alternative names
the original unfused operator sequence as a complete executable fallback.

Early selection is therefore provisional. A later phase may retain, split, or
reject the cluster when layout, placement, communication, residency, tiling,
or target-resource facts become available.

### 3.3.7 Downstream Consumers, Boundary, And Evidence

AI-P3 consumes and refines fusion boundaries with logical-layout
compatibility. AI-P4 and AI-P5 evaluate ownership and communication effects.
AI-P6 through AI-P9 determine physical residency, tiling, resources, kernel
implementation, and final scheduling. Runtime variants may retain more than
one statically certified physical realization.

AI-P2 owns high-level candidate discovery and semantic legality. It does not
choose a target kernel, allocate storage, finalize physical fusion, or erase
domain-visible operators before their gatekeepers run. The check-only AIO-5
stage does not rewrite WN, mapped-image tables, types, symbols, or binary
WHIRL.

Review evidence must show deterministic members and boundaries, eliminated
materializations, live-range growth, legality and unknown states, complete or
incomplete cost terms, explicit fallback, independent generation and selection
controls, resource/effect/descriptor/REGION negatives, and active-PU scope.
For a check-only milestone, before/after/repeat `.B` and `ir_b2a -st -src`
output remain byte-identical. The focused implementation contract is recorded
in `AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md`.

This architecture follows the broad industry direction represented by
OpenXLA fusibility predicates and cost-driven fusion, TVM operator-category and
post-dominator fusion, MLIR Linalg indexing-map composition, and
scheduler-driven candidate scoring in TorchInductor. DSC keeps its
TensorEvolutionGraph, domain semantics, Open64 scope discipline, explicit
fallback, and complete-plan profitability model around that generic core.

## 3.4 AI-P3 Logical Layout Rationale

### 3.4.1 Optimization Problem

Two tensors can have the same dtype, rank, logical shape, and numerical values
while admitting very different implementation performance. Axis order,
blocking, packing, and alignment determine how later mappings can access and
reuse those values. If the compiler commits to one physical representation too
early, it can hide profitable fusion, tiling, vectorization, tensor-core, and
communication choices. If layout remains an unstructured string, later phases
cannot compare alternatives or account for conversion cost.

AI-P3 therefore represents immutable logical-layout alternatives for one
semantic tensor before selecting local storage or target instructions. The
semantic tensor and its canonical `TY_IDX` remain unchanged. Each alternative
is connected to the semantic root through a semantics-preserving
TensorEvolutionGraph edge.

### 3.4.2 Performance Mechanisms

| Mechanism | Why logical layout matters |
| --- | --- |
| Global-memory coalescing | Axis order can place the dimension traversed by adjacent lanes on a contiguous or regularly strided axis, reducing memory transactions. |
| Cache and shared-memory reuse | Logical blocking exposes reusable submatrices or tensor tiles that later residency and tiling phases can keep near the compute units. |
| Vector access | Contiguous groups, alignment, and block factors determine whether later lowering can use wide loads and stores instead of scalar accesses. |
| Tensor-core fragments | Suitable axis and block organization reduces rearrangement needed to map operands into MMA, WMMA, WGMMA, or other instruction fragments. |
| Reduction efficiency | A contiguous or tiled reduction dimension can reduce strided traffic, synchronization, and reduction-tree overhead. |
| Fusion | Producer and consumer operations with compatible layout contracts can avoid an intermediate materialization; an incompatible layout can split a fusion cluster. |
| Placement and communication | Layout and sharding jointly determine local ownership, packing, collective shape, and whether an all-to-all or transpose is required. |
| Prefetch and pipelines | A regular blocked layout gives AI-P8 a predictable transfer unit and makes double buffering or multidimensional bulk transfer easier to prove. |

These are performance mechanisms, not automatic benefits. A transpose may
improve the consumer while adding a full read-plus-write conversion. Blocking
may improve reuse while increasing padding, boundary work, register pressure,
or shared-memory consumption. Logical-layout candidates must therefore remain
inside the common candidate, cost, plan, and fallback framework.

### 3.4.3 Inputs, Candidate Space, And Legality

AI-P3 consumes canonical TensorDescriptorIR identity, static or symbolic shape,
semantic tensor role, producer-consumer shape rules, effects, ownership,
lifetime/locality evidence, and REGION/control boundaries. Initial common
alternatives include axis permutation and divisible logical blocking. Packed
head and domain-provided layouts require their own reviewed contracts.

A check-only alternative is proven compatible only when its consumers are
representation-transparent under their logical operator contracts and the
required lifetime, effect, and ownership conditions are established. Unknown
control scope, dynamic dimensions, aliasing, effects, or consumer layout
requirements keep legality or profitability unknown. They must not be guessed
from Python source names or target-specific frontend annotations.

### 3.4.4 Cost, Selection, And Fallback

Static tensor size can determine conversion volume exactly. For example, a
`[16,16]` float32 tensor occupies 1024 bytes, so a separately materialized
layout conversion moves at least 1024 bytes in and 1024 bytes out. The 2048-byte
volume is useful evidence, but it is not itself a latency. Memory level,
bandwidth, cache residency, fusion, vector width, occupancy, and target
architecture determine the time.

AI-P3 records exact traffic when known and leaves latency incomplete until a
target cost model converts it into cycles, time, or another comparable AIO-2
unit. Every alternative names the unchanged semantic implementation as its
fallback. An incomplete plan remains inspectable but cannot displace the
complete executable baseline.

### 3.4.5 Boundary With Physical Layout

Logical layout describes a representation possibility and its constraints. It
does not choose HBM, L2, shared memory, registers, target-specific swizzles,
instruction fragments, or allocated storage. AI-P6 through AI-P9 own those
physical decisions after placement, residency, tiling, resource, and schedule
facts are available.

Check-only analysis must not create speculative persistent tensor types for
rejected alternatives. When a selected transformation is eventually applied,
the owner constructs the refined immutable tensor descriptor and calls
`TY_Intern_Tensor_Type()` so an identical canonical type is reused and the
original tensor type is never modified in place.

### 3.4.6 Downstream Consumers And Review Evidence

AI-P4 and AI-P5 combine layout with sharding and communication. AI-P6 chooses
local residency. AI-P7 refines layout into hierarchical tiles. AI-P8 uses the
regular access unit for transfer planning. AI-P9 selects the physical kernel
and schedule. Fusion candidate refinement may also consume layout
compatibility before finalizing a cluster.

The first reviewable implementation must show deterministic descriptors,
evolution edges, compatibility, conversion volume, incomplete target latency,
and baseline fallback. Because it is check-only, before/after/repeat binary
WHIRL and `ir_b2a -st -src` output must remain byte-identical. The focused
implementation contract is documented in
`AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md`.

## 3.5 Why Layout And Sharding Precede Communication

### 3.5.1 Optimization Problem

Distributed execution changes who owns each logical tensor element and when a
consumer can observe it. Replication, axis sharding, partial reduction, and
migration can all preserve the tensor's mathematical meaning while requiring
different communication. A collective cannot be selected correctly before the
compiler knows the ownership alternative that caused it.

AI-P4 therefore creates immutable, runtime-only placement and sharding
alternatives for a semantic tensor. Each alternative records device count,
shard axis, ownership class, per-device tile ranges, disjointness evidence,
and a visibility epoch. AI-P5 derives logical communication intent from that
alternative. Communication is not an unrelated frontend annotation and does
not become an executable collective during this check-only stage.

### 3.5.2 Performance Mechanisms

| Mechanism | Why ownership and communication matter |
| --- | --- |
| Parallel compute | Disjoint shards can divide independent output work across devices. Partial results can divide a contraction or reduction dimension. |
| Replication | A local replica can avoid repeated remote reads, but consumes capacity and requires an initial or refreshed copy. |
| Reduction | Partial ownership reduces local compute but requires a reduction collective before the complete value is visible. |
| Migration | Moving a uniquely owned value may improve producer-consumer locality, but introduces peer-copy latency and changes the visibility epoch. |
| Collective choice | Replication, disjoint distribution, partial reduction, and redistribution imply different AllGather, Scatter, AllReduce, ReduceScatter, AllToAll, or peer-copy intent. |
| Layout interaction | Axis order and packing determine whether a shard is contiguous and whether redistribution also needs transpose or packing work. |
| Fusion and residency | A fused cluster or retained resident value can eliminate a communication boundary; a split cluster can restore it. |

These are alternatives, not automatic wins. Communication volume,
synchronization, topology, bandwidth, capacity, overlap, and launch overhead
must be compared with the unchanged baseline through OptimizationPlanIR.

### 3.5.3 Inputs, Candidate Space, And Legality

AI-P4 consumes the semantic tensor root, canonical TensorDescriptorIR, shape
state, logical-layout evidence, producer shape rule, effects, ownership,
aliasing, lifetime/locality, and REGION/control scope. The first common slice
models replicated, axis-sharded, partial-reduction, and migrated ownership.
Tile ranges make disjointness or overlap explicit instead of inferring it from
a placement name.

Axis sharding is proven only for a static dimension divisible by the device
count. Partial reduction requires a contraction-producing logical operator.
Migration and disjoint ownership require proven unique ownership. Effects,
unknown aliasing, dynamic/non-divisible dimensions, and REGION boundaries are
rejected or remain unknown according to the missing proof; they are not guessed
from Python names or target annotations.

### 3.5.4 Communication Derivation, Cost, And Fallback

AI-P5 derives one logical communication intent in the alternative's visibility
epoch. The initial mapping is deliberately inspectable: replication implies
AllGather, axis sharding implies Scatter, partial reduction implies AllReduce,
and migration implies peer copy. Later topology-aware planning may refine a
logical intent into ReduceScatter, AllToAll, send/receive, RDMA, or an
implementation library without changing why the communication exists.

Known tensor size supplies a deterministic first communication-volume term.
This is low-confidence relative cost, not a target latency model. Every plan
retains the unchanged semantic implementation as fallback. Candidate
generation, communication derivation, plan selection, and transformation
application have independent controls; the first slice prohibits application.

### 3.5.5 Scope And Persistence Boundary

Ordinary compilation owns one active PU. AI-P4/P5 facts must therefore be
complete and verifiable for that PU without inspecting another PU's local
tables. Cross-PU placement summaries belong to an explicit later `-ipa` stage.

The first slice is runtime-only and check-only. It adds no ELF section, binary
WHIRL row, opcode, tensor type, executable collective, or frontend dependency.
Before/after/repeat `.B` and `ir_b2a -st -src` output must remain byte-identical.
Persistence or executable communication requires a separately reviewed
contract.

### 3.5.6 Downstream Consumers And Review Evidence

AI-P6 consumes placement when selecting local memory residency. AI-P7 through
AI-P9 combine ownership with tiles, transfers, pipelines, resources, and
schedules. AI-P10 may select among already-certified distributed variants.
Fusion refinement can eliminate or move an epoch only after proving the same
ownership and visibility semantics.

Review evidence must show replicated, disjoint, reduced, migrated, and unknown
ownership; exact or unknown ranges; deterministic epochs and communication;
explicit costs and fallback; effect, REGION, shape, control, and active-PU
negatives; and unchanged binary WHIRL. The focused implementation contract is
documented in `AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md`.

## 3.6 AI-P6 Memory Residency Rationale

### 3.6.1 Optimization Problem

Placement and sharding answer which device owns a tensor. They do not answer
where that device should keep the tensor during a particular producer-consumer
interval. A local tensor may remain in HBM, be expected to persist in L2, be
staged in shared memory, or be forwarded through registers. Host and pinned
host memory may also be legal fallbacks or staging sources. These choices have
different capacity, lifetime, ownership, alignment, spill, and eviction
requirements.

AI-P6 therefore separates a target-independent residency descriptor from typed
target capability adapters. It creates provisional alternatives and delays
allocation until later tiling, pipeline, resource, and implementation evidence
is available.

### 3.6.2 Performance Mechanisms

| Mechanism | Why residency matters |
| --- | --- |
| HBM traffic elimination | Keeping a reusable intermediate in an on-chip tier can avoid a full write/read round trip. |
| Latency hiding | Pinned-host, HBM, L2, and shared-memory choices determine which later prefetch and async-copy engines can overlap movement with compute. |
| Producer-consumer forwarding | Exact short lifetimes and unique ownership may permit shared-memory or register forwarding without materialization. |
| Capacity and occupancy | Shared-memory and register residency consume finite per-CTA/per-thread resources and can reduce active occupancy even when the object fits. |
| Cache reuse | An object that fits in L2 may benefit from persistence, but fit alone does not prove a hit because other live data can evict it. |
| Spill and eviction | A profitable fast tier needs a legal lower-tier fallback and explicit lifetime or pressure point for demotion. |
| Tiling | A full tensor may not fit in shared memory or registers while a later AIO-9 tile or fragment does. |

These mechanisms are conditional. A lower-latency tier can lose when promotion
cost, pressure, occupancy loss, synchronization, or eviction exceeds the saved
traffic. Residency must remain a candidate, not an allocation side effect.

### 3.6.3 Inputs, Candidate Space, And Legality

AI-P6 consumes canonical TensorDescriptorIR identity, static or symbolic object
size, access pattern, reuse benefit, lifetime/control scope, alias ownership,
placement/sharding results, and a typed target memory hierarchy. Initial tiers
are system, pinned host, HBM, L2, shared memory, and registers.

Known capacity overflow is rejected as a resource failure. Shared/register
alternatives additionally require exact basic-block lifetime and proven unique
ownership. Effects reject the candidate. Symbolic size and unknown capacity
remain incomplete rather than guessed. For cache tiers, capacity feasibility
means only that the object can fit; it does not promise retention.

### 3.6.4 Cost, Selection, And Fallback

The first target adapter supplies a relative latency class, capacity,
granularity, and alignment. Relative latency is low-confidence target-model
evidence, not cycles or nanoseconds. Every residency alternative retains the
unchanged baseline as fallback. Later phases refine the plan with simultaneous
live-set capacity, occupancy, tiling, transfer overlap, spill cost, and measured
cache behavior before transformation is allowed.

### 3.6.5 Scope, Persistence, And Evidence

Ordinary compilation remains PU-scoped. The active backend driver owns the PU
and local symbol table; AI-P6 does not inspect callers or callees. Cross-PU
residency analysis requires explicit IPA summaries.

The first implementation is runtime-only and check-only. It adds
`local_physical` TensorEvolutionGraph nodes but no persistent type, opcode, ELF
section, or executable allocation. CPU-baseline, Hopper, and Blackwell profiles
are accessed through typed APIs rather than free-form parsing. Before/after/
repeat `.B` and `ir_b2a -st -src` evidence remains byte-identical. The focused
contract is documented in `AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md`.

## 3.7 Why Runtime Variants Come Late

Runtime adaptation should not invent arbitrary schedules. The static compiler should generate legal variants, attach guards, and expose a selection policy. The runtime observes state and selects from certified variants.

The first implemented AI-P10 slice follows this rule for the selected AIO-11
rank-2 F4 `common.matmul.v1` plan. It preserves an unconditional direct
implementation and adds a certified cuBLASLt variant guarded by two runtime
buffer-alignment checks. Each check requires the corresponding operand address
to be at least 16-byte aligned. The checks contribute explicit
`runtime_selection` cost to AIO-2, and a false or missing observation reaches
the direct fallback. Canonical tensor TY alignment is not strengthened by a
runtime observation. This first slice is PU-local analysis and leaves binary
WHIRL unchanged; executable conditional dispatch is a later application step.

See `AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md` for the record schema,
invariants, compatibility boundary, and G15 certification.

RuntimeVariantIR definition, construction, structural verification, access,
and generic printing reside in `common/com`. PU fact capture, capability and
legality checks, profitability, selection, semantic verification, and guard
evaluation reside in VHO. This separation is normative for all AI optimization
IR families: common represents decisions; the phase owning the compilation
scope makes them.

# 4 Top Three AI Compiler Research Opportunities

The prior research opportunity analysis identified three architectural opportunities. They are not independent late passes. Each one spans the phase pipeline and requires persistent IR structures.

| Rank | Opportunity | Primary phases | Secondary phases | Required IR enhancements |
| --- | --- | --- | --- | --- |
| 1 | Joint fusion plus tiling plus parallelization | AI-P2, AI-P4, AI-P7, AI-P9 | AI-P3, AI-P6 | OptimizationCandidateIR; FusionCandidateIR; OptimizationPlanIR; CandidateCostIR |
| 2 | Tensor evolution across full hierarchy | AI-P0 through AI-P9 | AI-P10 and AI-P11 | TensorEvolutionGraph; TensorDescriptor core; RepresentationDescriptor hierarchy; MemoryHierarchyDescriptorIR |
| 3 | Static compiler plus runtime-adaptive scheduling | AI-P10 | AI-P1, AI-P4, AI-P6 through AI-P9, AI-P11 | RuntimeVariantIR; RuntimeGuardIR; RuntimeOptimizationStateIR; RuntimeSelectionPolicyIR |


## 4.1 Joint Fusion Plus Tiling Plus Parallelization

This opportunity covers fusion candidates that are refined by later layout, sharding, resource, and tiling decisions. A MatMul+Bias+GELU fusion may remove an HBM round trip, but may also increase register pressure and reduce occupancy. If the same intermediate tensor is a natural AllReduce, AllGather, or ReduceScatter boundary, fusion can also interfere with distributed execution.

Required enhancement: keep fusion as a persistent candidate rather than finalizing it in one pass. The FusionCandidateIR should hold operators, inputs, outputs, semantic legality, materialized bytes eliminated, alternative boundaries, layout constraints, parallelization constraints, tile families, estimated register growth, shared-memory growth, expected occupancy range, communication effect, and profitability state.

FusionCandidateIR { candidate_id; operators; inputs; outputs; semantic_legality; materialized_bytes_eliminated; required_layout_constraints; parallelization_constraints; candidate_tile_families; estimated_register_growth; estimated_shared_memory_growth; expected_occupancy_range; communication_effect; profitability_state; }

## 4.2 Tensor Evolution Across The Full Hierarchy

This is the highest-priority structural opportunity because it gives all later optimizations a shared object-lineage model. The compiler must preserve the identity of one logical tensor as it becomes a logical layout, distributed shard, local HBM representation, CTA tile, shared-memory tile, warp tile, register fragment, and tensor-core fragment.

Required enhancement: introduce TensorEvolutionGraph instead of a flat tensor metadata record. Nodes represent abstraction levels; edges represent transformations such as shard, replicate, reshape, layout cast, gather, reduce, materialize, tile, prefetch, promote memory, and demote memory.

TensorEvolutionNode { evolution_id; source_tensor_id; parent_evolution_id; abstraction_level; descriptor; transformation; validity_conditions; cost_snapshot; lineage; }

TensorEvolutionEdge { from; to; transformation_kind; preserves_semantics; communication_required; data_movement_bytes; }

## 4.3 Static Compiler Plus Runtime Adaptive Scheduling

LLM inference and serving workloads depend on runtime state that cannot be fully fixed at compile time: active batch, request sequence length, decode position, KV-cache pressure, prefix-cache hit rate, expert load, available HBM, network contention, and latency SLO. The compiler should generate legal variants and the runtime should select among them with guards.

RuntimeVariantIR { variant_id; applies_when; schedule; predicted_cost; resource_requirements; fallback_variant; }

RuntimeGuardIR { condition; required_properties; failure_action; }

# 5 Comprehensive IR Requirements And Ownership Boundaries

The ownership boundary is the most important governance rule in this design. Common substrate IR should be reusable by AI, FHE, and other domains. It describes optimization control, object lineage, memory hierarchy, candidate plans, costs, runtime guards, and generic communication intent. AI-domain-specific IR should describe AI semantics and serving state that a generic compiler should not own.

## 5.1 Common Compiler Substrate Candidates

| IR object | Tag | Ownership detail |
| --- | --- | --- |
| Tensor or Object descriptor core | Common Compiler Substrate | Stable identity, logical shape or object extent, dtype or element type, producers, consumers, aliasing, lifetime anchor. |
| TensorEvolutionGraph | Common Compiler Substrate | Object lineage across semantic, logical, distributed, local, tile, and execution-fragment forms. |
| RepresentationDescriptor hierarchy | Common Compiler Substrate | LayoutDescriptor, ShardingDescriptor, PlacementDescriptor, ResidencyDescriptor, TileDescriptor. |
| CommonLayoutIR | Common Compiler Substrate | Reusable layout names, strides, blocking, packing, swizzle, and alignment constraints. |
| CommonPlacementIR | Common Compiler Substrate | Device, NUMA, GPU, remote memory, replicated, partitioned, or staged placement. |
| CommonMemoryResidencyIR | Common Compiler Substrate | Home tier, preferred tier, capacity, lifetime, eviction point, spill policy. |
| CommonAccessPatternIR | Common Compiler Substrate | Sequential, tiled, strided, gather, scatter, reuse distance, producer-consumer timing. |
| CommonFetchPlanIR | Common Compiler Substrate | Prefetch, demand fetch, async transfer, promotion, demotion, staging, overlap window. |
| CommonTilePlanIR | Common Compiler Substrate | Hierarchical tiling alternatives across problem, device, CTA/threadblock, warp/warpgroup, thread, register fragment, tensor-core instruction, and memory-pipeline stages. It records candidate tile shapes, split/reduction axes, buffering depth, swizzle/layout assumptions, resource usage, legality, and cost-model inputs before transformation is committed. |
| CommonDistributedAliasIR | Common Compiler Substrate | Range-level alias, ownership, mod/ref/reduce, and visibility facts for tensor tiles across ranks, devices, sockets, boards, or nodes. It records owned, ghost, replicated, reduced, migrated, and unknown regions plus communication epochs and runtime guards. |
| CommonPipelineIR | Common Compiler Substrate | Pipeline stages, buffering depth, async barriers, dependency edges, overlap opportunities. |
| CommonParallelizationIR | Common Compiler Substrate | Partition axes, replication, work ownership, mapping to resources, parallel region boundaries. |
| CommonCommunicationIntentIR | Common Compiler Substrate | Collective or peer communication intent before backend-specific communication lowering. |
| OptimizationCandidateIR | Common Compiler Substrate | Candidate tree that records semantic, layout, placement, parallelization, tile, memory, and pipeline alternatives. |
| FusionCandidateIR | Common Compiler Substrate where generic | Generic operator-region fusion fields belong in common substrate; AI legality and semantic classes are AI-specific extensions. |
| OptimizationPlanIR | Common Compiler Substrate | The unit of end-to-end cost evaluation across choices. |
| CandidateCostIR and CommonCostModelIR | Common Compiler Substrate | Compute, memory, communication, sync, launch, overlap, resource, and confidence estimates. |
| MemoryHierarchyDescriptorIR | Common Compiler Substrate | Tier scope, capacity, bandwidth, latency, granularity, alignment, bank structure, async capability. |
| ComputeResourceIR | Common Compiler Substrate | Device, SM, vector unit, tensor core, thread, warp, CTA, stream, and occupancy-relevant resource descriptions. |
| RuntimeGuardIR | Common Compiler Substrate | Generic guards, validity ranges, fallback behavior, and failure actions. |
| Generic runtime-variant infrastructure | Common Compiler Substrate | Variant identity, applicability, selection policy hooks, hysteresis, fallback, telemetry binding. |


## 5.2 AI Domain Specific IR

| IR object | Tag | Ownership detail |
| --- | --- | --- |
| AIWorkloadStateIR | AI-domain-specific | Batch size, sequence length distribution, decode position, prefill/decode phase, request mix. |
| KVCacheStateIR | AI-domain-specific | KV-cache used/free, pressure, page residency, prefix-cache hit rate, eviction risk. |
| AttentionSemanticIR | AI-domain-specific | Q/K/V roles, attention mask, head grouping, context length, causal/prefix semantics. |
| MoESemanticIR | AI-domain-specific | Expert load, routing distribution, capacity factor, token dispatch, combine semantics. |
| AIServingSLOIR | AI-domain-specific | Latency SLO, throughput objective, fairness, queueing budget, admission-control interaction. |
| AICostModelExtensions | AI-domain-specific | Attention arithmetic, KV paging penalties, MoE imbalance, prefill/decode asymmetry, quantization-specific throughput. |
| AIDomainLegalityIR | AI-domain-specific | Legality rules for attention, MoE, KV-cache updates, residual compatibility, and numerical tolerance. |


## 5.3 Ownership Boundary Rules

- Common substrate owns structure, lineage, candidate composition, generic cost terms, generic runtime guards, and generic memory/communication concepts.

- AI domain owns model semantics, serving state, workload-specific legality, and cost-model extensions that depend on attention, MoE, KV cache, or AI-specific numerical behavior.

- A common IR object can be extended by AI traits, but common fields must remain meaningful outside AI.

- Research-derived mechanisms should be recorded as inspiration; DSC-specific proposals should be recorded as design commitments or hypotheses only after review.

# 6 Phase To IR Matrix

| Phase | Primary IR | Key information |
| --- | --- | --- |
| AI-P0 Semantic tensor | Tensor descriptor core, AI semantic descriptors, TensorEvolutionGraph root | Semantic roles, shape, dtype, dynamic dimensions |
| AI-P1 Lifetime reuse | CommonAccessPatternIR, CommonMemoryResidencyIR, CandidateCostIR seed | Reuse distance, last use, working set |
| AI-P2 High-level fusion | FusionCandidateIR, OptimizationCandidateIR | Candidate state unknown, semantic legality |
| AI-P3 Logical layout | CommonLayoutIR, RepresentationDescriptor hierarchy | Logical layout alternatives |
| AI-P4 Distributed placement | CommonPlacementIR, CommonParallelizationIR, CommonDistributedAliasIR, TensorEvolutionGraph | Sharding, replication, device ownership, tile-range ownership |
| AI-P5 Communication derivation | CommonCommunicationIntentIR, CommonDistributedAliasIR, CandidateCostIR | Collectives, peer transfers, and visibility epochs implied by sharding |
| AI-P6 Local layout residency | CommonMemoryResidencyIR, MemoryHierarchyDescriptorIR | HBM, L2, shared, registers, CPU, remote memory |
| AI-P7 Multi-level tiling | CommonTilePlanIR, CommonAccessPatternIR, CommonLayoutIR, CommonMemoryResidencyIR, ComputeResourceIR, CandidateCostIR | Output/reduction/CTA/SMEM/warp/thread/register/vector tile families, coalescing, arithmetic intensity, occupancy, resource pressure |
| AI-P8 Fetch prefetch async | CommonFetchPlanIR, CommonPipelineIR, CommonDistributedAliasIR | Async movement, buffering depth, overlap windows, safe early fetch windows |
| AI-P9 Kernel scheduling | OptimizationPlanIR, CommonPipelineIR, ComputeResourceIR | Final physical realization |
| AI-P10 Runtime variants | RuntimeVariantIR, RuntimeGuardIR, RuntimeSelectionPolicyIR, CommonDistributedAliasIR | Certified variants, selection guards, guarded no-alias and disjoint-tile predicates |
| AI-P11 Telemetry feedback | TelemetryProfileIR, CandidateCostIR updates | Measured traffic, stalls, overlap, selected variants |


# 6A Refined AI-P7 Multi-Level Tiling

AI-P7 should be treated as a structured tile-plan construction phase, not a single tiling pass. Its job is to produce CommonTilePlanIR alternatives that later phases can evaluate with the cost model. The CUDA MMM worklog by Simon Boehm is a useful concrete mapping because it manually walks through the same hierarchy that the compiler should represent: global-memory coalescing, shared-memory tiling, thread/register tiling, vectorized access, autotuning, and warp tiling.

AI-P7 receives logical layout, placement, ownership, and local residency constraints from AI-P3 through AI-P6. It should not finalize the physical kernel by itself. Instead it creates tile-plan candidates with enough structure for AI-P8 prefetch/pipeline planning, AI-P9 scheduling, and AI-P10 runtime variant selection.

## 6A.1 Tiling Sub-Phases

| P7 sub-phase | Purpose | CUDA MMM analogue | Primary IR fields |
| --- | --- | --- | --- |
| P7.0 Baseline access characterization | Describe the naive mapping, memory traffic, arithmetic intensity, and reuse opportunity before changing the plan. | Kernel 1 naive implementation and lower-bound math. | CommonAccessPatternIR; CandidateCostIR; TensorEvolutionGraph roots. |
| P7.1 Output tile and thread-to-element mapping | Choose how result elements are assigned to blocks, warps, and threads. | Naive one-thread-per-C element mapping. | CommonTilePlanIR output_tile_shape; thread_ownership; boundary_predication. |
| P7.2 Global-memory coalescing tile | Remap lanes so warp-level accesses become contiguous and aligned where possible. | Kernel 2 GMEM coalescing. | CommonAccessPatternIR lane_to_address_map; coalescing_width; alignment_assumption; transaction_width. |
| P7.3 Reduction-axis tile | Split the K/reduction dimension into reusable chunks. | BLOCKSIZE/BK loop over A columns and B rows. | CommonTilePlanIR reduction_tile_shape; split_axis; accumulation_scope. |
| P7.4 Shared-memory tile and prefetch hint | Promote A/B chunks from GMEM to SMEM, define SMEM tile shape/layout, and emit advice for later async prefetch planning. | Kernel 3 shared-memory cache blocking; later TMA/cp.async pipeline realization. | CommonMemoryResidencyIR; CommonLayoutIR smem_layout; CommonTilePlanIR smem_tile_shape; reuse_distance; tma_legality; buffering_feasibility; synchronization_points. |
| P7.5 Occupancy and resource feasibility | Estimate whether tile choices fit SMEM, registers, warp limits, CTA limits, and launch-grid utilization. | Occupancy calculation after Kernel 3. | ComputeResourceIR; CandidateCostIR occupancy_terms; resource_limits. |
| P7.6 Per-thread register tile | Assign each thread multiple output elements and accumulate in registers. | Kernel 4 1D blocktiling and Kernel 5 2D blocktiling. | CommonTilePlanIR thread_tile_shape; register_fragment_shape; accumulator_footprint. |
| P7.7 Register reuse and inner-loop ordering | Order dot/reduction loops to reuse SMEM values through register fragments. | Kernel 5 register caching and outer-product accumulation. | CommonTilePlanIR inner_loop_order; register_reuse_factor; smem_access_count. |
| P7.8 Vectorized memory-access tile | Require alignment/layout conditions for wider GMEM or SMEM transactions. | Kernel 6 transposed As, LDS.128, LDG.E.128, STG.E.128, float4. | CommonAccessPatternIR vector_width; RuntimeGuardIR alignment_guard; CommonLayoutIR transposed_smem_layout. |
| P7.9 Autotuned tile family | Keep BM, BN, BK, TM, TN and related tile parameters as candidate families rather than one fixed answer. | Kernel 9 autotuning. | OptimizationCandidateIR tile_family; CandidateCostIR confidence; RuntimeVariantIR candidate_id. |
| P7.10 Warp tile | Add an explicit warp-level tile between CTA tile and thread tile. | Kernel 10 warptiling. | CommonTilePlanIR warp_tile_shape; warp_lane_map; warp_scheduler_model; bank_conflict_terms. |
| P7.11 Instruction or tensor-core tile | Map the innermost tile to vector, FMA, WMMA, MMA, or WGMMA instruction fragments. | Discussed as the natural next mapping from warp tiling to tensor-core instructions. | CommonTilePlanIR instruction_tile_shape; fragment_descriptor; tensor_core_family. |

## 6A.2 CommonTilePlanIR Fields Required By The CUDA MMM Mapping

CommonTilePlanIR should be detailed enough to represent the following fields directly:

- problem_tile_shape: M, N, K tile shape at the logical operation level.

- output_tile_shape: C tile computed by one CTA, warp, thread, and instruction fragment.

- reduction_tile_shape: BK or equivalent reduction-axis chunk.

- cta_tile_shape: BM, BN, BK and CTA thread count.

- warp_tile_shape: warp-level M/N tile, warp row/column assignment, lane mapping.

- thread_tile_shape: TM, TN and per-thread output ownership.

- register_fragment_shape: accumulator array shape, A/B register cache shape, temporary footprint.

- shared_memory_tile_shape: A/B SMEM tile shapes, bytes per stage, double-buffering compatibility.

- layout_transform: transposed SMEM layout, swizzle, padding, or bank-conflict avoidance transform.

- low_precision_format: dtype family such as BF16, FP8, MXFP8, MXFP4, NVFP4, accumulation dtype, packing alignment, and supported Tensor Core instruction family.

- scale_tile_shape: scale-factor tensor tile shape, scale granularity, scale reuse scope, and scale layout compatibility when the value format uses block or micro-tensor scaling.

- effective_tile_bytes: packed value bytes plus scale metadata bytes, layout padding, alignment padding, and conversion or repacking traffic.

- smem_prefetch_hint: producer-consumer distance, reuse distance, expected stage count, and whether TMA, cp.async, vectorized LDG, or scalar fallback should be evaluated.

- tma_legality: affine source range, rectangular or regularly strided tile, compatible destination layout, required alignment, boundary handling, and alias-safety conditions.

- buffering_feasibility: one-stage, two-stage, or N-stage buffering feasibility under SMEM, register, CTA, and occupancy limits.

- access_vectorization: scalar, float2, float4, LDG/STG width, LDS width, alignment requirement.

- coalescing_model: lane-to-address mapping, transaction width, contiguous-lane count, wasted transaction estimate.

- occupancy_model_inputs: registers/thread, SMEM/CTA, CTAs/SM, warps/SM, launch grid, wave quantization.

- synchronization_model: barriers needed for SMEM tile population, SMEM reuse, and tile overwrite safety.

- arithmetic_intensity_model: FLOPs per GMEM byte, FLOPs per SMEM byte, reuse factors by tile level.

- autotune_dimensions: tunable symbols such as BM, BN, BK, TM, TN, warp tile shape, vector width, stages.

- legality_guards: divisibility, bounds masking, pointer alignment, dtype alignment, layout compatibility.

## 6A.3 Mapping To Later Phases

P7 creates the tile candidates, but later phases still decide whether and how to use them.

- AI-P8 consumes P7 tile shapes to build fetch, prefetch, double-buffering, cp.async/TMA, and SMEM-to-register pipeline plans.

- AI-P9 consumes P7 tile shapes to choose final loop order, synchronization placement, warp scheduling, instruction shape, and physical kernel realization.

- AI-P10 consumes P7 autotune dimensions and legality guards to build runtime variants keyed by hardware, shape, dtype, alignment, and measured performance.

- AI-P11 feeds measured occupancy, memory transactions, bank conflicts, achieved FLOPs, and guard hit rates back into CandidateCostIR.

The design lesson from CUDA MMM is that every tiling decision is entangled with memory layout, access pattern, residency, and scheduling. Therefore AI-P7 should create rich CommonTilePlanIR alternatives and defer final selection until cost-model and runtime-variant information are available.

## 6A.4 P7.4 To P8 Prefetch Hint Contract

P7.4 should not generate the async copy schedule directly. Its responsibility is to preserve enough tile structure for P8 to decide whether a TMA, cp.async, vectorized-load, or scalar fallback movement plan can hide memory latency. This is the same delayed-transformation principle as the rest of the design: P7.4 describes what is worth staging in shared memory; P8 decides how early and by which hardware engine it should be moved.

For each shared-memory tile candidate, P7.4 should emit:

- smem_tile_shape: A and B tile dimensions, bytes per stage, and edge-tile policy.

- smem_layout: canonical, transposed, swizzled, padded, or bank-conflict-avoiding layout.

- scale_tile_layout: associated scale-factor tile layout and whether it can be staged with, before, or independently from the packed value tile.

- tile_reuse_profile: consumers per staged tile, reuse distance, arithmetic intensity contribution, and expected SMEM-to-register access count.

- prefetch_distance_hint: the candidate number of K-tiles or pipeline stages to fetch ahead.

- buffering_feasibility: whether one-stage, double-buffered, triple-buffered, or deeper staging fits under SMEM, register, occupancy, and barrier constraints.

- tma_candidate: whether the source and destination form a regular multidimensional transfer suitable for TMA.

- tma_legality: affine source range, rectangular or strided tile shape, alignment, compatible destination layout, boundary policy, and alias-safety conditions.

- arrival_and_wait_requirements: barrier or mbarrier requirements between async arrival and consuming warp, warpgroup, or CTA.

- fallback_movement_candidates: cp.async, vectorized LDG plus SMEM store, ordinary LDG/STG, or demand load when TMA is illegal or not profitable.

P8 consumes those hints to build CommonFetchPlanIR and CommonPipelineIR. A P8 plan should bind the selected movement engine, source/destination memory levels, stage count, prefetch distance, arrival barrier, consumer wait point, edge-tile behavior, and fallback path. The profitability model should charge only the unhidden portion of movement latency, so the plan selection explicitly asks whether the P7.4 tile and P8 pipeline together reduce compute-unit bubbles.

## 6B AI-P8 Fetch And Async-Pipeline Rationale

### 6B.1 Optimization Problem And Inputs

A legal tile does not by itself hide the latency required to fill that tile.
Demand movement can leave compute units idle, while an overly early fetch can
race a producer, cross a communication epoch, consume excessive shared memory,
or add synchronization that costs more than the hidden latency. AI-P8 consumes
the selected P7 tile, lifetime and ownership facts, communication visibility,
memory tiers, target movement capabilities, and resource limits. It must not
rediscover or silently replace the P7 tile.

### 6B.2 Candidate Space And Performance Mechanism

The initial alternatives are demand load, vector load, asynchronous copy, and
multidimensional asynchronous transfer. Each plan binds source and destination
memory tiers, transaction size, buffering depth, prefetch distance, issue
point, arrival barrier, consumer wait, boundary policy, and fallback. Deeper
buffering can overlap more movement with computation, but consumes more
capacity and synchronization resources.

### 6B.3 Legality, Cost, And Fallback

Early movement requires proven lifetime, unique ownership, effect freedom,
communication-epoch safety, target capability, and capacity. AIO-10 records
the movement engine's minimum-alignment requirement, but exact address and
storage alignment remain physical-plan and target-lowering checks. An async
prefetch distance must remain within its buffer-stage window. Unknown or unsafe
evidence never becomes implicit permission. The demand plan is the conservative
fallback.

CandidateCostIR stores raw, hidden, and unhidden movement separately. Initial
static estimates are low-confidence relative costs; they exercise complete
plan selection but are not performance claims. AIO-P9 must compare the full
tile, movement, synchronization, resource, and kernel plan before executable
mutation.

### 6B.4 Ownership Boundary And Evidence

AI-P8 owns generic fetch and pipeline planning. It does not emit target
instructions, allocate shared memory, insert barriers, or choose a provider.
The first implementation is PU-local and runtime-only. Review evidence shows
single/double/triple buffering, target fallback, unsafe early-fetch rejection,
raw/hidden/unhidden cost, deterministic selection, and byte-identical binary
WHIRL. The concrete first-slice contract is in
`AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md`.

## 6C AI-P9 Physical Plan And Implementation Selection Rationale

### 6C.1 Optimization Problem And Inputs

A legal tile and movement pipeline do not identify the implementation that
should execute them. The compiler may retain direct DSL execution, generate a
kernel, call a reviewed library, select a Triton implementation, or use an
existing target kernel. AI-P9 consumes the semantic operator and tensor
identity, selected tile, selected movement pipeline, complete resource and
synchronization costs, target profile, and reviewed provider capabilities. It
must compare complete implementations rather than choose one local scheduling
decision in isolation.

### 6C.2 Candidate Space And Performance Mechanism

Each alternative binds a provider, implementation kind, target capability,
tile, movement plan, schedule, fallback, and stable identity. A generated
kernel can exploit compiler-selected tiling and overlap. A library may provide
a highly tuned implementation but add launch, workspace, layout, and ABI
costs. An existing kernel can avoid compilation cost but may constrain shapes
and layouts. The direct implementation remains the semantic baseline.

The first implementation uses deterministic low-confidence relative compute,
memory, synchronization, and launch terms. These terms establish complete-plan
selection and do not claim measured performance. Later target models,
profiling, and autotuning may refine the estimates without weakening legality.

### 6C.3 Legality, Cost, And Fallback

Selection requires an exact versioned capability match for logical operator,
dtype, rank, target profile, and runtime availability. Generated plans also
require proven selected tile and pipeline legality. Unknown evidence remains
unknown; provider mismatch and unavailability are explicit rejections. Every
nonbaseline plan names the direct implementation as a deterministic fallback.
`-O0` keeps only that direct baseline and performs no optional search.

### 6C.4 Ownership Boundary And Evidence

AI-P9 owns physical implementation comparison and selection. It does not by
itself emit provider calls, generate instructions, allocate storage, or bind a
runtime ABI. The first selector is PU-local and runtime-only, so provider names
appear in analysis evidence but never in executable WHIRL. Each later provider
or generated-kernel family must add a reviewed atomic prepare/apply/postprocess
transaction, concrete target and ABI validation, and retained before/after
`.B` and `ir_b2a -st -src` evidence. The concrete selector contract is in
`AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md`.

# 7 Candidate Generation And Profitability Architecture

Candidate generation should produce a candidate tree rather than a sequence of independent pass decisions. Fusion, layout, placement, sharding, tiling, memory residency, fetch, pipeline, communication, and runtime-variant choices become branches within OptimizationCandidateIR and are evaluated as OptimizationPlanIR instances.

OptimizationPlanIR { plan_id; semantic_region; tensor_evolution_graph; fusion_choices; layout_choices; placement_choices; sharding_choices; communication_choices; tile_choices; memory_residency_choices; fetch_prefetch_choices; pipeline_choices; kernel_choices; estimated_cost; legality; runtime_guards; }

## 7.1 Generic Fusion Candidate Discovery

The scalable AI-P2 implementation should perform the following work inside one
active PU:

1. Enumerate producer-consumer edges from the logical DSL value graph.
2. Query versioned operator fusibility contracts rather than switch on every
   operator-name sequence.
3. Reject edges with incompatible iteration or indexing relations, tensor
   descriptors, layouts, effects, ownership, control scope, REGION ownership,
   numerical contracts, or resource requirements.
4. Seed legal two-node candidates and grow larger clusters in deterministic
   graph order under explicit candidate and plan budgets.
5. Handle fan-out, diamonds, multiple results, and external uses through
   explicit cluster boundaries and conservative post-dominator/use analysis.
6. Record eliminated materializations, changed live ranges, alternative cuts,
   representation constraints, and incomplete evidence.
7. Publish baseline and fusion alternatives into OptimizationPlanIR; do not
   rewrite WHIRL during discovery or local selection.
8. Refine candidates when AI-P3 layout, AI-P4/P5 placement and communication,
   AI-P7 tiling, and AI-P9 target-resource information become available.

The initial AIO-5 implementation deliberately recognizes
`matmul+bias+activation` and `residual+activation` explicitly. Its purpose is
to establish the basic optimization-process skeleton: consume semantic and
locality analyses, describe members and boundaries, prove or reject legality,
construct baseline and alternative plans, attach costs and fallbacks, select a
complete plan, honor independent controls, and prove that check-only analysis
does not mutate binary WHIRL. It is not the final scalable fusion matcher and
does not imply that future operator combinations should all be encoded as
hand-written patterns.

Explicit pattern recognition remains above the generic engine for semantic
compounds and implementation idioms. A recognized pattern may seed a cluster,
add stronger legality requirements, or offer a provider alternative, but it
must use the same candidate, cost, fallback, and plan-selection services.

## 7.2 Cost Model Gate

The optimizer must not commit a fusion, tiling, layout, placement, sharding, memory-residency, fetch/prefetch, communication, or runtime-variant decision only because the transformation is locally legal. Selection requires an explicit cost record and a legality record on the whole candidate plan. This is the main design lesson drawn from Open64 LNO: candidate transformations should remain representational until enough dependence, reuse, memory hierarchy, loop/tile, and machine-resource information has been collected to compare plans.

This gate applies to static and runtime-adaptive plans. A static selected plan requires CandidateCostIR with sufficient confidence for the target hardware. A runtime variant requires both CandidateCostIR and RuntimeGuardIR so that the runtime can select only among certified variants.

## 7.3 Latency Equation

Use an explicit plan-level latency model so the optimizer can reason about overlap rather than simply summing all component costs.

T_total = T_compute + T_memory_unhidden + T_communication_unhidden + T_sync + T_launch + T_runtime_selection

T_memory_unhidden = max(0, T_memory_raw - T_memory_hidden_by_compute - T_memory_hidden_by_communication)

T_communication_unhidden = max(0, T_communication_raw - T_communication_hidden_by_compute - T_communication_hidden_by_memory)

T_compute = max(T_tensor_core, T_cuda_core, T_vector, T_reduction) adjusted by occupancy and issue constraints

For low-precision formats such as Blackwell NVFP4, the model must not only scale bytes by dtype width. It should expand the equation with scale metadata and layout terms:

T_total(dtype, tile) = T_compute(dtype, instruction_tile) + T_scale_metadata_load + T_scale_apply_or_fused_scale + T_memory_unhidden(value_tile + scale_tile) + T_sync + T_launch + T_runtime_selection

CandidateCostIR should store raw and unhidden terms separately. A plan with higher raw memory traffic can still win if the traffic is hidden by a stronger async pipeline; a plan with lower raw traffic can lose if synchronization or occupancy loss makes stalls visible.

## 7.4 Profitability Decision Timing

Early phases create candidates. Later phases refine candidates. Final selection should occur only after the optimizer can estimate register count, shared-memory footprint, occupancy, communication effect, memory overlap, synchronization, launch count, and runtime guard coverage.

# 7A CommonTilePlanIR And Hardware Cost Inputs

CommonTilePlanIR is the persistent representation of tiling choices before code transformation. It should be created early enough for fusion, layout, sharding, residency, and fetch planning to attach constraints, but selected late enough that the cost model can compare full plans.

CommonTilePlanIR should include:

- semantic region and tensor evolution nodes covered by the tile plan.

- logical tile shape over domain axes, including split/reduction axes.

- distributed tile shape across devices, CTAs, clusters, and pipeline stages.

- CTA or threadblock tile shape, including CTA count, cluster shape, and grid scheduling assumptions.

- warp or warpgroup tile shape and mapping to lanes.

- thread tile shape, per-thread output ownership, and register-fragment shape.

- tensor-core or vector-instruction tile shape, including MMA/WGMMA/WMMA instruction family where relevant.

- low-precision value tile and scale tile structure, including value dtype, accumulation dtype, block-scale granularity, scale-factor layout, scale reuse, and alignment requirements.

- shared-memory staging shape, swizzle, bank-conflict assumptions, and bytes per stage.

- register use, accumulator footprint, temporary footprint, and max register constraint.

- num_warps, num_stages, num_ctas, pipeline depth, async-copy/TMA usage, and barrier requirements.

- occupancy estimate, active CTA/warp/warpgroup estimate, launch-grid utilization, wave quantization, and tail inefficiency.

- memory traffic by tier: HBM/global, L2, shared memory, tensor memory if available, registers, and remote/peer memory.

- overlap assumptions: compute-memory, compute-communication, producer-consumer warp specialization, and epilogue overlap.

- legality constraints: dependence preservation, alignment, layout compatibility, boundary predication, synchronization, and hardware capability.

For NVIDIA-family GPUs, CandidateCostIR should explicitly model or store inputs for:

- SM count and compute capability.

- warp size and maximum resident warps/threads/CTAs per SM.

- register file size, register allocation granularity, max registers per thread, and spill risk.

- shared-memory capacity per CTA, per-SM carveout, bank structure, and dynamic shared-memory requirements.

- L2 behavior and grid/threadblock swizzling opportunities.

- Tensor Core instruction family and fragment shape.

- async copy or TMA availability, transaction granularity, mbarrier requirements, and pipeline-stage count.

- CTA cluster and distributed shared memory availability on newer architectures.

- tensor memory or architecture-specific storage where exposed by the backend.

- low-precision format support such as FP8, MXFP8, MXFP4, NVFP4, FP6, and backend-specific Tensor Core instruction availability.

- synchronization and fence costs, including cross-proxy or async-memory ordering costs.

- occupancy, wave quantization, and persistent-kernel scheduling constraints.

The important boundary is that CommonTilePlanIR records the alternatives and constraints. The backend-specific cost extension interprets them for NVIDIA, AMD, CPU, or future accelerators.

# 7B Blackwell FP4 Low Precision Cost Extension

Blackwell FP4 and NVFP4 should be modeled as a tiling and pipeline change, not merely as a smaller dtype. Native FP4 Tensor Core throughput can make compute much cheaper relative to movement, so the winning plan may shift from compute-maximizing tiles toward tiles that better hide value movement, scale metadata movement, synchronization, and epilogue work.

For Blackwell-class NVFP4 candidates, CandidateCostIR should add a LowPrecisionFormatCostExtension with:

- value dtype: NVFP4, MXFP4, FP8, BF16, FP16, or fallback format.

- accumulation dtype and epilogue dtype.

- value bits, packed value bytes per tile, and alignment or packing constraints.

- scale granularity, scale-factor dtype, scale tile shape, scale layout, and scale metadata bytes per tile.

- scale reuse distance and whether scale factors are consumed at CTA, warp, warpgroup, or instruction-fragment scope.

- Tensor Core instruction family, instruction tile shape, and achievable issue rate for the selected dtype.

- TMA or async-copy legality for both packed value tensors and scale-factor tensors.

- scale application cost, fused-scale availability, dequant/requant cost, and epilogue conversion cost.

- effective arithmetic intensity computed as tensor operations divided by packed value bytes plus scale metadata, padding, and conversion traffic.

The expected tiling consequence is conditional rather than automatic. FP4 may allow a larger reduction tile or deeper staging because packed values use fewer bytes, but that only wins when scale metadata is regular, reusable, and hidden by the pipeline. A larger BK tile can lose if it increases scale-factor traffic, register pressure, barrier pressure, or edge-tile complexity. Therefore P7 should generate multiple FP4 tile families, including large-BK TMA-oriented variants, smaller-BK higher-occupancy variants, scale-reuse-optimized variants, and FP8 or BF16 fallback variants.

P7.4 should also emit prefetch hints for the scale tensors. P8 must decide whether value tiles and scale tiles move together, whether scale tiles are prefetched earlier than value tiles, and whether both transfers are legal TMA candidates. P10 should preserve FP4 runtime variants because profitability depends on hardware generation, tensor shape, alignment, batch size, sequence length, scale layout, and measured hidden versus unhidden movement.

# 7C Distributed Alias Ownership And Prefetch Scheduling

Distributed alias and ownership information should be established earlier than hardware prefetch scheduling. It is part of the correctness substrate for MPI-style or rank-style parallelization across sockets, GPU boards, nodes, racks, or other motherboard-boundary domains. It answers who owns a tensor tile range, who may read it, who may write it, whether it is private, ghost, replicated, reduced, migrated, or unknown, and at what communication epoch the data becomes visible.

Prefetch scheduling comes later, during hardware board-level optimization. It consumes the distributed alias and ownership facts after tiling, placement, residency, pipeline, stream, DMA/NVLink/RDMA, and synchronization choices are sufficiently concrete. The prefetch scheduler should not rediscover global ownership. It should ask whether a particular tile range can be fetched early without racing a producer, observing stale data, invalidating a reduction, or crossing an unresolved communication epoch.

CommonDistributedAliasIR should include:

- object_id and tensor evolution node.

- base object and representation descriptor.

- tile or memory range expression, preferably affine when available.

- owner rank, device, socket, board, node, or rank group.

- access mode: read, write, readwrite, reduce, atomic, or unknown.

- region role: private, owned, ghost, replicated, reduced, migrated, remote, or unknown.

- alias class: noalias, may_alias, must_alias, disjoint_by_rank, disjoint_by_range, guarded_noalias, or unknown.

- producer epoch and consumer epoch.

- visibility condition: collective completion, send/receive completion, DMA event, stream event, barrier, fence, or runtime guard.

- communication source: none, send/receive, broadcast, allgather, reducescatter, allreduce, alltoall, peer copy, RDMA, or runtime-managed migration.

- separation proof: rank ownership, disjoint affine range, layout partition, type/object identity, runtime guard, or profile-derived hypothesis.

- prefetch safety window: earliest legal fetch point, latest useful fetch point, invalidation point, and producer-consumer hazard status.

This gives the common substrate a bridge between Open64-style parallel-region variable information and MPI-style distributed-memory planning. Open64's SMP/OpenMP model needs a region variable list because the memory space is shared. DSC's distributed model needs range-and-epoch alias facts because parallelization crosses device, board, and node boundaries before the late hardware scheduler decides how to hide latency.

# 7D Open64 Loop Multiversioning Design Lesson

Open64 loop multiversioning provides a useful precedent for AI-P10 runtime variants. When static alias analysis cannot prove that memory ranges are disjoint, Open64 does not have to permanently choose the conservative plan. It can generate two versions of the loop, attach a runtime range-disjointness predicate, and select the faster no-alias version only when the predicate proves that the relevant memory ranges do not overlap. Otherwise, execution falls back to the conservative version.

The design lesson for DSC is to treat guarded runtime selection as a first-class optimization mechanism, not only as a late code-generation trick. If a plan is profitable under a stronger alias, ownership, shape, residency, or communication-epoch assumption, the compiler may keep both the guarded fast plan and the conservative fallback as OptimizationPlanIR alternatives. RuntimeGuardIR records the predicate, and CommonDistributedAliasIR records the tile-range facts that the predicate proves or depends on.

The AI and distributed-memory analogue is:

- guarded no-alias tile plan: tensor tile ranges are disjoint by rank, affine range, layout partition, or runtime guard.

- guarded prefetch plan: a tile can be fetched early because the producer epoch, invalidation point, and visibility condition are satisfied.

- guarded communication-overlap plan: a receive, peer copy, or collective result is known visible before the consumer tile executes.

- fallback plan: use conservative ordering, reduced prefetch distance, explicit synchronization, or less aggressive fusion when the guard fails.

This strengthens the phase-ordering rule. AI-P4 and AI-P5 establish distributed ownership and visibility facts; AI-P7 through AI-P9 build candidate tile, pipeline, and schedule plans; AI-P10 can select among certified variants using guards derived from those earlier facts. The runtime guard does not justify an unsafe transformation. It selects among already-legal plans whose assumptions have been explicitly modeled and costed.

## 7D.1 Historical Lineage For Guarded Multiversioning

Historical compiler literature from the 1980s and 1990s supports this design direction, although the terminology was usually run-time disambiguation, run-time dependence testing, inspector/executor, speculative run-time parallelization, or dynamic memory disambiguation rather than loop multiversioning.

The strongest direct precedent is Nicolau's 1989 run-time disambiguation work. The compiler identifies memory operations whose static dependence relationship is uncertain, emits address-comparison code, and selects between code that assumes interference and code that assumes no interference. This is the same architectural pattern as Open64's guarded no-alias loop version: pay a bounded run-time predicate cost to escape a permanently conservative plan when the predicate proves the fast case.

Related work broadened the same idea in different directions. Midkiff and Padua generated synchronization for concurrent loops when dependences could not be ignored. Saltz, Crowley, Mirchandaney, and Berryman studied run-time scheduling and execution of loops on message-passing machines, especially for indirect array references. Rauchwerger and Padua's Privatizing DOALL and LRPD tests moved further toward run-time validation of aggressive parallel plans, including speculative execution with fallback or re-execution when the run-time dependence test fails. Bernstein, Cohen, and Maydan's IBM XL work used dynamic memory disambiguation for array references to enable optimizations such as software pipelining, loop-invariant motion, and redundant load elimination.

The key takeaway for DSC is:

- Static uncertainty should not force one conservative plan when a run-time predicate can cheaply prove a better case.

- The compiler should represent both the guarded fast plan and the conservative fallback plan.

- RuntimeGuardIR should record the predicate, predicate cost, expected hit rate, and failure action.

- CandidateCostIR should model the expected value of the guarded plan, including guard overhead and fallback probability.

- CommonDistributedAliasIR should provide the tile-range, ownership, and visibility facts used by the guard.

- Telemetry should record guard hit rate, miss rate, and realized cost so later compilation can keep, refine, or remove a guarded variant.

# 8 Data Layout Placement Fetch Prefetch Async Pipeline And Bubble Minimization

The compiler should minimize compute bubbles by making data movement a first-class plan component. The central question is not only where a tensor lives, but over what time interval it lives there, who consumes it next, whether the next transfer can be started early, and whether the transfer can be hidden behind compute or communication.

## 8.1 Layout And Placement

- Keep logical layout separate from physical realization so one semantic tensor can have multiple plan-specific representations.

- Use placement to model device, remote HBM, CPU memory, pinned memory, and replicated or partitioned ownership.

- Use residency to model transient on-chip placement such as L2 working sets, shared-memory tiles, and register fragments.

- Represent layout conversion as an explicit evolution edge with cost and validity conditions.

## 8.2 Fetch Prefetch And Async Movement

- Attach fetch plans to producer-consumer timing rather than treating prefetch as a late annotation.

- Consume P7.4 smem_prefetch_hint, tma_legality, buffering_feasibility, tile_reuse_profile, and arrival_and_wait_requirements to construct the actual movement schedule.

- Prefer TMA for legal multidimensional bulk transfers when the tile is regular, aligned or guardable, and the destination SMEM layout is compatible with the compute layout. Fall back to cp.async, vectorized global loads, or demand loads when those conditions fail.

- Consume CommonDistributedAliasIR to prove that the prefetched tile range will not be overwritten, invalidated, reduced, migrated, or made visible only after a later communication epoch.

- Model async copies, double buffering, triple buffering, stream assignment, and dependency barriers in CommonPipelineIR.

- Represent CommonFetchPlanIR as the selected movement plan: source memory, destination memory, tile range, movement engine, stage count, prefetch distance, issue point, arrival barrier, consumer wait, boundary policy, and fallback path.

- Expose KV-cache paging and prefix-cache hits as AI-specific runtime-state inputs to common runtime-variant selection.

- Measure hidden versus unhidden memory movement in telemetry so the model improves over time.

## 8.3 Compute Bubble Minimization

A compute bubble appears when a compute resource is ready but its operands, communication result, synchronization condition, or launch dependency are not ready. Bubble minimization requires coordinated decisions across layout, placement, fetch, tiling, fusion, stream assignment, and communication scheduling.

# 9 Implementation Priority Order

1. Priority 1: TensorEvolutionGraph. Build TensorDescriptor core, RepresentationDescriptor hierarchy, and evolution nodes/edges first.

1. Priority 2: Candidate and plan infrastructure. Add OptimizationCandidateIR, OptimizationPlanIR, CandidateCostIR, and legality/profitability states.

1. Priority 3: Fusion and tiling refinement. Start with local fusion candidates, tile families, occupancy/resource estimates, and memory-materialization savings.

1. Priority 4: Parallelization integration. Add CommonParallelizationIR, placement, sharding, and communication-intent derivation to the same plan model.

1. Priority 5: Runtime variants. Generate certified variants, attach runtime guards, and add policy selection after static scheduling is robust.

# 10 Research Inspirations And DSC Design Proposals

This section records provenance. Research papers motivate design directions, but the IR structures and phase ordering in this document are DSC design proposals unless explicitly implemented and validated.

| Research item | Venue or date | Research-derived idea | DSC design inspiration |
| --- | --- | --- | --- |
| Mercury | SOSP 2025 | Asynchronous multi-GPU operator optimization for LLMs with remote-memory scheduling and communication-aware planning. | Inspiration for treating remote HBM, communication, and overlap as first-class plan components. |
| Neptune | 2025 arXiv, PLDI 2026 line | Advanced ML operator fusion for locality and parallelism on GPUs, especially reduction-operator sequences. | Inspiration for persistent fusion candidates refined with layout, tiling, and parallelism. |
| PipeThreader | OSDI 2025 | Software-defined pipelining for efficient DNN execution on heterogeneous GPU hardware units. | Inspiration for CommonPipelineIR, explicit buffering, async movement, and bubble minimization. |
| XNNC Fusion | CC 2025 | Greedy clustering operator fusion in a production neural-network compiler with profitability-guided expansion. | Inspiration for practical candidate generation and bounded profitability search. |
| HELM | 2026 | Compiler-guided heterogeneous LLM execution with compile-time CPU/GPU partitioning and KV-cache management for memory-constrained systems. | Inspiration for placement planning, heterogeneous residency, and runtime memory-state awareness. |


DSC design proposals in this v0.1 skeleton include the TensorEvolutionGraph-centered control plane, common-versus-AI ownership split, OptimizationCandidateIR and OptimizationPlanIR as profitability units, phase-to-IR matrix, explicit unhidden-latency accounting, and the stated implementation priority sequence.

## 10.1 Open64 LNO TVM CUDA CUTLASS And Triton Design Comparison

Open64 LNO represents the classical automatic compiler model. It delays loop transformation until dependence, reuse, loop-order, block-size, cache, TLB, loop-overhead, unroll, and target-resource information have been collected. Its cache model explicitly chooses tiling and block size from legal loop/tile alternatives, evaluates memory and loop overhead costs, and repeats the decision across memory-hierarchy levels. This strongly supports the DSC rule that transformation legality is not enough; plan selection must be cost-model gated.

TVM represents the modern tensor-compiler search lineage. AutoTVM starts from expert-written schedule templates with tiling knobs, so the search space is practical but bounded by the template author. Ansor moves toward automatic sketch generation from tensor expressions and uses learned cost models plus hardware measurement to choose schedules. TensorIR and MetaSchedule make this cleaner by representing schedulable tensor programs and applying schedule rules such as multi-level tiling, parallelization, vectorization, unrolling, and tensorization. In the MetaSchedule RFC, matmul tiling is expressed as sampled perfect tiles and an SSRSRS spatial/reduction ordering, which is a compact example of making tiling alternatives explicit before selection.

TVM's newer DLight path adds a complementary design point: deterministic rule-based GPU scheduling. DLight applies predefined rules for common patterns such as GEMM, GEMV, reductions, and fallback kernels, trading some peak adaptability for fast compile time and reliable known-pattern coverage. The lesson for DSC is that search and rules should coexist. CommonTilePlanIR should support both generated candidate families for cost-model search and rule-produced candidates for common, latency-sensitive workloads.

CUDA and CUTLASS represent a different point in the design space. NVIDIA GPU tiling is not only a loop transformation. It is a mapping to threadblocks, warps, warp groups, shared memory, registers, Tensor Cores, synchronization, async copy, and occupancy. CUDA therefore exposes execution configuration and shared-memory use to the programmer/runtime, while CUTLASS expresses a hierarchical tiling model: threadblock tile, warp tile, instruction tile, shared-memory stage, register fragment, epilogue, and scheduler. The lesson is that our IR must represent hardware hierarchy explicitly rather than treating tile size as a scalar.

Triton sits between these two models. Triton kernels expose program/block meta-parameters such as BLOCK_M, BLOCK_N, BLOCK_K, num_warps, num_stages, num_ctas, and maxnreg through configuration and autotuning. The compiler then lowers through MLIR-based Triton and TritonGPU representations and applies target-specific passes for layout assignment, CTA locality, TMA lowering, MMA lowering, barriers, tensor-memory allocation, tensor-memory layout, and interleaving. Triton therefore does not fully recover a whole-program LNO view, but it does keep tile/program choices visible as compile-time parameters long enough for backend lowering and autotuning.

The DSC direction should combine the strongest parts:

- From Open64 LNO: delayed transformation, legality plus cost-model gated selection, and reuse/memory hierarchy reasoning before rewriting.

- From TVM/Ansor/MetaSchedule: explicit schedule-space construction, multi-level tiling candidates, learned cost models, measurement feedback, and persistent tuning records.

- From TVM DLight: deterministic rule-produced tile plans for common GPU patterns when compile time or deployment constraints make full search unattractive.

- From CUDA/CUTLASS: explicit hierarchical tiling that matches hardware execution levels and resource constraints.

- From Triton: tile choices as first-class compile-time parameters, MLIR-style staged lowering, backend-specific layout/pipeline passes, and empirical autotuning when analytical cost confidence is insufficient.

This comparison upgrades the design requirement: tiling decisions should be represented as CommonTilePlanIR alternatives inside OptimizationCandidateIR and selected only through OptimizationPlanIR cost evaluation, with backend-specific hardware extensions supplying the NVIDIA, AMD, CPU, or accelerator constraints. CommonTilePlanIR should be able to record both search-generated alternatives and rule-generated alternatives, while CandidateCostIR records whether the evidence came from analytical modeling, learned prediction, hardware measurement, deterministic rule confidence, or runtime telemetry.

# 11 Open Sections For Continuous Expansion

## 11.1 Detailed paper notes

Add one subsection per paper with problem statement, mechanism, result, limitations, and DSC mapping.

## 11.2 Concrete IR schemas

Expand each IR object into fields, invariants, serialization, validation rules, and examples.

## 11.2.1 CommonTilePlanIR schema

Define the exact field schema for hierarchical tile levels, hardware resource estimates, backend-specific extensions, legality records, cost-model bindings, and lowering contracts. Include explicit fields for problem/output/reduction/CTA/SMEM/warp/thread/register/instruction tile shapes, coalescing model, vectorization width, layout transform, occupancy inputs, synchronization points, arithmetic intensity, autotune dimensions, and legality guards.

## 11.2.2 CommonDistributedAliasIR schema

Define the exact field schema for tensor tile ranges, ownership domains, mod/ref/reduce modes, alias classes, communication epochs, visibility conditions, runtime guards, and prefetch safety windows.

## 11.2.3 Guarded runtime variant schema

Define the exact field schema for guarded fast plans, conservative fallback plans, guard predicates, runtime cost of guard evaluation, guard confidence, and telemetry feedback for guard hit rate.

## 11.3 Legality model

Define legality rules for fusion, layout conversion, sharding, runtime variant selection, and numerical tolerance.

## 11.4 Cost model calibration

Add hardware calibration sources, benchmark kernels, fitted parameters, confidence intervals, and fallback heuristics.

## 11.5 Telemetry schema

Define runtime counters, profile record format, aggregation windows, and recompilation triggers.

## 11.6 Integration with FHE common substrate

Compare AI requirements against FHE optimization-control requirements and identify reusable substrate fields.

## 11.7 Prototype milestones

Break implementation priority into deliverable milestones and acceptance tests.

## 11.8 Risk register

Track risks such as candidate explosion, inaccurate overlap modeling, runtime policy instability, and excessive IR complexity.

# 12 Reference Notes

- Mercury: SOSP 2025 paper listing and metadata reviewed as research inspiration for asynchronous multi-GPU LLM operator optimization via remote-memory scheduling.

- Neptune: arXiv 2025 and project metadata reviewed as research inspiration for advanced ML operator fusion for locality and parallelism on GPUs.

- PipeThreader: OSDI 2025 paper metadata reviewed as research inspiration for software-defined DNN pipelining.

- XNNC Fusion: CC 2025 paper metadata reviewed as research inspiration for production-style greedy operator fusion.

- HELM: 2026 compiler-guided heterogeneous LLM execution metadata reviewed as research inspiration for CPU/GPU placement and KV-cache memory management.

- Simon Boehm 2022, How to Optimize a CUDA Matmul Kernel for cuBLAS-like Performance: concrete CUDA SGEMM worklog used as a one-to-one case study for refined AI-P7 multi-level tiling, CommonTilePlanIR fields, resource modeling, autotuning, warp tiling, and runtime kernel selection.

- Midkiff and Padua 1987, Compiler Algorithms for Synchronization: historical precedent for compiler-generated synchronization when loop dependences prevent unconditional parallel execution.

- Polychronopoulos 1988, compiler optimizations for enhancing parallelism: historical precedent for compiler-inserted run-time dependence analysis through bookkeeping and control statements.

- Nicolau 1989, Run-Time Disambiguation: historical precedent for emitting run-time address checks and selecting between conservative and no-interference code paths.

- Saltz, Crowley, Mirchandaney, and Berryman 1990, Run-time scheduling and execution of loops on message passing machines: historical precedent for run-time planning of loops with indirect accesses on distributed-memory systems.

- Rauchwerger and Padua 1994, Privatizing DOALL Test: historical precedent for run-time identification of fully parallel loops and dynamic privatization.

- Bernstein, Cohen, and Maydan 1994, Dynamic memory disambiguation for array references: historical precedent for deferring array-reference disambiguation to run time to enable loop optimizations.

- Rauchwerger and Padua 1995, LRPD Test: historical precedent for speculative run-time parallelization with validation and serial fallback or re-execution when dependence tests fail.

- TVM AutoTVM and Ansor documentation: used as design evidence for the transition from expert schedule templates to automatic sketch generation, learned cost modeling, hardware measurement, and tuning feedback.

- TVM TensorIR and MetaSchedule documentation and RFC: used as design evidence for explicit schedulable tensor programs, multi-level tiling rules, sampled tile factors, SSRSRS spatial/reduction organization, search strategies, tuning databases, and measured schedule selection.

- TVM DLight documentation: used as design evidence for deterministic rule-based GPU scheduling that complements full search for common GEMM, GEMV, reduction, and fallback patterns.

- Open64 LNO: local source review of LNO cache modeling, loop-nest modeling, and tiling/block-size selection used as design evidence for delayed cost-gated plan selection.

- NVIDIA Blackwell architecture, NVFP4, Transformer Engine, and cuDNN frontend documentation: used as design evidence that FP4/NVFP4 changes tiling decisions through native Tensor Core throughput, packed value layout, scale-factor metadata, scale layout requirements, TMA/pipeline legality, and dtype-specific runtime variants.

- NVIDIA CUDA and CUTLASS documentation: used as design evidence for explicit GPU hierarchy, occupancy, shared memory, Tensor Core tiling, asynchronous movement, and pipeline constraints.

- Triton compiler documentation and source metadata: used as design evidence for block meta-parameters, autotune configurations, MLIR-based lowering, target-specific layout/pipeline passes, TMA/MMA lowering, barriers, and tensor-memory handling.
