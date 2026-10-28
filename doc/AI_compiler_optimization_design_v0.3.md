AI compiler optimization design

Version v0.3 | Living design skeleton | Draft for continuous expansion

Scope: AI memory hierarchy optimization architecture, research opportunity mapping, common optimization IR substrate, AI-domain-specific extensions, candidate generation, profitability analysis, and implementation priorities.

2026-10-27 Python stable-Softmax connection:
the [exact source-to-native recipe](AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md#python-ingestion-increment-2026-10-27)
now shares structural preflight with R1 analysis and emits the published native
contract through runtime formals and an explicit result/return. Five API/driver
images reach actual PREOPT with division-site source positions and strict
numerical permissions/unknowns. No target algorithm, executable lowering or
MMA1 change is implied; native normalization remains next.

2026-10-27 native stable-Softmax amendment:
[Native stable Softmax and canonical WHIRL characterization](AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md)
adds the bounded FP32 max/subtract/exp/sum/divide contract and derives native
state/access/numerical facts at PREOPT entry/exit. No online-state or relaxed-FP
permission is inferred; exp/order/exception/accuracy/storage obligations remain
explicit. Controlled compact traces and audited phase reports preserve R1's
conventions. Python Softmax ingestion and executable lowering remain open;
MMA1 is unchanged.

2026-10-27 native characterization amendment:
[Canonical-WHIRL row-sum characterization](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md#canonical-whirl-characterization-2026-10-27)
now derives state, logical accesses and strict numerical obligations from
verified native assignments and types, not FX/sample/debug facts. PREOPT
rederives at entry/exit; additive FACT traces preserve existing FORM records.
Unknown order/accuracy/alias/storage and forbidden FP32 merge remain explicit.
This does not select an executable realization or modify MMA1.

2026-10-27 source-to-native R1 amendment:
the [bounded row-sum connection](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md#python-ingestion-increment-2026-10-27)
now consumes the native v1 contract from the exact Python matcher and standalone
driver, using a runtime tensor formal and explicit rank-1 result/return.
Canonical axis, strict numerical attributes and the Python call-site survive
native inspection/PREOPT. Executable reduction lowering and general native
characterization remain open. No MMA1 contracts or profiles change.

2026-10-27 native R1 amendment:
[Native reduction contract and PREOPT canonical form v0.1](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md)
adds the bounded typed row-sum form and verifies it in actual PREOPT without
choosing an accumulation schedule. Native `.B/.T`, controlled `.t` and strict
phase reports are separate from FX evidence. Source ingestion, full native
characterization and executable reduction lowering remain open; MMA1 is
independent.

2026-10-27 R1 implementation amendment:
[Row semantic characterization and numerical acceptance v0.1](AI-COMPILER-ROW-SEMANTIC-CHARACTERIZATION-v0.1.md)
derives reduction, stable Softmax and affine LayerNorm domains/accesses/state
from actual FX dataflows. Numerical permissions and CPU acceptance witnesses
remain separate gates; native canonical-WHIRL characterization, optimization
and physical scheduling remain open. This frontend-only slice is independent
of MMA1 and changes no compute profile or native IR contract.

2026-10-27 compute-realization proposal:
[MMA compute realization and SIMT fallback v0.1](AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md)
defines Hopper WGMMA and Blackwell SM100 `tcgen05` as separate complete-plan
alternatives, with numerical/target/layout/event/resource legality before
schedule-aware profitability. SIMT stays the fallback and initial default;
native MMA, target rates and proposed controls are not implemented. This
shared amendment governs P6-P9 in both living skeletons without renumbering
their historical versions.

2026-10-27 adopted amendment:
[Common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md) defines
Work Partition, Resource Lifetime and Execution Events as jointly refined
views of one complete plan. Computation characterization and verified
algebraic alternatives remain upstream. The shared amendment governs both
living skeletons without renumbering their historical versions. Its
[four-case research milestone](research_library/building_block_planning/README.md)
demonstrates GEMM, reduction, stable Softmax and mean/variance normalization;
host examples and synthetic cost comparisons are not new compiler/GPU support.

Status: v0.3 adds a phase-by-phase optimization reporting contract and identifies trace fields still needed for a trustworthy cross-phase report. The 2026-10-18 amendment fixes the matmul-first trace, report, and durable-artifact conventions in Section 9.7. The 2026-10-21 amendments add the predicted and measured Roofline contract in Section 7.2A, the high-level WHIRL/NVISA convergence contract in Section 6B, and the accepted `whirl2c -cuda` source-emission direction in Section 6B.5. The 2026-10-22 amendment makes SMEM cache-blocking a costed alternative within an opt-in delayed joint-plan search (Section 6A.3B), with rationale preserved in a separate white-paper candidate. The design retains the existing `.cu` emitter as a migration baseline and specifies a default-on control for it; neither implies that device WHIRL is complete. AIO-9 has a compact WOPT-style selection trace and a tile-phase report; full cross-phase coverage, Roofline reporting, joint-plan search, and WHIRL device-loop lowering remain proposals. Research-derived inspiration and DSC design proposals remain distinct.

2026-10-26 design amendment: Section 7.2B introduces the proposed
dependency/resource bubble model and a shadow candidate-ranking experiment.
The current D/S1/T2 model-3 selector and initial read-only source-warp replay
are implemented; production cycle selection and the new controls below are
not. The implementation/progress ledgers govern
current maturity where the historical status paragraph above is older.

# 1 Executive Summary And Design Goals

The design goal is to make AI compiler optimization compositional across semantic tensors, layouts, sharding, memory residency, tiling, asynchronous movement, scheduling, and runtime adaptation. The central architectural choice is to preserve tensor identity through a TensorEvolutionGraph while optimization candidates remain provisional until enough layout, placement, tiling, resource, and runtime-state information exists to make a profitability decision.

The common compiler substrate should own reusable descriptions of objects, representations, memory hierarchy, placement, access patterns, fetch plans, pipelines, parallelization, communication intent, optimization candidates, plans, costs, and runtime guards. AI-specific layers should own workload semantics and runtime state such as batch size, sequence length, KV-cache pressure, prefix-cache hit rate, expert load, and latency service-level objectives.

The recommended implementation priority is now action-gated: first build the measurement harness, then implement one real tiling rewrite, one real fusion rewrite, the P7.4/P8 pipeline path, cost-model calibration, runtime variants, and only then broaden into distributed parallelization.

The host matmul rewrite still records a generated-provider call and plan metadata. A separate selected device PU now contains the complete synchronous F32 tile loop nest in high-level WHIRL and round-trips through `whirl2c -cuda`; its `.cu` agrees numerically with the legacy emitter for the exact-divisible 128x128x128 fixture in a CPU CTA simulation. The device image is currently an opt-in sidecar built in VHO, not yet a driver-registered PU in the main compilation unit. It has no P8/TMA movement, target NVISA/PTX output, or native GPU verification. The default-on legacy emitter remains a clearly labeled transition baseline. Section 6B defines the remaining integration boundary.

## 1.1 Design Goals

- Keep high-level AI meaning visible long enough to guide memory and scheduling decisions.

- Represent tensor evolution explicitly from semantic tensors to distributed shards, local buffers, tiles, shared-memory tiles, registers, and tensor-core fragments.

- Avoid committing fusion, tiling, layout, placement, or parallelization choices too early; plan selection is gated on explicit cost modeling and legality checks.

- Make profitability a property of complete plans, not isolated transformations.

- Separate reusable compiler-control infrastructure from AI-domain-specific semantics.

- Allow static compilation to generate certified runtime variants selected by guarded runtime policy.

- Create a living skeleton that can absorb future paper reviews, measurements, and implementation notes.

## 1.2 Optimization Action Contract

This design is not complete until it produces transformed code. The optimizer must not stop at TensorEvolutionGraph annotation, candidate recording, or cost-model scaffolding. Every implemented optimization feature must eventually pass through the following action loop:

1. Detect a concrete optimizable region.

1. Generate at least one transformed candidate and one conservative baseline candidate.

1. Prove candidate legality or attach a runtime guard and fallback.

1. Estimate candidate cost with explicit compute, memory, synchronization, launch, occupancy, and code-size terms.

1. Select one candidate or explicitly reject all candidates with a recorded reason.

1. Rewrite the program into the selected fused, tiled, pipelined, vectorized, or parallelized form.

1. Compile, run, verify numerical correctness, and record measured performance.

An IR object without a candidate generator, selector, rewrite path, and measurement hook is only infrastructure. It should be marked as scaffolding, not counted as an implemented optimization.

# 2 Recommended AI Optimization Phase Ordering

The phases below are ordered to preserve semantic information early, introduce physical constraints gradually, and delay final profitability decisions until the optimizer can evaluate the interaction between fusion, layout, sharding, tiling, memory residency, communication, and runtime state.

| Phase | Name | Purpose |
| --- | --- | --- |
| AI-P0 | Semantic tensor analysis | Preserve Attention, GEMM, Conv, MoE, RMSNorm, residual, activation, model weight, activation, and KV-cache meaning. Attach shape, dtype, semantic role, producer-consumer relationships, dynamic dimensions, and reuse traits. |
| AI-P1 | Lifetime reuse and locality analysis | Compute producer, consumers, last use, reuse distance, object size, working-set size, critical-path position, and likely residency benefit. |
| AI-P2 | High-level fusion | Create semantic fusion, decomposition and verified algebraic/state alternatives. Preserve numerical permissions and equivalence obligations; retain the original realization. No fusion is implied for standalone GEMM. Candidates remain provisional. |
| AI-P3 | Logical layout optimization | Choose logical tensor layouts such as row-major, channel-major, blocked, packed-head, sequence-major, KV-cache layout, and MoE expert-major layout without fully committing to local physical storage. |
| AI-P4 | Distributed placement and sharding | Select data parallelism, tensor parallelism, pipeline parallelism, expert parallelism, sequence/context parallelism, replication, shard ownership, and remote-HBM placement alternatives. |
| AI-P5 | Communication derivation | Derive AllReduce, ReduceScatter, AllGather, send/receive, peer copy, RDMA, and collective boundaries implied by sharding and fusion decisions. |
| AI-P6 | Local physical layout and residency | Assign local HBM, L2, shared memory, registers, pinned host memory, CPU memory, or remote memory residency. Track capacity, lifetime, reuse, spill, and eviction. |
| AI-P7 | Multi-level tiling | Build and cost hierarchical tile plans across output tile, reduction tile, CTA/threadblock tile, shared-memory tile, warp tile, thread tile, register fragment, vectorized access, and tensor-core/instruction tile. Connect each level to reuse, coalescing, arithmetic intensity, register pressure, shared-memory footprint, occupancy, synchronization, and autotuning parameters. |
| AI-P8 | Fetch prefetch and async movement | Jointly refine movement events, storage-access lifetimes, physical reuse and buffering for each work-partition candidate. Prove readiness and safe overwrite separately; cost waits, reservation and overlap before P9 selection. |
| AI-P9 | Physical kernel fusion and scheduling | Select the complete costed plan; materialize a high-level WHIRL device-kernel PU with explicit loops, memory spaces, and synchronization; then choose final fusion boundaries, launch structure, stream assignment, and target schedule constraints. |
| AI-P10 | Runtime variants and adaptation | Generate certified variants with runtime guards for batch size, sequence length, KV-cache pressure, prefix-cache hit rate, expert load, HBM availability, network contention, latency SLO, and guarded alias or disjoint-tile predicates. |
| AI-P11 | Telemetry and profile feedback | Capture measured latency, achieved occupancy, memory traffic, communication time, overlap effectiveness, cache hit behavior, launch overhead, and runtime variant choices for future recompilation or policy tuning. |


# 3 Rationale And Ordering Constraints

The ordering is driven by the fact that AI tensors with identical numerical type may have very different optimization behavior. A KV-cache block, an activation tensor, and a model weight may all be low-precision tensors, but their lifetime, reuse distance, placement, fetch policy, sharding, and runtime sensitivity differ sharply.

## 3.1 Why Semantics Come First

Semantic tensor analysis must occur before aggressive lowering because attention, MoE, KV cache, residual paths, and normalization have domain-specific lifetimes and reuse structures. Once lowered too early into generic loops or opaque kernels, the optimizer loses the reason a tensor should remain resident, be replicated, be sharded, or be prefetched.

## 3.2 Why Fusion Cannot Be Finalized Early

Fusion saves materialization and launch overhead, but can increase register pressure, shared-memory footprint, live ranges, synchronization constraints, and communication cost. A candidate that looks profitable before tiling and sharding may become unprofitable after occupancy loss or communication-boundary disruption is accounted for.

## 3.3 Why Layout And Sharding Precede Communication

Communication is derived from layout and placement decisions. The optimizer should first describe candidate ownership and sharding of tensors, then derive collective and peer-to-peer operations from those choices. This avoids treating communication as a fixed artifact independent of the plan that caused it.

## 3.4 Why Runtime Variants Come Late

Runtime adaptation should not invent arbitrary schedules. The static compiler should generate legal variants, attach guards, and expose a selection policy. The runtime observes state and selects from certified variants.

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
| CommonLayoutIR | Common Compiler Substrate | Describe a tensor's persistent storage representation (shape, strides, packing, alignment) separately from transient SMEM/register layouts. A physical GMEM repack creates a new representation and a costed TensorEvolutionGraph edge; a lane remap does not. |
| CommonPlacementIR | Common Compiler Substrate | Device, NUMA, GPU, remote memory, replicated, partitioned, or staged placement. |
| CommonMemoryResidencyIR | Common Compiler Substrate | Home tier, preferred tier, capacity, lifetime, eviction point, spill policy. |
| CommonAccessPatternIR | Common Compiler Substrate | Per-memory-operation lane/warp-to-address mapping, active-lane mask, vector width, alignment, distinct requested sectors, useful bytes, reuse distance, and producer-consumer timing. Access mapping is not persistent storage layout. |
| CommonFetchPlanIR | Common Compiler Substrate | Prefetch, demand fetch, async transfer, promotion, demotion, staging, overlap window. |
| CommonTilePlanIR | Common Compiler Substrate | Hierarchical tiling alternatives across problem, device, CTA/threadblock, warp/warpgroup, thread, register fragment, tensor-core instruction, and memory-pipeline stages. It records candidate tile shapes, split/reduction axes, buffering depth, swizzle/layout assumptions, resource usage, legality, and cost-model inputs before transformation is committed. |
| CommonDistributedAliasIR | Common Compiler Substrate | Range-level alias, ownership, mod/ref/reduce, and visibility facts for tensor tiles across ranks, devices, sockets, boards, or nodes. It records owned, ghost, replicated, reduced, migrated, and unknown regions plus communication epochs and runtime guards. |
| CommonPipelineIR | Common Compiler Substrate | Pipeline stages, buffering depth, async barriers, dependency edges, overlap opportunities. |
| Work Partition view | Common Compiler Substrate; extensions of existing tile/parallelization records, not a newly published IR type | Logical contribution/output partition, partial/final ownership and provisional execution maps. |
| Resource Lifetime view | Common Compiler Substrate; residency/pipeline extensions proposed | Value/access/reservation lifetimes, range conflicts, completion joins, aligned storage assignment and generations. |
| Execution Events view | Common Compiler Substrate; fetch/pipeline extensions proposed | Issue, visibility, access/operation completion, scope, loop-carried dependencies and target realization; one candidate/plan identity. |
| CommonParallelizationIR | Common Compiler Substrate | Partition axes, replication, work ownership, mapping to resources, parallel region boundaries. |
| CommonCommunicationIntentIR | Common Compiler Substrate | Collective or peer communication intent before backend-specific communication lowering. |
| OptimizationCandidateIR | Common Compiler Substrate | Candidate tree that records semantic, layout, placement, parallelization, tile, memory, and pipeline alternatives. |
| FusionCandidateIR | Common Compiler Substrate where generic | Generic operator-region fusion fields belong in common substrate; AI legality and semantic classes are AI-specific extensions. |
| OptimizationPlanIR | Common Compiler Substrate | The unit of end-to-end cost evaluation across choices. |
| CandidateCostIR and CommonCostModelIR | Common Compiler Substrate | Compute, memory, communication, sync, launch, overlap, resource, and confidence estimates. Embed per-tier Roofline evidence: work, traffic, arithmetic intensity, bandwidth and compute ceilings, bound, provenance, and uncertainty; do not turn the bound into an additive latency term. |
| MemoryHierarchyDescriptorIR | Common Compiler Substrate | Tier scope, capacity, sustainable bandwidth by transfer path, latency, granularity, alignment, bank structure, async capability, and calibration provenance. Capacity is not a bandwidth estimate. |
| ComputeResourceIR | Common Compiler Substrate | Device, SM, vector unit, tensor core, thread, warp, CTA, stream, and occupancy-relevant resource descriptions, including instruction-path and dtype-specific sustained compute ceilings. |
| RuntimeGuardIR | Common Compiler Substrate | Generic guards, validity ranges, fallback behavior, and failure actions. |
| Generic runtime-variant infrastructure | Common Compiler Substrate | Variant identity, applicability, selection policy hooks, hysteresis, fallback, telemetry binding. |
| High-level WHIRL device-kernel lowering contract | Common Compiler Substrate | Bind a selected plan ID to a device PU and to structured loop IDs, memory-space and alias facts, synchronization dependencies, source provenance, and verification results. Reuse WHIRL nodes and symbol attributes rather than creating a competing kernel AST. Target-specific NVIDIA operations remain backend extensions. |


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

- High-level WHIRL owns executable structured loops and typed memory operations after selection. Plan IR owns alternatives and estimated costs before selection; the target backend owns PTX/CUDA-specific instruction realization. Neither plan metadata nor a host-side generated-provider call substitutes for a device-kernel WHIRL PU.

- Research-derived mechanisms should be recorded as inspiration; DSC-specific proposals should be recorded as design commitments or hypotheses only after review.

# 6 Phase To IR Matrix

| Phase | Primary IR | Key information |
| --- | --- | --- |
| AI-P0 Semantic tensor | Tensor descriptor core, AI semantic descriptors, TensorEvolutionGraph root, computation characterization | Domains/accesses, payload, state algebra, effects, numerical semantics and source |
| AI-P1 Lifetime reuse | CommonAccessPatternIR, CommonMemoryResidencyIR, CandidateCostIR seed, Resource Lifetime view | Value vs access vs reservation lifetime, ranges, completion obligations and reuse |
| AI-P2 High-level fusion | FusionCandidateIR, OptimizationCandidateIR, verified algebraic alternatives | Equivalence/preconditions, numerical permission, extra work and unchanged fallback |
| AI-P3 Logical layout | CommonLayoutIR, RepresentationDescriptor hierarchy | Logical layout alternatives; preserve the existing persistent GMEM representation as a distinct fact |
| AI-P4 Distributed placement | CommonPlacementIR, CommonParallelizationIR, CommonDistributedAliasIR, TensorEvolutionGraph | Sharding, replication, device ownership, tile-range ownership |
| AI-P5 Communication derivation | CommonCommunicationIntentIR, CommonDistributedAliasIR, CandidateCostIR | Collectives, peer transfers, and visibility epochs implied by sharding |
| AI-P6 Local layout residency | CommonMemoryResidencyIR, CommonLayoutIR, MemoryHierarchyDescriptorIR | Existing GMEM storage, optional costed repack alternatives, HBM/L2/SMEM/register residency |
| AI-P7 Multi-level tiling | CommonTilePlanIR, CommonAccessPatternIR, CommonLayoutIR, CommonMemoryResidencyIR, ComputeResourceIR, CandidateCostIR | Joint A/B/C lane access maps, output/reduction/CTA/SMEM/warp/thread/register tiles, per-operation GMEM transaction utilization, arithmetic intensity, occupancy, resource pressure |
| AI-P8 Fetch prefetch async | CommonFetchPlanIR, CommonPipelineIR, CommonDistributedAliasIR, Resource Lifetime and Execution Events views | Generation-aware access completion, storage conflicts/offsets, visibility, safe reuse, progress and complete movement costs |
| AI-P9 Kernel scheduling | OptimizationPlanIR, CommonPipelineIR, ComputeResourceIR, high-level WHIRL device PU | Cost-gated plan selection, structured device-loop materialization, final physical realization |
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
| P7.2 GMEM access mapping | Remap output ownership and A/B/C warp accesses against the existing GMEM representation; do not imply physical repacking. Evaluate a repacked representation only as a separate, costed candidate. | Kernel 2 improves coalescing by remapping threads, with A/B storage unchanged. | CommonAccessPatternIR per_op_lane_to_address_map; active_mask; requested_sectors; useful_bytes; CommonLayoutIR representation_id; optional repack_edge. |
| P7.3 Reduction-axis tile | Split the K/reduction dimension into reusable chunks. | BLOCKSIZE/BK loop over A columns and B rows. | CommonTilePlanIR reduction_tile_shape; split_axis; accumulation_scope. |
| P7.4 Shared-memory cache-blocking alternatives | Keep a direct/cache-served A/B path and propose distinct one-slot synchronous and feasible multi-slot SMEM staging paths; describe A/B layouts, copy maps, reuse, and P8 prefetch hints without selecting residency. | Kernel 3 shared-memory cache blocking is one implementation, not a universal winner; later TMA/cp.async paths are alternatives. | CommonMemoryResidencyIR; CommonLayoutIR A/B smem_layout; CommonTilePlanIR smem_tile_shape, copy_map, stage_footprint, reuse; CommonFetchPlanIR hints. |
| P7.5 Occupancy and resource feasibility | Estimate whether tile choices fit SMEM, registers, warp limits, CTA limits, and launch-grid utilization. | Occupancy calculation after Kernel 3. | ComputeResourceIR; CandidateCostIR occupancy_terms; resource_limits. |
| P7.6 Per-thread register tile | Assign each thread multiple output elements and accumulate in registers. | Kernel 4 1D blocktiling and Kernel 5 2D blocktiling. | CommonTilePlanIR thread_tile_shape; register_fragment_shape; accumulator_footprint. |
| P7.7 Register reuse and inner-loop ordering | Order dot/reduction loops to reuse SMEM values through register fragments. | Kernel 5 register caching and outer-product accumulation. | CommonTilePlanIR inner_loop_order; register_reuse_factor; smem_access_count. |
| P7.8 Vectorized memory-access tile | Require alignment/layout conditions for wider GMEM or SMEM transactions. | Kernel 6 transposed As, LDS.128, LDG.E.128, STG.E.128, float4. | CommonAccessPatternIR vector_width; RuntimeGuardIR alignment_guard; CommonLayoutIR transposed_smem_layout. |
| P7.9 Autotuned tile family | Keep BM, BN, BK, TM, TN and related tile parameters as candidate families rather than one fixed answer. | Kernel 9 autotuning. | OptimizationCandidateIR tile_family; CandidateCostIR confidence; RuntimeVariantIR candidate_id. |
| P7.10 Warp tile | Add an explicit warp-level tile between CTA tile and thread tile. | Kernel 10 warptiling. | CommonTilePlanIR warp_tile_shape; warp_lane_map; warp_scheduler_model; bank_conflict_terms. |
| P7.11 Compute realization and instruction tile | Retain legal SIMT and MMA families with compatible instruction/CTA/warp covering, numerical contract, operand/accumulator storage and completion protocol; decide only after P8 complete-plan costing. | Contraction semantics do not imply hardware MMA; Hopper WGMMA and Blackwell tcgen05 have separate recipes. See the MMA detail proposal. | Proposed CommonComputeRealizationIR view; CommonTilePlanIR instruction_tile_shape; fragment_descriptor; numerical, resource and event references. |

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

- residency_alternatives: direct/cache-served GMEM, one-slot synchronous SMEM, and target-legal multi-slot SMEM; each binds separate A/B copy maps, SMEM layouts, consumer maps, and resource ledgers.

- persistent_gmem_representation: existing A/B/C storage identity, strides, packing, and alignment; optional repacked representation and its explicit conversion edge.

- access_layout: per-operation lane/warp-to-element mapping for A loads, B loads, and C stores, distinct from persistent storage and from SMEM/register layout.

- layout_transform: transposed SMEM layout, swizzle, padding, or bank-conflict avoidance transform; charge any required conversion between access, staging, and compute layouts.

- low_precision_format: dtype family such as BF16, FP8, MXFP8, MXFP4, NVFP4, accumulation dtype, packing alignment, and supported Tensor Core instruction family.

- scale_tile_shape: scale-factor tensor tile shape, scale granularity, scale reuse scope, and scale layout compatibility when the value format uses block or micro-tensor scaling.

- effective_tile_bytes: packed value bytes plus scale metadata bytes, layout padding, alignment padding, and conversion or repacking traffic.

- smem_prefetch_hint: producer-consumer distance, reuse distance, expected stage count, and whether TMA, cp.async, vectorized LDG, or scalar fallback should be evaluated.

- tma_legality: affine source range, rectangular or regularly strided tile, compatible destination layout, required alignment, boundary handling, and alias-safety conditions.

- buffering_feasibility: one-stage, two-stage, or N-stage buffering feasibility under SMEM, register, CTA, and occupancy limits.

- access_vectorization: scalar, float2, float4, LDG/STG width, LDS width, alignment requirement.

- coalescing_model: for each warp memory instruction, active lanes, addressed bytes, distinct target-profile sectors requested, useful bytes, sector utilization, and cache-path assumptions. Keep GMEM request bytes distinct from estimated HBM bytes.

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

## 6A.3A GMEM Coalescing Candidate Algorithm

1. Record the existing A/B/C GMEM representations, strides, alignment, alias ranges, and cache assumptions. Keep physical storage unchanged for the default candidate.
2. Enumerate legal CTA/warp/lane ownership and per-operation access maps together with SMEM layouts and compute fragments. An optional physical GMEM repack is a different representation candidate, with an explicit allocation/conversion plan and reuse horizon.
3. Reject candidates that violate bounds, alias/dependence rules, unique C-store ownership, alignment, target instruction requirements, or SMEM/register/barrier limits.
4. For each warp load/store instruction, map active lanes to byte addresses and count distinct aligned sectors using the target's transaction granularity. Report useful distinct bytes divided by requested sector bytes. Model cache hits and HBM traffic separately; coalescing at the instruction does not establish HBM bytes.
5. Combine A/B/C transaction costs with SMEM bank effects, layout conversions, occupancy, P8 movement/overlap, and any amortized repack allocation and traffic. Select only the lowest-cost legal complete plan at the P9 profitability gate; retain the unchanged-representation baseline and rejection reasons.

The trace must distinguish `gmem_representation_id`, `access_map_id`, and `repack_plan_id` (or `-`), and report per-operation requested sectors, useful bytes, cache assumption, conversion/repack cost, and selected-plan reason. No source or WHIRL rewrite occurs merely because an access map improves coalescing in isolation.

## 6A.3B Delayed SMEM Cache-Blocking Selection

The P7.4 action is **propose**, not **promote**. A/B SMEM staging can reduce repeated GMEM requests, but the benefit depends on the already coalesced/cache-served path, consumer register reuse, SMEM bank behavior, synchronization, and occupancy. Boehm's Kernel 3 makes the staging and two per-K-tile synchronization points concrete; his later register tiling shows that SMEM traffic itself can become the next bottleneck. This is research-derived evidence, not a measured result for Open64.

For each legal `(BM, BN, BK, warp/thread tile)` family, build a bounded set of complete alternatives: (D) direct or cache-served A/B loads with no A/B SMEM slots; (S1) one-slot synchronous A/B SMEM staging; (SN) multi-slot async staging only when the target, movement engine, and buffer lifetime permit it. A and B may have different copy maps and SMEM layouts. Keep output ownership, GMEM representation, A/B lane access maps, SMEM consumer maps, register fragment, edge policy, and CTA order attached to the same candidate ID. An optional physical GMEM repack remains a separate costed representation edge, not a consequence of coalescing.

1. Enumerate deterministic, bounded candidate tuples; retain D and the current supported S1 plan as reference candidates. Prune only with proved semantic, dependence/alias, alignment, SMEM/register, bank-layout, barrier-uniformity, and target-capability constraints. Record the bound and every rejection reason.
2. Compute per-operation warp GMEM requested sectors and useful bytes, then estimate L1/L2/HBM bytes under named cache scenarios. For S1/SN also count SMEM writes and consumer reads, bank serialization (including broadcast behavior), layout conversions, and actual padded A/B slot footprints. Do not replace requested traffic with HBM traffic or treat SMEM capacity as free.
3. Pass each surviving tuple to P8 for an explicit load/arrival/wait/compute/release event schedule. Model synchronous barriers by dynamic K-tile frequency, not a fixed two-barrier charge for the entire GEMM. For async plans prove that a producer never overwrites a slot before all consumers complete.
4. At P9 compare complete legal plans using compute, exposed movement, exposed communication, synchronization, launch, occupancy/grid-wave, and conversion/repack costs. Obtain overlap from the event schedule/critical path, not a fixed stage-count credit. Require a configurable benefit margin when uncertainty can reverse a ranking; otherwise select the conservative fallback and report why.
5. Materialize only the selected plan in device WHIRL. Reopen/verify the selected image, report selected versus baseline estimates, and calibrate the ranking with measured traffic, stalls, barriers, and runtime when a GPU is available. No estimated speedup is a verified performance claim.

**Experimental control.** `-Wb,-DSL:gemm_plan_search=baseline|joint` is registered, default `baseline`. `baseline` preserves the tile-first selector. Joint model 2 compares direct register-load D, one-slot synchronous S1 and permitted two-slot TMA T2 within the existing proven P7 family, then lowers one complete-cost-selected WHIRL kernel. It retains a distinct S1 compile-time fallback. Joint requires `-O3` and `cuda_whirl_output`; legacy-template output is unsupported, and host application remains S1-only. No precision or target-legality permission is bypassed. `AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md` specifies the current algorithm, actual direct warp requests, controls and limits; the joint-search model-1 Markdown/PDF remains the preceding checkpoint. Mode, budget, lineage, D/S1/T2 kind, full costs, winner/fallback and actual WHIRL actions appear in `.t`, `physical-plan.log` and aggregate reports. `cp.async` and native calibration remain open.

This option isolates the potential contribution for evaluation: delayed transformation permits *cross-axis* choices that a sequential, one-pass tile or staging decision may discard. The claim is an architectural hypothesis until equal-search-budget comparisons against the baseline, schedule-search systems, and hardware measurements establish a gain. The rationale, evaluation questions, and related work are retained in `AI-COMPILER-WHITE-PAPER-CANDIDATE-DELAYED-GEMM-PLAN-SELECTION.md`.

**Numerical planning extension.** Reduced-precision plans must distinguish operand/product precision, internal MMA accumulation and outer accumulation. Hopper FP8 promotion intervals are independent of SMEM BK and prefetch depth; Blackwell NVFP4/MXFP4 require separate scale/packing/instruction profiles rather than inheriting Hopper's empirical precision limitation. Numerical acceptance precedes profitability. See [Numerical Accumulation And Scaled GEMM Design v0.1](AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md) for DeepSeek-V3 evidence, pinned Triton behavior, common versus AI ownership, WHIRL actions, costs and required reports. The first contract/scalar-WHIRL slice is implemented separately from the FP32 D/S1/T2 pipeline; native FP8/FP4 lowering and optimizing-phase contract protection remain open.

## 6A.4 P7.4 To P8 Prefetch Hint Contract

P7.4 should not generate the async copy schedule directly. It describes both direct/cache-served and SMEM-staged alternatives, preserving enough tile structure for P8 to decide whether a TMA, cp.async, vectorized-load, or scalar movement plan can hide latency. P8 may return a schedule that makes an S1 or SN candidate unprofitable; it does not itself commit the residency choice.

For each shared-memory tile candidate, P7.4 should emit:

- smem_tile_shape: A and B tile dimensions, bytes per stage, and edge-tile policy.

- smem_layout: canonical, transposed, swizzled, padded, or bank-conflict-avoiding layout.

- scale_tile_layout: associated scale-factor tile layout and whether it can be staged with, before, or independently from the packed value tile.

- tile_reuse_profile: consumers per staged tile, reuse distance, arithmetic intensity contribution, and expected SMEM-to-register access count.

- separate_copy_and_consumer_maps: A/B warp-lane GMEM load maps, SMEM store/read maps, expected requested sectors, SMEM bank conflicts or broadcasts, and register-fragment reuse.

- direct_path_reference: matching non-staged tile with its cache scenario, GMEM transactions, register demand, and no A/B SMEM slot or staging barriers.

- prefetch_distance_hint: the candidate number of K-tiles or pipeline stages to fetch ahead.

- buffering_feasibility: whether one-stage, double-buffered, triple-buffered, or deeper staging fits under SMEM, register, occupancy, and barrier constraints.

- tma_candidate: whether the source and destination form a regular multidimensional transfer suitable for TMA.

- tma_legality: affine source range, rectangular or strided tile shape, alignment, compatible destination layout, boundary policy, and alias-safety conditions.

- arrival_and_wait_requirements: barrier or mbarrier requirements between async arrival and consuming warp, warpgroup, or CTA.

- fallback_movement_candidates: cp.async, vectorized LDG plus SMEM store, ordinary LDG/STG, or demand load when TMA is illegal or not profitable.

P8 consumes those hints to build CommonFetchPlanIR and CommonPipelineIR for each candidate. A P8 plan should bind the proposed movement engine, source/destination memory levels, stage count, prefetch distance, arrival barrier, consumer wait point, release/overwrite point, edge-tile behavior, and fallback path. P9 selects the complete plan after charging dynamic synchronization, SMEM bank effects, occupancy, and only the unhidden portion of movement latency. It must answer whether staging reduces compute-unit bubbles *relative to the matching direct/cache-served plan*, not merely whether an async instruction is available.

# 6B High-Level WHIRL And NVISA Convergence

## 6B.1 Current Gap And Design Hypothesis

The current matmul `.cu` output contains the outer reduction-tile loop, cooperative A/B loads, barriers, compute loops, and output stores. In the corresponding WHIRL, the selected path is a host-side generated-provider call plus plan metadata; the device loops are emitted as source text. This is useful for external CUDA compilation and review, but it does not let Open64's loop and dependence analyses inspect or transform the selected kernel. A `for` loop in a generated file is not evidence of a `DO_LOOP` in WHIRL.

The design hypothesis is that preserving structured loops and memory facts in WHIRL through AI-P9 will make plan refinement, legality checking, tracing, and target lowering more effective than lowering this workload prematurely to a low-level control-flow IR. The relevant NVIDIA comparison is nvcc's LLVM-based NVVM path, not MLIR: NVIDIA announced the LLVM-based CUDA compiler with CUDA Toolkit 4.1 in December 2011, and its NVVM specification describes an LLVM-based GPU IR and PTX generator. We should compare the information our path preserves and the generated results without assuming visibility into proprietary nvcc passes. The intended Open64 advantage is an integrated, cost-gated path from semantic tensors and LNO-style loop reasoning to a target kernel, with less reconstruction of lost structure. This remains a hypothesis, not a demonstrated performance claim.

MLIR affine, structured-control-flow, and transform dialects are a separate, later architectural comparison, especially for Triton-like compilers. Their existence does not make nvcc an MLIR-based compiler and should not be used to answer a claim specifically about nvcc/NVVM.

## 6B.2 Representation And Ownership Contract

| Layer | Required representation | Owner and commit point |
| --- | --- | --- |
| Candidate planning, AI-P7/P8 | CommonTilePlanIR, CommonFetchPlanIR, CommonPipelineIR, OptimizationPlanIR, CandidateCostIR; baseline and alternatives remain intact | Common substrate; no executable rewrite before whole-plan legality and profitability selection |
| Selected GPU kernel, AI-P9 | Separate device PU with high-level WHIRL `DO_LOOP` nests for selected tile, cooperative load, compute, and epilogue loops; explicit edge `IF` predicates; typed global/shared/local storage and alias facts; synchronization dependencies | WHIRL rewriter; link every loop and memory operation to selected plan ID and input source location |
| Host integration | Launch/provider operation, arguments, grid/block configuration, selected variant identity, and fallback | Host PU and runtime interface; never confuse this call with the device-kernel body |
| Target lowering and source emission | Thread/block coordinates, memory-space address formation, barriers, PTX instructions, async copy/TMA, matrix operations, and launch packaging | In the completed path, NVISA/backend and `whirl2c -cuda` consume the same selected device PU. The existing template emitter remains a labeled migration baseline, not the authoritative device definition; target-specific details do not enter common plan IR as hard-coded semantics. |

`WN_CreateDO` and `WN2C_do_loop` already establish a structured WHIRL loop and C emission route. The NVISA tree already contains thread/block special-register handling, global/shared symbol-space flags and lowering, `INTRN_SYNCHRONIZE` to `bar.sync`, and PTX `.entry` emission. These are concrete reuse points, not proof that the historical NVISA backend supports Hopper or Blackwell. First test the old path with a minimal device PU; extend or replace unsupported instruction paths deliberately.

Reuse audit anchors in this tree: `osprey/common/com/wn.cxx` and `osprey/be/whirl2c/wn2c.cxx` for structured loops and source emission; `osprey/common/com/symtab_access.h`, `osprey/be/cg/NVISA/exp_loadstore.cxx`, `osprey/be/cg/NVISA/expand.cxx`, and `osprey/be/cg/NVISA/cgemit_targ.cxx` for memory spaces, execution coordinates, barriers, and PTX entry/variable emission. `osprey/be/lno/NVISA/lnotarget.cxx` provides LNO-to-target opcode/resource hooks, but contains unfinished resource-estimation paths; reuse the interface and validated mappings, not uncalibrated cost values.

`DO_LOOP` alone does not encode CTA/warp ownership, address spaces, barrier uniformity, async transfer completion, register/SMEM capacity, or tensor-core instruction shape. Keep those as validated annotations and target lowering obligations. Materialize the selected synchronous kernel first; introduce `cp.async`/TMA pipeline operations only after the P8 event, arrival, wait, and buffer-lifetime model is explicit. Preserve a legal synchronous fallback.

## 6B.3 Lowering And Verification Sequence

1. Freeze the selected plan and its cost/legality record; retain baseline and rejected candidates for traceability.
2. Create a device PU and bind tile axes to `DO_LOOP` indices, bounds, steps, source-line provenance, and plan IDs. Represent K-tile staging, cooperative loads, compute, and epilogue as separate structured regions.
3. Attach global/shared/local storage symbols, tile-range alias and ownership facts, edge predicates, barriers, and buffer lifetimes. Check bounds, absence of unsafe overlap, resource limits, and barrier convergence before target lowering.
4. Run `whirl2c -cuda` on the selected high-level device PU to derive `.cu` for the present remote-validation workflow. Independently lower that same PU through NVISA/PTX where the target feature is supported. Do not maintain an unrelated string-template kernel as the authoritative implementation after this milestone.
5. Verify a before/after WHIRL dump contains the complete `DO_LOOP` nest, memory operations, and synchronization in the device PU; check the emitted `.cu` against that PU and selected plan; run numerical and GPU performance validation when hardware is available. A detached loop, a host provider call, or a matching source comment by itself fails this milestone.

AI-P9 tracing must name the device PU, selected plan, loop IDs/bounds, global and shared buffers, barrier sites, source line, target path, verification status, and actual rewrite outcome. The analyzer's `physical-plan.log` and `optimization-summary.log` must distinguish `plan_selected`, `host_dispatch_rewritten`, `device_pu_materialized`, and `device_target_lowered`; no one event implies the next.

## 6B.4 Comparative Evaluation Gate

Compare equivalent GEMM shapes, dtypes, layouts, numerical tolerances, launch conditions, and hardware in distinct lanes: (1) nvcc/NVVM compiling comparable CUDA, (2) Clang CUDA as a separate LLVM implementation, and (3) CUTLASS and Triton as library/kernel-system baselines rather than synonyms for nvcc. Report correctness, kernel latency and throughput, HBM/SMEM/register traffic or proxies, occupancy, bank conflicts, compile time, and generated PTX/SASS when available. Measure both the structured-WHIRL path and the existing source-emission baseline. Claim superiority only for demonstrated workloads and metrics; where NVIDIA proprietary compiler internals are unavailable, compare observable outputs rather than inventing pipeline details. Section 12 separates NVIDIA's nvcc/NVVM evidence from LLVM and MLIR documentation.

## 6B.5 CUDA C Derived From WHIRL

**Accepted DSC design decision and partial implementation.** In the completed path, the selected device PU is the single executable kernel definition. The implemented standalone `whirl2c -cuda <selected-whirl.B>` translates the selected synchronous device PU to `.w2c.c`; the existing C mode retains its behavior. The compiler emits that image with `-Wb,-DSL:cuda_whirl_output=<new-file.B>`. The translation currently runs as a separate command, not in-process, and NVISA/PTX lowering from this PU remains unverified. Emit before lowerings that destroy structured loops or memory-space intent.

**Migration baseline.** The existing source-template emitter remains under `-Wb,-DSL:cuda_legacy_emit=ON`, defaulting to ON during development. `-Wb,-DSL:cuda_tile_output=<new-file.cu>` requests its output; ordinary compilations do not gain an unsolicited sidecar. `cuda_legacy_emit=OFF` suppresses it, while `cuda_whirl_output` can independently drive cost selection and the synchronous host provider rewrite. The phase report must distinguish legacy-source suppression from selection performed for device WHIRL. Retain the legacy `.cu` for comparison, but never label it WHIRL-derived.

The CUDA mode must translate the full synchronous GEMM body: device entry and parameters, CTA/thread coordinates, selected `DO_LOOP` nests, edge predicates, global-memory addresses, shared-memory A/B buffers, cooperative loads, barriers, register accumulators, FMA/compute loops, and stores. It must obtain type, shape, address space, ownership, and layout from verified WHIRL symbols/operations and selected-plan bindings. A host launch wrapper is a distinct artifact that consumes the same plan ID, grid/block dimensions, arguments, and guards; it is not a second definition of device behavior. Existing `WN2C_do_loop` supplies generic loop emission, but CUDA qualifiers, GPU builtins, memory spaces, barriers, and later TMA need explicit CUDA-mode handling.

The first `whirl2c -cuda` implementation accepts only the synchronous F32 matmul subset and fails closed on an unsupported device operator, memory-space annotation, intrinsic, or barrier contract. It must not fill a gap by splicing in a handwritten CUDA kernel body. P8 may add `cp.async` or TMA only after transfer, buffer lifetime, barrier arrival/wait, and fallback semantics exist in WHIRL and have a tested CUDA translation. New target operations must use compatible existing WHIRL mechanisms where possible; binary WHIRL format changes require an explicit versioned contract.

Driver-owned program-level code creates and registers the device PU after whole-plan legality and profitability selection; the per-PU VHO pass must not silently fabricate a second PU. The same verified PU can feed `whirl2c -cuda` and, for supported features, NVISA/PTX lowering. Retain two separate, immutable review lanes for an equivalent selected plan: legacy `.cu` plus its `.t` trace and receipt, and WHIRL-derived `.cu` plus the saved device `.B`, translation trace, and receipt. Compare loop bounds, memory spaces, buffer sizes, barrier/transfer order, launch geometry, numerical output, and target performance; source text need not match byte-for-byte. The selected-plan trace and artifact receipt must identify the device PU when present, `.B` hash, source hash, emitter mode, translated loop/buffer/barrier counts, unsupported-feature result, and host launch binding. Accept the WHIRL-derived vertical slice only when the saved device WHIRL contains the complete synchronous kernel, its generated `.cu` compiles, its numerical result matches the baseline, and trace-to-source checks establish that no executable kernel behavior came from a parallel template. GPU timing remains a separate performance gate.

**2026-10-21 checkpoint.** The first sidecar device PU is still constructed inside VHO and written as its own mapped `.B`; moving PU creation and registration to driver ownership remains required before the main compilation unit can feed NVISA directly. Its current legality gate accepts only fixed F32 shapes exactly divisible by BM/BN/BK, so redundant edge predicates are omitted in the WHIRL-derived kernel. The default legacy kernel retains those guards. A 256-thread CPU CTA simulation of the selected 128x128x128 plan reports zero maximum error against both reference GEMM and the legacy kernel, and both sources pass C++ syntax checks with CUDA qualifiers stubbed. This is functional parity evidence for the synchronous subset, not NVCC compilation, PTX generation, Hopper execution, or an M2 acceptance receipt. P8/TMA, general edge masks, source/plan provenance in the sidecar, and native target validation remain open.

# 7 Candidate Generation And Profitability Architecture

Candidate generation should produce a candidate tree rather than a sequence of independent pass decisions. Fusion, layout, placement, sharding, tiling, memory residency, fetch, pipeline, communication, and runtime-variant choices become branches within OptimizationCandidateIR and are evaluated as OptimizationPlanIR instances.

OptimizationPlanIR { plan_id; semantic_region; tensor_evolution_graph; fusion_choices; layout_choices; placement_choices; sharding_choices; communication_choices; tile_choices; memory_residency_choices; fetch_prefetch_choices; pipeline_choices; kernel_choices; estimated_cost; legality; runtime_guards; }

## 7.1 Cost Model Gate

The optimizer must not commit a fusion, tiling, layout, placement, sharding, memory-residency, fetch/prefetch, communication, or runtime-variant decision only because the transformation is locally legal. Selection requires an explicit cost record and a legality record on the whole candidate plan. This is the main design lesson drawn from Open64 LNO: candidate transformations should remain representational until enough dependence, reuse, memory hierarchy, loop/tile, and machine-resource information has been collected to compare plans.

This gate applies to static and runtime-adaptive plans. A static selected plan requires CandidateCostIR with sufficient confidence for the target hardware. A runtime variant requires both CandidateCostIR and RuntimeGuardIR so that the runtime can select only among certified variants.

## 7.2 Latency Equation

Use an explicit plan-level latency model so the optimizer can reason about overlap rather than simply summing all component costs.

T_total = T_compute + T_memory_unhidden + T_communication_unhidden + T_sync + T_launch + T_runtime_selection

T_memory_unhidden = max(0, T_memory_raw - T_memory_hidden_by_compute - T_memory_hidden_by_communication)

T_communication_unhidden = max(0, T_communication_raw - T_communication_hidden_by_compute - T_communication_hidden_by_memory)

T_compute = max(T_tensor_core, T_cuda_core, T_vector, T_reduction) adjusted by occupancy and issue constraints

For low-precision formats such as Blackwell NVFP4, the model must not only scale bytes by dtype width. It should expand the equation with scale metadata and layout terms:

T_total(dtype, tile) = T_compute(dtype, instruction_tile) + T_scale_metadata_load + T_scale_apply_or_fused_scale + T_memory_unhidden(value_tile + scale_tile) + T_sync + T_launch + T_runtime_selection

CandidateCostIR should store raw and unhidden terms separately. A plan with higher raw memory traffic can still win if the traffic is hidden by a stronger async pipeline; a plan with lower raw traffic can lose if synchronization or occupancy loss makes stalls visible.

## 7.2A Roofline Bound And Data Supply Contract

Roofline is a lower bound on execution time and a diagnostic for a complete OptimizationPlanIR, not another term to add to the latency equation. For a candidate, let W be the counted floating-point operations on the code path being evaluated. For each memory level L with B_L > 0, let B_L be the bytes predicted to cross that level, I_L = W / B_L its arithmetic intensity, and beta_L the sustainable bandwidth available to that path. Let pi_path be the sustained compute ceiling for the actual instruction family and dtype. Then:

P_roof = min(pi_path, min_L(I_L * beta_L))

T_roof = max(W / pi_path, max_L(B_L / beta_L))

The data-availability metric is beta_L in bytes per second, not memory capacity. The compiler may also report beta_required,L = pi_path / I_L to show the bandwidth needed to sustain the compute ceiling. Capacity, cache residency, occupancy, coalescing, shared-memory banking, and concurrent consumers influence B_L or attainable beta_L, but are not interchangeable with bandwidth. A published peak and a calibrated sustainable ceiling are distinct evidence classes. The target profile must name the specific device, relevant clocks, memory path, instruction path, calibration method, and confidence; a generic Hopper or Blackwell capacity envelope is not a calibrated Roofline target.

CommonTilePlanIR supplies W and logical movement by tile and stage. CommonFetchPlanIR and CommonPipelineIR identify the actual movement path. CandidateCostIR carries a per-level Roofline evidence record with predicted read/write bytes, cache-policy assumptions, arithmetic intensity, ceiling values and units, limiting resource, bound, provenance, and uncertainty. MemoryHierarchyDescriptorIR and ComputeResourceIR own reusable performance-capability fields; NVIDIA target adapters own SKU-specific calibrations. AI-specific semantics supply GEMM/attention/MoE operation counts, dtype behavior, scale metadata, and workload-state distributions. Global load requests are not automatically HBM bytes: an L2 hit changes the HBM denominator. Emit separate HBM, L2, and shared-memory intensities or mark a level unavailable. Do not mix a tensor-core peak with a scalar/vector FMA candidate.

The compiler can construct a predicted Roofline before GPU execution using modeled B_L and documented or calibrated ceilings. It must label the result `predicted`, carry cold/warm cache or other traffic scenarios when uncertain, and reject impossible predictions such as a modeled kernel time below T_roof under the same assumptions. The bound does not prove a candidate profitable: small GEMMs may be limited by launch latency, insufficient CTA waves, dependency stalls, or pipeline fill/drain, none of which the classical steady-state Roofline explains. Keep the scheduled latency model in Section 7.2 as the profitability estimate and use Roofline to check its consistency and explain headroom. The current AIO-9 relative score and `selection_reason=lowest_complete_relative_cost` must not be relabeled as Roofline-derived latency.

To measure effectiveness, compile and run the baseline and alternatives as comparable GPU kernels on the same device, verify numerical results, and retain kernel time, actual instruction path, resource use, and observed traffic at each available memory level. Join those measurements to the compile-time plan and source identity. Plot observed intensity W / B_observed,L and achieved performance W / T_kernel against the matching measured or calibrated ceiling. Report Roofline efficiency P_achieved / P_roof, model error T_predicted versus T_kernel, and whether the selected plan actually outranks rejected candidates. A GPU Roofline must not use the current direct CPU fallback as its GPU baseline. Profiling changes execution conditions, so preserve unprofiled timing separately from counter collection. Where no native GPU evidence exists, report `measured=unavailable`, not an inferred speedup.

## 7.2B Analytical Readiness Selection And Bubble Diagnostics

Current bounded F32 joint selection is extended by the
[occupancy-aware resource model](AI-COMPILER-OPTIMIZATION-OCCUPANCY.md).
Model6 rounds logical demand using table-owned quanta, estimates resident
CTAs/warps and grid underfill, and compares lockstep full/tail rounds while
sharing per-SM throughput and device bandwidth. It records the legacy
one-CTA/SM winner as a counterfactual, including unchanged outcomes.
Native registers, spills, runtime carveout and certified occupancy remain
unknown; neither capacity maxima nor logical live values certify performance.

Update 2026-10-26: production joint selection now follows the separate
[analytical readiness design](AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md).
It replaces traffic-score overlap credit with a bounded schedule recurrence
for operand readiness, consumer work, slot release/refill, bandwidth service,
startup and drain. Complete D/S1/T2 alternatives share one assumed-cycle
profile before transformation. Joint model 5 defaults to readiness; explicit
`gemm_cost_model=relative` preserves model 3 for comparison. Baseline mode is
unchanged. The source-warp simulator below remains a separate diagnostic;
its proposed production integration/select controls are not implemented.

Hardware descriptions are the single target-data authority. Familiar exact
`sm_*` values resolve to our own versioned common tables; per-path latency,
bandwidth, compute/local service and synchronization timing are not private
constants in an optimizer. See
[Hardware Description IR And Target Selection](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md).
Current quantitative rows are explicitly estimated, not measured NVIDIA
timing. New `HW_DESC/HW_PERF` trace records identify the table and overrides.

This initial analytical selector is assumption-driven, not confidence-certified
or calibrated GPU performance. Native occupancy/cache/queue refinements remain
open. Its explicit profile, formulas, provenance, candidate costs and waits
must be audited in `.t` and the phase/summary logs. Do not mix reference cycles,
relative traffic scores, Roofline nanoseconds or simulator waits in one total.

The objective is minimum GEMM completion latency, not maximum occupancy,
allocated storage or bandwidth consumption independently. Tiling reduces
operand demand through reuse; coalescing/banking improve delivery efficiency;
prefetch and scheduling make operands ready before useful compute needs them.
A waiting warp is not necessarily a compute-unit bubble if other legal work
covers it. Summed actor stalls are not kernel elapsed cycles.

Adopt the review proposal in
[GEMM Bubble Prediction And Candidate-Selection Experiment](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md).
Its pseudocode builds a read-only event graph from the existing lowerable
plan, models dependencies plus finite issue/memory/queue resources, and
predicts completion, compute issue opportunities and attributed waits. P7
provides tile/access/resource alternatives, P8 provides actual movement and
slot-order contracts, and P9 compares complete schedules before mutation.
Assembly/SASS may later refine dependencies and instruction work; instruction
spacing alone does not establish cycles or memory-service latency.

The first implementation milestone is shadow-only on the same bounded
D/S1/T2 census, preserving the existing actual winner, S1 fallback and
WHIRL-derived CUDA. Compare the old winner with the hypothetical cycle-model
winner under identical versioned scenarios; report changed, unchanged or
incomplete ranking with causes and confidence. Never mix cycle predictions
with relative model-3 scores, add a second overlap discount, or add Roofline
as another latency term. Unknown facts cannot be treated as zero. Cycle-based
selection is a separately gated follow-on after completeness, consistency and
uncertainty review; the documented controls are not registered yet.

Hardware/software co-design should use this causal account to compare both
fixed plans and reoptimized plans across explicit hardware revisions. The
intent is to reduce dependence on broad empirical search, not to claim that
analytical prediction eliminates native correctness checks or calibration.
All added phase reports must summarize operand readiness, resource demand,
remaining bubbles, ranking sensitivity and actual versus shadow decisions.

The first source-warp replay implementation and its explicit synthetic timing
and source-expansion assumptions are documented in the bubble design's
Section 10. It operates downstream of authenticated compiler traces; it is
not the production VHO adapter or a cycle-accurate NVIDIA model.

## 7.3 Profitability Decision Timing

Early phases create candidates. Later phases refine candidates. Final selection should occur only after the optimizer can estimate register count, shared-memory footprint, occupancy, communication effect, memory overlap, synchronization, launch count, and runtime guard coverage.

## 7.4 Mandatory Candidate To Rewrite Loop

The optimizer must contain an explicit commitment point. Delaying transformation is useful only if the compiler eventually selects a plan and rewrites code. For every optimization family, the implementation must provide:

- detector: finds the region and records why it is eligible.

- candidate generator: creates baseline and transformed alternatives.

- legality checker: proves safety or emits a RuntimeGuardIR plus conservative fallback.

- cost builder: fills CandidateCostIR for every candidate, including confidence and missing-cost flags.

- selector: chooses a candidate or records a no-transform decision.

- rewriter: changes the IR into the selected physical form.

- verifier: checks structural validity, numerical correctness, and performance counters.

The minimum useful selector is intentionally simple:

1. Prefer the transformed candidate only if legality is proven or guarded.

1. Require estimated benefit above a configurable threshold.

1. Reject the transform if register pressure, shared-memory footprint, code size, or synchronization cost crosses a hard limit.

1. Emit the selected candidate id, rejected candidate ids, and rejection reasons into telemetry.

This loop prevents the design principle "do not commit too early" from becoming "never commit." The document's delayed-decision architecture is successful only when AI-P9 can select and lower a real plan.

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

Precision-changing GEMM candidates require explicit compiler-option authorization. The proposed `-DSL:gemm_precision=preserve|allow_narrow` mode defaults to `preserve`; `allow_narrow` admits numerically permitted, target-supported formats such as FP8, MXFP4, or NVFP4 for consideration but never forces the smallest bit width. A higher-precision source also requires a region-specific numerical acceptance contract or validated quantization recipe. Hardware capability, `-DSL:quant`, and cost-model preference alone do not authorize reduced precision. The source-format plan remains the baseline and fallback.

The target profile describes supported input/scale/accumulator/output formats, instruction shapes, layouts, movement requirements, and resource/cost terms. Common candidate and plan records carry option provenance and legality evidence, while AI policy owns the model-quality criterion. The hardware detail design specifies the proposed option and its gates; neither the option nor operand conversion is implemented yet.

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

- Do not conflate persistent GMEM storage layout with lane access layout or transient SMEM/register layout. Remapping lanes can improve coalescing without moving the stored tensor; repacking GMEM requires an explicit allocation, conversion, lifetime, and amortization decision.

## 8.2 Fetch Prefetch And Async Movement

**P8 enhancement note, adopted 2026-10-27.** Movement planning must include
storage-access lifetime analysis and physical storage assignment. A range is
eligible for reuse only after every conflicting access completes and the
required happens-before/visibility proof covers the next overwrite. Consumer
issue is not access completion; empty dependence information is not a reuse
proof. Include aliases, all consumers, barrier state and repeated generations.
Compare repeated slot reuse and cross-purpose packing against disjoint-buffer
fallbacks. Extra synchronization and lost overlap are costed alternatives,
not free memory savings. Physical reservation, not logical death, determines
occupancy. The complete normative amendment and phase-report obligations are
in [Common execution planning, sections 5-6](AI-COMPILER-COMMON-EXECUTION-PLANNING.md).
Current bounded F32 P8 slot reuse remains implemented; a general lifetime
allocator/event scheduler and the other research cases remain unimplemented.

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

The implementation order must now be measured by transformed programs, not by IR objects alone. Each milestone below has an action requirement and an acceptance gate.

The first execution case is a standalone `common.matmul.v1`. It exercises multi-level tiling without fusion. The fusion priority below is a separate, later case and is not a prerequisite for certifying the first tiled matmul.

1. Priority 1: Minimal optimization harness. Add a benchmark driver, before/after IR dump, correctness checker, timing harness, and telemetry record. Acceptance gate: every optimization run emits baseline runtime, transformed runtime if any, selected candidate id, and rejection reasons.

1. Priority 2: First real tiling path. Select one canonical GEMM, convolution, or matmul-like loop/tensor region. Detect it, generate at least two tile candidates plus baseline, estimate memory traffic and resource pressure, rewrite the loop or kernel, and measure. Acceptance gate: the selected device PU contains visible high-level WHIRL tile loops, and either improves performance or records a cost-model miss. A tiled `.cu` sidecar alone is an intermediate milestone.

1. Priority 3: First real fusion path. Select one narrow producer-consumer pattern such as MatMul+Bias, Bias+Activation, or elementwise chain. Generate fused and unfused candidates, prove legality, estimate materialization savings versus register pressure, rewrite, and measure. Acceptance gate: transformed IR has one materialization removed or records why fusion was rejected.

1. Priority 4: P7.4/P8 pipeline path. For the tiled kernel, generate a shared-memory staging plan and a fetch/prefetch plan. The first implementation may use a simple double-buffer model or backend-specific async-copy hook. Acceptance gate: the selected plan records stage count, movement engine, barriers, and hidden versus unhidden memory estimate.

1. Priority 5: Cost-model calibration. Compare predicted benefit against measured benefit for the first tiling and fusion paths. Acceptance gate: CandidateCostIR records prediction error and identifies whether the miss came from compute, memory, synchronization, occupancy, or launch terms.

1. Priority 6: Runtime variants. Generate at least two certified variants for one optimization, attach guards, and select between them at runtime. Acceptance gate: guard hit/miss rate and selected variant id are recorded in telemetry.

1. Priority 7: Parallelization integration. Add CommonParallelizationIR, placement, sharding, and communication-intent derivation after the local candidate-to-rewrite loop is working. Acceptance gate: the distributed plan either lowers into explicit communication or records the exact unresolved ownership/alias blocker.

## 9.1 Minimum First Optimization Slice

The first coding target should be deliberately small. A recommended slice is:

1. Input: one canonical matmul-like region or loop nest.

1. Baseline: current lowering without tiling.

1. Candidate A: simple fixed tile over M/N/K or equivalent loop axes.

1. Candidate B: alternative tile with different reduction tile or register tile size.

1. Cost: estimate bytes moved, loop overhead, register pressure proxy, shared-memory footprint if used, and launch count.

1. Rewrite: emit the selected tiled form.

1. Validate: compare output numerically and measure runtime.

This slice is intentionally smaller than the full design. It proves the compiler can make a choice and act. Once this loop exists, the architecture can grow without remaining purely descriptive.

For the current matmul path, the immediate representation step is narrower still: make one selected K-tile loop a verified `DO_LOOP` in a device PU, then move cooperative load, compute, and epilogue loops into that PU without changing the cost gate or trace identity. Only after structural verification should the emitted `.cu` be driven from this PU. The following target milestone reuses NVISA memory-space, thread-index, barrier, and PTX-entry machinery where validated; TMA and modern matrix instruction support require separate target work.

## 9.2 Minimum First Fusion Slice

The first fusion target should also be narrow:

1. Input: producer-consumer elementwise chain or MatMul plus simple epilogue.

1. Baseline: materialized intermediate.

1. Candidate: fused region that removes the intermediate write and read.

1. Legality: shape compatibility, single-consumer or safe multi-consumer handling, no unresolved side effects.

1. Cost: materialized bytes removed, launch removed, register pressure added, occupancy risk.

1. Rewrite: replace the region with fused IR.

1. Validate: compare output numerically and measure runtime.

Fusion is not considered implemented until the rewrite removes a real materialization or launch, not merely when FusionCandidateIR exists.

## 9.3 No Optimization Is Also A Result

A rejected transform is acceptable only if it is explicit. The compiler should emit a WOPT trace record into the normal `.t` trace file with:

- region id and pattern name.

- generated candidates.

- legality failures.

- cost failures.

- missing analysis or backend support.

- next implementation blocker.

This makes minimal optimization visible as an engineering fact rather than a mystery.

## 9.4 WOPT Trace Integration For AI Optimization

The implementation trace should follow the existing Open64/WOPT trace convention rather than introducing a separate compiler-side log as the primary record. WOPT writes diagnostic trace content to the `.t` file through `TFile`, and trace masks are queried with `Get_Trace(phase, mask)`.

Relevant source anchors:

- `common/util/tracing.h`: `TP_WOPT1`, `TP_WOPT2`, `TP_WOPT3`, `TP_GLOBOPT`, and `TFile`.

- `common/util/tracing.c`: trace phase ids `OPT`, `OP2`, and `OP3`; `Get_Trace_Phase_Number`; `Get_Trace`.

- `common/com/config.cxx`: `Process_Trace_Option`; `-ttPHASE:MASK` selects trace mask bits, while `-trPHASE` selects IR dumps for a phase.

- `be/opt/opt_defs.h`: WOPT trace-mask definitions.

Proposed allocation:

```text
phase: TP_WOPT2 / OP2 / numeric phase 26
mask:  0x00800000
name:  AI_OPT_TRACE_FLAG
use:   AI optimization candidate, cost, selection, rewrite, and no-transform trace
```

The source-level constant should be added near the existing `TP_WOPT2` flags in `be/opt/opt_defs.h`:

```c
#define AI_OPT_TRACE_FLAG 0x00800000 /* trace AI optimization planning */
```

The operational switch should be:

```text
-Wb,-ttOP2:0x00800000
```

The numeric equivalent is:

```text
-Wb,-tt26:0x00800000
```

If an IR dump is also needed around WOPT, use the existing IR trace selector separately:

```text
-Wb,-trOPT
-Wb,-trOP2
```

This distinction matters: `-tt` owns mask-bit debugging, while `-tr` owns per-phase IR dumps. The AI optimization decision trace should therefore be gated by `Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)` and printed to `TFile`; optional IR dumps can be requested with `-tr`.

Trace records use whitespace-separated positional columns. Tokens must contain no whitespace; `-` means not applicable. The first ten columns are fixed, and any remaining columns follow the record schema below. The first `PASS` row carries the schema version so a summary tool can reject an unknown layout.

```text
AI_OPT RECORD PU PHASE REGION CAND KIND DECISION TOTAL_CYCLES REASON [PAYLOAD...]
```

The payload is positional for each record; values and units are fixed by this table. `TOTAL_CYCLES` is an estimate, not a measured runtime.

| Record | Payload columns, in order |
| --- | --- |
| PASS | schema_version target_model |
| REGION | Matmul: M N K dtype. Attention: batch sequence heads head_dim dtype. |
| CAND | Tile: CTA_tile warp_tile stages warps. Fusion: fusion_pattern. |
| LEGAL | shared_bytes registers_per_thread active_CTAs_per_SM |
| COST | compute memory_unhidden communication_unhidden sync launch runtime_selection memory_raw memory_hidden; all in cycles |
| SELECT | no payload |
| HINT | movement_engine stages prefetch_distance pingpong |
| REWRITE | loops_changed materializations_removed launches_removed |
| SKIP | next_blocker |
| END | regions candidates rewrites skips |
| DETAIL | subtype-specific columns; subtype is the REASON column |

For `COST`, `TOTAL_CYCLES = compute + memory_unhidden + communication_unhidden + sync + launch + runtime_selection`. `memory_hidden` is already excluded from `memory_unhidden`; do not subtract it again. A candidate's `COST` record should be explainable by the resource estimates in its `LEGAL` and optional `DETAIL` records.

Example of two legal tiling candidates and a selected prefetch plan:

```text
AI_OPT PASS    matmul_0 P0 -   -  -        begin  -      ok       1 sm90
AI_OPT REGION  matmul_0 P1 r12 -  matmul   -      -      ok       4096 4096 4096 fp16
AI_OPT CAND    matmul_0 P7 r12 c1 tile     -      -      ok       128x128x64 64x64x64 3 8
AI_OPT LEGAL   matmul_0 P7 r12 c1 tile     yes    -      ok       131072 112 1
AI_OPT COST    matmul_0 P7 r12 c1 tile     -      163840 ok       131072 28672 0 4096 0 0 53248 24576
AI_OPT CAND    matmul_0 P7 r12 c2 tile     -      -      ok       64x128x64 32x64x64 4 4
AI_OPT LEGAL   matmul_0 P7 r12 c2 tile     yes    -      ok       98304 96 2
AI_OPT COST    matmul_0 P7 r12 c2 tile     -      159744 ok       131072 24576 0 4096 0 0 49152 24576
AI_OPT SELECT  matmul_0 P7 r12 c2 tile     select 159744 min_cost
AI_OPT HINT    matmul_0 P8 r12 c2 prefetch yes    -      ok       tma 4 2 1
AI_OPT REWRITE matmul_0 P9 r12 c2 tile     yes    159744 applied  3 0 0
AI_OPT END     matmul_0 P9 -   -  -        done   -      ok       1 2 1 0
```

A rejected candidate still ends with a concise record:

```text
AI_OPT REGION attention_3 P1 r44 -  attention -    - ok            8 2048 32 128 fp16
AI_OPT CAND   attention_3 P2 r44 c9 fusion    -    - ok            qk_softmax_v
AI_OPT LEGAL  attention_3 P2 r44 c9 fusion    no   - alias_unknown - - -
AI_OPT SKIP   attention_3 P2 r44 c9 fusion    skip - alias_unknown tile_range_alias
```

Other pattern-specific `CAND` and `REGION` payloads need documented schemas before emission. Optional detail remains one line per fact, with the same fixed columns and a subtype in `REASON`; `occ` means active CTAs, registers per thread, shared bytes, while `pipe` means engine, stages, bubble cycles, hidden memory cycles:

```text
AI_OPT DETAIL matmul_0 P7 r12 c2 cost - 159744 occ  2 96 98304
AI_OPT DETAIL matmul_0 P8 r12 c2 cost - 159744 pipe tma 4 4096 24576
```

The first summary tool should scan only lines beginning with `AI_OPT ` in the `.t` file. It should parse columns by record type and schema version, group by PU/region/candidate, and ignore unrelated WOPT output.

Minimum trace contract for the first optimization slice:

1. With `-Wb,-ttOP2:0x00800000`, the `.t` file must contain `AI_OPT PASS` and `AI_OPT END`.

1. Every analyzed region must produce exactly one terminal decision: `AI_OPT REWRITE` or `AI_OPT SKIP`.

1. Every selected rewrite must have preceding `AI_OPT CAND`, `AI_OPT LEGAL`, `AI_OPT COST`, and `AI_OPT SELECT` records for the same candidate.

1. Every skipped region must include a reason and next blocker so “no optimization” remains explainable.

1. Summary tooling should report counts by pattern, decision, skip reason, and transformed IR kind.

## 9.5 Optimization Report Contract By Phase

Every phase must state what it learned, which alternatives it kept or rejected, and what the next phase must resolve. A trace analyzer must distinguish "not run," "ran with no eligible region," "analyzed but blocked," and "selected and rewritten." The table below is the minimum report contract, not a claim that the current trace schema or compiler already supplies every field.

| Phase | What the tool must summarize | Minimum evidence or gap |
| --- | --- | --- |
| AI-P0 Semantic tensor analysis | Regions found and missed; operator pattern, tensor role, shape, dtype, dynamic dimensions, and unresolved semantic assumptions. | REGION plus a phase status; current format does not record missed patterns. |
| AI-P1 Lifetime reuse and locality | Producer/consumer span, last use, reuse or working-set estimate, dominant traffic, and alias uncertainty that limits reuse. | Per-region DETAIL data with units and provenance; not yet standardized. |
| AI-P2 High-level fusion | Fused and unfused alternatives, intermediate bytes and launches potentially removed, legality result, and why each fusion remains provisional or is rejected. | CAND, LEGAL, COST, and a baseline candidate; baseline is not yet mandatory. |
| AI-P3 Logical layout | Layout alternatives, conversion bytes/cost, expected coalescing or reuse effect, and constraints passed to placement. | CAND and cost/detail records for layout; payload schema still missing. |
| AI-P4 Distributed placement and sharding | Ownership and shard alternatives, cross-device bytes, memory balance, alias or visibility assumptions, and rejected mappings. | Plan lineage and distributed-alias facts; neither has a required trace record yet. |
| AI-P5 Communication derivation | Collectives and transfers implied by each placement, bytes, topology/path, estimated exposed time and overlap, and unresolved communication boundaries. | Communication-specific DETAIL schema and plan linkage; both remain open. |
| AI-P6 Local physical layout and residency | HBM/L2/SMEM/register placement, footprint versus capacity, spill or eviction risk, occupancy effect, and rejected residency plans. | LEGAL, COST, and residency DETAIL with resource limits; detail schema remains open. |
| AI-P7 Multi-level tiling | Baseline and all tile candidates, legal and resource failures, cost terms, winner versus baseline, expected gain, confidence, and predicted Roofline work/traffic/intensity by memory level where inputs exist. | CAND, LEGAL, COST, SELECT; Roofline ceilings and per-level byte provenance need a versioned trace extension. |
| AI-P8 Fetch prefetch and async movement | Engine, stages, distance, barriers, fallback, raw and hidden memory time, residual compute bubbles, and why the chosen pipeline wins. | HINT, COST, pipeline DETAIL; barrier/fallback and bubble schemas remain open. |
| AI-P9 Physical kernel fusion and scheduling | Final whole-plan composition, rejected alternatives, fusion boundary, launches and syncs, selected cost, host versus device PU changes, structured loop IDs, memory-space and barrier facts, target path, and structural verification. | Stable plan identity, SELECT, REWRITE, and a verification record; device-PU loop materialization and target lowering remain incomplete. |
| AI-P10 Runtime variants and adaptation | Each guarded variant, guard coverage and fallback, selection overhead, expected cost by workload bucket, and chosen static default. | Runtime-guard/variant detail and workload-bucket schema; not yet specified. |
| AI-P11 Telemetry and profile feedback | Observed latency and per-level traffic versus predictions; achieved versus predicted Roofline position, efficiency, selection-ranking error, model error by cost term, variant hit rate, and next tuning action. | External measured profile joined to compile trace and target calibration identity; a compile-only .t file must report measurements as unavailable. |

The tool should produce a phase rollup and, for each PU/region, a compact candidate ledger: baseline, alternatives, legality, predicted total and cost components, confidence, selected plan, selection reason, rewrite result, verification, and remaining blocker. It should state the source of each number (analytical estimate, learned estimate, measurement, or unavailable). A cost estimate must never be presented as measured speedup.

## 9.6 Trace And Analyzer Gaps To Close Before Implementation

The v0.2 positional records are a useful starting point, but they cannot yet support the complete report above. The following additions are required before claiming the report is implemented:

- **Phase coverage:** emit a compact phase-end status and counts for each enabled phase, including zero eligible regions and a reason for disabled or blocked phases. Otherwise an absent record is ambiguous.
- **Baseline and plan lineage:** cost a conservative baseline for every region and identify which component candidates form each whole OptimizationPlanIR. Keep ids stable from generation through selection and rewrite. Otherwise the tool cannot compute a defensible expected gain or connect an early fusion choice to a later tile and pipeline choice.
- **Profitability evidence:** record cost-model source, confidence or missing-cost flag, target/resource assumptions, baseline and selected totals, and the reason for each rejection. Preserve raw and unhidden memory/communication separately; compute gain from comparable whole plans under the same assumptions. For Roofline, retain unscaled FLOPs, predicted bytes per memory level, matched sustained compute/bandwidth ceilings, assumptions, and provenance; the existing scaled relative-cost columns cannot reconstruct a validated Roofline.
- **Decision and transformation evidence:** identify the selected whole plan, distinguish cost rejection from legality rejection, record before/after IR identities and concrete change counts, and emit structural verification pass/fail. A SELECT without a successful REWRITE and verification must not be reported as an applied optimization.
- **Runtime and profile boundary:** record guard/fallback definitions and workload buckets for variants. Join measured telemetry only through a documented compilation/profile identity; keep predicted cycles and observed latency in separate columns and units.
- **Versioned schemas and validation:** define positional payloads and units for every emitted pattern and DETAIL subtype; increment the trace schema version when adding records. Reject unknown schemas, duplicate terminal decisions, missing baseline or cost, unmatched ids, inconsistent COST sums, and SELECT/REWRITE mismatches rather than silently producing a polished but false report.

The proposed analyzer command is `ai-opt-report <compile.t> [--pu NAME] [--region ID] [--phase P7] [--candidates]`. Its default output should be a readable text report with a coverage table, phase rollup, candidate/profitability ledger, decision reasons, transformation/verification results, and an explicit "data unavailable" section. It must ignore unrelated WOPT lines. Machine-readable export may be added later, but should not replace the compact human report or reintroduce repetitive compiler-side JSONL.

Acceptance gate for the first optimization slice: a selected region and a skipped region both produce complete, checkable reports; the selected report shows baseline versus selected cost and the verified IR change; the skipped report names its blocker; a malformed or incomplete trace fails visibly. Report requirements should be refined with each new optimization family before its trace records are emitted.

## 9.7 Matmul Trace, Report, And Durable Artifact Convention

The first matmul optimization uses the compiler's normal `.t` file as the unmodified source of truth. `ai-opt-report` derives focused, readable logs from `AI_OPT` records; these logs are not separate compiler emissions. The raw `.t` file and every derived log are retained in the same immutable review receipt. The initial `common.matmul.v1` case has no fusion candidate or fusion rewrite; AI-P2 is explicitly `not_applicable`.

Every derived `.log` file begins with exactly one line of the form `# What it summarizes: <phase-specific description>`. No timestamp, banner, or source metadata precedes it. The next lines identify the raw `.t` SHA-256, trace schema version, target, compilation identity, PU/region, and phase status. The report generator owns these descriptions and tests the exact first-line prefix.

| Phase | Derived file under `reports/phases/` | Required first-line description after `# What it summarizes:` |
| --- | --- | --- |
| AI-P0 | `semantic.log` | Matmul regions found, tensor roles, shapes, dtypes, and unresolved semantic facts. |
| AI-P1 | `locality.log` | Tensor lifetimes, reuse, traffic baseline, and alias uncertainty. |
| AI-P2 | `fusion.log` | Fusion candidates, legality, costs, and decisions; out of scope for the first matmul case. |
| AI-P3 | `layout.log` | Logical layout alternatives, conversion costs, and access implications. |
| AI-P4 | `placement.log` | Device ownership, shard alternatives, and rejected placements. |
| AI-P5 | `communication.log` | Required transfers or collectives, exposed cost, and unresolved boundaries. |
| AI-P6 | `residency.log` | Memory-level footprints, capacity, occupancy constraints, and rejected residency plans. |
| AI-P7 | `tile.log` | Matmul baseline, G0-G11 tile refinements, legality, resources, estimated costs, and tile selection. |
| AI-P8 | `fetch-pipeline.log` | Movement engines, stages, prefetch distance, barriers, overlap, and fallback. |
| AI-P9 | `physical-plan.log` | Complete plan selection, actual rewrite, before/after IR identity, and verification. |
| AI-P10 | `runtime-variant.log` | Certified variants, guards, guard cost, and fallback. |
| AI-P11 | `feedback.log` | Measured versus predicted results and cost-model corrections when profile data exists. |

`reports/optimization.log` begins `# What it summarizes: Complete P0-P11 optimization process, including every phase decision and non-execution reason.` It contains the complete content of each phase log in phase order, separated by a phase heading; no phase detail is omitted merely because it rejected or did not apply a transformation. `reports/optimization-summary.log` begins `# What it summarizes: Cross-phase coverage, selected plans, applied rewrites, measurements, and remaining blockers.` It is the compact entry point, not a replacement for `optimization.log`. It has one row for every AI-P0 through AI-P11 phase with status, reason, region/candidate counts, selected plan, and rewrite count. The phase log body appears exactly once in `optimization.log`; the summary contains only rollup facts.

The driver must emit explicit phase coverage records even when a phase is not invoked. The report distinguishes `disabled_by_option`, `disabled_by_policy`, `not_applicable`, `no_eligible_region`, `analyzed_no_rewrite`, `rewritten`, and `failed`. A disabled phase records the controlling option or optimization-level policy; `not_applicable` records the workload scope; `analyzed_no_rewrite` records the legality or profitability reason and next blocker. Missing required coverage, unmatched candidate/plan IDs, duplicate terminal decisions, a selected plan without verified rewrite evidence, and inconsistent cost units are trace errors, never inferred phase statuses. Compile-time estimates and measured runtime stay in separate fields with explicit units and provenance; AIO-9's current low-confidence relative score must not be relabeled as cycles.

The durable destination mirrors the FHE effort's receipt convention without reusing its domain directory: `/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/<target-id>/<milestone>/sha256-<full-primary-artifact-digest>/`. Milestones are `m0-baseline`, `m1-trace-report`, `m2-multilevel-tiling`, and `m3-prefetch-pipeline`.

The pre-trace `m0-baseline` receipt retains source, binary `.B`/separate-process `.T`, correctness and timing evidence, and explicitly marks AI trace reports unavailable. From `m1-trace-report` onward, every receipt also retains the unmodified `traces/compile.t`, all derived reports above, and plan/cost records. Every receipt retains exact commands, `receipt.json`, and `SHA256SUMS`, and records producer commit and binary hash, input hashes, options, target/driver identity, evidence class, and the preceding receipt identity.

Produce in fresh staging, verify, hash, promote atomically without replacement, reopen from the final path, then update `INDEX.md`, `index.json`, and `current`. Never clean or overwrite an accepted receipt. Existing `artifacts/ai_optimization/` output remains legacy evidence and is not moved or pruned by this convention.

# 10 Research Inspirations And DSC Design Proposals

This section records provenance. Research papers motivate design directions, but the IR structures and phase ordering in this document are DSC design proposals unless explicitly implemented and validated.

| Research item | Venue or date | Research-derived idea | DSC design inspiration |
| --- | --- | --- | --- |
| Mercury | SOSP 2025 | Asynchronous multi-GPU operator optimization for LLMs with remote-memory scheduling and communication-aware planning. | Inspiration for treating remote HBM, communication, and overlap as first-class plan components. |
| Neptune | 2025 arXiv, PLDI 2026 line | Advanced ML operator fusion for locality and parallelism on GPUs, especially reduction-operator sequences. | Inspiration for persistent fusion candidates refined with layout, tiling, and parallelism. |
| PipeThreader | OSDI 2025 | Software-defined pipelining for efficient DNN execution on heterogeneous GPU hardware units. | Inspiration for CommonPipelineIR, explicit buffering, async movement, and bubble minimization. |
| XNNC Fusion | CC 2025 | Greedy clustering operator fusion in a production neural-network compiler with profitability-guided expansion. | Inspiration for practical candidate generation and bounded profitability search. |
| HELM | 2026 | Compiler-guided heterogeneous LLM execution with compile-time CPU/GPU partitioning and KV-cache management for memory-constrained systems. | Inspiration for placement planning, heterogeneous residency, and runtime memory-state awareness. |


DSC design proposals in this v0.3 skeleton include the TensorEvolutionGraph-centered control plane, common-versus-AI ownership split, OptimizationCandidateIR and OptimizationPlanIR as profitability units, phase-to-IR matrix, explicit unhidden-latency accounting, the WOPT trace and analyzer contracts, and the stated implementation priority sequence.

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

Add hardware calibration sources, benchmark kernels, fitted parameters, confidence intervals, and fallback heuristics. Maintain versioned, device-specific compute ceilings by dtype/instruction path and sustainable bandwidth ceilings by HBM, L2, shared-memory path, transfer engine, clock regime, and relevant occupancy range. Record whether a ceiling is a published specification, independent microbenchmark, or fitted observation. Compare predicted and observed per-level bytes before fitting latency; otherwise a cache-traffic error can be mistaken for a bandwidth error. Test candidate ranking and Roofline efficiency across multiple M/N/K shapes, edge tiles, cold/warm cache states, and pipeline depths. Keep a held-out shape set and preserve unprofiled timing separately from profiler counter runs. Do not promote Roofline-based selection from exploratory to cost-gated until the prediction error, ranking error, and fallback policy have explicit acceptance thresholds.

## 11.5 Telemetry schema

Define runtime counters, profile record format, aggregation windows, and recompilation triggers.

## 11.6 Integration with FHE common substrate

Compare AI requirements against FHE optimization-control requirements and identify reusable substrate fields.

## 11.7 Prototype milestones

Maintain the action-gated milestone list from section 9. Each prototype milestone must include the detector, candidate generator, legality checker, selector, rewriter, verifier, and measured outcome.

## 11.8 Risk register

Track risks such as candidate explosion, inaccurate overlap modeling, runtime policy instability, and excessive IR complexity.

## 11.9 WHIRL device PU and NVISA modernization

Specify the selected-plan-to-`DO_LOOP` schema, plan/source provenance, GPU execution mapping, barrier and alias legality checks, memory-space symbols, `whirl2c -cuda` option and fail-closed source-emitter contract, the default-on legacy baseline switch, and rollback behavior. Audit NVISA by feature and architecture rather than assuming present-day GPU support: PTX version, 64-bit addressing, warp/CTA mapping, shared-memory layout and banks, `cp.async`, TMA/mbarrier, WGMMA/TMEM, low-precision formats, and launch packaging. Record each feature as reused, extended, replaced, or unsupported with a test and target GPU. Define comparable nvcc/NVVM measurements first; keep Clang CUDA and MLIR/Triton comparisons separate before claiming a compiler advantage.

# 12 Reference Notes

- NVIDIA, "NVIDIA Opens Up CUDA Platform by Releasing Compiler Source Code," December 13, 2011, https://nvidianews.nvidia.com/news/nvidia-opens-up-cuda-platform-by-releasing-compiler-source-code: dates the LLVM-based CUDA compiler in CUDA Toolkit 4.1, earlier than a 2014 start date.

- NVIDIA CUDA LLVM Compiler, https://developer.nvidia.com/CUDA-LLVM-Compiler: identifies nvcc as LLVM-based and libNVVM/NVVM IR as the compiler SDK's GPU path.

- LLVM Language Reference, https://llvm.org/docs/LangRef.html: documents the lower-level basic-block and branch representation used in the Section 6B comparison.

- MLIR Affine, SCF, and Transform dialect documentation, https://mlir.llvm.org/docs/Dialects/Affine/, https://mlir.llvm.org/docs/Dialects/SCFDialect/, and https://mlir.llvm.org/docs/Dialects/Transform/: a separate structured-IR comparison, not evidence that nvcc is MLIR-based.

- NVIDIA NVVM IR specification, https://docs.nvidia.com/cuda/nvvm-ir-spec/: documents an LLVM-based GPU IR path, without exposing proprietary nvcc optimization internals. Section 6B therefore treats performance superiority as an empirical question.

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

- NVIDIA Nsight Compute Profiling Guide, Roofline Charts and Memory Workload Analysis (https://docs.nvidia.com/nsight-compute/ProfilingGuide/): research-derived definition of arithmetic intensity, compute and bandwidth ceilings, hierarchical Roofline and measured achieved performance. Section 7.2A's compiler evidence fields, consistency gate, and selection-feedback use are DSC design proposals, not features attributed to NVIDIA.

- Triton compiler documentation and source metadata: used as design evidence for block meta-parameters, autotune configurations, MLIR-based lowering, target-specific layout/pipeline passes, TMA/MMA lowering, barriers, and tensor-memory handling.

# SMEM Implementation Addendum (2026-10-25)

After PR 188, synchronous F32 P7 planning retains compact/padded A and
compact/column-XOR B alternatives. `CommonTilePlanIR` now carries per-operand
staging bytes, physical B layout, A/B fill and compute bank-excess rounds,
B compute issue work and incremental address work. A target adapter supplies
the shared bank model. The default-ON
`-Wb,-DSL:cuda_smem_layout_search=ON|OFF` controls B-layout search.
`COST_MODEL 5` gates selection on all four shared-access classes, GMEM
sectors, read-issue work, XOR address work and synchronization. These are
low-confidence relative scores, not measured latency. Selected mappings are
lowered into device WHIRL before `whirl2c -cuda` emission; source templates
remain a parity baseline. `.t` adds positional `TILE_SMEM` evidence, which the
tool independently checks and summarizes in `tile.log` and the aggregate
logs. The complete formula, bijection, legality and scope are in the
[hardware planning addendum](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md#synchronous-smem-planning-package-2026-10-25).
Multi-stage/TMA WHIRL lowering, occupancy calibration and native GPU
acceptance remain open; this package does not close them.
