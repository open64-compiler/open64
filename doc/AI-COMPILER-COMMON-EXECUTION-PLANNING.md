# Common Execution Planning For AI Building Blocks

H4 implementation update: [GEMM hierarchy v1.0](AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md) consumes H2/H3 ownership and finite request budgets to compare staged B scalar retention with the existing consumers. The saved shared/issue work and extra RF reservation are explicit; no source/global traffic reduction or hardware speedup is inferred. Fixed selected application and COMMENT generation stay in the existing VHO/driver path. See [plan v1.5](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.5.md).


H3 implementation update, 2026-11-12:
[physical costing v1.0](AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md) consumes the
owned H2 projections with exact H20/profile/model dependencies. A separate ns
adapter and request-wide finite estimate cache reuse the existing family owners
and M3 portfolio. Source/physical/legacy-cycle fields and capability remain
separate; candidate estimation creates no native IR. See [plan v1.4](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.4.md)
for current executable versus conditional/modeled scope and remaining gates.


H2 implementation update, 2026-11-12:
[owned movement facts v1.0](AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md)
projects the existing admitted family owners into detached work/storage/access,
byte-ledger and lifetime/event records with immutable H20 dependency bindings.
This is a source/conditional-contract projection, not general four-family
executable admission. Missing canonical routes and later materializers retain
the explicit gates in [plan v1.3](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.3.md).
No candidate mutation, hardware-service claim, new instruction or runtime
lease is implied. The architecture and future common planning scope below remain.


Version v0.1 | 2026-10-27 | Adopted architecture; general implementation pending

## H1 resolved device context, 2026-11-12

[Resource/memory v0.3](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.3.md)
and [H20 plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md)
require exact gpu_device selection for H20 96 GB and 141 GB. Driver-owned
immutable ISA/device/resource/partition/profile/budget resolution precedes
search. Work Partition, Resource Lifetime and Execution Events consumers query
the same context through their owning phases; common records stay policy-free.
Shared sm_90 ISA does not establish H20/H200 resource or timing equivalence.
Dependent cache keys, selected checks and qualification retain these separate
bindings. This planned extension preserves two-phase search and compatible
fallback; it changes no existing schema or implementation status.

## H20 four-family design update, 2026-11-12

[Optimization design v0.9](AI_compiler_optimization_design_v0.9.md) applies the
existing Work Partition, Resource Lifetime and Execution Events abstractions
to confirmed H20 96 GB servers. The
[v0.2 resource/memory design](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.2.md)
jointly evaluates reuse, coverage, raw/decoded storage, transfer and compute
service for GEMM, MLA, LayerNorm and Reduction. This proposed extension uses
WHIRL opcodes, tensor traits and the existing owned planning records. Open64
analyses and phase verifiers enforce native numerical contracts, phase/scope
ownership, two-phase search and Q3 publication. H20 measurements and new
materializers are pending.

## 1. Decision And Scope

Adopt the colleague's three abstractions, **Work Partition**, **Resource
Lifetime**, and **Execution Events**, as the common execution-planning
framework. Retain computation characterization and verified algebraic
alternatives upstream. A shared characterization does not require identical
mathematics or a GEMM-shaped implementation for every operation.

This document is the common amendment to both living optimization skeletons,
the P8 contract, and the hardware/resource design. It does not rename their
versions, publish a binary IR schema, register an option, or replace WHIRL.
The source-to-device umbrella remains
[Open64 AI compilation and runtime architecture](OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md).
Its driver, package, provider and service boundaries remain unchanged.

Compute realization is another jointly refined view, not a replacement for
the three abstractions: [MMA/SIMT proposal](AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md).
It binds contraction instruction/fragment mapping to work partition, RF/SMEM/
TMEM lifetimes and typed compute-completion events. Existing scalar F32
D/S1/T2 support does not imply MMA lowering. General implementation remains open.

2026-10-28 specialization proposal:
[LayerNorm execution planning v0.1](AI-COMPILER-LAYERNORM-OPTIMIZATION-DESIGN-v0.1.md)
applies both layers to direct/retained centered recipes and explicitly authorized
statistics reformulations. It defines numerical-binding gates, buffer-generation
reuse, lane/bank/transaction accounting, complete costs and required phase
summaries. No new normalization executable support or generic P8 allocator is
claimed. Reduction profitability/lowering proceeds independently in the coding
chat; shared services must be reused rather than duplicated.

Its separate [LN-REF-1 arithmetic definition](AI-COMPILER-LAYERNORM-REFERENCE-ARITHMETIC-v0.1.md)
now fixes ordered mean finalization and the two-rounding sqrt/div reference.
Native v1 remains unresolved; reference definition, authorized native binding,
target conformance and candidate profitability remain separate gates.

The [native v2/admission design](AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md)
now specifies that reference's exact schema and typed ternary identity without
changing v1. Native meaning and producer-provenance evidence are distinct;
schema/record verification remains common/com-owned and policy-free. Native
registration/implementation is still open, independent of reduction coding.

The initial analysis scope is one canonical region in an active PU. PREOPT
provides canonical form; VHO owns PU-local semantic analysis and planning;
canonical-loop dependence/locality work uses LNO services. WOPT owns its
CFG/SSA facts. Cross-PU optimization requires the explicit IPA path. GPU
execution scope is not compiler analysis scope.

## 2. Terms Before Use

| Term | Definition |
| --- | --- |
| Semantic characterization | Domains, access relations, expressions, state/update/merge/finalize rules, effects, alias facts and numerical permissions describing what is computed. |
| Algebraic alternative | A different computation/state formulation with explicit equivalence obligations, preconditions and numerical acceptance; not just a different schedule. |
| Work Partition | Logical division of output/contribution domains and partial-result ownership, followed by a provisional mapping to execution resources. |
| Resource Lifetime | Separate value, storage-access and hardware-reservation lifetimes, plus constraints on physical storage assignment. |
| Execution Events | Symbolic issue, visibility, access-completion, operation-completion and synchronization events with typed dependency edges. |
| Logical buffer | Named tensor region or scratch object with size, alignment, layout, scope and access requirements; not yet a physical address. |
| Storage assignment | Concrete memory tier, arena, offset, extent and alignment assigned to a logical buffer in a candidate plan. |
| Generation | A particular logical use of a repeated slot; loop iteration and epoch distinguish successive contents at one physical address. |
| Happens before | A proved ordering with the required execution participants and memory visibility, not a source-text position or predicted timestamp. |
| Access completion | Point after which an operation will no longer read/write the specified storage range. This may precede whole-operation completion only when the target contract proves it. |
| Completion join | An event requiring completion of every relevant outstanding access before reuse. |
| Reservation | Storage/resources charged to a CTA, cluster or other target allocation scope; logical death does not itself return the reservation. |
| Complete plan | One semantic alternative plus mutually compatible partition, representation, placement, storage, movement, schedule, guards, fallback and cost evidence. |

## 3. Two Layers And One Authority

Native discovery requirement, 2026-10-29: computation characterization must
also be derivable from ordinary scalar/loop WHIRL, independently of frontend
operator names, framework classes, `normalized_shape`, or preattached DSL
normalization tags. An explicitly supplied native operation remains a valid
input contract, but verifying that contract is not evidence of discovering the
computation from its implementation. Existing bounded R1 normalization work
implements the former path. The 2026-10-30
[native partition-axis slice](AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md) now
derives bounded sum/squared-sum I/K domains from ordinary scalar WHIRL in LNO;
complete independent LayerNorm/RMSNorm recipe recognition remains open.

Derive iteration domains, access functions, accumulator recurrences,
initialization/update/finalization and broadcasts before assigning an operation
family. For normalization, the feature domain is the input-index domain reduced
into statistics and broadcast back to the corresponding outputs; unreduced
indices identify independent instances only after dependence/effect checks.
Neither a trailing axis nor a dimension named hidden/channels establishes that
domain. Logical domain flattening does not establish contiguous physical layout.

PREOPT exposes reviewed canonical forms without changing strict arithmetic;
the owning VHO/WOPT/LNO phase combines its existing def-use, recurrence, loop,
access and alias services within driver-selected scope. Common/com may carry
policy-free characterization/proof references, not perform recognition or
selection. Unknown accesses, effects or arithmetic remain explicit obligations
or refusals. Source annotations may be cross-checked as evidence but cannot
override contradictory native facts. No opaque library call is inferred to be
normalization without an admitted semantic contract or an analyzed body.

Native discovery publishes a read-only characterization, not an immediate
replacement operator or kernel. Preserve the actual source reduction order,
rounding, conversions, epsilon placement and observable intermediate results.
Only separately authorized equivalent formulations and complete cost-gated
plans may reach WHIRL transformation. The bounded algorithm is specified in
[LayerNorm design section 3.1](AI-COMPILER-LAYERNORM-OPTIMIZATION-DESIGN-v0.1.md#31-native-discovery-and-feature-domain-inference);
the actionable **R1-ND1 through R1-ND4** queue is in the
[implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#native-whirl-discovery-queue-r1-nd).

```text
canonical WHIRL + domain semantics and numerical contract
  -> computation characterization + verified algebraic alternatives
  -> candidate execution planning:
       Work Partition <-> Resource Lifetime <-> Execution Events
  -> complete-plan legality + resource-constrained cost + uncertainty
  -> selected-plan materialization in WHIRL
  -> target lowering/emission + package/runtime consumers
```

TensorEvolutionGraph preserves value/representation lineage. It is not a
substitute for computation semantics. Conversely, an algebraically equivalent
expression is not an executable plan until ownership, storage and ordering
are established. The three execution views reference the same region, value,
representation, candidate and plan identities. They must not become three
competing executable graphs. Only selected/lowered WHIRL authorizes emitted
device code; `whirl2c -cuda` validates and prints, never repairs a plan.

## 4. Contract And Ownership Map

These are required concepts, not newly allocated public type/opcode names.
Prefer additive analysis views and established common records; a concrete
schema needs its own review, invalidation and compatibility contract.

| Contract | Classification and existing home | Required facts |
| --- | --- | --- |
| Computation-region core | Common Compiler Substrate; existing WHIRL/domain descriptors and CommonAccessPatternIR | Iteration/output domains; affine or indirect accesses; payload; effects; source locations; domain constraints. |
| Reduction/state core | Common Compiler Substrate; proposed reusable analysis view | Identity, update, optional merge, finalization inputs, partition coverage, state invariant, proof/precondition and numerical-permission references. |
| Work Partition | Common Compiler Substrate; CommonTilePlanIR, CommonParallelizationIR, CommonPlacementIR | Logical partition separate from device/CTA/warp/lane maps; partial and final ownership; boundaries, masks, merge owner, deterministic fallback. |
| Resource Lifetime | Common Compiler Substrate; CommonMemoryResidencyIR and CommonPipelineIR extensions | Logical ranges, access modes, completion joins, storage conflicts, aligned offsets, static/dynamic reservations, generations and target legality. |
| Execution Events | Common Compiler Substrate; CommonFetchPlanIR and CommonPipelineIR extensions | Producer/consumer identity, issue/completion kinds, visibility/scope, loop-carried edges, execution resource, target realization and proof status. |
| Plan/cost/guard | Common Compiler Substrate; existing OptimizationCandidateIR, OptimizationPlanIR, CandidateCostIR, RuntimeGuardIR | Complete tuple, invalidation, fallback, units, provenance, bounds, launch/workspace and resource costs. |
| Attention contracts | AI-domain-specific | Q/K/V meaning, masks, allowed score functions, KV state and normalization-state specialization. |
| MoE contracts | AI-domain-specific | Routing/top-k policy, token multiplicity, expert capacities, weights, permutations, serving load and combine meaning. |
| Target realization | Target extension, not generic policy | Address-space and layout restrictions, barriers/proxies, async instruction completion and launch-resource ABI. |

Common/com owns policy-free records, identities, accessors, structural checks
and printing. Owning optimization phases derive facts, prove legality,
generate/cost/select plans and transform within driver-established scope.
Domain semantics supply meaning and acceptance, not target handles. Numerical
reassociation, alternate statistics and precision changes are distinct
permissions; an algebraic proof over reals is not a bitwise FP proof.

## 5. P8 Enhancement Note: Access Lifetime And Physical Reuse

**Adopted requirement; generic allocator/event graph not implemented.** Keep
the fixed synchronous/two-slot F32 P8 proof as the existing implementation.
Extend P8 to plan movement and storage reuse together with P6/P7, rather than
adding prefetch after storage allocation has become irreversible.

For each logical region and generation require:

```text
previous conflicting accesses complete
  -> release/acquire ownership of range
  -> next producer issue -> required data-visible event
  -> consumer accesses -> completion join -> next conflicting overwrite
```

The source of an async copy stays valid until its reads finish; its
destination is not readable before completion and visibility are established.
Consumer issue is not consumer access completion. The join covers all
consumers and any other conflicting reader/writer, including alias views.
Use a conservative operation-complete event if earlier access completion is
not explicitly supported. Barrier state itself has a lifetime: it cannot be
overlaid, reinitialized or invalidated while earlier generations can reach it.
The planner records finite, uniform arrival/wait counts and checks progress;
adding dependencies must not introduce a cyclic wait or a divergent barrier.

Physical overlap is allowed only if the ranges fit and their access lifetimes
are ordered, or a proved mutually exclusive control condition applies. A
lack of dependency is not a disjoint-lifetime proof. Predicted times cannot
establish correctness. Introducing an ordering edge is a separate candidate
with synchronization and lost-overlap cost; retain the nonoverlaid fallback.

Retain two distinct reuse opportunities: repeated generations of a slot and
cross-purpose reuse (e.g. staging followed by epilogue scratch). The latter
can reduce requested allocation, but only when offset/alignment, target
swizzle/bank layout and every access are valid in the reassigned range.
Packing is bounded and deterministic; no global-optimum claim is made.

```text
for each semantic/partition/layout candidate:
    create logical buffers and symbolic per-generation event constraints
    derive conflicts using alias ranges and access-completion ordering
    propose disjoint storage and eligible overlay assignments
    rederive schedule, conflicts and capacity after each assignment
    reject unsafe, cyclic, unsupported or over-capacity assignments
    compute requested bytes and target-rounded reservation
    evaluate complete latency, including any added ordering and waits
retain alternatives until the P9 cost gate; lower only the winner
```

Safe reuse is mandatory; maximal reuse is not the objective. One slot may
save storage but lose prefetch overlap. A dead logical value does not release
part of a static CTA reservation to another CTA. Occupancy consumes physical
allocation/reservation, not the instantaneous sum of live tensor sizes.
GMEM cache service is separately unknown unless modeled; SMEM does not evict
old contents automatically to satisfy a TMA write.

## 6. Phase Responsibilities And Required Summaries

Fixed phase order remains P0-P11; refinement revisits provisional choices
within the complete-plan search, not arbitrary pass reordering.

| Phase | New responsibility | What the tool must summarize |
| --- | --- | --- |
| P0 | Derive domains, recurrences, payload, effects and numerical semantics from native WHIRL, or verify an explicit native contract; keep these evidence paths distinct. | Discovery/contract origin, independent/reduction domains, access/broadcast and recurrence evidence, source, unresolved facts and refusal reasons. |
| P1 | Separate value/access/reservation lifetimes and aliases. | Access ranges, future uses, completion obligations, unknown/conflicting views. |
| P2 | Propose verified algebraic/decomposition alternatives as well as fusion. | Original vs alternate state/work; proof, numerical permission, fallback and extra work. |
| P3-P7 | Refine representations, partition/ownership and logical resources. | Mapping choices, partial/final ownership, bytes/alignment and rejected combinations. |
| P8 | Construct event, storage and movement alternatives jointly. | Range/generation, issue/ready/access-complete/reuse events, join participants, offset/extent, ordering proof, physical bytes saved, waits and lost overlap. |
| P9 | Select complete plans; materialize and verify WHIRL. | Candidate census, all cost components and uncertainty, selection/fallback, plan-to-WHIRL allocation and synchronization actions. |
| P10-P11 | Guard dynamic domains/variants and attribute measured feedback. | Guard decisions, workload state, actual selected variant, calibration and remaining unknowns. |

Extend existing phase reports, especially `fetch-pipeline.log`,
`residency.log`, and
`physical-plan.log`; do not rename existing phase files in this amendment.
Every phase log starts with `# What it summarizes:`. Preserve raw option-controlled
WOPT `.t`, positional rows, source line last on candidate records, phase
coverage including disabled/not-applicable states, and trace-schema versioning.
The enhancement does not claim these additional rows already exist.

## 7. Action Gates And Non-Goals

The [four-case research milestone](research_library/building_block_planning/README.md)
demonstrates both layers with at least two formulations per case, storage and
event obligations, and reproducible analytical comparisons. A checked
host-reference experiment is research evidence, not a compiler rewrite or GPU
performance certification. Follow with real ingestion/census and per-case
WHIRL milestones; do not allocate opcodes merely from expected model shapes.

Attention next tests composition of normalization/weighted-value states and
legal online vs partitioned realizations. MoE then tests data-dependent/ragged
domains, routing ownership, indirect aliases and runtime scheduling. Neither
is declared implemented or generically solved by completing the four cases.

Production adoption requires reviewed numerical/legality contracts, explicit
option and optimization-level scope, bounded candidate generation, target
completion lowering, negative/progress tests, independently reopened
before/after WHIRL and CUDA, trace audits and eventual native certification.
Existing GEMM controls are unchanged. No new production option is registered.

## 8. Provenance

- [Colleague's three-abstraction proposal](https://chatgpt.com/s/t_6ad72f513a208191b65ba17bd33c5fe6): framework inspiration, not an implemented IR or proof.
- [TensorIR](https://arxiv.org/html/2207.04296): computational block signatures.
- [DaCe](https://htor.inf.ethz.ch/publications/img/dace-sc19.pdf): stateful, subset-aware data movement.
- [Welder](https://www.usenix.org/conference/osdi23/presentation/shi): tile propagation and reuse choices.
- [Neptune](https://arxiv.org/html/2510.08726): algebraic repair of dependent reductions; not a universal FP proof.
- [Nautilus, 2026 preprint](https://arxiv.org/html/2604.14825): high-level scheduling with successive lowering.
- [CUDA pipelines](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/pipelines.html): acquire/wait/release coordination.

The contracts, bounded planner, research cost model and Open64 ownership map
above are Open64 design decisions/proposals, not attributed implementations
of those systems.


## H5 selected MLA panel ownership

[H5 MLA](AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md) extends only the admitted closed F32 MLA route. Current H2 latent/rotary proofs and H20/H3 bindings admit a finite G1/G2/G4 comparison. Executable grouped recipes use one warp per head, bounded shared panels, explicit readiness/release and the old tuple/merge ABI. The common fact/cost services retain no native authority; LNO owns selection, fresh validation and the atomic High transaction. Selected worker High owns memory/collectives, Mid lowers structured memory, Low maps ownership and the PTX backend emits the result. No MLIR interface or tensor-type representation change is introduced. Hardware qualification, quantized adaptation and AOT serving selection remain distinct gates.
