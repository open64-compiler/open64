# AI compiler optimization design v0.9

2026-11-12 | H20 96 GB memory-hierarchy design phase

Status: proposed implementation architecture and qualification plan. The user
has confirmed H20 96 GB servers. Their live inventory and measurements are
pending. This version extends
[v0.8](AI_compiler_optimization_design_v0.8.md) by reference, preserving its
complete text, numerical contracts, common execution abstractions and
two-phase search. No existing PR branch or uncommitted original is changed.

## Main decision

Select a complete data-supply and compute plan for each workload using an
immutable device/partition profile. Jointly cost useful traffic, cache
scenarios, reuse scope, work coverage, RF/SMEM layout, transfer stages,
compute service, synchronization and full invocation overhead.

The common compiler machinery remains Work Partition, Resource Lifetime and
Execution Events. WHIRL opcodes and tensor traits remain the semantic
interfaces. Operation-specific numerical rules distinguish GEMM, MLA,
LayerNorm and Reduction.

| Decision | H20 96 GB direction |
| --- | --- |
| Hardware identity | Exact ISA plus separately discovered device/partition resources and measured execution profile; no H100 timing alias |
| Optimization objective | Complete-call latency and declared serving constraints; attained bandwidth is a bottleneck diagnostic |
| Reuse | Reduce materialized/repeated accesses while retaining enough independent work; evaluate L2 effects separately from guaranteed within-CTA reuse |
| Transport | Compare direct, cooperative and qualified asynchronous movement; jointly choose raw/decoded depth and compute mapping |
| Numeric legality | Preserve source order and precision; trees, Welford, online softmax, split merges and Tensor Core formats need explicit admission |
| Search | Estimate detached candidates only; apply and qualify one selected new realization per round, retaining fallback |
| Evidence | Device correctness, ordinary timing and counter profiling remain separate lanes; calibration feeds later AOT compilations |

## Four operator families

| Family | First performance hypothesis and limiting condition |
| --- | --- |
| GEMM | Persistent accumulator lifetime and an actual issue-next/compute-current pipeline can reduce carrier/reload and supply cost; output coverage, numerical permissions and resource pressure remain gates |
| MLA | Within-CTA cross-head latent-cache reuse can reduce requests; co-search group/chunk/query tiles and account for L2, reduced CTA count and tuple merging |
| Quantized MLA | Audit full-cache/scale copies under source/Q3 alias and version proofs, parallelize remaining independent work, and qualify raw/decode pipelines; preserve the current source-ordered numerical family and Q3 publication |
| LayerNorm | Coalesced transport, centered-value retention and pointwise fusion can reduce round trips; ordered statistics, special functions and spills can dominate |
| Reduction | Coalesced streaming and local accumulation can improve supply; a parallel tree or split realization requires its own numerical contract |

Higher bandwidth relative to compute lowers the arithmetic-intensity
crossover at which compute limits performance. It does not imply every family
will benefit from deeper staging. The first measured profiles decide whether
traffic, instruction issue, ordered recurrence, SMEM, occupancy or launch cost
limits a particular case.

## Phase and delivery authority

The full AI-P0 through AI-P11 updates, High/Mid/Low/PTX boundaries, cost
equations, page/scale contracts, current implementation gaps and primary
references are in
[hardware/resource/memory design v0.2](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.2.md).
The concrete H0-H8 milestones, owning source files, tests, search/materialization
budgets, device acceptance and transferable package are in the
[H20 implementation plan](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.0.md).

All new joint loop/memory search is O3 within the driver-selected PU or admitted
REGION. PREOPT canonicalizes; LNO provides dependence/locality and owned facts;
the owning phase selects and atomically changes High WHIRL. Mid lowers
structured memory operations, Low maps GPU participants/layouts, and PTX/
whirl2c emit selected semantics with COMMENT explanations. Cross-PU work
requires IPA.

AOT selection policy, PREOPT COMMENT repair and OpenCTI remain deferred.
The current request starts the next design phase and H20 qualification plan;
it does not claim new compiler support or measured speedup.
