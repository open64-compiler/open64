# MLA data movement and reuse design v1.0

2026-11-12 architecture terminology correction:
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
governs opcode properties, tensor traits, phase legality and scheduling
services. Embedded earlier revisions include these editorial corrections;
historical implementation status and acceptance evidence retain their original
scope.

November 8, 2026 | Design adopted; new grouped-head implementation remains open

## 1. Authority, baseline and objective

This amendment prioritizes data movement and reuse in AI schedule design,
especially cross-head reuse of the compressed latent cache in absorbed MLA.
It supplements [design v0.8](AI_compiler_optimization_design_v0.8.md),
[search methodology v1.5](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.5.md)
and [search plan v1.9](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md).
The existing [hardware/resource/memory planning design](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md)
continues to own resource envelopes, storage lifetimes, movement readiness and
overlap contracts. Reuse those services rather than create a parallel planner.

The reference implementation is the M6 admitted absorbed-MLA production route:
implementation commit `860e1db6`, final review fix `709c4e93`. Its current
12-descriptor scalar-F32 space, v8 fixed-policy recipe, v2 production bindings,
COMMENT workaround and verified High/Mid/Low/PTX boundaries remain unchanged.
M6 verified compiler/host/assembly behavior, not GPU performance. This document
does not implement cross-head grouping, cache-aware costing, asynchronous
pipelines, new precision or Tensor Core lowering. No new CLI is introduced.

The optimization objective remains minimum estimated **whole-call latency**
under a declared workload distribution, followed by selected-only correctness
and measured acceptance. Bytes are an explanatory cost component, not a
replacement objective or a lexicographic rule that always prefers fewer bytes.
Prioritize engineering and proposal coverage in this order:

1. Remove avoidable materialization and preserve existing fused attention.
2. Exploit proved reuse, with cross-head latent-cache reuse the first new family.
3. Improve concrete access layouts/coalescing and tile ownership jointly.
4. Overlap remaining transfers and computation only through admitted pipelines.
5. Refine barriers/instructions where the critical-path evidence justifies it.

This ordering does not force a grouped plan to win. Reduced traffic can cost
more synchronization, registers, shared memory, spills or lost SM coverage.
The G=1 baseline must remain available and may be best for a workload.

## 2. Semantics and the cross-head reuse proof

For dense single-query decode let B be batch, H query heads, L cache length,
C latent width, R rotary width, and s bytes per cache element. This document's
C corresponds to the latent rank called R in some historical generated comments;
do not reinterpret existing metadata or tuple offsets because of notation.

With absorbed query a, rotary query qR, latent cache c and rotary cache r:

    score[b,h,t] = scale * (sum_k a[b,h,k]*c[b,t,k]
                         + sum_u qR[b,h,u]*r[b,t,u])
    p[b,h,:] = source_masked_softmax(score[b,h,:])
    latent_out[b,h,k] = sum_t p[b,h,t]*c[b,t,k]

Value projection, output stores and LSE remain part of the whole-call boundary.
Sharing c and, when proved, r across heads does not share queries, masks,
softmax statistics, probabilities, partial vectors or head-specific weights.

A reusable-cache fact must prove, from admitted storage/view identities and
actual affine accesses, that the chosen heads read the same storage version,
base, offsets, strides, element encoding and token/column ranges. Shape equality
or matching variable names is insufficient. Respect alias/effect/dependence
facts, source lifetime and caller-established cache readiness. Reuse cannot
cross a cache update or assume two requests/batches contain the same data.
Reject unsupported head-dependent cache addressing and retain G=1 fallback.
Storage identity here is symbolic source/view/effect identity and the kernel's
read lifetime, not a hash of runtime cache values or physical pointer addresses.
A legal cache append ordered before a call within the admitted shape/layout
contract does not require recompilation. Changed shapes/views/strides/effects
need the existing guard/specialization and proof revalidation, preserving AOT.

The initial scope is G heads cooperating **within one CTA**, one batch and one
token chunk, inside the active PU. GPU L2 reuse across CTAs is a modeled cache
effect, not guaranteed shared placement. Cross-CTA/cluster/DSM reuse requires a
separately admitted provider, synchronization/residency proof and backend; it
is not an initial eligible family. No cross-PU or caller refinement is implied.

## 3. Traffic accounting and useful analytical limits

Count separately: allocation footprint; useful operand bytes; source-level
global requests; sector/transaction bytes at each cache path; physical HBM
reads/writes; SMEM reads/writes; register/local-spill traffic; metadata/scales;
and partial/merge transfers. A global load may hit cache. A smaller allocation
is not an HBM measurement, and cooperative loads do not imply a fixed speedup.

Ignoring projections, softmax and boundary work, the two contractions require:

    F_core ~= 2*B*H*L*(2*C + R)

Under cold-entry, dense active-token coverage and the ideal assumption that
each required cache element is read from HBM once for both contractions and
all heads, with no cache-output stores:

    D_cache,ideal = s*B*L*(C + R)
    I_ideal = F_core / D_cache,ideal
            = 2*H*(2*C+R) / (s*(C+R)) ~= 4*H/s  when C >> R

These are cache-only asymptotic limits, not a complete kernel cost, achievable
bandwidth claim or latency certificate. L cancels in this simplified intensity;
long context alone does not prove memory-boundedness. Reuse can move a kernel
toward a compute limit. Use the actual scalar-F32 implementation roof for the
M6 baseline, not the peak of a Tensor Core/low-precision implementation.

Let N_G = ceil(H/G). A conservative grouped implementation that stages latent
tiles in separate score and weighted-accumulation sweeps has approximately:

    D_cache,requested(G) ~= s*B*L*N_G*(2*C + R)

This counts only cache operands, assuming once-per-group reads in each sweep.
It assumes both c and r are proved head-invariant as in the equations above;
if only c is shared, charge head-dependent r reads separately rather than divide
them by G. Predicated useful traffic depends on each group's valid-token union.
Queries, masks, projections, output, padding, coalescing/replays and scratch are
additional. A fully retained tile could reduce this toward
`s*B*L*N_G*(C+R)`, but only if it really remains available through softmax.
Column tiling alone does not prove that property: if the full tile is not
resident, account for the second sweep. Further rereads must be charged too.
Per-pass reuse is bounded by actual valid heads, not padded G. Physical HBM
savings can be much smaller if the G=1 baseline already receives L2 hits.

For chunk width W and N_S=ceil(L/W), compact F32 partial allocation remains
`4*B*H*N_S*(C+2)` bytes when every real head has a normalized partial tuple.
Once writing and once reading each tuple costs at least twice that allocation;
count actual repeated merge/statistic requests separately. Grouping heads alone
does not remove per-head partials. Smaller W raises partial/merge work.
Here W is a positive logical chunk extent. The existing direct-width-zero
recipe sentinel is unchanged and represents one full-context chunk for counting;
it must never be used as a divisor in this formula.

For the retained B=1, H=3, L=8193 example at W=256, attention grids are 99,
66 and 33 CTAs for proposed G=1,2,4 respectively. G=4 has one invalid head slot.
Its potential cache reuse also removes independent CTAs. A smaller W can restore
coverage at the price of extra tuples and merges. These are geometry examples,
not supported new implementations, target-specific speedups or measured HBM
counts. Do not choose G and W independently.

## 4. Bounded candidate and implementation contract

The first proposed grouped family compares G in {1,2,4,8}, with a finite,
declared subset of token widths, latent-column tile widths and ownership/layout
variants. These values are a proposed census, not a currently supported catalog.
Keep the exact existing G=1 direct/register-latent variants as baseline entries.
Prune inadmissible combinations with detached proofs and checked resource
arithmetic; do not enumerate an unbounded Cartesian product.

| Decision | Required explicit description |
| --- | --- |
| Head group | G, active/tail heads, group-to-head map and cache invariance proof |
| Token partition | W, full/tail chunks, per-head mask behavior and split count |
| Column tiling | Cache/accumulator tile widths; score/value sweep count and retention lifetime |
| Ownership | Producer lanes/warps, each head's consumers/reduction domain and neutral inactive participants |
| Placement/layout | Direct cached reads versus explicit SMEM staging, address/sector map, padding and bank-conflict assumptions |
| Resources | Threads, rounded register estimate, all live SMEM objects/barriers, scratch, occupancy and CTA coverage |
| Pipeline | Initially synchronous copy/compute/release; later admitted stage count, readiness/completion and slot-reuse edges |
| Numerics | Original encodings, accumulation/order/rounding permissions and current normalized partial tuple ABI |

The initial grouped realization uses synchronous intra-CTA staging. Latent and
rotary tiles have an identified producer and are visible before every head
consumer reads them. All consumers must finish before overwrite. No global
score/probability arrays are reintroduced merely to implement grouping; reject
an infeasible grouped candidate or explicitly cost any separately admitted
materialization variant. Preserve the M6 static-SMEM limit (48 KiB without a new
launch ABI), existing resource reservation/rounding and source precision gates.
Do not assume the full W*C tile fits shared memory.

Head-tail and masked lanes remain neutral participants in uniform collectives;
they cannot return before a CTA barrier needed by other heads. Partition warp
shuffles/reduction slots by head and prove disjoint score/statistic/accumulator
ownership. An all-masked head must not contaminate another valid head. Producer
election and barrier scopes belong in High WHIRL, not printer heuristics.
Prove that cooperative producer reads stay within the legal source domain,
using the union of valid head/token read predicates when required. Loading a
tile for one head does not permit another head to consume masked NaN/poison:
zero probability times NaN is not a neutral product. Guard each head's use and
never issue padding/out-of-bounds loads just because their result is discarded.

Async copies/TMA, double buffering, cross-CTA sharing and Tensor Core mappings
are later optional families. Each needs complete target, alignment, numerical,
resource, completion and native emission contracts. Unsupported families remain
analysis-only and ineligible for production selection. Existing F32 input and
accumulation contracts are not narrowed to gain bandwidth.

## 5. WHIRL level ownership and continuity

| Boundary | Required behavior |
| --- | --- |
| Very High | Retain domain semantics, cache/view identity, encodings and numerical/effect contracts |
| Common High / LNO | Analyze actual DO_LOOP/OPR_ARRAY accesses, head-invariant reuse, alias/dependence/lifetime and loop schedules. Existing Wolf/unimodular and optional Cohen/polyhedral/hybrid providers may propose legal loop orders; this does not implement a new provider |
| Selected High exit | Realize the fixed G/W/column/layout/ownership plan as explicit group loops, staging/copy/compute/release DO_LOOPs, storage and typed synchronization under driver-owned worker contexts |
| Mid | Lower structured memory/control using existing services; preserve the selected reuse/ownership semantics and invalidate prior analysis keys |
| Low | Bind fixed abstract owners to GPU lanes/warps/CTAs and entry/storage ABI; no late change of G, W, placement or pipeline |
| PTX / whirl2c | Verify and emit that same fixed native product. Neither consumer infers a missing reuse plan nor reopens search |

Reuse the existing LNO access/dependence/snapshot/epoch services, detached VHO
schedule and resource/occupancy helpers, memory-readiness/lifetime planning,
M4 selected transaction, M5 native bridge and M6 capture/publication boundary.
Common/com may carry policy-free facts/contracts; it must not acquire schedule
selection policy. Use WHIRL opcode/version contracts, tensor descriptors and
tensor traits for semantic checks, with phase-owned analyses and verifiers for transformation
legality. Preserve the existing backend library dependencies.

## 6. Two-phase search, eligibility and convergence

Phase 1 consumes an owned immutable High snapshot and a bound traffic/reuse/
resource/model profile. Construct only detached descriptors, facts and cost
records. It may model grouping, layout, sector efficiency, cache scenarios,
spills, synchronization, coverage and overlap. It may not construct or mutate
WN/ST/TY/DST, create candidate kernels, lower WHIRL, emit CUDA/PTX, invoke nvcc/
ptxas, run GPU probes or execute correctness tests for candidates.

Baseline characterization and offline calibration are separately declared
measurement jobs on fixed probes/corpora, with their own costs and provenance.
They do not turn an active search iteration into a compile/benchmark loop.
Architecture options supply static capability/latency facts; sufficient static
information can support AOT estimation, but unknown cache hits, bandwidth or
contention cannot be inferred as constants solely from `arch=sm_*`.

A production candidate is eligible only after its reusable materialization rule,
legality checks and cost implementation are verified. Before that, report a
grouped opportunity as `analysis_only/unimplemented`, excluded from the winner.
Rule-development fixture tests are separate from parameter exploration. Enabling
a verified parameterized family does not require lowering every combination on
every search invocation. This amendment does not enable G>1 in existing tools.

Choose one best estimated eligible complete plan under the declared objective.
Evaluate the compatible G=1 baseline under that same new model/profile before
grouped proposals, charging it to the shared budget/cache exactly once. An old
model's baseline score is not a valid seed. Insufficient budget for additional
proposals retains the baseline; an incompatible baseline requires the existing
valid fallback rather than execution under an unsupported target/contract.
Phase 2 revalidates that winner and its original input/proofs, performs one
atomic selected High application and its fixed lowering, then runs correctness,
resource and required performance acceptance. Failure preserves the accepted
executable baseline. It does not start an unbounded next-candidate validation
loop. Any existing bounded retry policy must state cause, cap and invalidations.

Exhaustive, random, local/multistart and portfolio strategies remain available
under shared global proposal, unique-estimate and time budgets. Prioritized
coverage of reuse families is explicit versioned proposal policy, not free
extra evaluations. Deduplicate using full descriptor and binding identities;
retain deterministic ties, rejected/unvisited distinctions and equal-budget
strategy comparisons. Preserve budget/exhaustion/plateau/target/economic stops.
Reprove estimator-gap lower bounds after any model/catalog/launch change; the
legacy five-launch additive bound cannot be transplanted without proof. Disable
unsupported certificates, not hard budgets. No global GPU optimum is claimed.

## 7. Cost-model and profile extension

The current `mla-additive-waves-v1` model has scalar/global/shared rates and
special/latency/synchronization/launch components. Its `global_bytes` is not a
measured HBM counter. Preserve this model and its historical results unchanged.
Introduce a separately versioned hierarchy/reuse model, not new meanings for
old fields. Re-estimate all compared candidates under that same model/profile.

Record per operand, stage and workload: useful/requested/transaction bytes;
read/write direction; cache level and hit/reuse assumptions; SMEM transfers and
conflicts; live storage and register/spill estimates; SIMT/Tensor Core/special
work actually supported; grid/residency; barriers; and all launch/projection/
partial/merge/output work. State whether units are per CTA, SM or device. Do not
divide device-wide HBM bytes independently by full device bandwidth on every SM.

Service ratios such as `F_SIMT/P_SIMT`, `D_HBM/BW_HBM`, `D_L2/BW_L2` and
`D_SMEM/BW_SMEM` are useful ceilings/diagnostics. Their maximum is a resource
lower bound under its assumptions, not an achieved latency prediction. Build
the estimate from the actual copy/ready/compute/softmax/merge dependency graph,
resource service and overlap constraints, using existing readiness machinery.
Do not add and overlap the same service twice or assume synchronous stages hide
transfer latency. Occupancy loss and serial reductions must remain visible.

Bind cache/warmth/contention scenarios to the input profile with provenance and
uncertainty; candidates cannot pick an imaginary hit rate to improve their own
score. Use declared expected-workload weights or an explicit robust objective
when regimes differ. Synthetic, estimated, measured-input and calibrated values
remain distinct. Missing required facts disable that model/candidate; they do
not become zero-cost assumptions. New precision requires independent admission.
Runtime mask-density estimates do not authorize eliminating required operations
or relaxing read-domain safety. Static CTA register/SMEM/thread reservations
include padded/inactive participants even when their expected work is zero.

## 8. Versioned handoff, invalidation and refusal

Existing v2 bindings, v8 recipes and 12-entry reports remain reproducible as G=1
baseline contracts. New head-group, tile, sweep, layout, lifetime, model and
capability facts need an explicitly versioned DTO/report/selected-plan/binding
extension. Names and layouts are to be reviewed before implementation; existing
strict parsers must continue refusing unknown fields/versions. Do not hide G in
chunk-policy bits or silently extend accepted old schemas.

Full candidate/cache/report/selected-source/worker identities must bind the new
facts, original source/access/dependence/effect/storage identities, epochs,
numerical contract, workload/counter profile, target/resources, model/catalog,
capability and strategy controls. Preserve M6's original-input equality seal.
Mutation of source, alias/stride/storage version, masks, layout, G/W/tiling,
resource/pipeline or profiles invalidates the affected proofs and estimates.
Rebuild parent/DU/alias/access/dependence/reduction facts after selected mutation;
verify the new High exit and retire old level keys during Mid/Low lowering.

The original host output, accepted executable baseline and existing selected G=1
implementation remain usable on unsupported scope, insufficient resources,
stale/missing facts, parser refusal or selected lowering/verification failure.
Maintain active-PU/process ownership, no-replace publication and completion
manifest gating. Printers never repair a missing plan. General PREOPT COMMENT
preservation remains deferred; retain the exact workaround and explanations.

## 9. Correctness, evidence and generated explanations

Preserve the current normalized tuple `[m_hj,d_hj,z_hj[0..C-1]]`, where z_hj is
already divided by the chunk-local denominator. For nonempty finite chunks:

    m_h = max_j m_hj
    w_hj = d_hj * exp(m_hj - m_h)
    D_h = sum_j w_hj
    z_h = sum_j (w_hj / D_h) * z_hj
    LSE_h = m_h + log(D_h)

Empty chunks contribute zero before evaluating a potentially invalid exponential
product; take the maximum over nonempty chunks, not their safe empty sentinels.
Preserve the source's nonfinite/safe-max/denominator guards and the
all-empty result (zero vector, LSE -infinity). Do not treat normalized z_hj as
an unnormalized numerator or mix heads' maxima/denominators. A tuple-format
change would require separate versioned producer/consumer/numerical acceptance.

Rule and selected-plan checks cover G=1 equivalence; H=1, H<G, H not divisible
by G; batch and head isolation; token/column tails; distinct queries/masks per
head; one all-masked head beside a valid head; empty first/interior/final chunks;
all-masked inputs; equal/extreme scores and existing nonfinite/rounding cases;
masked poisoned/padded storage; stride/layout refusal; repeated CTA execution,
buffer reuse, canaries and immutable inputs. Check the declared reduction order
or authorized reassociation against the independent reference with unchanged
tolerances. Grouping does not authorize lower precision or a larger error budget.

Retain actual before/after B, full `ir_b2a -st -src` T, phase traces and raw diffs;
verify exact source/plan keys, metadata/launch joins and COMMENT parity through
High/Mid/Low, readable CUDA, whirl2c CUDA and native PTX. Add changed-G/layout/
cache-version/profile/ownership/pipeline rejection and transaction rollback tests.
Prove Phase 1 native isolation and selected-only materialization counters.
Host/assembler evidence does not replace selected GPU correctness/race/sanitizer
checks or paired performance acceptance. Reuse expensive evidence only with
complete producer and exact executable/token/launch identity receipts.

Generated comments must explain which cache tile is shared across which real
heads, where it resides, whether one or two sweeps occur, producer/consumer
ownership and barriers, head/token/column tails, per-head normalized partials
and stable merge, remaining query/cache/partial traffic, resources and expected
tradeoffs. Distinguish logical requests and estimated HBM from measured counters.
Readable algorithm views and native source must describe the actual fixed plan;
comments cannot substitute for missing reuse in executable WHIRL.

## 10. Implementation, acceptance and transfer

The [v1.9 implementation plan](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md)
defines MR0-MR6 with concrete dependencies, tests and go/no-go gates. Baseline M7
GPU characterization can proceed before grouped lowering exists. Grouped-head
deployment needs its own selected GPU correctness and measured improvement or
non-regression under a declared workload objective. Cache savings without an
end-to-end win do not replace the baseline. Work on G=1 calibration and scalar
compute bottlenecks remains valid if cross-head reuse is unprofitable.

Key risks: baseline L2 reuse already eliminates HBM duplication; grouping reduces
CTAs; score/output state spills; extra SMEM/barriers exceed the saved traffic;
two sweeps or head padding erase reuse; cache assumptions overfit a corpus;
head-tail/mask ownership corrupts results; and stale proofs select an unsupported
backend. Mitigate through explicit ledgers, resource/lifetime proofs, independent
numeric cases, held-out scenarios and conservative baseline retention. Async or
Tensor Core experiments cannot bypass their capability/numerical gates.

Primary owner, successor, independent reviewer and approving lead remain TBD.
The technical transfer package must contain the versioned schemas/facts/model,
materializer capability matrix, reproduction commands, selected native/source
artifacts and comments, reference/sanitizer/results, profiler/counter provenance,
rejection logs, risks and immutable receipt. This documentation update alone
does not close MR0-MR6, M7 GPU acceptance, M8 study or M9 personnel transfer.

## 11. Primary references

- [FlashAttention paper, revision v2](https://arxiv.org/abs/2205.14135v2): IO-aware tiling and exact attention motivate removing intermediate HBM traffic.
- [FlashAttention-3 paper, revision v2](https://arxiv.org/abs/2407.08608v2): asynchronous movement and compute/softmax overlap motivate explicit pipeline/resource modeling.
- [FlashMLA dated technical analysis](https://github.com/deepseek-ai/FlashMLA/blob/main/docs/20250422-new-kernel-deep-dive.md): head/query reuse can make MLA compute-bound; its implementation-specific assumptions are not Open64 performance results.
- [NVIDIA Nsight Compute profiling guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/): hierarchical memory, workload and roofline counters guide separately recorded calibration. Pin the actual tool/version/counter mapping in each future receipt.

The equations and grouped geometry above are this design's analytical model
under explicit assumptions. No cited throughput or speedup is transferred to
the current Open64 scalar-F32 implementation.
