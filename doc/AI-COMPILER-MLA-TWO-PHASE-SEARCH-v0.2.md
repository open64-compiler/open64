# MLA two-phase search design v0.2

November 8, 2026 | Data movement and cross-head reuse amendment

Adopt [MLA reuse design v1.0](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md),
[methodology v1.5](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.5.md)
and [MLA plan v1.1](AI-COMPILER-MLA-TWO-PHASE-IMPLEMENTATION-PLAN-v1.1.md).
The complete v0.1 text follows verbatim. Historical prototype/integration status
below is superseded by the M1-M6 bounded production route; its original evidence
and algorithms remain unchanged. New grouping/hierarchy work is design only.

Data movement and reuse, particularly shared latent-cache tiles across query
heads within one CTA, now receive priority in family development and declared
proposal coverage. G, token/chunk W, column tiles, ownership and layout are joint
decisions. Keep the current 12 G=1 scalar-F32 descriptors and complete-call
latency objective; do not use minimum bytes as a proxy for minimum time.

Prove identical cache storage/version/access maps, keep head-specific queries,
masks and statistics separate, count score and value sweeps, and model cache
hits, SMEM/barrier/register/spill costs and lost CTA coverage. The existing tuple
contains normalized local latent values; preserve its stable per-head rescaling
merge and source numerical guards. No new precision or async policy is implied.

Phase 1 continues to use only immutable owned facts and detached estimates.
No native candidate construction, emitted source, tool compilation, GPU probe
or candidate correctness test occurs in exploration. Hypothetical grouped plans
without a verified implementation are analysis-only and cannot win. Offline
fixed-probe calibration and new-rule development tests are separate activities.
Phase 2 materializes/verifies only the best eligible fixed plan under fresh
source/reuse/model/resource/control bindings, retaining G=1/host baseline on
failure and any existing explicit bounded retry discipline.

Extend pure DTO/report/model/recipe/capability schemas explicitly before enabling
new fields. Preserve old v2/v8 contracts and reproduce legacy estimates. High
realizes fixed grouping and memory lifetime before Mid/Low lowering; emitters
cannot select a schedule. Invalidate changed facts and certificates, preserve
global budgets and all supported strategy/stopping mechanisms, and make only
scoped estimated/measured claims. No global GPU optimum follows from reuse.

MR0-MR6 in the production search plan specify measurement, facts, modeling,
selected synchronous rule, GPU acceptance, optional pipelines and held-out
strategy/transfer gates. All are open. Existing M7 native GPU evidence and
M8/M9 acceptance remain open. Preserve automatic CUDA/WHIRL/PTX explanations,
independent reference/canary/input checks, before/after B/T/diffs and immutable
receipts. Personnel roles remain TBD.

---

# MLA two-phase search design v0.1

November 6, 2026. This amendment replaces the freeze-as-convergence wording in
AI-COMPILER-MLA-WARP-OWNED-WALKTHROUGH-v0.1.md. A completed implementation is a
candidate, not evidence of search convergence. AOT remains baseline. This
milestone implements an opt-in standalone PU-local VHO exploration/selection
boundary; it does not claim integrated be_driver execution, new kernel
algorithms, calibrated GPU latency or resumed runtime profiling infrastructure.

## 1. Two distinct phases

Phase 1 consumes an admitted detached absorbed-MLA recipe, explicit device
capacities, a complete timing profile and search controls. It generates compact
candidate schedules, preflights legality/resources and estimates whole-call
costs. It allocates only ordinary host records. It must not initialize native
compiler contexts, allocate WN/ST/TY/DST tables, emit B/CUDA, invoke nvcc, launch
kernels or run candidate regression tests. The source recipe remains unchanged.

Phase 2 admits the winning report, writes one fixed selected recipe, and lets
the existing native materializer/whirl2c/reference path construct and validate
that configuration only. Its multiple stages are one selected implementation,
not multiple candidate trials. The lowerer and emitter cannot pick a different
width/family. Complete deterministic ledger replay is permitted once at each
standalone command's input admission; per-kernel materialization/annotation/
auditing operates on the fixed plan and does not rerun candidate exploration.

New transformation-rule development still requires its own correctness and
regression evidence. Parameter search through already verified rules does not
repeat that rule-development suite for every candidate. The cost changes from
N*(estimate+materialize+validate) to N*estimate+K*(materialize+validate), with
K normally one and bounded reselection only after an identified rejection or
backend-resource mismatch. Correctness alone cannot confirm performance ranking.

## 2. Bounded implemented search space

The initial version contains12 stable candidate identities:

| Ordinal | Width | Policy/realization |
| --- | --- | --- |
|0|direct|coverage-v6 direct128-thread attention|
|1|32|coverage-v6 register-only warp|
|2|64|coverage-v6 one-barrier warp-owned tuples|
|3..8|128,256,512,1024,2048,4096|coverage-v6 register-latent128-thread CTAs|
|9|32|coverage-v3 shared statistics/three warp fences|
|10|64|coverage-v3 shared statistics/five CTA barriers|
|11|64|coverage-v5 disjoint statistics/three CTA barriers|

Equivalent historical policy realizations are not duplicated. Invalid direct
capacity, width>=context, partial capacity, scratch budget and all-stage resource
rejections remain in the ledger. This covers implemented full-F32 realizations,
not all possible MLA algorithms. No tensor-core, precision, new staging or
cross-PU transformation is introduced by search. O3 and explicit reassociation
permission are required. Existing coverage-v1..v6 commands retain their policy,
recipe shape, fingerprints and output behavior.

## 3. Architecture, resources and explicit estimation inputs

Reuse DSL_HARDWARE_RESOURCE_RECORD, DSL_hardware_resources_valid,
DSL_gemm_occupancy and DSL_readiness_add/mul/ceil. VHO_tensor_schedule constructs
detached executable plans without native mutation; reuse its exact kernel
geometry, shared footprint and admission instead of independently approximating
lowerability. Every stage, including projections and final copies, must fit.
No new be.so or common/com policy dependency is introduced.

The existing generic architecture descriptions explicitly lack several scalar
instruction timings. Do not invent exp/div/shuffle/launch timings from sm_90.
Require open64.mla-cost-profile.v1 with id, architecture, provenance, unit
reference_cycles and12 positive bounded integer parameters:

- device-global bytes/cycle, per-SM scalar lane operations/cycle and shared bytes/cycle;
- dependent scalar cycles and full-warp exp/div/shuffle cycles;
- CTA barrier, warp fence and kernel-launch cycles;
- load latency and the latency-hiding warp envelope.

Provenance is synthetic, estimated or measured-input; it is not certification.
The explicit architecture must match the separate GPU resource profile, whose
SM count, carveout and register estimate remain caller-supplied. Missing, zero,
unknown, wrong-unit, duplicate and oversized inputs diagnose before publication.
The supplied synthetic test profile is not an NVIDIA specification.

## 4. Auditable additive-wave model

All counts below describe the selected implementation at full-chunk,
all-mask-true activity. Tail chunks are conservatively charged a full span;
mask sparsity, cache reuse, compiler instruction elimination and overlap are not
credited. These estimates are neither hardware measurements nor physical bounds.

For each stage record per-CTA scalar work F, global bytes G, shared bytes S,
shuffle/exp/div call counts, synchronization counts, dependency steps D, selected
shared allocation, threads and estimated resident CTAs R. Local attention charges
both score dots and latent products, query/cache reads, mask and tuple writes.
The inter-chunk merge charges tuple reads, rescaling, normalization and output.
Projection/copy stages are included, not discarded as a common constant. Only
eligible entries have complete comparable costs; rejected entries can retain
prefix diagnostics and never enter ranking.

For one occupancy round with A active CTAs, q=ceil(A/SMs) and active warps W:

    compute = max(q*ceil(F/scalar_rate), D*dependent_scalar_cycles)
    memory = ceil(A*G/global_rate) + ceil(load_latency/min(W,hiding_warps))
    special = q*(shuffles*shuffle_cycles + exps*exp_cycles + divs*div_cycles)
    shared = q*ceil(S/shared_rate)
    sync = q*(CTA_barriers*CTA_cycles + warp_fences*warp_cycles)
    round = compute + memory + special + shared + sync
    stage = launch_cycles + sum(full_and_tail_rounds)
    whole_call = sum(all_selected_stages)

Rounded occupancy comes from the existing service. Resident CTAs share each
SM's work/issue bandwidth; global traffic shares one device bandwidth. No
throughput is multiplied by the residency count. All sums/products use existing
checked arithmetic; overflow rejects a candidate, never wraps into a cheap win.

Width64 families differ explicitly: one-barrier tuples use8*(rank+2) shared
bytes, double latent column reductions and four broadcasts per warp plus two
leader rescaling exponentials/divisions each. The three-barrier version uses272
shared bytes and shared probabilities; the five-barrier version retains288.
Width32 register statistics use zero shared bytes/fences. These differences
are costed before transformation, not inferred by the source emitter.

## 5. Strategies, portfolio and convergence mechanisms

Strategy and stopping policy are independent controls. Implement exhaustive
stable traversal, seeded random traversal without replacement, structural local
search with global restarts, and a portfolio of all three. Neighbors share a
width or adjacent widths within a family. Each run has its own visited sequence,
incumbent, first-best trial and termination reason. All runs use the same model,
resource inputs, legal space and per-run proposal budget. A shared lazy cache
costs each candidate at most once. Cache hits are reported; this is not a
wall-clock benchmark declaring one search strategy universally fastest.

Select the lowest estimated whole-call cost across all runs; ties use candidate
ordinal. A representative strategy is the tied-best run reaching that cost in
the fewest proposals. Search reports retain every run rather than stopping the
portfolio when its first member plateaus. Available stopping controls are:

| Control | Evidence and permitted conclusion |
| --- | --- |
|exhausted|All12 candidates visited; best estimated in the declared space. A smaller explicit budget still terminates as budget exhausted.|
|budget|Stop at the declared per-run evaluation count; best among explored.|
|plateau|Patience proposals without a gain exceeding minimum_gain_cycles; empirical stagnation, not global optimality.|
|target|An explicit estimated cycle target is met; goal achieved under the model.|
|relative-gap|Estimator lower bound L=5*launch_cycles covers every modeled realization. Stop when best-L<=L*gap_ppm/1e6. This is an estimator-only certificate, not GPU optimality.|
|cost-aware|Stop when remaining_invocations*(best-L)<=evaluation_cost_cycles, comparing an upper bound on modeled deployment benefit to explicit caller-converted evaluation cost. Use division to avoid overflow. This is a conservative model-based rule, not the PBGI algorithm.|

If the union of cached candidates covers all12, the overall portfolio can report
complete declared-space estimation even if individual runs stopped early.
Otherwise report best_estimated_among_explored. Never replace either label with
global_GPU_optimum. A probabilistic stopping provider remains unavailable until
a calibrated posterior and assumptions exist; requesting it diagnoses rather
than fabricating confidence. A hardware lower-bound certificate is likewise not
provided by the synthetic estimator. New algorithms expand the space and require
new rules/proofs/model versions; the latest candidate does not erase older ones.

## 6. Sealed handoff and invalidation

explore-attention writes open64.mla-exploration.v1: admitted source recipe,
GPU/cost/options snapshots, per-candidate counts/costs/rejections, strategy runs,
winner, completeness/claim and actual native-initialization/materialization
counters, both zero. It writes no selected recipe or executable artifact.
select-attention verifies/replays this deterministic evidence and emits
open64.tensor-kernel.v7 containing the fixed family, width and complete census.

The standalone driver alone parses/serializes JSON. VHO receives typed plan
fields and an opaque identity; canonical archival JSON is driver-owned data,
not a new backend policy parser. Recipe integrity uses the existing deterministic
checksum, not an authenticity signature; immutable bundles retain SHA256 too.
Native metadata binds the search identity. Audits reject modified identities,
winners, costs, source graphs, resources, model inputs or options unless the
entire deterministic handoff is valid. Explicit manual/coverage overrides clear
the fixed selection before replanning. Refusion/remapping/optimization commands
reject a fixed recipe until it is explicitly invalidated, rather than writing
a stale selected artifact. Malformed inputs publish no selected/B
output; existing files are never overwritten.

Changing shape, numerical policy, target capacities, model version/timings,
search options or source graph invalidates the census and fixed recipe. Actual
backend register/spill/shared results are observations; a material mismatch
requires explicit bounded replanning or rejection, not silent emitter choices.
Automatic runtime feedback/reselection is outside this initial slice.

## 7. Phase ownership and validation

Production architecture remains Very High domain admission -> High common
algorithm/canonical DO_LOOP/ARRAY and LNO facts -> detached exploration/selection
-> selected High transformations/exit verification -> Mid memory lowering ->
Low GPU mapping -> PTX. The standalone MLA VHO prototype demonstrates the
search/materialization boundary; it does not move High work into later levels
or claim integration with all H0-H8/be_driver gates.

Verify estimation-only behavior from unchanged source hashes, zero native
counters, absence of B/CUDA/object outputs, source-level dependency/symbol audit
and deterministic repeated reports. Test ranking and independent arithmetic
oracles, strategy cache budgets/stops, rejected probabilistic claims, resource
failures, stale/tampered handoffs and low optimization/numerical permission.
Then lower only the selected best short and long configurations, reopen genuine
before/after B/T using -st -src, audit bindings, emit normal-whirl2c CUDA, compare
explanations and execute against FP64 oracles with forward/reverse host workers.
Compile selected CUDA, retain actual resources and compare to estimates.

Validation of this compiler change can exercise old transformation-rule tests;
that one-time development regression is distinct from the exploration loop.
GPU execution, race-sanitizer and measured performance remain hardware-gated.
Owner, successor, independent reviewer and approving lead remain TBD.
