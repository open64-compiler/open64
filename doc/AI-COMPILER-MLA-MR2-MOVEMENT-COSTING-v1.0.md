# MLA MR2 data-movement and reuse-aware costing v1.0

November 8, 2026. Initial analytical model; MR3 grouped materialization and
MR4 device acceptance remain open. Owner, successor, reviewer and lead: TBD.

This implements the bounded MR2 census in the
[movement/reuse design](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md),
consuming the [MR1 owned proofs](AI-COMPILER-MLA-MR1-OWNED-REUSE-v1.0.md).
The [search plan v1.11](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.11.md)
records the remaining gates. This is AOT estimation, not a compile/benchmark
iteration or a claim of globally optimal GPU code.

## Scope, ownership and compatibility

LNO's active-PU canonical High observer validates source, numerical permission,
producer, ABI and native analyses, then derives MR1 facts once. It passes only
owned values to detached VHO costing: complete proof identity and separate
latent/rotary payloads. The new model requires a proved invocation-local MR1
cache family. Names, shapes, tensor traits alone, or head count do not grant
reuse. Missing traits may use MR1's closed-graph proof with identical checks.

No candidate construction calls WN/ST/TY/DST allocation, lowering, emission,
compilers, GPU probes or correctness references. The only additional shared
services are existing checked readiness/max-plus and occupancy arithmetic.
Native tree/table/analysis equality and live recapture guard the entire observer.
Pure object dependency auditing covers the complete detached closure.

Old binding v1/v2, additive model v1, control v2, recipe v8, 12-entry catalog,
scores, proposal order, COMMENT workaround and G1 materializers remain intact.
They do not silently acquire new fields or cost meanings. The new path is an
explicit binding v3 opt-in. Below O3, or with search disabled, it does not run.

## Versioned contracts and capabilities

- Binding: `open64.lno-high-search.v3`, the v2 fields plus required `hierarchy`.
- Hierarchy: `open64.mla-hierarchy-profile.v1`, cycles in `reference_cycles`,
  separate device-wide HBM/L2 byte rates, sector envelope, request amplification,
  SMEM conflict factor, objective and bounded fixed cache scenarios.
- Controls: `open64.mla-search-controls.v3`, space
  `mla-movement-space-v1-36`. No new provider strings are accepted.
- Descriptors: `mla-movement-descriptor-v1`, recording G, W, old G1 policy,
  column tile, sweep/retention choice, placement, ownership, synchronous
  pipeline and unchanged normalized F32 tuple ABI.
- Capability registry:
  `mla-materializers-v1:G1=verified-v8;G2,G4,G8=analysis-only`.
  This means existing source/compiler correctness qualification, not measured
  GPU acceptance. JSON cannot enable an unimplemented rule.
- Selected source: `lno-selected-mla-v2`, with explicit head group 1 and
  `mla-movement-selected-v1` proof/model/descriptor binding. The fixed executable
  algorithm still uses recipe v8; it has no new physical grouped semantics.
  G>1 will require MR3's reviewed recipe/launch/ownership extension.

Strict parsing rejects unknown fields, duplicates, NUL strings, wrong versions,
missing/zero required rates, unsupported architectures, invalid ppm values,
unknown capability strings and unsupported certificates. Architecture spelling
does not supply cache hits or measured performance.

Use the existing `-LNO:high_search=on:high_search_bindings=<binding.v3.json>`
option. Set `high_apply=off` for estimation only or `high_apply=on` for the single
selected implementation. The review harness writes complete short/long v3
bindings and exact command vectors; its `hierarchy()` function is the synthetic
profile example. Existing command paths keep accepting their old schemas.

The full model identity binds every hierarchy field and scenario, scalar and
special timing inputs, resource envelope, catalog and capabilities. The owned
reuse identity binds original source/analysis/owned-memory/producer identities,
owner, activation, mutation epoch and the MR1 summary. Per-search cache entries
use immutable catalog ordinals inside this single bound context; no estimate
cache is reused across calls or profiles. Same-ID field changes change the key.

## Finite census and production eligibility

The first 12 entries are the unchanged implemented G1 family/width variants,
including direct width zero, widths 32 through 4096, and the existing narrow
block/statistics alternatives. They are all re-costed under this model.

The other 24 entries are G={2,4,8} x W={32,64,128,256} x two choices:

1. Column tile min(C,16), two latent-cache sweeps, synchronous SMEM staging.
2. Full latent tile C retained in SMEM, one global latent-cache sweep.

Both propose one warp per reserved head slot, row-major staging, full rotary
width, synchronous visibility/release and per-head normalized tuples. They
reserve 32*G threads, including inactive head slots. They have no materializer
and cannot become incumbents even when their estimate is lower than G1.
Their status is `analysis_only_unimplemented_grouped_materializer` when modeled;
infeasible entries instead retain their explicit resource/capacity refusal.

Admission reuses the existing detached G1 schedule and capacity/storage checks.
It collects its instruction counts without computing or using the old score.
The selected High private tuple-buffer capacity is checked before a new-model
entry can win. There is no free candidate lowering. Unsupported widths, source forms and
resource envelopes are visibly rejected, rather than modeled as executable.

## Request and storage equations

All cache data is FP32. B batches, H heads, L valid tokens, C latent columns,
R rotary columns, G reserved heads/CTA and N_G=ceil(H/G):

    unique cache payload = 4*B*L*(C+R)
    two-sweep cache requests = 4*B*L*N_G*(2*C+R)
    retained-tile cache requests = 4*B*L*N_G*(C+R)

G1 retains two logical latent contractions; no cross-head reuse is credited
through the global request count. All scenarios can still give G1 L2 hits.
The retained alternative receives one-sweep credit only with its full tile
reserved for the proposed lifetime. A small column tile receives no such credit.

Token/head tails are counted as separate classes. Only real tokens and heads
produce cache/query/mask/tuple requests. All static threads, registers, shared
arrays and collective participants remain reserved. H<G does not multiply
useful traffic by padded G; it can still increase resource/synchronization cost.
Masks use a dense all-active work envelope. No runtime mask estimate authorizes
load-domain changes, eliminating operations or consuming masked NaNs.

For N_S=ceil(L/W), with width zero interpreted as one full-context chunk:

    partial allocation = 4*B*H*N_S*(C+2), if N_S > 1, otherwise 0
    partial writes = partial allocation
    merge reads = 4*B*H*N_S*(C+4), if split
    score/probability global allocation = 0

Merge reads include repeated statistic requests, not merely one read per tuple.
Grouping preserves per-real-head tuples and merge work. Smaller W can restore
CTA coverage but increases allocation, merge traffic and launches/waves.
Query reads, mask bytes, attention output, projection/copy operands and output
writes are charged separately. Existing non-attention scalar/shared instruction
envelopes remain conservative estimates; contraction/copy request envelopes
include CTA tail padding and do not establish physical address-sector coverage.

For B=1,H=3,L=8193,C=16,R=4, two-sweep cache requests are 3,539,376 bytes at G1,
2,359,584 at G2 and 1,179,792 at G4/G8. G2's retained projection is 1,310,880 bytes.
These are logical requests, not HBM measurements or speedup ratios.

## Hierarchy assumptions, provenance and objective

Each profile supplies one to four named cache scenarios. Each has a positive
weight and cache/other L2-hit assumptions in ppm. All candidates use the same
scenarios. The objective is either the worst scenario's complete latency or the
ceiling of the fixed weighted mean. Scenario cycles remain visible separately.
Synthetic, estimated and measured-input provenance stays explicit; the current
test profile is synthetic and uncalibrated. A measured-input profile labels
inputs only; its candidate results remain estimates.

A per-CTA, per-operand request envelope is multiplied by the provided
amplification, rounded to a supplied sector size, then multiplied by class CTAs.
It is an assumption envelope, not a coalescing/replay proof. The model ignores
L1, sends every transaction to L2 and charges read misses to HBM. Writes are
charged write-through without a hit discount. Global request, transaction,
L2, HBM and SMEM quantities are distinct in the ledger. This conservative
policy can overestimate actual traffic; a future L1/write policy needs a new
reviewed contract, not an unexplained score adjustment.

No hit rate, residency, bandwidth, contention or register allocation is inferred
from `sm_90`. Reuse distance and live hardware working set remain unknown.
The model has no bank map or instruction scheduler; its sector/conflict factors
are explicit uncertainty assumptions. No cache contents or runtime addresses
are retained. Input scenarios can bracket cold/warm behavior, but this does not
bound all possible hardware workloads or turn modeled HBM bytes into counters.

## Resources, dependencies and service accounting

Legacy G1 uses its actual detached launch threads/shared inventory and explicit
register estimate. Grouped proposals reserve:

    shared = 4*(W*column_tile + W*R + G*(C+2))
    registers/thread = profile_base + ceil(C/32) + 2*ceil(W/32) + 4

The proposed accumulator/statistic/token values and per-head shared workspace
reservation envelope are explicit. The workspace is a conservative capacity
allowance; the reported SMEM transfers describe the cache-staging hypothesis,
not an implemented grouped instruction trace.
The retained form sets column_tile=C; two-sweep form uses min(C,16). These are
uncalibrated logical resource estimates, not ptxas allocation results.

Existing occupancy arithmetic rounds register warps/quantum and SMEM reservation/
quantum, and applies block/warp/thread/register/shared limits. Static arrays above
48 KiB are rejected without adding a launch ABI. Excess registers or zero
residency reject the candidate; the model does not invent a spill implementation.
Actual spills remain unknown even when estimated resource demand fits.

The ledger retains class grids, threads, rounded resources, residency, unused
slots, waves, scalar/dependency, special, memory, local, sync and launch costs.
A class is homogeneous in valid head/token extent. Class grids are serialized
as a conservative full/tail envelope, not an assertion about device issue order.

Within a wave, SM work shares that SM's throughput across resident CTAs. L2/HBM
traffic shares each device-wide bandwidth exactly once. Delivery, consume and
release use the existing readiness max-plus composition with synchronous edges.
Scalar/dependency work, exponent/divide/shuffle service, SMEM conflicts, barriers
and fences stay visible. No asynchronous overlap is credited; no service is both
added and discounted as hidden. Each actual kernel launch is charged once,
including when its grid has multiple tail classes. Complete scenarios sum all
projection, attention, merge and final output stages.

## Budgets, convergence and selected application

MR2 uses the same controlled-search implementation, proposal strategies,
fair interleaving, lazy evaluated-entry cache, deterministic ordinal ties and
global proposal/unique/time accounting as v2, with a 36-entry bound. Maximum
proposals are 576; at most 36 unique estimates and 16 runs are admitted.
The initial compatible G1 proposal is direct for L<=4096, width128 otherwise.
It consumes the first normal proposal/unique estimate. If that seed is
incompatible, it is refused and ordinary fallback remains available.
With a one-estimate budget and a compatible seed, G1 remains the incumbent.

Exhaustive, random and local/multistart strategy runs retain their separate
reports. Exhaustion, budgets, plateau and explicit estimated targets remain.
Relative-gap and economic-benefit certificates are refused for this new model:
the old five-launch estimator bound is not transplanted. Hard budgets remain
mandatory. Completion means this finite census was estimated; a winner is best
among its executable, evaluated entries under the bound profile. It does not
mean best among analysis-only ideas or global GPU optimum.

Only the selected implemented G1 winner is transformed. Preflight revalidates
live native facts, MR1 ownership, hierarchy/profile/catalog/capabilities,
controls, descriptor identity and G1 eligibility before the existing atomic
High transaction. The full versioned selected binding is covered by the native
accepted-tree key and original-input equality seal. M6 verifies these fields
without search, then uses the unchanged structured-memory/GPU/PTX route.
Changed profiles, proof loss, grouping injection or stale descriptors refuse
before mutation. A late failure rolls back to the ordinary path.

An unproved MR1 cache family disables this new model and uses ordinary LNO.
It does not masquerade as a zero-byte cache. The independently usable old v1/v2
G1 search/application remains unchanged. Unsupported grouped materializers
never enter phase 2. Generated selected WHIRL/CUDA comments explicitly explain
that MR2 chose an implemented G1 plan and that modeled traffic is not measured.

## Validation, retained evidence and remaining work

Analytical tests cover exact tails, real versus padded heads, cache sweep
retention, level-specific scenarios, tuple/repeated-merge counts, weighted/worst
objectives, rounded resource rejection, common budgets and capability gating.
Native fixed-fixture review checks read-only exploration, legacy trace/PTX
parity, independent reference correctness of the selected short/long plans,
strict profile refusals, live stale-input/descriptor/proof checks, rollback,
M6 profile/seal refusal, O-level boundaries and generated explanation comments.
Raw before/after mapped WHIRL, independently reopened symbol/source traces,
diffs, selected High/Mid/Low/PTX, generated sources, inputs and commands are
retained in a fresh immutable receipt.

No Hopper machine is available. MR0/MR4 measurements, representative held-out
workload calibration, physical coalescing/bank/spill prediction, MR3 grouped
correctness and materialization, optional async/Tensor Core extensions, and
independent personnel transfer acceptance remain open. Further model/family
extensions must version their contracts and repeat the affected admission,
isolation, resource, selected-only correctness and fallback checks.


The focused acceptance passes 144 analytical assertions, 37 existing convergence
checks and 44 generated-host reference executions (30 compatibility/MR1 plus
14 selected MR2 executions). The synthetic profile selects direct G1 for L=127
and G1 width256 for L=8193. Native profile/proof/descriptor refusals, rollback,
M6 metadata/profile rejection and lower-O-level guards pass. Legacy full source
and selected High/Mid/Low traces and PTX remain byte-identical. be.so remains
byte-identical; production LNO restoration is verified after test-only builds.
These are compiler/host checks, not GPU correctness or measured performance.
