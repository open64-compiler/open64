# MLA MR1 owned storage, access, version and lifetime facts v1.0

November 8, 2026 | Initial dense-affine reuse provider; grouped materialization remains MR3

This implementation completes the remaining MR1 work for the declared initial
proof domain. It follows the [cache-trait implementation](AI-COMPILER-MLA-CACHE-TRAITS-v1.0.md),
[movement/reuse design](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md), and
[search plan v1.10](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.10.md).
It adds necessary data-reuse proofs, not a complete grouped schedule legality
certificate. MR0 hardware measurement, MR2 hierarchy-aware costing and MR3
grouped-head lowering remain open. Personnel roles remain TBD.

## Ownership and phase boundary

LNO captures the facts at the existing canonical High boundary, after PREOPT
and successful access/DU/alias/full-dependence construction. Each active PU
owns its compiler activation and mutation epoch. Collection borrows native
objects synchronously, copies all retained information into strings, integers
and vectors, and retains no WN, ST, TY, analysis-manager or memory-pool pointer.

`DSL_HIGH_SNAPSHOT` now owns formal storage identities, native encodings,
pointer-use facts, flat ARRAY extents/offsets, execution groups, loop identities,
affine vectors, conditionality and direct copy value-flow references. Existing
dependence, DU and pairwise alias answers remain the source of analysis facts.
The projection records an explicit `disjoint` answer from LNO; the pure layer
does not guess native alias enum values or infer independence from types.

The function's original recipe, schema and disjoint-buffer ABI contract are
also copied. New projections have a separate `Owned_memory_key`. Legacy source
and fact keys keep their exact spelling so old v1/v2 bindings remain usable.
Full native revalidation additionally compares the new owned key.

`dsl_high_reuse.cxx` consumes only those owned records and the common algorithm.
It reuses the existing closed absorbed-MLA recognizer, checked readiness
arithmetic and policy-free locality-record vocabulary. It creates no native
objects, normalizes no loops, emits no CUDA/PTX, invokes no compiler/GPU and
runs no correctness reference. Collection is charged once in validation
accounting, outside candidate exploration. Proposal/evaluation budgets, the
12-descriptor portfolio, old cost scores and convergence controls are unchanged.

## Storage, view and content identities

Every allocation has a stable formal identity `F/<ABI ordinal>`, joined to the
source recipe's logical storage ID. Names and canonical tensor TY equality do
not merge allocations. Each owned storage record contains its shape, dense
strides, element encoding/bytes, aliasing-reshape value IDs and object footprint.
Reshapes preserve a flat allocation; they do not create a new storage version.
Nonzero-offset, opaque-base, symbolic or incompatible strided forms decline
this initial proof rather than acquire an invented dense interpretation.

Content lineage includes the original latent/rotary buffers and their actual
materialized transposes. Common High currently reads a transpose in the score
contraction, while weighted-latent accumulation reads the original latent
buffer. The copy is proved from a complete dense producer, its direct native
load-to-store value flow, the admitted permutation and actual affine indices.
Equal shapes or matching array names are insufficient.

A declared latent-KV or rotary-key trait must agree with the native formal
binding and the admitted DAG. If ingestion supplied no trait, the same closed
graph and native access proofs can establish invocation-local roles, labeled
`closed-graph-derived`. This does not synthesize a persistent-cache type or a
cross-invocation update/lifetime contract.

## Exact access and head-invariance proof

For each relevant read, the provider records its stable native access ID,
storage/version, semantic operand/operation, execution group, loop identities,
actual affine vector, dynamic source element count and unique footprint. It
matches output stores to the source operation by allocation identity, then
proves dense output coverage from strides and loop bounds. That mapping locates
the semantic head coordinate without relying on IV names or matching extents.

For the latent buffer the physical element coordinate is:

    c[b,t,k] -> b*(L*C) + t*C + k

The query-head coefficient must be zero over the admitted head domain. Rotary
reads require the analogous property. Actual source loop extents, offsets,
encoding and coefficients must match the semantic contraction or copy. A head
coefficient of zero alone does not establish the full proof.

PREOPT fully unrolls the baseline four-column rotary reduction. The provider
therefore supports either a complete unit-step reduction loop or a complete
set of distinct fixed-coordinate slices. For a materialized rotary transpose,
slice j has a constant contribution `j*L`. All j in `[0,R)` must occur exactly
once, with the expected remaining coefficients; missing, duplicate, shifted or
misaligned slices are refused. Per-slice footprints and source requests are
counted individually, and the union proves one logical score sweep.

## Versions, effects and lifetimes

An input cache's version is its symbolic incoming value, such as `entry/F/2`.
A materialized transpose receives a definition identity containing its source
operation and complete producer group, and retains the incoming origin version.
These are value/definition identities, not runtime addresses, cache-content
hashes, frozen tensor contents or globally unique type attributes.

The initial proof requires stable pointer bases used only as represented ARRAY
bases, the admitted pointer ABI, no writes to original cache storage, and a
complete no-alias-write answer for every relevant read against all other writes.
A transpose definition must precede its consumers in the structured source.
Read/read aliasing is not promoted to globally unique allocation state.

First/last source-use groups and producer/last-consumer positions are retained
as liveness bounds. The input read-only version is stable within this invocation;
the producer's complete definition establishes the derived copy version.
Caller readiness and well-defined input-buffer validity are ABI preconditions.
GPU event readiness, asynchronous slot publication and CTA synchronization
still require the later physical schedule's proof. No buffer-release decision
or cache residency is implied by these source liveness records.

`DSL_HIGH_REUSE_FACTS` binds the complete source, analysis, owned-memory and
producer-recipe identities plus owner/activation/mutation epoch. An application
must first recapture/revalidate live native facts, then check this owned context.
The existing M4 preflight does both when a proved reuse record is present.
Facts expire before LNO mutation or selected application. They remain readable
as historical values after native cleanup, with `live=false` explicitly shown.
A cache append ordered before a subsequent invocation does not require JIT
compilation; it supplies that invocation's incoming value under the same shape,
layout and read-only-call contract.

## Traffic, units and provenance

All cache operands in this initial lane are FP32, s=4 bytes. With B batches,
H query heads, L tokens, latent width C and rotary width R:

    unique cache payload = s*B*L*(C+R)
    projected fused logical requests = s*B*H*L*(2*C+R)
    common High cache reads = payload + projected fused logical requests
    common High transpose writes = payload

The extra payload in common High reads is the explicit transpose-copy pass.
Latent content has two logical contraction sweeps; rotary content has one.
These counts describe the dense High IR, including complete unrolled slices.
They exclude query/weight/mask/output and other temporary traffic, and do not
predict mask-skipping behavior or generated machine instruction counts.

| Fixture | Unique payload | Common High cache reads | Projected fused logical requests | Transpose writes |
| --- | ---: | ---: | ---: | ---: |
| B=1,H=3,L=127,C=16,R=4 | 10,160 B | 65,024 B | 54,864 B | 10,160 B |
| B=1,H=3,L=8193,C=16,R=4 | 655,440 B | 4,194,816 B | 3,539,376 B | 655,440 B |
| B=2,H=4,L=127,C=16,R=4 | 20,320 B | 166,624 B | 146,304 B | 20,320 B |

GPU global requests, sectors/transactions, HBM/L2 traffic, SMEM traffic, register
spills and selected partial-tuple traffic remain explicitly unknown at this
source boundary. Unknown locality metrics use the existing unknown sentinel;
they are not reported as measured or zero. A global payload is not a CTA tile
size, a residency guarantee, a bandwidth bottleneck result or a speedup claim.

## Conservative scope and existing fallback

The initial provider accepts the closed common absorbed-MLA algorithm with
constant canonical loops, dense flat ARRAYs, compatible encodings, provable
storage bases and complete alias/effect facts. It supports loop reductions and
complete fully unrolled consumer slices. A cache transpose currently needs one
complete dense store site; partially or fully unrolled producers with multiple stores are not
treated as a proved complete definition in v1. General unions of producer
write domains are a future provider extension.

For example, PREOPT turns the B=2,H=4,L=16 copy into multiple stores. MR1 reports
`reuse_multi_store_producer_unproved`, preserves the existing G=1 search and
selected application, and verifies that result against the reference. A proof
refusal clears all partially derived facts and never changes source IR or
silently grants grouping permission. Unknown aliases, writes, changed storage,
head-dependent indices, unsupported offsets/strides and incomplete coverage
follow the same rule.

Neither the new facts nor their absence changes the legacy model, recipe v8,
binding v2, selected-plan seals or kernel materializer catalog. The new records
are analysis outputs, not a production G>1 handoff. MR2 must introduce an
explicitly versioned fact/model/capability binding before any new grouped plan
can win. MR3 must prove its actual placement, ownership, barriers and resources.
The COMMENT workaround and generated CUDA explanations remain unchanged;
read-only MR1 emits review records, not transformation COMMENT nodes.

## Validation and review

`run_high_reuse_review.py` authenticates its retained inputs, executes the
existing short/long common-and-selected correctness/bridge lane, and audits the
pure object dependency closure. Its test-only LNO operates on copies of actual
captured records to exercise head-dependent and strided addresses, cache and
alias writes, escaped/reassigned bases, wrong encodings/extents, conditional and
symbolic reads, producer ordering/value flow, multiple definitions, incomplete
unrolled coverage, unknown metrics, overflow, graph-derived roles and stale
owner/manifest/producer bindings. The test hook and its decline option are
absent from production.

The separate live snapshot test temporarily edits existing cache-trait and
producer-recipe metadata, restores each edit, and verifies native recapture
rejects the old snapshot. The producer edit preserves the legacy source/fact
keys while changing the owned-memory key. Both contexts pass these checks.

Production/test-decline candidate ledgers and full decoded native outputs must
match. Existing source and selected High/Mid/Low traces and PTX must also match
the prior cache-trait receipt. Source/reference tests use guarded buffers and
immutable inputs through actual whirl2c-generated host code; they are separate
from estimation. Additional B=2/H=4 positive and unproved-producer cases each
have independent scalar FP64 reference checks. Existing global-budget and
convergence tests remain required. No GPU execution or physical profiling is
claimed. Review artifacts retain original and lowered `.B`/`.T`, raw diffs,
native proof records, refusal diagnostics and exact commands in a new receipt.

The focused acceptance run passes 30 generated-host reference executions, 37
unique owned-record checks in each baseline context, and 37 existing
convergence-control checks. Production backend symbol/dependency sets remain
unchanged. These are host/compiler checks, not GPU performance validation.
