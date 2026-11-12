# MLA MR0 cache-trait audit and static characterization v0.1

November 8, 2026 | Static audit complete; MR0 measured acceptance remains open

This report records the user's clarification that no Hopper machine is
available and the request to verify whether MLA KV-cache identity is retained
in tensor traits. It supplements the [data movement design](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md)
and [MR0/MR1 plan](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md).
It changes no compiler code, tensor schema, production recipe, search space,
generated CUDA or numerical behavior. Ownership assignments remain TBD.

## Finding

The generic TensorDescriptorIR has a TensorTraitSet carrier, but the retained
M6 absorbed-MLA inputs do **not** have a KV-cache trait. Their traits are already
missing in original tensor WHIRL, before conversion to common algorithmic
loops. This is an ingestion/propagation gap, rather than evidence that PREOPT
removed a previously supplied cache trait.

Fresh `ir_b2a -st -src` reopening of both retained input `.B` files shows:

| Input | Short shape | Long shape | TensorTraitSet.traits | runtime_state / lineage |
| --- | --- | --- | --- | --- |
| `cache` | `[1,127,16]` | `[1,8193,16]` | `<missing>` | `<missing>` / `<missing>` |
| `rope_cache` | `[1,127,4]` | `[1,8193,4]` | `<missing>` | `<missing>` / `<missing>` |

Both have `kind=tensor`, `dtype=float32`, rank 3 and `layout=logical`.
The names and source comments describe caches to a reader; neither is a typed
semantic guarantee. `cache` is the compressed latent operand used for both the
absorbed key contraction and weighted latent accumulation. `rope_cache` is
the rotary key operand. They are not conventional separate expanded K and V
arrays, nor are they the attention partial-tuple scratch buffer.

## Trace through the implementation

1. [Generic descriptor](../osprey/common/com/symtab.h):
   `TY_TENSOR_CANONICAL_DESCRIPTOR.traits` and
   `TENSOR_DESCRIPTOR_RECORD.traits` provide the existing carrier.
   Runtime state and lineage are excluded from canonical type identity.
   This infrastructure does not automatically classify a buffer as KV cache.
2. [MLA source](../osprey/torch2whirl/python/tests/models/absorbed_mla.py):
   the read-only core accepts caches as ordinary formals. Cache append,
   capacity, valid-length state and cross-call ownership are outside this fixture.
3. [Tensor-expression ingestion](../osprey/torch2whirl/python/open64_dsc/tensor_ingestion.py):
   the local `tensor_type` constructs only kind, dtype, rank, logical shape
   and logical layout. It does not bind traits, runtime state or lineage.
   This is the actual MLA lane; generic example-input/parameter traits in
   other interpreter paths do not annotate this fixture.
4. [Tensor-to-loop admission](../osprey/be/vho/tensor_kernel_lower.cxx):
   `VHO_TENSOR_CAPTURE::Value` has an explicit descriptor-field allowlist.
   `traits`, `runtime_state` and `memory` are not supported there. Supplying a
   `traits` field today would produce an unsupported-descriptor refusal,
   rather than retain it in the production route. The existing
   `test_unimplemented_tensor_traits_are_not_silently_dropped` documents this
   fail-closed behavior; it was inspected, not rerun for this read-only audit.
5. [Detached value recipe](../osprey/be/vho/tensor_kernel_lower.h):
   `VHO_TENSOR_VALUE` carries name, dtype, shape, role, ordinal and storage.
   The two retained selected JSON recipes contain exactly these six fields
   on each value. Role 1 means ordinary input, not KV cache. Argument metadata
   retains binding/size/name information, not a cache-lifetime contract.
6. [Attention recognition](../osprey/be/vho/tensor_attention_pattern.inc):
   `VHO_tensor_attention` verifies the closed contraction/softmax DAG and
   compatible shapes/dataflow, then derives `a.cache` and `a.rope_cache`.
   The compiler therefore knows which operands serve these roles inside the
   admitted attention algorithm without a source-name heuristic. This
   derived within-call role does not prove persistent cache state across calls.
7. [Owned High snapshot](../osprey/be/lno/dsl_high_snapshot.h):
   affine read/write accesses, dependence/DU/alias facts and mutation epochs
   exist. No dedicated KV semantic-role or persistent-state field is present.
   Those access facts are useful for proving head-invariant reuse, but are
   distinct from an imported cache trait. Inspected selected High/Mid/Low
   dumps provide no TensorTraitSet on these scalar buffer representations.

There is separate existing infrastructure for
`transformer.kv_cache_state` in [the contract registry](../osprey/common/com/dsl_contract.cxx)
and [decode profile verifier](../osprey/common/com/dsl_gatekeeper.cxx), plus
named mutable state objects in the Llama decode frontend. That profile
requires rank-4 BHSD functional append and equal query/KV head counts.
It is not an MLA latent-cache annotation and cannot be reused unchanged for
rank-3 head-shared latent/rotary buffers.

## Static movement characterization of the fixed M6 baseline

The retained fixture uses B=1, H=3, latent width C=16, rotary width R=4,
FP32 cache elements, L=127 or L=8193. These small unequal dimensions are
correctness fixtures, not a representative full DeepSeek performance workload.

| Quantity | Short | Long |
| --- | ---: | ---: |
| Ordered kernel launches | 5 | 6 |
| Attention CTAs | 3 | 99 |
| Attention threads/CTA | 128 | 128 |
| Attention static shared bytes/CTA | 544 | 1,056 |
| Unique latent + rotary cache payload, bytes | 10,160 | 655,440 |
| Unfactored logical cache reads for two contractions, bytes | 54,864 | 3,539,376 |
| Two-contraction FLOPs | 27,432 | 1,769,688 |

Here unique payload is `4*B*L*(C+R)`, logical reads are
`4*B*H*L*(2*C+R)`, and core FLOPs are `2*B*H*L*(2*C+R)`.
The logical-read quantity is an analytical dense active-token count before
cache reuse/load elimination, not measured transactions or HBM reads.
Mask sparsity, generated control flow, redundant loads, compiler optimization
and hardware caching can change actual traffic. Projection, mask, output,
softmax, tuple/merge and spill traffic are excluded from these cache-only
formulas. There is no claim that payload can be loaded only once by the
current algorithm, nor that these ratios predict a latency speedup.

The admitted equations index latent/rotary cache by batch, token and column,
with no head coordinate. Thus head-invariant reuse is a concrete optimization
opportunity within a proved read lifetime. The fixed baseline uses separate
head CTAs, and a KV semantic trait alone would not create cooperative SMEM
staging or guarantee cache residency. Launch cost, limited parallelism,
scalar arithmetic, synchronization and cache hits all remain possible
bottlenecks. No measured critical bottleneck can be selected without execution.

## Required follow-through

Use the existing WHIRL tensor-trait infrastructure for declared semantic
roles. Define separate latent-KV and rotary-key roles with explicit contracts,
rather than infer persistent state from `cache` in a source name. Preserve
symbolic storage/view identities and access/effect versions as value/analysis
facts; allocation identity, valid length, lifetime and readiness must not be
encoded as unique allocation IDs in canonical tensor types. An immutable read
during one invocation is not a promise of immutability between invocations.

The MR1 implementation must add supported admission and lossless propagation
through the detached recipe/common High boundary as well as frontend
annotation. Keep old recipes readable under the established versioning and
fallback policy; unsupported/contradictory traits must still fail closed.
Prove actual same-version, head-independent accesses before grouping heads.
The trait identifies purpose; affine/effect/alias facts establish legality.

Focused follow-up checks should cover native image reopening, view propagation,
two distinct allocations with otherwise equal types, a legal cache append
before the call, stale epoch/effect rejection, head-dependent indexing refusal,
ordinary unannotated input fallback and unchanged numerical permissions.
Any production transformation remains selected-only, followed by reference
correctness checks. No candidate search/lowering or GPU execution took place
in this audit. Existing M6 correctness receipts remain the baseline evidence.

MR0 GPU correctness, event timing, cache/DRAM counters and actual bottleneck
identification remain open because the user has no Hopper access. Static
traffic analysis and MR1 metadata work can proceed without those measurements.
Fixed-baseline profiling remains separate from estimation-only exploration.

## Evidence and reproducibility

Source baseline: `b85910cf61216bf21ef6ce5d1d9cb0db28773155`.
Immutable M6 receipt:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/m6-selected-production-dependence-recovery/sha256-abe46395f5a3e619dc2d2024d82c0aaf64cff0d25c91e6514ad11d9b54d167b1`.
Original source images: `m3/fixtures/selected/{short,long}/mla.B` within it.
Selected recipes/launch manifests: `bridge/{short,long}/`.

The accompanying `cache-trait-audit.json` records source/artifact SHA-256,
four exact descriptor excerpts, six native-stage summaries, launch facts and
analytical arithmetic. Input `.B` copies compare byte-for-byte by SHA-256
with their immutable originals. Fresh standalone `ir_b2a` reopening succeeded
for both contexts with empty stderr. The audit reads retained artifacts and
does not regenerate, overwrite or transform the M6 baseline.
