# MLA Q6b native staging specification v1.0

November 10, 2026. Continues [Q6a staging contracts](AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md)
and [Q5 selected strict algorithms](AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md).

## Delivered boundary

Q6b constructs a genuine native High WHIRL body with explicit raw staging,
waits, dequantization, decoded-data publication, consumer completion and drain.
DO_LOOP and OPR_ARRAY retain logical work ownership. The source formal ABI,
Q3 source/snapshot join, ordered F32 arithmetic and source fallback remain.
The output is a selected algorithm with abstract staging operations, **not a
mapped GPU worker**. Q6c owns device participants and CUDA/PTX instructions;
Q6d owns actual device readiness, generations and atomic publication.

The construction is an explicit, selected-only O3 command in the existing
`tensor_selected_bridge` tool:

```
tensor_selected_bridge q6-apply selected-Q5.B control.json before.B after.B manifest.json --opt-level=3
tensor_selected_bridge q6-audit after.B keys.json
ir_b2a -st -src before.B before.T
ir_b2a -st -src after.B after.T
```

`control.json` uses the unchanged canonical Q6a offline control schema. It is
read once before construction; its exact bytes bind the output. Input must be
one accepted optimized Q5 High PU, with an exact native key, canonical fixed
W/G descriptor, supported S8/U4 source, exact Q3 snapshot identity, no live
dependence manager and a fitting explicit resource envelope. Baseline/floating,
O0, stale, already-applied and insufficient-resource requests refuse. No
alternate W/G is searched or lowered on refusal.

## Native representation and ownership

VHO owns construction in the driver-selected active PU. The existing private
Q1/Q5 builder is instantiated for the standalone staging tool under
`OPEN64_Q6_NATIVE_STAGE`. Its arithmetic and source guards are shared, not
reimplemented as a CUDA template. The ordinary instantiation still exports
only the existing Q1/Q5 services. Q6 construction and parsing dependencies are
not added to `be.so`, the frontend or `lw_inline`.

One aligned U1 array carries a shared-storage role, exact byte size and the
Q6a layout: one or two raw slots, each with latent payload, latent scale,
rotary payload and rotary scale ranges; then one decoded F32 tile and the
reserved async barrier identities. Ordinary host C uses function-frame
storage for this arena. Q6c must realize the role as actual CTA-local shared
memory and enforce the resource/launch contract.

Typed pointer views of raw S8/U4, F32 scales and decoded F32 tiles are explicit
local symbols. TAS preserves the byte address while carrying pointer type;
OPR_ARRAY indices use that view's scalar units. This prevents source emission
from confusing a float access with a byte access. WHIRL.pdf sections 1.13-1.14
define the representation-preserving TAS/high-level-type contract. The existing
complete native-key consumer now accepts TAS and records its type through the
existing generic type fields; old supported keys retain their spelling.

PREOPT may propagate these views into nested rank-one ARRAY addresses. The
whirl2c memory-reference printer now reconstructs those addresses by summing
each index in its own WHIRL element-size units and casting the resulting byte
address to the actual scalar memory type. The bounded case is a plain rank-one
byte arena, not an array of C array objects. Other structured/vector addressing
keeps the existing printer. This fixes source reconstruction without changing
the native tree, placement, arithmetic or scheduling policy.

No opcode, node layout, mapped-image section, type-kind encoding, reader or
writer changes. Existing CALL/PARM, TAS, ARRAY, DO_LOOP, IF and COMMENT carry
the implementation. The original formal symbols, tensor/cache traits and
persistent Q1 semantic recipe remain; no packed formal is reinterpreted as an
old FP32 kernel input.

## Versioned High staging operations

All operations are statement-level void CALLs with explicit prototypes,
conservative normal call effects and callee metadata
`tensor.q6.primitive=q6b-byte-staging-v1`. The prefix below is
`__open64_mla_stage_`. Existing foreign declarations with reserved names refuse;
only declarations owned by this construction are reused.

| Suffix | Arguments | Semantic obligation |
| --- | --- | --- |
| `init` | identity, raw base, U8 raw bytes, decoded base, U8 decoded bytes | Begin one logical raw-slot lifetime; raw slots are disjoint and share exactly one decoded region |
| `acquire` | identity, U8 tile, U4 pass, U8 logical bytes | Own an idle slot for this generation; record exact bytes to join |
| `copy` | identity, destination, source, U8 bytes, U4 delivery | Copy exactly this range; delivery 1 is synchronous, 2 permits deferred completion until wait |
| `wait` | identity | Complete all copies for this slot before raw loads/dequantization |
| `publish` | identity | Make decoded writes available to every logical consumer |
| `consume` | identity | Join all consumers before decoded storage or a raw slot can be reused |
| `release` | identity | End the consumed tile generation and permit overwrite |
| `end` | identity | Drain and invalidate the slot; a never-used initialized slot may also end |

Pointer arguments have explicit void-pointer TYs. Counts/tile IDs are unsigned
64-bit; pass/delivery are unsigned 32-bit. Identity pointers are scope tokens:
the High contract does not read or write barrier state through them. The async
layout reserves hardware barrier storage for Q6c; the synchronous path can use
its raw base as an identity without allocating hardware barrier state.

`acquire.logical_bytes` counts all four logical streams, including synchronous
fragments. **It is not an mbarrier expected-transaction count.** Q6c must split
the exact runs using actual device alignment, account only async bulk bytes in
hardware transaction counts, and emit initialization, proxy ordering, waits,
consumer collectives, safe reuse and invalidation. No emitter may silently
turn every High copy into a TMA instruction.

These operation contracts require a lowering or an implementation. Q6b's
compiled reference links a test-only host model. The manifest explicitly says
`stage_operations_lowered=false` and
`requires_stage_operation_implementation=true`. There is no production stage
runtime dependency and no claim that the generated C links by itself.
Unmapped PUs retain DEVICE_SYMBOL_NONE; the existing CUDA emitter gate rejects
them as device entries until Q6c supplies the physical contract.

## Algorithm and pipeline

1. Preserve Q5's prefix: source quantization, functional updates, index/mapping
   construction and query projection. Validate all paged indices before any
   staging scope is initialized, so a source guard can return without pending
   copies referring to abandoned storage.
2. For each batch/query/head group, initialize the selected raw slots and
   original per-head maximum, denominator and weighted-latent accumulators.
3. In each of the three source-ordered passes, acquire/issue tile zero. For a
   tile, derive maximal consecutive physical-slot runs from original index SSA.
   Emit exactly the same run split for all four payload/scale streams; guard
   tails before reads and pass exact source/destination/count operands.
4. Wait for the current slot. With two slots, issue the next tile into the
   other slot before preparing/consuming the current tile. With one slot,
   issue the successor only after current consumers release.
5. Decode S8 with sign extension, or U4 with even-low/odd-high unpack and F32
   offset-eight subtraction, then multiply the corresponding F32 token scale.
   Publish the decoded tile, run all logical head consumers, join consumption,
   and release. Logical heads remain ordered High loops for Q6c to map safely.
6. Finish global max/safe-max, ordered denominator/safe-denominator, and
   normalized weighted-latent passes without online-softmax or split-sum
   reassociation. Preserve masked nonfinite multiplication. Drain/end both
   slots, then retain value projection, statistics and all cache/scale copies.

The original ABI scratch allocations remain. Staging changes where selected
tile traffic occurs; it does not silently shrink serving allocations or turn
functional cache updates into in-place mutations. Full source capacity still
executes in zero-active-query cases, as required by the source semantics.

## Binding, invalidation, explanations and failure

`tensor.q6.schema=mla-native-staging-v1`, the exact control, complete mapping,
original Q5 descriptor, explanation and full native key bind the result. The
key covers TAS types, calls/effects/prototypes, all shared declarations, private
operation versions and formal cache traits. Independent reopen/audit rejects
altered operations or views before accepting a product. Metadata equality is
not a signature or a live serving lease.

Q3 host execution reuses the existing source/snapshot-bound selected callback
route. Buffers/page maps/cache and scale generations remain pinned; status,
completion and parent-CAS gate the atomic four-buffer publication. The host
reference implementation drains all staging scopes before callback return.
Copied host facts never authorize device pointers, streams or events.

The process owns the mutation journal. Capture the before image immediately
before construction; expire its epoch; admit no old native analyses. Original
Q1 source-form facts remain ineligible for the selected loops. New locals,
shared storage, helper declarations and TYs are intentional; F32 constants
reuse source TCONs. Construction/verification refusal abandons the compiler process, retains input/before
images and publishes no after image or completion manifest. This is process
isolation, not an in-process rollback claim. Each binary is published only
after its writer completes, without replacement; the manifest is the final
completion gate. A later output I/O failure may leave a verified diagnostic
checkpoint, but an incomplete output family is not an accepted plan.

The existing backend-linked reader bootstrap reserves an extra unused empty
BLK row before transferring global tables. The before image reflects that real
initialized scope. Review retains the raw input-to-before diff and permits
exactly that appended zero row only; every other full-trace byte must match.
The transformation's before/after pair includes the same bootstrap row, so it
does not obscure the changes made by staging. No reader/writer fix is inferred.

One generated entry COMMENT explains actual dimensions, arena layout,
contiguous runs, three passes, waits, publication, consumer join and drain,
plus the remaining physical-mapping boundary. The existing Q1 PREOPT workaround
restores the persisted Q6 explanation before the older Q5 explanation. Both
High and normal-backend output are emitted by whirl2c with those comments.
General PREOPT COMMENT retention repair remains a separate effort.

## Qualification and remaining work

The review builds only the chosen Q5 W/G after estimation. Async delivery is
an explicit requested transport fixture, not a newly cost-selected winner.
Two explicit synchronous fixtures exercise the transport implementation; no
timings, runtime observations or reference results feed candidate exploration.

The test-only delayed-copy model poisons raw/decoded storage, completes async
copies only at wait in reverse issue order, validates ranges and byte totals,
enforces decoded ownership/publication/consumer completion, and reports final
quiescence. Negative tests validate the model itself. Actual generated High C
and normal backend C are compared exactly with Q5/source values, with PyTorch
and the independent scalar reference retained through Q3. Coverage includes
S8/U4, dense/paged/append, masks/nonfinite values, tail tokens/heads, zero active
queries, genuine 8193-token cases, canaries, stale binding/type/protocol seals
and failed selected construction. Host arena review uses strict F32 flags and
`-fno-strict-aliasing` for its explicit byte-storage views.

Q6c still must lower memory operations and physical GPU participants to
CUDA/PTX. Q6d still must implement actual device authority and completion.
Prefix construction/projections, attention and the tail have global data
dependencies in this sequential High body. Q6c must preserve them with ordered
kernel boundaries or another qualified global synchronization mechanism;
the per-tile publication/consumer collectives alone are insufficient.
Complete pipeline costing must include raw storage, preparation, barriers,
startup/drain, synchronous fragments and occupancy before automatic comparison
with Q5's synchronous model. Q7 requires NVIDIA execution/race/performance
evidence; none is inferred from this host model.

The initial Q6a envelope still caps static shared storage at 48 KiB. Larger
latent dimensions can therefore refuse even at the smallest catalog width.
For example, W=32, R=512 and Ro=64 need 72 KiB for the decoded tile alone,
before raw slots and barriers. This milestone does not qualify that shape:
the source fallback must remain. Q6c needs a reviewed shared-memory opt-in
contract or a separately qualified finer staging layout before admitting it.
The small-latent correctness fixtures are not throughput proxies for that case.

AOT selection remains deferred. No selector/parser, runtime dispatch policy,
threshold or packaging ABI is introduced. Owner, successor, independent reviewer
and approving lead remain TBD, blocking knowledge-transfer acceptance.
