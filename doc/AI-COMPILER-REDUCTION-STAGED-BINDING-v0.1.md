# Staged Ordered Reduction Binding

Version v0.1 | 2026-10-28 | R2-c2, independent of MMA1

## Scope

`WOPT_DSL_bind_reduction_staged` extends the existing numerical recipe and
[direct source/ISA/storage/host-event binder](AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md).
It binds one synchronous, bit-preserving global-to-shared copy realization,
thread-private ownership of shared-memory subranges, generation reuse and exact
column/row tails. `WOPT_DSL_reduction_staged_slice` freshly revalidates that
binding before deriving one bounded row/generation slice.

This is a **conditional compiler contract**, not runtime discharge, candidate
selection or executable lowering. The [direct runtime guards](AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md)
continue to enforce direct invocations only. They must not be used as if they
enforced a staged kernel's scratch/copy/lifetime contract. No staged runtime
ABI, selected WHIRL kernel, CUDA template or MMA capability is introduced here.
Python/source v1 remains order-unresolved and cannot acquire a staged binding.

## Defined Terms

| Term | Meaning |
| --- | --- |
| `R`, `N` | Native static row count and reduction width of FP32 `X[R,N] -> Y[R]`. |
| Row owner | One global linear thread ID `r`, guarded by `r < R`, owns both copying and ordered consumption for that row. |
| `B` | One-dimensional block size, expressed as row-thread slots per CTA, not a tensor tile width. |
| Scratch object | Symbolic identity of one CTA-local shared-memory allocation; it is not a GMEM allocation/root ID. |
| Slot | One row-thread's disjoint byte interval inside that object, reused by that same thread. Initial support has exactly one slot per owner. |
| `C` | Effective chunk width `min(requested_chunk, N)`; request must be positive. |
| Generation `g` | One consecutive chunk copied to and consumed from the owner's slot; no independent partial sum. |
| Visibility point | Same-thread program point after the complete synchronous bit-copy loop; not an asynchronous completion token. |
| Release | Program point after the last scratch read; authorizes the next generation to overwrite the slot, not deallocation of a whole CTA object. |
| Payload budget | Logical bytes `4*C` per active owner, as used by existing recipe discovery; row padding is charged separately to the CTA allocation. |
| Reservation | Rounded declared scratch allocation plus other shared use, then planning headroom; not occupancy or actual runtime allocation proof. |
| Tail policy | Copy and read exactly the remaining count; neither padded reads nor zero-fill arithmetic is authorized. |

## Ownership And Reuse Of Existing Infrastructure

WOPT owns this read-only active-whole-PU binding and legality policy. Common/com
continues to own native descriptors/meaning and immutable hardware lookup; no
new common record, source opcode/version, TY/WN layout or mapped-image section.
The direct binder is invoked first, retaining strict `add.rn.f32`, FP policy,
contiguous layout, canonical GMEM subranges, leases and host dependencies.
`WOPT_DSL_query_reduction_alternatives` supplies clipped chunk and payload bytes.

Existing `DSL_hardware_description_get_arch` supplies the shared-budget,
allocation-quantum, reserved-headroom and maximum-CTA-thread planning envelope.
Its quantitative table is explicitly estimated; exact architecture-name lookup
does not turn estimates into probed limits. The selected target adapter must
eventually verify actual shared-memory launch/function limits and configuration.
No new constants are installed in hardware/MMA profiles.

The five-event synchronous protocol is a constrained total order, not another
general event-DAG framework. Every adjacent edge is required; forward shortcuts
are allowed, all backward/self/unknown edges are refused. Cross-generation
support is exactly release(g) before acquire(g+1). More general carry edges,
multi-slot, async and cooperative producer/consumer schemes need separate review.

## Storage And Ownership Gate

For shared byte offset `O`, row stride `S` and declared object size `A`:

```text
C             = min(requested_chunk, N)
payload       = checked(4*C)
ctas          = 1 + (R-1)/B
launched      = checked(B*ctas) == direct_request.launched_threads
slot(t)       = [O + t*S, O + t*S + payload), 0 <= t < B
scratch_end   = checked(O + (B-1)*S + payload) <= A
reservation   = checked(round_up(A + other_shared_bytes, HW.shared_quantum)
                        + HW.shared_reserved)
require payload <= payload_budget
require 0 < B <= HW.max_threads_per_cta
require O and S divisible by 4, S >= payload
require reservation <= requested_CTA_budget <= HW.shared_budget
```

All intermediate products/additions/rounding are overflow checked. Full `B`
slots are reserved even when the last CTA has fewer active rows. Different CTAs
have different scratch objects despite equal local offsets. Shared and global
address spaces are distinct; scratch cannot alias input/output GMEM by reusing a
numeric label. Other shared allocations must be fully accounted in the supplied
other-use contract and must not overlap the scratch object.

Padding and extra allocation extent are legal but paid for; this binder does
not select a bank-optimal layout. Headroom is resource-model reservation, not a
new scratch symbol or data-bearing buffer. Capacity alone proves neither
register occupancy nor profitability. The first staged realization may be
slower than direct loads; it is never automatically preferred.

## Copy Visibility, Reuse And Arithmetic

Only `SYNC_BITS32` and `SAME_THREAD` visibility are admitted. The producer and
consumer are the same owner: scalar 32-bit bit-copy semantics, no floating-point
conversion, scaling, FTZ copy, vector overread or cooperative lane exchange.
Selected code must preserve a complete copy loop before the read loop; the
visibility marker is not allowed to signal after only the first store.

```text
per-generation: acquire -> copy_bits -> visible -> ordered_reads -> release
carry:         release(g) -> acquire(g+1)
```

No CTA barrier is needed for this same-thread data dependency. This does not
authorize barrier removal from a cross-thread realization. `cp.async`, TMA,
cross-thread readers, two-slot ping-pong and partial sums are explicitly refused
by this first binder rather than accepted under a generic 'copy completed' flag.

```text
for each launched row-thread r:
    if r >= R: return                 # before every input/scratch/output access
    t = r % B
    state = positive_zero_f32          # initialized once, outside chunk loop
    for g in [0, 1+(N-1)/C):
        base = g*C
        count = min(C, N-base)
        acquire(slot(t), generation=g) # requires preceding release if g > 0
        for j in [0,count):
            copy_bits32(X[r,base+j], scratch[O+t*S+4*j])
        visible_same_owner()          # full synchronous copy is complete
        for j in [0,count):
            state = add_rn_f32_no_ftz(state, bits_as_f32(scratch[O+t*S+4*j]))
        release_after_last_read(slot(t), generation=g)
    Y[r] = state
```

The markers describe the plan's required ordering; no new WHIRL operations or
runtime calls are fabricated by this query. One accumulator persists across
generations. There are exactly `N` additions per row, including the first
`+0 + X[r,0]`. Finite/infinite/signed-zero bits and the existing NaN-class policy
are unchanged. Reassociation, tree reduction, chunk-local accumulators and
merging partial sums remain forbidden.

## Exact Column And Row Tails

```text
chunks           = 1 + (N-1)/C
last_count       = 1 + (N-1)%C
last_active_rows = 1 + (R-1)%B
input_offset     = input_root_offset + 4*(r*N + g*C)
scratch_offset   = O + (r%B)*S
output_offset    = output_root_offset + 4*r
```

For divisible widths `last_count=C`, never zero. A chunk request larger than
`N`, including UINT64_MAX, is clipped before footprint math. Width one still
performs its ordered addition. Inactive rows have no slice and execute no copy,
scratch read or output access. The slice API refuses row>=R, generation>=chunks,
stale source or corrupted derived fields without touching output. Arithmetic
stays O(1), with no compile-time expansion proportional to a general tensor.

## Obligations And Integration Boundary

Successful staged bindings retain the direct mask 15 plus scratch allocation=16,
copy visibility=32, slot lifetime=64 and exact tail/order=128: mask 255. These are
**outstanding obligations**, not 255 proven runtime facts. Refused staged queries
grant no staged mask; their embedded direct binding can still report its own
conditional contract, which must not be mistaken for staged success.

Selected WHIRL must eventually carry the shared symbol/extent, row mapping,
guarded loops, bit-copy semantics, strict scalar addition and ordering/reuse.
Emitters consume that one executable truth. The runtime adapter must verify the
actual resource/launch/entry contract; the existing direct-only guard ABI must
not silently drop the extra staged obligations. No staging benefit or hidden
overlap credit is granted before complete cost comparison.

## Trace And Tool Summaries

The optional caller-controlled trace adds `AI_REDUCE_STAGE 1 BIND` and
`AI_REDUCE_SLICE 1 PLAN`, with positional headers and source line last. Existing
AI trace options/bit/default remain unchanged; automatic production invocation
is not enabled by this query. Trace-off contracts and slices must be identical.
`PLAN` means planned bounds, never executed movement or transformation.

| Log | What it summarizes |
| --- | --- |
| `alternatives.log` | Every binding query, reason, outstanding obligations and selected=false. |
| `residency.log` | Scratch-object/slot identity, padding, byte extent, other shared use, reservation and estimated-envelope provenance. |
| `fetch-pipeline.log` | Copy/visibility/read/release edges, generation carry, exact tails and active-row predicates; no async/native-copy claim. |
| `numerical.log` | Host slice witnesses versus independent integer binary32 recurrence, exact read coverage and persistent accumulator. |
| `optimization.log` | Full successful phase-report collection with explicit refusals. |
| `optimization-summary.log` | Completed binding coverage and still-open runtime, complete cost/selection, selected lowering and native certification gates. |

Every log begins with `# What it summarizes:`. Independent auditing recomputes
capacity/rounding with unbounded integers, DAG closure, slice coverage, offsets,
source identity joined to actual PREOPT, and refusal reasons. Host witnesses
poison unused scratch, copy raw bits and consume only native-derived exact
slices; they are not generated-device numerical execution.

## Validation And Next Slices

Native fixtures cover all four existing target names, exact and nondivisible
widths, one-element chunks, clipped width-one requests, partial CTA rows, source
v1 refusal and unsupported layout. Each positive fixture covers malformed
requests, overflow, payload/CTA budget, overlapping row slots, visibility edges,
premature reuse, unsupported copies/tails, stale derived snapshots and read-only
native tables. Retain `.B`, independently reopened `-st -src` `.T`, actual
PREOPT `.t`, binding/slice `.t`, host witnesses, reports, sources, commands and
checksums in immutable `r2-reduction-staged-binding` receipts. No selected
transformation occurs, so no before/after transformation images are invented.

Next: complete direct/staged delivery, dependent-add, bank/occupancy, staging
traffic, synchronization/launch and overlap costs; compare all legal alternatives
with explicit provenance and reason. Then select/materialize high-level WHIRL,
wire a staged-aware target/runtime adapter, and certify emitted strict-add/copy
semantics and GPU numerics/resources/performance. Async/cooperative/multi-slot
extensions require their own progress/visibility/reuse proof and cost model.

## Accepted Bounded Evidence

Eight native cases pass 539 assertions. The six supported cases each produce
42 binding rows (baseline, forty refusals and a legal forward shortcut); v1 and
unsupported layout each produce one refusal. Total: 254 queries, 12 conditional
successes and 242 refusals. Native-derived slices cover 181 row/generation
intervals, including 33 rows spanning a partial second CTA. Forty-six host row
witnesses match the independent integer binary32 recurrence.

All 42 trace/auditor tests pass, including five staged tests and 2,050 independent
event-graph comparisons. Existing direct guard/runtime, numerical recipe,
Softmax/normalization, PREOPT/driver, native syntax/layout, focused CKKS/FHE and
backend/inline link boundaries pass. No staged runtime or GPU run is claimed.
Durable evidence is indexed under
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r2-reduction-staged-binding/`.
