# Quantized MLA GPU staging specification v1.0

November 10, 2026. Continues [Q5 selected strict plans](AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md).
The first implemented boundary is **Q6a: detached placement, transfer and
pipeline admission**. Q6 as a whole is not complete. No quantized GPU worker,
device serving adapter or TMA instruction stream is emitted by Q6a.

## Decode staging and the compute bubble

Decode has few queries and many cached tokens. The paired streams are latent
payload, latent F32 scale, rotary-key payload and rotary-key F32 scale. A tile
is fetched once per query/head group/pass and reused by the group's heads.
S8 sign-extends before F32 conversion. Offset-eight U4 uses the low nibble for
the even component, the high nibble for the odd component, then F32 subtraction
of eight and multiplication by that token's corresponding scale.

The strict Q5 implementation has three ordered token passes: raw global
maximum, denominator, and normalized weighted-latent accumulation. It recomputes
scores to avoid full score/probability and decoded-cache materialization. This
does not authorize substituting an online-softmax tuple merge or a parallel
sum with different rounding. Those need an explicit numerical contract and
selected-only reference qualification.

For TMA delivery, two raw slots hold packed payload and scales; one decoded
F32 tile is shared by the heads. The steady-state schedule is:

1. Wait until every raw fragment of tile n has arrived.
2. Issue tile n+1 into the other raw slot.
3. Dequantize tile n into the decoded shared tile and publish it to all heads.
4. Compute the current pass for tile n, preserving each head's token order.
5. Join all consumers before reusing decoded storage or releasing its slot.

The raw transfer can overlap dequantization/computation. This schedule does
not claim that dequantization itself overlaps attention arithmetic; a producer
warp or a second decoded tile would require another selected resource/protocol
contract. A copy wait is insufficient to prove decoded-data publication or
last-consumer completion.

For a simplified producer/consumer pipeline, steady-state tile time is at
least max(transfer time, preparation plus compute time), with exposed transfer
delay approximately max(0, transfer time - available independent work).
This is a lower-bound explanation, not the Q5 cost model or a speedup estimate.
Startup, drain, synchronous fragments, conversion throughput, occupancy and
insufficient CTAs can leave bubbles. Reducing every bubble is not guaranteed.

## Implemented Q6a contracts

`tensor_quantized_gpu.h/.cxx` is VHO-owned and detached. It reuses Q5's exact
source recognizer and canonical descriptor; it does not add another candidate
catalog, selector, runtime observer or native materializer. The standalone
driver's `q6-contract selected.B control.json result.json`:

- Reopens a genuine Q5 selected High image using the normal native reader.
- Checks its accepted full native key, source semantics, fixed W/G descriptor
  and exact Q3 snapshot identity supplied in the explicit offline control.
- Refuses the floating/source-baseline descriptor, stale native product,
  unsupported architecture/toolchain, insufficient resources and bad controls.
- Derives CTA count B*Q*ceil(H/G), the 32*G thread reservation, raw stream
  ranges, decoded storage, barriers and the complete shared-memory requirement.
- Builds and independently validates a bounded three-pass action trace.
- Checks WN executable identity and ST/TY/TCON table sizes remain unchanged.

The control is canonical sorted compact JSON with a trailing newline. Its
schema is `open64.q6.placement-control.v1`, scope
`offline-selected-contract`, and fields are `delivery`, `arch`, `ptx`,
`toolkit`, `max_threads`, `shared_limit`, and `q3_binding_key` in addition to
schema/scope. Delivery 1 means synchronous; 2 means bulk TMA. Initial support
is restricted to SM90 and at most 48 KiB static shared storage, avoiding a
new shared-memory opt-in ABI. TMA uses the conservative PTX 8.6 / toolkit 12.8
implementation floor already adopted by the reviewed MMA5-H service. These
are implementation gates, not claims about the earliest hardware/ISA support.

Every result says `gpu_execution_eligible=false`,
`device_bindings_implemented=false`, `kernel_emitted=false`, and
`Q5_cost_reused_for_async_pipeline=false`. Its key includes the complete native
selection, Q3 identity, control and derived layout. It is equality evidence,
not a signature, a live lease or a performance certificate. A copied Q3 key
cannot become current merely by passing this offline check.

## Transfers and page/scale joins

`VHO_q6_partition` accepts one in-bounds contiguous physical byte run. It
generates a synchronous prefix, an aligned bulk middle, and a synchronous
suffix when source and destination residues permit that decomposition.
Different residues retain a fully synchronous copy. No fragment rounds up
beyond the requested range. Bulk expected-transaction bytes exclude every
synchronous byte. Unsupported/overflowing inputs leave both outputs unchanged.

`VHO_q6_transfers` uses **one common physical-token run list** for all four
streams. Source row widths are derived from the selected S8/U4 source; scale
rows remain four bytes. It packs runs into the selected tile's exact raw slot,
guards the final tile, requires complete logical-token coverage, validates all
four source ranges and sums asynchronous transaction bytes across the streams.
The device owner must prove that the runs match actual source index SSA and
the leased page map. This pure mechanism does not observe that map or grant
authority to an untrusted list.

Page discontinuities remain separate runs. Allocator page size P is not
silently changed into compute width W. Small or misaligned runs may have no
bulk middle; their synchronous path preserves the same bytes and scales.
Q3 host alignment is never consumed as proof of device alignment.

NVIDIA requires the relevant bulk copy addresses and transfer size to satisfy
16-byte constraints, with a correctly aligned shared barrier. The backend must
also honor async-proxy visibility and mbarrier completion; ordinary CTA
synchronization alone does not complete an async transfer. See the official
[CUDA async-copy contract](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/async-copies.html)
and [PTX ISA](https://docs.nvidia.com/cuda/archive/12.8.1/parallel-thread-execution/index.html).
The one-barrier transaction-count envelope is conservatively capped at
0xffff0 bytes; the selected 48 KiB storage bound is tighter for these tiles.

## Lifetime verification

The independent automaton distinguishes acquire, issue, wait, dequantize,
publish, consume, and release. Issue is not completion. A slot's pass and tile
identity cannot change while occupied. Dequantization requires arrival of the
current tile; a decoded tile cannot be overwritten before its consumers finish.
Every publication and final release must be present. Tokens/tiles retain
increasing source order within each pass, and a pass drains before the next.

The trace is detached verification data. It is not WHIRL and must not be
reported as a transformed kernel or as evidence that hardware barriers were
inserted. The test-only tool independently corrupts actions, ownership and
pass identity and exhaustively checks small byte alignment/residue cases.
Four-stream byte oracles cover discontinuities, tails, padding canaries and
late-stream failures without partial output publication.

## Remaining Q6 work and acceptance gates

| Gate | Required implementation and evidence |
| --- | --- |
| Q6b native realization | Produce selected-only High DO_LOOP/OPR_ARRAY staging, explicit typed transfer/wait/consumer-publication semantics and matching COMMENT explanations; independently reopen before/after .B/.T and retain full diffs |
| Q6c memory/GPU/PTX bridge | Reuse native ARRAY/SCF lowering and device symbol/address-space services; extend the existing bridge with a separately versioned quantized admission; consume actual native operations in whirl2c/PTX, with no CUDA algorithm template or emitter scheduling |
| Q6d device authority | Bind actual allocations/ranges, page/index maps, cache and scale generations, stream/event readiness and exclusive output lifetimes; keep all inputs pinned until every launch/copy finishes; status/completion/parent-CAS gate atomic four-buffer publication; abandon unpublished device work on failure |
| Complete cost contract | Include raw slots, decoded storage, barrier state, conversion/unpack service, synchronous fragments, startup/drain and resource occupancy; do not inherit Q5 synchronous scores or claim the selected W/G is optimal under a different pipeline |
| Qualification | Reference-test only the selected emitted plan, including S8/U4, dense/paged/append, tails, masks, nonfinite behavior, stale generations and failure/drain cases; compile/assemble and run memory/race checks on NVIDIA hardware before GPU correctness/performance claims |

Review WHIRL.pdf sections 1.3, 1.4, 1.7-1.9 and existing ASM_STMT semantics
before adding native carriers. The current Q6a service changes no opcodes,
binary layouts, WN representation or reader/writer. The normative High
DO_LOOP/ARRAY and Mid control/address-lowering boundaries were reviewed for
this design; target-specific actions must not be invented by source emission.

The existing Q5 host callback/reference checks and source fallback remain
valid. The old FP32 device worker cannot consume packed quantized formals.
There is no NVIDIA GPU available in the current environment; host contract
checks are not GPU correctness or performance evidence.

Two-phase exploration remains unchanged and read-only. General PREOPT COMMENT
retention repair remains separate; native Q6 realization must preserve the
established selected-entry COMMENT workaround. AOT selection design remains
deferred: no phase selector/parser, dispatch threshold, policy or packaging
ABI is introduced. Primary owner, successor, independent reviewer and approving
lead remain TBD, blocking knowledge-transfer acceptance.
