# MLA Q6c native GPU lowering specification v1.0

November 10, 2026. Continues [Q6b native staging](AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md).
This boundary maps the selected native staging tree to GPU participants, emits
CUDA through whirl2c, and emits native sm_90 PTX through the existing bounded
NVISA selected consumer. Q6d device authority and Q7 NVIDIA execution remain
separate gates. AOT selection remains deferred.

## Scope and architecture

The standalone `tensor_selected_bridge` driver owns one reopened active PU,
its mutation epoch, snapshots and publication. VHO's new
`tensor_quantized_device.cxx` consumes that PU only. It neither traverses other
PUs nor invokes a search. The driver invokes it in a separate compiler process
for each fixed stage. No frontend, Python, CUDA library or new optimization API
is linked into be.so. The existing backend sealed-Low CUDA admission is extended
by read-only inline identity checking before ordinary VHO/ABI processing.

No WHIRL operator, binary section, type-kind encoding or reader/writer change
is introduced. Existing CALL/PARM, DO_LOOP/ARRAY, TAS, scalar operations,
COMMENT and device-symbol attributes carry all meaning. The actual accepted
Q6b body is transformed; there is no second MLA implementation in CUDA text.
The entry/body and declarations are keyed at each boundary. Standard ARRAY/SCF
lowering produces Mid memory/control operations; Low replaces abstract owner
formals with existing dim3 blockIdx/threadIdx field loads and restores the exact
original pointer-formal order. These are the selected bridge's bounded phase
contracts, not a claim that all historical general NVISA CG/RVI passes ran.

WHIRL.pdf sections 1.3, 1.8 and 1.13-1.14 control the level, EVAL and typed-view
contracts. The native-key vocabulary additionally covers EVAL's expression and
EXTRACT_BITS' actual offset/width fields; both arise from standard lowering.
Previous supported keys retain their spelling. Unsupported nodes refuse with
their opcode. Neither an executable key nor metadata is a signature or a lease.

## Ordered launch and work ownership

The full Q6b body contains globally dependent preparation, attention and tail
statements. A per-tile CTA barrier cannot synchronize separate CTAs. The native
phase therefore splits at the exact six typed-view assignments and the unique
three-loop attention nest of the verified Q6b producer:

| Stage | Grid/block | Native behavior |
| --- | --- | --- |
| prepare | 1 / 1 | Existing quantization, functional updates, index construction, query projection and source guard checks; initializes status |
| attention | B * Q * ceil(H/G) / 32G | One CTA owns batch/query/head group, independent from other CTAs; shared raw slots/decoded tile retain Q6 layout |
| finalize | 1 / 1 | Existing value projection, statistics and all four cache/scale result copies |

All three launches must use this order in one stream, with the same disjoint
buffers and pinned generations. The manifest records that obligation. Attention
and finalize uniformly return on nonzero source status. Prepare checks all
source/page guards before any stage starts; no error return leaves transfers
pending. Preparation and finalization are deliberately serial initial mappings;
Q6c does not claim they are throughput optimized.

For attention, block x decomposes in query, head-group and batch order. Decode
token IVs start at thread x and advance by the selected block width. Head IVs
also start at thread x and stride by block width, with the original head-tail
guard. Thus one lane owns each head's three ordered token folds; inner dot
products, global maximum, denominator and weighted-latent arithmetic preserve
source F32 order. With the current G=4 fixtures, only four lanes own head folds;
other lanes assist decoding and still participate in every CTA collective.
No warp reduction, online softmax, token split or reassociation is inferred.
Physical run construction is currently redundantly evaluated per lane; only
lane zero performs transfer effects. These are explicit performance limitations.

## Concrete instruction protocol

Q6b's abstract scope operations are replaced in native WHIRL. The new private
CALL declarations carry `tensor.q6c.primitive=sm90-byte-ops-v1`, explicit argument
TYs, void results and conservative normal call effects. CUDA headers and native
CG only spell these selected effects:

| Concrete suffix, prefix `__open64_q6c_` | Operands | Device meaning |
| --- | --- | --- |
| init | shared barrier pointer | mbarrier.init, pending/expected arrival count one, phase zero |
| proxy | none | fence.proxy.async.shared::cta |
| expect | shared barrier pointer, U4 bytes | mbarrier.expect_tx.relaxed.cta.shared::cta.b64 |
| bulk | shared destination, global source, U4 bytes, shared barrier | cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes |
| arrive | shared barrier pointer | mbarrier.arrive.release.cta.shared::cta.b64 |
| wait | shared barrier pointer, U4 phase | loop on mbarrier.try_wait.parity.acquire.cta.shared::cta.b64 |
| cta_sync | none | full CTA __syncthreads / bar.sync 0 |
| invalidate | shared barrier pointer | mbarrier.inval after all transfers and consumers drain |

This is byte-addressed bulk asynchronous movement, not descriptor-based 2-D
`cp.async.bulk.tensor` or a new tensor-map ABI. It accepts paged contiguous runs
without inventing a tensor-map descriptor. The current explicit compilation
envelope remains sm_90, PTX >=8.6 and CUDA >=12.8. The native PTX text uses 8.6.
These are conservative supported toolchain gates, not instruction minima.

For each exact Q6b run and each of its four streams, native scalar statements
inspect actual source/destination address residues modulo 16. Equal residues
admit a synchronous alignment head, a multiple-of-16 bulk middle and a
synchronous tail. Unequal residues produce only synchronous byte copies. Every
range remains inside the original run; no page discontinuity is crossed and
padding is never read. All casts to U4 are bounded by the admitted <=48 KiB raw
layout. Source pointer alignment is not assumed from Q3 host buffers.

Only bulk middles increment the hardware expect count, before each issue.
Logical four-stream byte totals from Q6b acquire are not used as hardware
transaction counts. Arrival occurs once at the selected tile's wait, after all
issues and synchronous fragments. Each slot has a producer-local parity
register; it toggles after successful wait, including zero-bulk tiles, and
persists across all three passes. A zero-bulk phase completes through arrival
alone. An initialized but unused second slot can drain/invalidate without a
spurious wait. Producer-only branches contain no CTA barriers.

Barrier initialization is proxy-published and followed by a full CTA barrier.
Each async acquire also emits a producer proxy fence after prior consumer
completion, ordering prior generic stores before reuse by the async proxy.
After producer arrive/wait, a CTA barrier publishes completed raw data and
synchronous fragments to decoding lanes. Another CTA barrier publishes all
decoded writes before head consumers. A consumer CTA barrier completes every
head before decoded/raw reuse. Final barriers surround producer invalidation.
Padded head lanes and decode tails reach every collective. Two raw slots permit
next-tile issue before current decode/compute; this is structural overlap, not
a measurement of hidden latency or elimination of pipeline bubbles.

The instruction semantics follow NVIDIA's [PTX ISA 8.1 documentation](https://docs.nvidia.com/cuda/archive/12.1.1/parallel-thread-execution/index.html),
sections on cp.async.bulk, mbarrier expect/arrive/wait, proxy fences and barriers.
The shared-to-cluster spelling targets the issuing CTA's shared destination;
there is no multicast, remote-CTA consumer or cluster launch requirement here.

## Identity, invalidation, comments and failure

Before any mutation, admission requires the exact Q6b native seal, canonical
control, Q5 selected source/descriptor, Q3 binding, complete layout and no live
dependence image. O3 is explicit. Stale, baseline, unsupported-resource and
already mapped inputs refuse without alternate-plan construction. A driver
mutation epoch expires before application; old Q1/Q5 analysis facts never
transfer. No PREOPT round is inserted into this realization. Normal backend
source emission must verify the sealed Low tree before any ABI rewrite.

Each output preserves the original Q3 binding, Q5 descriptor, Q6 control and
cache tensor traits. Its Q6c key additionally binds stage/level, original Q6b
key, explanation, launch/resource records, complete shared inventory, concrete
operation prototypes/versions and coordinate declarations. Intermediate High
and Mid have two explicit scalar owner formals; physical Low has exactly the
original pointer ABI. Device-symbol attributes retired in serial stages become
UNKNOWN records, not illegal DEVICE_SYMBOL_NONE attribute payloads.

One native COMMENT explains the actual launch stage, ownership, transfers,
phase/count rules, publication, consumer synchronization and remaining device
boundary. The original Q6b COMMENT is retained as historical logical context.
whirl2c and native PTX retain both. General PREOPT COMMENT repair stays separate.
CUDA printing additionally preserves restrict as C++ __restrict__ and explicit
F32 truncation as the corresponding round-toward-zero conversion intrinsic;
these spell existing native semantics without introducing scheduling policy.

Construction failure abandons its compiler process. Before/High/Mid checkpoints
may remain as diagnostics; no failed family gets a completion manifest. Test
injection after memory lowering verifies no Low/PTX/manifest publication. Files
are fresh/no-replace; the JSON manifest is last. A later I/O failure can leave
a diagnostic checkpoint or incomplete text; it is not an accepted stage. The
three stage manifests must agree on exact source/Q3/layout identities before
a launch bundle may be considered. This is not an in-process rollback claim.

## Qualification and reproducibility

The focused Q3 review option is `--q6c --cuda /cuda`, alongside `--q6b`. It
finishes the existing estimation-only Q5 study before constructing the selected
W/G. Explicit sync/async fixtures qualify transport behavior; neither timing
nor reference results feed exploration. It lowers the three native stages,
reopens each High/Mid/Low image, retains `ir_b2a -st -src` full traces and raw
diffs, emits CUDA from Low, compiles with nvcc, assembles native PTX with ptxas
and device-links the three native objects with explicit exp/log math wrappers.

The emitted CUDA is also compiled against a test-only CTA model. All physical
lanes execute; barriers diagnose divergence. Bulk copies are poisoned and
completed only at matching phase waits, in reverse issue order. Forward and
reverse lane/CTA launches compare all six outputs exactly with the selected
source. Eight-byte shifted buffers test real address-residue fallback while
preserving scalar alignment. Canaries, immutable inputs, source status guards,
S8/U4, paged/append, nonfinite masks, head/token tails, zero active queries and
actual 8193-token fixtures remain covered. Negative native mutations remove
wait/bulk/consumer collective or alter binding; both reopened audit and CUDA
admission refuse stale products. Host simulation is serialized per review
library because its static shared arena is a test instrument, not a runtime.

Fresh Q3 source/snapshot leases and atomic host publication are exercised by the
producer suite. Closed-packet replay uses frozen inputs only for reproduction;
recorded Q3 identity strings are never reused as live leases. Required evidence
includes full before/High/Mid/Low .B/.T, raw diffs, launch manifests, annotated
.cu, native .ptx, CUDA objects, assembled native objects/cubin, exact commands,
reference results, compiler source hashes, runtime closure and checksums.

Example single-stage command sequence, repeated in isolated processes for the
three stages (this does not launch or select a prefill/decode version):

```sh
tensor_selected_bridge q6-device staged.after.B attention attention --opt-level=3
tensor_selected_bridge q6-device-audit attention.low.B low attention.keys.json
ir_b2a -st -src attention.low.B attention.low.T
whirl2c -cuda -TARG:abi=n64 -O0 -ft,attention.w2c.phase.t \
  -CLIST:dotc_file=attention.cu -CLIST:doth_file=attention.h attention.low.B
nvcc -arch=sm_90 --fmad=false --ftz=false --prec-div=true --prec-sqrt=true \
  -I/path/to/osprey/include -c attention.cu -o attention.cuda.o
ptxas -arch=sm_90 -c attention.ptx -o attention.ptx.o
```

## Remaining gates and risks

Q6d must bind real device allocations, bounds, page/index maps, cache/scale
versions, stream/event readiness, completion, launch/resource checks and atomic
four-buffer publication. Copying a Q3 host snapshot is insufficient. No GPU
launch ABI, runtime dispatcher, phase selector or AOT selection is introduced.
The manifest denies GPU execution eligibility until that authority is present.

Q7 requires actual NVIDIA execution, race/memory checks, numerical acceptance
(including device exp/log differences), resource reports and performance
measurements. Offline CUDA/PTX compile/link and host-model equality do not prove
GPU correctness, TMA throughput or speedup. No NVIDIA host is available.

The original 48 KiB envelope remains. W32/R512/Ro64 already needs 72 KiB for the
decoded tile alone and stays refused; a reviewed opt-in or finer layout is
required. Prefix/tail serialization, few active head lanes, three source-ordered
passes, fragmented paged runs, producer byte copies, barriers, register pressure
and startup/drain can dominate performance. Complete pipeline/resource costing
must precede automatic comparison with Q5; Q5's synchronous winner is not a
certificate that this async mapping is optimal.

Primary owner, successor, independent reviewer and approving lead remain TBD.
Those assignments still block knowledge-transfer acceptance, not implementation
or preservation of this review package.
