# MLA quantized/paged cache and TMA design v1.0

2026-11-12 architecture terminology correction:
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
governs opcode properties, tensor traits, phase legality and scheduling
services. Embedded earlier revisions include these editorial corrections;
historical implementation status and acceptance evidence retain their original
scope.

November 9, 2026 | Design specification; implementation and hardware acceptance open

## 1. Authority, baseline and scope

This specification extends the [movement/reuse design v1.1](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.1.md)
and [implementation plan v1.12](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.12.md).
MR1 `f4fb3a7b` supplies owned storage/access/version/lifetime proofs. MR2
`68bf4369` supplies the opt-in hierarchy/reuse model, 12 implemented G1 entries
and 24 analysis-only grouped proposals. Their compiler/host evidence remains
historical evidence for those exact products, not acceptance of this extension.

The current MLA lane requires FP32 dense cache inputs and one query position.
Its short/long fixtures vary cache length; neither is a general prefill test.
Quantized representations, page-table addressing, serving-phase certification,
TMA and dequantization pipelines are not implemented by this document.
Current hardware characterization remains unavailable without an NVIDIA host.
Primary owner, successor, independent reviewer and approving lead remain TBD.

The objective stays estimated whole-call latency with selected-only correctness
and separate hardware acceptance. A KV-cache role prioritizes reuse/movement
analysis; it does not certify an HBM bottleneck. Quantization, query/head reuse,
warm-cache service or conversion cost can change the limiting resource.

The initial proposed scope is active-PU inference attention with static or
guarded dimensions, an explicitly admitted quantized representation, read-only
cache versions during each selected kernel, a paged or dense storage contract,
and synchronous or separately qualified asynchronous CTA-local staging.
Cross-request sharing, cross-PU rewrites, cluster multicast, cache eviction,
cache compaction and new Tensor Core precision are separate capabilities.

## 2. Separate semantic, representation and invocation facts

| Fact class | Required contents and owner |
| --- | --- |
| Cache role | Latent KV / rotary key, logical axes, producer/consumer lineage, evidence provenance; semantic tensor descriptor and owning-phase proof |
| Quantized representation | Storage encoding and packing, logical versus physical shape, bit order/sign, quantization formula, grouping axes/extents, scale/zero-point encoding, rounding/saturation/nonfinite semantics and compute encoding |
| Storage/view/effect | Allocation and view identities, offsets/strides, physical page layout, alias answers, valid regions, cache/metadata versions and access lifetime; LNO owned snapshot and serving ABI |
| Workload | Per-request query length, existing and resulting KV lengths, positions, causal/window masks, append/read ordering, request segmentation, phase evidence and runtime guards |
| Physical plan | G, query/token/column tiles, page segmentation, movement mechanism, raw/decoded slots, conversion placement, producer/consumer owners, readiness/release graph and resource profile |

Use the established tensor trait/descriptor carrier and policy-free common IR
records. Static semantic/encoding properties may participate in type identity
where existing canonical descriptor rules require it. Runtime addresses, scale
values, page IDs, valid lengths, request IDs and serving-phase state must not
be interned into canonical tensor types. Access/version/lifetime facts remain
separate even when two tensors share the same type.

The proposed fact vocabulary is a design DTO, not a released opcode, binary
row, accepted JSON field or new numerical permission. Q0 must publish exact
versioned schemas after a representative source census. Use WHIRL
opcode/version contracts, TensorDescriptorIR and tensor traits, with the
existing native gatekeepers and phase-owned legality checks. Preserve the
existing be.so dependency boundary.

## 3. Quantized-cache inference without source declarations

A source declaration is optional when the owning phase proves the entire
relevant graph and its storage/effect joins. The proof chain is:

    key/latent producer -> quantize -> packed cache allocation/update
        -> cache read -> unpack/dequantize -> attention contractions

Recognize the latent operand's role in both absorbed score and weighted-latent
contractions, and rotary-key use in the corresponding score contribution.
Use logical operators, tensor descriptors, dataflow and actual accesses.
A variable name, integer dtype, equal shape, a quantize node alone, or a cache
annotation alone is insufficient. Explicit declarations must agree with the
proved graph; contradictions refuse optimization rather than override evidence.

For an illustrative affine quantizer, preserve the source meaning:

    q = clamp(round(x / scale) + zero_point, q_min, q_max)
    x_hat = source_compute_convert(scale * (decode(q) - zero_point))

This is not a universal FP8 or block-floating definition. FP8 formats, integer
quantizers, codebooks and packed sub-byte formats require distinct schemas.
Record group axes, group boundaries, scale lookup and the exact conversion
order. Invalid, zero, negative or nonfinite scales retain the source's defined
behavior or require an explicit admission guard; do not silently replace them. Runtime scale/zero-point buffers are explicit operands with storage,
version, extent, encoding, readiness and alias/effect facts. Bit packing and
padding are physical storage semantics, not new logical tensor elements.

Do not fold away quantize/dequantize provenance before native contracts record
the representation and lineage. Distinguish real packed storage from fake
quantization that still stores FP32; only actual storage layout changes can
reduce global byte estimates. A packed read that feeds a verified dequantizer
can establish an incoming representation without seeing the producer, provided
the ABI proves its format, scale mapping and validity. It cannot invent missing
state persistence or update history.

Opaque calls, erased dequantization provenance, unsupported encodings, unknown
scale aliasing or ambiguous consumers leave the role/representation unproved.
Use a declared and verified caller contract when available; coordinated
cross-PU inference belongs to explicitly enabled IPA, not a PU-local traversal.
Missing information does not become a guessed INT8/FP8 format.

Each derived role is labeled declared-and-verified, graph-derived or
ABI-derived, with stable supporting value/access/effect identities. A known
representation with an unknown role remains distinct from a known cache role
with an unknown representation. Refusal publishes no partially proved family.

## 4. Prefill/decode detection and workload specialization

Serving phase and attention geometry are related, distinct facts. The phase
record is per request/segment, with states prefill, decode, speculative-verify,
mixed and unknown. A trusted versioned serving contract can provide the label;
it must remain consistent with lengths, positions and effects. Structural
analysis may derive a more limited fact such as single-query attention over
existing history. It must not upgrade that fact into an engine-phase certificate.

Neither cache dtype nor cache length determines phase. Q=1 also occurs in a
prefill tail; Q>1 can occur in speculative verification. Prefix reuse may give
prefill an existing history, and chunked prefill can share a batch with decode.
The [vLLM chunked-prefill documentation](https://docs.vllm.ai/en/v0.23.0/configuration/optimization/)
provides a concrete mixed-batch example. Long-context and short-context are
orthogonal to prefill/decode.

Retain query count per request, history/valid cache lengths, absolute query/key
positions, causal/window rules, append extent and ordering, and segment offsets.
Packed/ragged batches must not substitute total batch tokens for per-request Q.
Zero-query/zero-work segments need an explicit source-compatible no-work path;
they must not issue empty transfers or leave a barrier awaiting nonexistent work.
For a known append contract, check resulting length = prior length + admitted
append extent, subject to separately admitted sliding-window/eviction semantics.

Kernel dispatch may use proved/guarded geometry without a phase label, if the
chosen implementation is correct for every admitted invocation. AOT guards
select only already compiled, qualified paths. They do not start a runtime
search or JIT. Cache updates ordered before the call do not inherently require
recompilation. Runtime guards and dispatch/setup costs belong in the full-call
accounting. Requests with incompatible geometry require a qualified mixed
kernel or explicit segmentation/launches; uniform collectives cannot diverge
on individual request labels inside a CTA.

A decode-shaped MLA kernel is not automatically a prefill implementation.
Adding Q>1 must capture a new source census, prove attention masks/positions and
the per-query softmax/tuple domain, and qualify its materializer. Query-axis
reuse may be proposed only after proving identical cache versions and valid
read-domain overlap; it is not a multiplication of head reuse by a label.

## 5. Quantization versions, updates and read-domain safety

MR1 incoming/defined symbolic versions extend to packed bytes, scales, zero
points, page mappings and any decoding tables. Consumers must observe one
consistent published bundle. A scale update can change interpretation of old
bytes even if the packed array does not change. An append that requantizes an
entire group may rewrite earlier tokens; it is not a read-only prefix append.

The serving ABI must order publication before reads and prohibit overwrite,
page remapping or reclamation until all asynchronous and arithmetic consumers
finish. Shared prefix pages are immutable during reads or use an explicitly
versioned copy-on-write protocol. Pointer equality or a recycled physical page
ID is insufficient: allocation generations prevent stale/ABA reuse. No content
hash of runtime cache values is required or implied.

Prove physical load bounds independently of logical masks. Loading a tile for
one valid head/query does not permit a masked neighbor to consume a NaN.
Invalid head/token/column participants remain neutral through uniform
collectives. Quantized padding must follow its format and valid-domain contract:
a packed zero byte need not dequantize to mathematical zero. Do not read past
allocated bytes merely because TMA or a later mask can discard the result.

## 6. Paged-cache layout and alignment contract

The serving engine owns allocation and its legal page-size set. The compiler
consumes or recommends a compatible layout before allocation; it does not
silently resize/reinterpret a live cache. Bind page ABI/configuration version,
tokens per page P, pool/page capacity, packed row strides and offsets,
scale/zero-point layout, alignment guarantees, page-table encoding, logical to
physical mapping, valid tail counts, sharing and publication/reclamation rules.
Keep latent and rotary allocations separate unless the source ABI admits a
joint physical layout. A compiler hint is not an allocator guarantee.

P is an allocation granularity; W is a kernel token tile. They need not be
equal. A tile can fit within a page, use part of a page, or require multiple
page-local segments. For token t, an admitted mapping has the conceptual form:

    logical_page = floor(t/P)
    page_offset = t % P
    physical_page = page_table[request, logical_page]
    address = admitted_pool_base + physical_page*page_stride
              + page_offset*row_stride + column_byte_offset

Do not substitute logical_page for physical_page or assume adjacent logical
pages are physically adjacent. Check page-table bounds, physical page capacity,
valid final-page extent and the matching metadata version. Distinguish affine
page-local offsets from the indirect page-table mapping. The existing affine
LNO provider does not thereby prove arbitrary pages disjoint: extend canonical
admission and owned segment/alias facts from actual native operations and ABI
evidence, or retain an unknown result and compatible fallback. A regular contiguous
page pool may be described by a tensor map with a physical-page dimension.
Arbitrary pointer lists or gathers require page-local copies, another qualified
mechanism, or fallback. Descriptor construction/update visibility and lifetime
are explicit ABI obligations; the tensor map remains opaque.

For a fixed physical row stride r, separately stored metadata and alignment A:

    page_bytes(P) = P*r
    aligned page stride requires P*r mod A = 0
    P must be a multiple of A/gcd(A,r), when no additional page padding is used

Also require an aligned page base. With interleaved metadata or padded page
strides, apply the actual byte-layout function instead of this simplified
equation. For example r=288 and A=128 permit P=16 and P=32; neither follows as
optimal from alignment. NVIDIA describes 32-byte sectors and 128-byte cache
lines in its [profiling guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/).
Those granularities guide access accounting, not a universal serving page size.

Choose from engine-supported P using fragmentation, prefix-sharing granularity,
allocation/page-table overhead, quantization-group mapping, transfer geometry,
tails, residency and full-call latency. A scale group need be contained in a
page only if its schema requires it; a cross-page group needs an explicit
versioned scale mapping. Align each auxiliary allocation independently.

## 7. Hopper TMA and dequantization pipeline

TMA is a transport mechanism, not a general quantizer/dequantizer. The initial
proposal copies admitted packed bytes and metadata, then uses SM instructions
to unpack, convert and apply scales/zero points. Conversion can publish a decoded
SMEM tile reused by several heads/queries, or decode into registers when that
qualified plan is cheaper. It must preserve the source compute encoding/order;
TMA does not authorize narrowing FP32 arithmetic or introducing Tensor Cores.
See the [Hopper tuning guide](https://docs.nvidia.com/cuda/hopper-tuning-guide/).

Target admission must distinguish one-dimensional bulk copy from tensor-map
copy. For the pinned Hopper-era API, global bases and transfer sizes need
16-byte alignment/granularity; multidimensional nontrivial strides are multiples
of 16 bytes and tensor-copy SMEM bases require 128-byte alignment. A 1D bulk
SMEM destination instead has a 16-byte requirement. Selected swizzle modes can
impose stronger constraints. Verify the chosen instruction, toolkit/PTX target,
tensor-map fields, box bounds, alignment and out-of-bounds behavior rather than
generalize these example requirements to every future architecture.
[CUDA 12.3 TMA documentation](https://docs.nvidia.com/cuda/archive/12.3.2/cuda-c-programming-guide/index.html#asynchronous-data-copies-using-the-tensor-memory-accelerator-tma)

The selected pipeline has explicit raw and decoded slot states:

    raw slot: free -> copy_in_flight -> raw_ready -> converting -> free
    decoded slot: free -> being_written -> decoded_ready -> in_use -> free

Each slot has an owner, generation, barrier scope/phase and dependent events.
An illustrative steady state overlaps copy(i+1) with dequantize/consume(i);
a three-stage schedule may overlap copy(i+2), dequantize(i+1) and consume(i).
Stage depth and prefetch distance are selected parameters, not unconditional
double buffering. One-slot synchronous and direct paths remain candidates.

The correctness obligations are:

1. Resolve and validate the physical segment, packed format and matching
   scale/zero-point versions before issue. Elect legal producer threads.
2. Initialize/arrive on the proper completion barrier, account for all expected
   transaction bytes, and wait for the matching copy generation.
3. Convert only after every required packed/auxiliary byte is ready. Copy
   completion is not conversion completion.
4. Publish decoded values to all intended consumers with the required memory
   visibility. Use target-required proxy ordering when an async consumer is
   involved; a generic CTA barrier is not a substitute for every proxy rule.
5. Keep the raw slot live through conversion and the decoded slot live through
   every head/query read and any asynchronous arithmetic completion.
6. Release before refill, with phase tracking that prevents stale arrivals,
   premature overwrite, deadlock or page reclamation while DMA reads continue.

A source-level early return for a masked/tail head must not strand another
participant at a barrier. Prologue, short loops, drain and tail segments require
their own verified paths. No issuing thread, arrival count or buffer lifetime
may be inferred later by a CUDA printer.

Use the existing resource/lifetime/readiness planning services. For raw row
bytes r_q, decoded row bytes r_d, token tile W and raw/decoded slot counts N_r,N_d:

    S_CTA = separately_rounded(N_r*W*r_q, N_d*W*r_d,
                              scale/zero_point slots, barriers,
                              head/query statistics, accumulators,
                              swizzle/padding and other live objects)

Actual packing requires nonoverlapping proven lifetimes, not just this sum.
Reserve all statically allocated arrays and inactive participants. Initially
retain the 48-KiB static-SMEM launch limit. Larger dynamic allocations require
a separately versioned launch/resource contract and qualification.

Dequantization occupies SM execution/register resources. TMA can hide latency
only when the dependency/resource schedule has enough independent work and
bytes in flight. A decoded SMEM tile may cost more capacity/bandwidth than a
packed tile; register decode may repeat conversion across heads. Compare both
only when their rules and numerical contracts are admitted. No stall-free or
bandwidth-bound conclusion follows from TMA availability.

## 8. Extended costing and two-phase search

Preserve MR2's released synchronous profile/census and historical scores. New
quantized/paged/async estimates require new exact profile, fact, descriptor,
report, control-space, recipe/selected-plan and capability versions. Q0/Q4 must
publish the schema table before parsers accept it. This document does not make
old v3 bindings accept extra fields or enable TMA via a profile string.

Phase 1 copies admitted facts once and explores detached descriptors only.
Bind source/producer/access/alias/effect/numerical identities, quantization and
scale mapping, page/layout ABI, workload guards, slot/event graph, target and
resource/model/scenario/catalog/capability/control identities. Distinguish
compile-time structural keys from invocation-time validated addresses/page IDs/
valid lengths and publication tokens. A legal runtime page selection satisfying
the ABI/guards is not a new JIT schedule; a changed format/layout ABI invalidates
the relevant plan or chooses a precompiled compatible path.

Model separately packed useful/requested/sector/L2/HBM bytes, scale/zero-point
and page-table traffic, decoded SMEM/register traffic, repeated conversion,
page segmentation, tensor-map setup/update, DMA issue/wait, publication/release,
resource rounding, warp roles, occupancy, prologue/drain, tails, dispatch,
launches and unchanged per-head/query tuples/merges. Reuse reduces conversion
only when the decoded tile actually survives across its consumers. Page size
changes may affect engine memory capacity/fragmentation as well as kernel time.

Use the existing readiness event graph and checked arithmetic. The optimistic
independent-stage throughput max(T_copy,T_dequant,T_consume) is not a latency
prediction: shared SM issue resources may serialize dequantization and attention.
Charge synchronous edges additively, and overlap only independent work with
proven slots/completions. Never discount the same service twice. Fixed cache/
contention scenarios and provenance/uncertainty apply equally to all candidates.
Unknown required rates or layouts disable that model/family; they are not zero.

Bound the catalog rather than form an unbounded Cartesian product of
P/G/query/W/column/format/slot/layout/prefetch choices. Page P is an engine
configuration input during an invocation; only a separate offline co-design
experiment may compare legal P values. The active search uses one bound ABI.
Charge the compatible synchronous quantized baseline first under the same new
model. Global proposal/unique/time budgets, shared estimate cache, deterministic
ties and scoped conclusions remain. Unimplemented materializers cannot win.
Reprove any changed convergence certificate or refuse it while retaining hard
budgets; MR2's unsupported relative-gap/economic certificates remain disabled.

Phase 2 revalidates one eligible winner, applies one atomic High plan, rebuilds
analyses, checks the High exit and lowers it through fixed Mid/Low/PTX/CUDA.
Run selected-only correctness/resource acceptance outside exploration. Rule
development tests and offline fixed-probe calibration are separate activities,
not hidden candidate compilation or benchmarking. No runtime adaptive search
or JIT is required.

## 9. Phase ownership, native High exit and invalidation

| Boundary | Required responsibility |
| --- | --- |
| Source ingestion / Very High | Capture real quantization, scale operands, cache state and page/gather/update census; preserve domain semantics and diagnostics through existing native APIs |
| Common High / LNO | Derive owned roles/encoding/phase-geometry/storage/version/read-domain proofs; reuse canonical DO_LOOP/OPR_ARRAY, alias/dependence/epoch and readiness services; search detached complete plans |
| Selected High exit | Explicit page lookup and valid segment bounds, packed/auxiliary transfers, conversion DO_LOOPs, producer/consumer ownership, slot state/readiness/release and source numerical guards |
| Mid | Structured address/control/memory lowering without changing page resolution, quantization or event order |
| Low | Fixed GPU thread/warp/CTA mapping and address-space/launch ABI; bind legal target copy/barrier/conversion operations |
| PTX / whirl2c | Verify and emit the fixed semantics and matching explanations; no new tiling, page policy, precision choice or pipeline repair |

Review normative WHIRL rules and existing operator/intrinsic contracts before
publishing any representation extension. This design reserves no opcode enum,
WN layout, managed-table section or binary image change. Generic schema and
structural verification can live in common/com; inference, costing, legality
and mutation stay with their owning phase. Reuse the existing prefill/decode
contracts where applicable, but do not pretend the current rank-4 BHSD
`transformer.kv_cache_state` profile already covers head-shared MLA pages.

High-exit verification requires complete effects and target obligations,
bounds/read-domain safety, version-consistent auxiliaries, all raw/decoded
lifetimes, uniform collectives, completion-before-use and release-before-refill,
rounded resources/launch limits, precision and tuple coherence, and supported
native operations. Missing representation or backend coverage refuses before
publication. Printers cannot substitute comments or metadata for executable
copies, dequantization, waits and releases.

Source/effect/alias/view/format/scale mapping/page ABI/geometry or plan changes
invalidate the affected proof and estimate keys. Dynamic page-table, cache and
scale publication generations are runtime guard/event obligations. Before
selected mutation, retire old compile-time analysis epochs; after mutation,
rebuild parent/DU/alias/access/dependence/reduction facts and seal the result.
Mid/Low retire prior-level keys. M6's original-input equality seal must cover all
new fixed obligations and reject deletion/rebinding as well as changed values.

## 10. Correctness oracles, fallbacks and evidence

The first oracle is the actual source quantized computation with its original
encodings, packing, scales, rounding, saturation and arithmetic order or already
authorized reassociation. Test unpack/dequantization separately before attention.
Require exact byte/bit results where the source contract specifies them; retain
existing attention tolerances for the corresponding compute/reduction contract.
A comparison against an unquantized model measures source quantization quality
separately and cannot excuse compiler error or silently enlarge tolerances.

Test decode, prefill, chunked prefill, Q=1 prefill tails, speculative Q>1,
prefix reuse and mixed/ragged batches, including unknown/contradictory phase
metadata. Test source declarations present/absent/contradictory, real versus
fake quantization, opaque/erased producers, signed/sub-byte extremes, FP8
nonfinites as supported, group/tile/page tails, scale updates, quant-group
requantization, aliases, pointer/page ABA and publication order.

Test nonmonotonic physical pages, partial/empty pages, incompatible strides,
separate auxiliary layout/alignment, invalid table entries, page sharing/COW,
stale mappings, head/query isolation, masked poisoned padding and all-empty
softmax. Exercise raw-ready versus decoded-ready, repeated ring reuse,
barrier phases/arrival/byte counts, prologue/drain, early-return deadlock,
async consumer completion and overwrite/reclamation faults. Include canaries,
immutable inputs, resource refusal, stale selected binding and transaction
rollback. Hardware qualification needs selected-kernel correctness plus
race/memory sanitizer and paired whole-call performance on the declared GPU.

Fallback is representation-aware. Prefer a verified synchronous quantized/paged
implementation with the same ABI. Otherwise use the source-equivalent generic
lowering or serving-engine fallback. Explicit dequantize/gather to a dense
fallback is legal only when its numerical, allocation, lifetime and cost
contract is admitted. The existing FP32 G1 path accepts FP32-compatible inputs;
it must never receive raw packed bytes. If no compatible implementation exists,
issue a stable unsupported diagnostic rather than publish a false success.
An unknown phase may use a geometry-compatible qualified path; a mislabeled
phase cannot bypass its guards. Selected failure never starts an unbounded
next-candidate correctness loop.

Generated comments in optimized CUDA, WHIRL COMMENT, whirl2c and PTX explain
inference provenance, geometry/phase evidence, quantization/group/scale mapping,
P versus W, physical page segments, copy mechanism, conversion location, raw/
decoded slots, readiness/release/proxy ordering, real versus padded consumers,
remaining traffic and resources. Keep the current PREOPT COMMENT workaround.

Retain original and selected before/after B/T with ir_b2a -st -src, raw diffs,
typed facts, model/capability/control ledgers, launch/completion manifests,
native and readable sources, exact commands, independent oracles, rejection/
rollback logs, and separately labeled hardware/calibration results. Immutable
receipts bind producers and all relevant inputs. Host or compiler-only evidence
does not certify asynchronous GPU behavior.

## 11. Implementation and transferable package

Q0-Q7 in [plan v1.12](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.12.md)
define the implementation sequence and evidence gates. Existing MR3/MR4 and
M7-M9 are not renumbered or closed. CPU-only infrastructure can progress now;
GPU correctness/performance remains an explicit acceptance gate. An async rule
without the required qualification remains analysis-only.

A transfer package must contain this specification and plan, versioned schema/
capability matrix, source census and mapped images, inference/guard examples,
quantized reference semantics and tests, proof/event/resource ledgers,
before/after native/source explanations, exact reproduction commands and
toolchain/profile identities, fallback/refusal/rollback evidence, measured
results or explicit unavailable status, unresolved risks and assigned handoff
roles. Current roles remain TBD; documentation completion is not personnel or
hardware transfer acceptance.

## 12. Principal risks and decisions

| Risk | Required decision or mitigation |
| --- | --- |
| Type/length heuristic mislabels phase | Keep per-request geometry and unknown/mixed; trusted phase metadata cannot override contradictory facts |
| Quantization role or representation guessed | Preserve graph/ABI lineage, distinguish fake storage, refuse incomplete proof |
| Scale update or page reuse changes old values | Bundle publication/version/generation, no-alias read lifetime, COW/reclamation protocol |
| Aligned page size wastes capacity or prefix reuse | Engine-supported layout co-design, separate P/W, fragmentation and end-to-end objective |
| TMA tile is an irregular page gather | Resolve page-local segments or use a qualified alternate transfer |
| Conversion and expanded SMEM dominate | Joint resource/readiness cost; compare packed-register and decoded-shared paths without free overlap |
| Buffer/barrier protocol races or deadlocks | Explicit generations/events, uniform participant proofs, selected device tests and sanitizers |
| Metadata claims outrun executable WHIRL | Native High-exit verification, fixed downstream lowering and sealed emitter admission |
| No hardware/calibration or missing baseline | Keep family analysis-only or retain compatible accepted fallback; no speedup claim |

Primary sources describe hardware/serving behavior; the contracts, equations
and implementation gates above are Open64 design requirements, not claims that
the current compiler or cited systems already implement this exact pipeline.
