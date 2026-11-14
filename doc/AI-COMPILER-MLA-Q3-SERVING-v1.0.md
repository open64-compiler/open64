# Q3 MLA serving buffer, mapping and publication binding v1.0

November 10, 2026 | Native synchronous host adapter

This revision implements the binding boundary left open by [Q2](AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md). It supplements [Q1 production](AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md), [the Q0 serving semantics](AI-COMPILER-MLA-Q0-SERVING-CONTRACT-v1.0.md), [the quantized/paged design](AI-COMPILER-MLA-QUANTIZED-PAGED-CACHE-AND-TMA-v1.0.md) and [plan v1.18](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.18.md). The new [phase-specialized AOT direction](AI-COMPILER-MLA-PHASE-SPECIALIZATION-v0.1.md) is adopted; its selection mechanism remains explicitly deferred. No compiler phase option or dispatcher is implemented here.

## Implemented scope and trust boundary

Q3 provides a native adapter for real borrowed **host-synchronous** allocations and the existing Q1 disjoint-storage/status entry ABI. The adapter registers actual address spans, issues allocation identities/generations and content versions, reads workload and page controls from registered memory, pins inputs and reserves writable buffers, invokes one qualified synchronous entry, and atomically publishes the latent-cache/latent-scale/rotary-cache/rotary-scale successor tuple only after completion and status zero.

This goes beyond Q2's conditional JSON witness: the masks, page table, token indices, update slots and workload metadata are read from actual registered buffers, and lifetime/version checks consult private live registry state. No caller-provided generation integer proves freshness. Known phase still originates with the serving owner's declaration in its metadata buffer; code analysis cannot discover the intent of indistinguishable graphs. The adapter validates that declaration against the actual invocation. Unknown phase remains usable by the admitted generic source route.

The memory owner must supply accessible host storage, retain it until successful retirement, use the registry for mutation/reclamation, and call only a qualified source-compatible synchronous entry. Raw C pointers cannot prevent an uncooperative owner from freeing or modifying memory behind the registry. The adapter re-observes control bytes around execution but deliberately does not hash cache payloads. The initial `Import` is an explicit owner assertion that all four incoming components are already ready and semantically paired; Q3 cannot reconstruct an external producer's history or quantizer rounding from bytes alone. All successor publications are then tracked by the adapter itself.

Device/opaque memory, asynchronous callbacks, GPU stream/event ordering, vendor serving-engine integration, arbitrary pointer-list pages, in-place updates and external writer protocols are not admitted. This milestone does not certify Hopper execution or TMA. A host address's measured alignment is not a GPU alignment guarantee. Generic packed inputs retain their actual representation; failure never redirects packed bytes to an FP32 ABI.

## Source-to-serving ABI discovery

`tensor_serving_schema.h/.cxx` is a pure VHO service over the owned Q1 program. It reuses Q2's closed attention recognition and Q1's operator/type/attribute/storage admission. Discovery derives storage-root argument order, query/mask identities, returned cache/scale copy joins, incoming encoded pools and scale roots, and the actual source page/index formula. It does not trust model names, positional guesses or a standalone dtype label.

For paged sources, the service verifies the connected gather/divide/remainder/multiply/add/reshape chain and all cache/scale index joins. It derives P from the actual pool shape and source integer constants, checks [Nphysical,P,Cstore] and [Nphysical,P,Rstore] pools, matching scale shapes/types and successor representation, [B,ceil(L/P)] int64 table, [L] int64 token enumeration and, for updates, [B,A] int64 slots. Every pool/scale gather must use the same source slot computation. Unsupported or contradictory schemas refuse only this adapter; the ordinary compiler route remains unchanged. Dense construction, including fake quantization with float storage, has no incoming encoded bundle and receives no compression credit.

The standalone `q3-schema image.B schema.json` command independently reopens genuine High WHIRL and exports the native descriptor plus canonical Q1 program metadata. Repeated discovery checks complete native identity and ST/TY counts. It does not import a JSON allocation manifest as authority. Source/High ABI meaning remains unchanged; no WN/ST/TY layout, physical opcode, type-kind, managed image table or reader/writer format changes.

The engine supplies its legal page-size set when constructing the adapter. The derived source P must belong to it; a disagreement refuses before a callback. Q3 never changes the live allocation or equates allocator P with compute chunk width W. v1 supports the exact contiguous source layout; padded/noncontiguous strides and arbitrary page pointer lists need a separately versioned adapter.

## Registered buffers and actual workload metadata

`tensor_serving.h/.cxx` implements `VHO_TENSOR_SERVING`. Each registry owns one source/cache timeline. `Register` receives an actual address, capacity and memory kind, then issues a non-reusable owner/allocation/generation handle. Process-wide overlap checking rejects another registry claiming the same active span. `Write` advances a registry-issued content version and refuses while a buffer is leased or published. `Retire` succeeds only when no invocation or bundle still references the allocation. Re-registration at the same address gets a fresh identity; old handles fail before dereference.

Q1 arguments are exact contiguous views within these allocations. All source roots, scratch/output roots, final int32 status and serving metadata must be mutually disjoint at the byte-range level, satisfy exact source extents and scalar alignment, and refer to live handles. All source buffers are bound, even for masked or zero-active work. The runtime borrows storage; it never allocates or reshapes the engine's cache.

The metadata allocation is an int64 matrix with B rows. Its row width is `9 + Qcapacity + Lcapacity + A`, where A is the source's known update count or zero. The row is:

```text
phase, active_queries, history, valid, logical_query_begin,
mask_policy, window_left, update_kind, append_tokens,
query_positions[Qcapacity], key_positions[Lcapacity], update_positions[A]
```

Phase codes are unknown=0, prefill=1, decode=2 and speculative-verify=3. Mask codes are opaque=0, causal=1 and causal-window=2. Update codes are none=0, construction=1, append=2 and indexed-unknown-order=3. Active position prefixes are read at the declared cardinality; unused slots confer no fact. Integers and enums are bounded and checked. B is bounded by the Q2 v1 sixteen-request contract.

The adapter copies mask bits directly from the actual source mask root, constructs the typed Q2 invocation internally, and repeats Q2's per-request query/history/valid lengths, segment offsets, positions, mask, window and logical append consistency checks. A phase label does not override these checks. Workload metadata itself has a registry generation/content version and is pinned with the call. The owned report includes the metadata allocation alongside source/status allocations.

## Page mappings, source ranges and scale pairing

For each actual token t in the source capacity L, Q3 requires tokens[t]=t and checks the actual page table:

```text
physical_page = table[b, floor(t/P)]
physical_slot = physical_page*P + t%P
byte_offset(component) = physical_slot * component_row_bytes
```

The table may be nonmonotonic and shared across read-only prefixes. Physical page IDs must lie in the real pool's source capacity. Each page record separates indirect page selection from its affine row offsets, source-capacity tokens from logical-valid tokens, and independent latent/rotary/scale row widths. All four page-local byte ranges are checked against their actual registered views. A partial last page is explicit; zero logical validity does not eliminate source loads.

The input buffers are immutable; the Q1 functional scatter writes fresh successors. For an append, actual slot values must match the mapped logical append positions, be globally unique, and avoid every request's retained-history slots. A write aliasing a shared prefix or another append refuses before the callback. This qualifies the existing functional-copy behavior, not an in-place or page-granular COW allocator. Unproved update order remains unsupported by this serving adapter.

Scale positivity/finite checks use actual source decoder-consumed slots, not just visible mask entries. Append destinations overwritten by the source before decode do not require a valid old scale; the resulting scale must validate afterward. Unused physical padding may carry NaN poison without becoming a consumer. All allocations remain fully bounded, including source copies of unused physical rows. No content hash of encoded caches is required. Component generation/version identity and bundle publication prevent later scale reinterpretation or mixed successor components.

Reported base/page alignment is measured independently for each actual host span. It is advisory for future costing. There is no tensor map, architecture-specific copy admission, swizzle, SMEM readiness or TMA flag; those require Q6. Preferred P/fragmentation/traffic decisions belong to later costing and serving policy, not this verifier.

## Lease and publication state machine

1. **Initial readiness:** For paged input, `Import` validates complete actual views/controls/pages/scales and atomically introduces one owner-ready incoming bundle. Its recorded valid lengths are the invocation's history lengths. Dense construction has no incoming bundle.
2. **Admission:** `Begin` requires the current parent bundle, exact incoming cache/scale/mapping handle/range/content joins and matching published-history lengths. It reads current controls, records registry-issued identities/versions, acquires shared input/metadata leases and exclusive output/scratch/status leases, then returns an owned fact record and non-reusable ticket. Failure invokes nothing and changes no bytes. A ticket is fresh only while the registry retains it.
3. **Execution:** `Run` revalidates the controls and parent, sets status to -1 so an unexecuted callback cannot reuse an old zero, marks the ticket running, and invokes exactly one qualified synchronous entry outside the registry mutex. A second run, cancellation, teardown, mutation or retirement that conflicts with the live call refuses.
4. **Completion:** The callback must complete synchronously and the actual status must be zero. Controls are re-observed; resulting scales and the parent are revalidated. The adapter reserves the new record before commit, advances writable content versions, releases the call leases, and publishes all four cache/scale successors with their mapping and metadata as one bundle. The new valid lengths are the invocation's resulting valid lengths.
5. **Race handling:** The current-parent comparison is repeated at completion. Concurrent calls may read the same immutable parent with private writable storage; exactly one publication from that parent can win. A loser retains dirty private output but publishes nothing. It cannot overwrite a newer bundle. This is a correctness CAS, not an algorithm-selection mechanism.
6. **Failure/cancellation:** Nonzero status, incomplete callback, changed controls, bad successor scales or a stale parent expire the ticket and publish no successor. Writable generations advance when execution may have touched them. Inputs remain immutable under the owner protocol. Prepared cancellation releases its leases without pretending execution occurred. No partial result is a fallback or a published cache version.
7. **Readers/reclamation:** `Acquire_reader` pins the complete bundle. Old readers survive a newer publication. `Retire_bundle` refuses outstanding readers or pending calls; component writes/reclamation remain blocked until all bundle references are retired. Reader completion is exactly once. Teardown through the embedding ABI refuses pending calls/readers. Copied diagnostic reports do not extend a lease.

Bundle IDs, allocation generations and content versions are issued by the live registry, with process-wide non-reusable tokens with permanent counter exhaustion and per-registry ownership. Matching caller integers, pointer equality, equal bytes or a recycled page index are insufficient. `Inspect_bundle` copies the complete published component tuple and valid lengths; it is diagnostic and does not acquire a reader lease. Cross-registry ownership transfer/shared multi-variant registries are not defined in v1; overlapping registration refuses until the old owner is quiescent and releases ownership. The future AOT selection ABI must address that integration separately.

## Compiler integration, comments, isolation and fallback

Only the pure schema/description service enters be.so/LNO. The actual mutex/atomic registry and host embedding ABI are a separate adapter product (`libmla_serving_review.so` in the focused build); they add no runtime/pthread/JSON dependency to be.so. The embedding ABI uses native program decoding and C callbacks and has no Python dependency. Python in the tests merely owns real host buffers and calls the compiled native API/reference entry.

The High builder emits a Q3 COMMENT after executable declarations have been allocated, preserving prior executable symbol identities. The existing Q1 post-PREOPT workaround restores the explanation at function entry. The existing whirl2c COMMENT emitter prints it into actual generated C. Production compiler traces say `AI_Q3_SERVING ... binding=unknown ... publication=unbound`: runtime registry leases cannot be persisted as live facts in a `.B` file or substituted for compiler mutation epochs. General PREOPT COMMENT retention remains deferred.

Estimation-only search, global budgets/convergence controls, selected-only High application and analysis invalidation remain unchanged. This runtime adapter is not invoked from exploration, does not profile candidates, construct native IR, choose a tile, launch a GPU kernel or JIT compile. AOT remains baseline. The phase-specialized direction is recorded separately; AOT selection will be designed later, as requested.

Unavailable bindings refuse before execution. The caller may retain the existing source-compatible Q1 route or an explicitly compatible generic serving path. Unknown phase with valid structural/buffer facts is supported; it never authorizes a phase-specific optimized entry. Contradictory phase/geometry is rejected, and a correctly declared unknown generic invocation can still execute the same source. Packed storage is not reinterpreted as FP32. Source status failure after execution discards private results; no automatic fallback call is inserted that could replay side effects or publish partial scratch.

## Acceptance evidence and limitations

Focused native tests use genuine retained High images and the actual WHIRL-derived compiled source entries. They bind real canary-protected host buffers for all ten S8/U4/dense/paged/fake cases, execute through the adapter, and compare all six results to eager and independent expanded references. Tests cover actual generations and published tuples, mixed cache/scale or mapping joins, nonmonotonic/shared pages and partial tails, bad tables/tokens/scales/slots, shared-prefix or duplicate writes, capacity/alias/memory-kind/P refusal, address ABA, cross-owner handles/overlap/tickets, mutation/reclamation/teardown gates, old-reader lifetime, source status/incomplete callback/control/scale failure, and two actual concurrent callbacks with one publication winner. Same-code prefill-tail intent and an unknown generic path demonstrate that intent comes from the registered metadata. Zero active queries still execute source effects.

Backend qualification retains source/High/PREOPT/final native B/T, raw annotation diffs and actual emitted C; checks Q3 explanations, numerical regressions, exact prior High executable keys/source-before traces, legacy fallback and PREOPT invalidation, Q2 inference, search/control/movement tests and consumer/symbol closure. Independent final-path replay must use only retained tools/source/inputs. No GPU correctness, sanitizer/device-race result or performance speedup is claimed.

Focused qualification passes **182 native host-serving checks** across the ten real source entries, including actual concurrent callbacks, complete publication/version reports and all base reference results. Additional source executions cover the publication race's private outputs, same-code prefill-tail intent, unknown generic fallback and zero active queries. The backend matrix passes **15 configurations / 45 compiled emitted host libraries / 270 base results**, with Q3 comments in High, post-PREOPT and final C. All fifteen source-before full traces and ten High executable keys match Q2; the new COMMENT/NAME records are the intentional raw annotation diff. Q2's 66 cases and seven altered-mask executions, seven native/search tests, 181 control/movement assertions, two PREOPT invalidation cases, exact legacy traces/keys/fallback and consumer closure pass. The default SIMD route compiles separately; final emitted-C numerical checks use scalar LNO because existing whirl2c SIMD emission is incomplete. be.so shared dependencies and undefined imports remain unchanged. These are compiler/host results, not GPU qualification.

The adapter is bounded, contiguous and synchronous. Initial import readiness and cooperative ownership are preconditions. It does not stop arbitrary raw pointer misuse, certify external producer semantics, install a vendor engine integration, support GPU memory/events, select AOT variants, implement shared cross-registry ownership, dynamic remap/in-place COW, quantized optimized realization or TMA. Q4-Q7 and the deferred AOT-selection design remain open. Owner, successor, independent reviewer and approving lead remain **TBD**.
