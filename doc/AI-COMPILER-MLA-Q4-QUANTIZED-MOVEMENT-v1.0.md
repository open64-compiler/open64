# MLA Q4 quantized/paged cache movement and reuse costing v1.0

November 10, 2026. Extends [Q3](AI-COMPILER-MLA-Q3-SERVING-v1.0.md),
[MR2](AI-COMPILER-MLA-MR2-MOVEMENT-COSTING-v1.0.md) and the established
[two-phase search](AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md). Prior specifications
remain retained. Owner, successor, independent reviewer and approving lead: TBD.

## Delivered scope and authority

Q4 adds a **cache-service cost study**, using genuine Q3 source/buffer bindings.
It compares a materialized decoded-cache baseline with sixteen synchronous
retained decoded-tile hypotheses: head groups G in {1,2,4,8}, token widths W in
{32,64,128,256}. The finite catalog is `mla-quantized-cache-space-v1-17`.
All seventeen entries, including the model incumbent, are **analysis-only**.
`model_eligible` means model/resource admission within this new space;
`application_eligible` is always false. No existing FP32 kernel recipe is
reinterpreted as accepting packed inputs. Q5 must supply legality, verified
materializers and selected-only correctness qualification before execution.

The score covers cache maintenance, encoded/scaled delivery, page lookup,
conversion/unpacking, decoded global/shared traffic and split-tuple traffic.
It excludes query/output projections, attention arithmetic, softmax/merge
arithmetic, producer instruction-level instruction counts and actual host
registry overhead. **It is not total MLA latency, a global optimum certificate,
a GPU measurement or a recommendation to deploy the model winner.** It supports
analysis of the data-movement tradeoffs that Q5 must evaluate in a complete
selected-plan model. The baseline is an idealized materialized cache-service
model; it is not a measured or instruction-exact timing of the scalar Q1 source.

The VHO/LNO estimator consumes only detached, owned, immutable records. Source
schema recognition, serving observation and profiling do not execute inside
candidate exploration. No native WN/ST/TY service, source callback, compiler,
CUDA runtime, profiler, regression runner or allocator probe is reachable from
the Q4 evaluator. Optimization scope remains the bound single source entry;
this work provides no IPA or cross-PU transformation.

## Q3 export and expiry

`VHO_TENSOR_SERVING::Freeze_costing(ticket, snapshot, reason)` is a runtime
boundary separate from exploration. Under the existing registry mutex it:

1. Requires a prepared, live, non-running Q3 ticket.
2. Reobserves actual metadata/masks/page-table/token/slot controls and consumed
   scale domains using Q3's existing bounded checks; compares the copied
   controls with Begin; revalidates the complete incoming parent bundle.
3. Checks actual allocation generations/content versions for every ABI view
   and metadata. Copies source geometry/encoding, all page records, workload
   facts and full generation tuples. No pointer survives in the snapshot.
4. Derives common source-maintenance counts by backward traversal from the
   four returned cache/scale SSA values. Reshape/transpose/unsqueeze views
   allocate no sweep; other ancestor operations charge one output sweep and
   one sweep of each operand, plus one modeled scalar operation per output.
   This is explicitly an operand-sweep model, not actual emitted loads/stores
   or a complete quantization instruction timing model. All candidate studies
   receive the same maintenance charge, including functional copy/update and
   returned buffers; no in-place/COW transformation is assumed.
5. Seals the full source/workload/generation/page/count projection in
   `mla-q4-snapshot-v1`. Keys are complete framed identities, not signatures.

`Revalidate_costing(ticket, exact_key, reason)` rebuilds the projection and
compares it. Cancelled, completed, running, foreign or stale tickets refuse;
controls/versions/parent changes refuse. Refusal publishes no estimate or
buffer writes. Copied diagnostics and `facts.bound` alone are not authority.
Typed pure callers must receive their snapshot from the owning Q3 boundary;
synthetic unit records qualify arithmetic only. The embedding review wrapper
freezes before exploration and revalidates afterward. A later consumer must
revalidate again at use time; no extra lease is granted by the cost record.
The cooperative host ownership and qualified synchronous callback assumptions
of Q3 continue to apply. This API does not bind opaque device pointers/events.

## Byte model and reuse

Let B,H,Q,L,R,Ro denote source capacities, P the allocator page size, G the
proposed head group and W the compute tile. C=B L counts logical capacity
reads. Active/valid facts are reported separately. Q3 intent or a false mask
does not by itself authorize source-loop elimination; even zero-active query
metadata retains Q capacity, source maintenance and reads in these studies.

For each token, paired scale bytes are 8 (two independent F32 scales). Encoded
payload bytes are R+Ro for S8, (R+Ro)/2 for supported even-width offset-U4, and
4(R+Ro) for the floating fake-quantization control. The last case earns no
compression credit. The model uses Q3's actual encoding/shape joins.

The baseline decodes once into global storage. Fused hypotheses decode once
per query/head group, so repetitions n=Q ceil(H/G). Thus:

- Encoded payload requests: C times encoded row bytes times n; baseline n=1.
- Scale requests: 8 C n. Conversion/scale work: C(R+Ro)n lanes; U4 additionally
  charges C(R+Ro)n unpack lanes. All required conversion/unpack rates are
  explicit positive per-SM inputs, even for formats with zero unpack work.
- Baseline decoded global traffic: 4 C(R+Ro) writes plus
  4 C Q H(2R+Ro) reads. Latent is consumed for both score and weighted sum.
  Paged baseline additionally charges gathered payload/scale writes and reads.
- Fused decoded shared traffic: 4 C(R+Ro)n staging writes plus
  4 C Q H(2R+Ro) consumer reads; decoded global traffic is zero. Retention
  capacity is reserved for the complete W(R+Ro) tile, including padding.
- Split scratch for K=ceil(L/W)>1: 4 B Q H K(R+2) bytes, charged once as
  allocation and twice as write/read movement. Scratch for K=1 is zero.
  Tuple merge arithmetic is outside this cache-service score.

The owner-proven page mapping determines each logical page's physical page,
capacity/valid tail, four row widths and offsets. The estimator validates
page order/bounds and every paired cache/scale stride/offset. A tile crossing
a page becomes separate page/tile fragments. Each of the four streams is
rounded independently per fragment; P is never substituted for W. The
baseline charges B L page lookups, reflecting source token-wise index work;
fused studies charge one lookup per page/tile intersection per repetition.
Each lookup charges 8 bytes plus explicit lookup service cycles.

`unique_physical_tokens` computes the union of physical-page prefix ranges.
It diagnoses shared-prefix overlap but grants **no cross-request reuse or L2
hit credit**. Cache requests remain logical per-request work. Head-group reuse
is restricted to one query/CTA's retained tile. No cross-query, cross-CTA or
inter-invocation residency is promised. Different scale/version tuples never
receive reuse from matching shape/address alone.

## Transactions, resources, timing and identity

The existing explicit MR2 hierarchy profile supplies device-wide HBM/L2
bytes/cycle, sector width (32/64/128), request amplification, shared conflict
factor, weighted/worst objective and up to four fixed hit scenarios. Q4 adds
conversion and unpack lanes/cycle/SM, and page-lookup cycles. All are positive,
bounded, explicit; `arch=sm_90` does not supply absent rates. Provenance remains
synthetic, estimated or measured-input; none means hardware qualification.

Unknown device-base alignment receives a worst-offset sector envelope:
S ceil((ceil(bytes amplification/1e6)+S-1)/S) for nonzero fragments. Host
alignment is retained in the Q3 identity but provides no GPU alignment or TMA
eligibility. Encoded/scaled reads receive the same scenario hit fractions for
all candidates. Mixed maintenance/decoded/tuple/page-control traffic is
conservatively charged through HBM without read-hit credit, since its writes
are not split by this model. `other_hit_ppm` is preserved in the common profile
identity but unused for this mixed stream. Per-scenario reports expose requested,
transaction, L2, HBM and shared bytes, including the four separate cache streams.

The shared rounded occupancy service checks thread/register/SMEM/block/warp
limits. Fused blocks reserve 32G threads, padded W(R+Ro) F32 tile bytes and
G(R+2) F32 tuple slots; estimated register demand adds ceil(R/32) per thread
to the explicit profile estimate. The baseline uses a 128-thread cache-service
work envelope. Active SM service is limited by min(SMs, CTAs); occupancy rounds
charge load startup and two synchronous CTA barriers for staged studies.
No TMA/asynchronous overlap is credited. Readiness composition serializes
movement, conversion/shared/lookup service and release. The baseline charges
two service launches; a fused unsplit study one, a split study two. These are
hypothesis service stages, not an assertion about actual Q1/Q5 kernel launches.

Checked shared arithmetic guards bytes, rounding, products and totals. Overflow,
scratch exhaustion, invalid page/scale layout or zero residency makes the
candidate ineligible. Admission failure leaves the caller's result unchanged;
a bounded search with no model incumbent is reportable and cannot produce a
fake executable fallback. Source execution and its guards are unchanged.
Full keys bind the snapshot, exact model/profile values/scenarios, every GPU
resource value, descriptor and all controls. Same-ID changed rates/resources
cannot reuse prior scores. Archived MR2 catalogs, scores and controls retain
their exact versions.

## Two-phase integration, explanations and fallback

`VHO_mla_explore_quantized` is a separate typed entry to the **existing** fair
portfolio, proposal generators, lazy shared estimate cache, deterministic ties,
cooperative deadlines, global proposal/unique/time budgets and supported
exhausted/budget/plateau/target stops. The baseline consumes the first ordinary
proposal. Exhaustive/local/random runs share the global ledger. Relative-gap
and economic cost-aware stopping refuse because Q4 supplies no certified
whole-program lower bound. Exhaustion certifies only the best score in the
admitted finite cache-service catalog, never a global GPU optimum.

There is no automatic compile-time Q3 binding: production source-only facts
remain unknown. The embedding study entry illustrates the live export boundary;
it does not install a compiler or serving dispatcher. Q5 will connect a
selected, legal, complete quantized plan to atomic application and selected-only
regression, without exploring through candidate lowering. The ordinary Q1
semantic source route and old verified FP32 route remain available on their
existing contracts. An unknown/ineligible quantized study cannot force a cast,
phase guess, buffer skip or execution of an unimplemented candidate.

The existing Q3 source explanation now states Q4's freshness, byte categories,
analysis-only scope and fallback. It is emitted as High WHIRL COMMENT,
regenerated at function entry by the Q1-specific post-PREOPT workaround and
printed by existing whirl2c. Individual candidate explanations are in the
owned study report, not inserted into native IR during exploration. General
PREOPT COMMENT retention repair remains deferred.

Separately qualified prefill/decode AOT variants remain the adopted direction.
**AOT selection design remains deferred by the user**: no option parser,
threshold, dispatch location, packaging, selector ABI or policy is added.
Q6 owns TMA descriptor/layout/alignment/device completion/barrier/scale lifetime
and on-the-fly dequantization realization. Q7 owns GPU correctness/performance.
No Hopper machine is available; software qualification cannot close those gates.

## Qualification and remaining risks

Focused native tests cover analytical S8/U4 counts, partial pages, mismatched
scale layouts, shared-prefix/ragged facts, zero-active work, scratch/resource
refusal, explicit cold/warm scenarios, changed keys, overflow and shared global
budgets. Actual Q3 tests export studies from all ten genuine WHIRL-derived
source entries, check deterministic read-only exploration and stale-ticket
refusal, then run the existing entries against both references. The Q1
production matrix reopens real High/PREOPT/final images, verifies comments in
emitted C and compares executable keys against Q3. Archived controls/MR2,
selected-reference, invalidation, legacy fallback and backend consumer closure
checks remain required. Immutable artifact receipts retain commands, source,
B/T/raw diffs, generated C, ledgers and replay evidence.

Important limitations remain explicit: heuristic register demand, optimistic
aggregate scalar service, conservative mixed-stream store/read treatment,
unknown real device coalescing/cache hits, no async overlap, incomplete
whole-kernel timing and no Q5 realization. A model-best cache study may lose
once attention compute/merge and other stages are included. Q5 must qualify
numerics, legal reuse and the actual resource pipeline before lowering only
its selected configuration. Personnel/independent acceptance roles remain TBD.

Initial implementation qualification: 309 detached Q4 assertions and 570
combined native serving/costing checks pass; the latter include all original
182 Q3 checks, ten real source entries, the zero-active case, actual-control
change/refusal/restoration and completion-expiry checks. Each source entry
still executes against both references after exploration. The unchanged suites
pass 37 search-control and 144 MR2 assertions, seven selected/native reference
tests, two actual PREOPT invalidation cases, and legacy short/long exact
recipe/trace/key and refused-route fallback comparisons. Fifteen backend
configurations execute 45 host libraries and compare 270 base result values;
ten High executable keys match Q3. be.so NEEDED and undefined imports are
unchanged, and the adapter and lw_inline consumer remain link-closed. Q4
estimate-object symbol inspection excludes native construction, serving
callbacks, GPU launch, builder and frontend dependencies. Final immutable
receipt/replay results are recorded separately with exact paths and hashes.
