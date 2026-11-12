# Q0 MLA serving/cache contract v1.0

November 9, 2026 | Review fixture ABI; production engine adapter deferred to Q3

This contract names the invocation facts required by the [native contracts](AI-COMPILER-MLA-Q0-NATIVE-CONTRACTS-v1.0.md).
The executable review constructor/validator is
[`mla_q0_contract.py`](../osprey/torch2whirl/python/tests/mla_q0_contract.py).
No production compiler hook imports it. Every retained case includes a concrete
`serving.v1.json`. This is a bounded CPU source contract, not a deployed serving
ABI, an engine integration, or a proof of allocator guarantees.

## Exact document and version policy

The schema string is `open64.mla-serving-cache.v1`. The exact top-level keys are
`schema`, `encoding`, `shape`, `paging`, `workload`, `state`, `alignment`,
`numerics` and `capabilities`. Each nested record has an exact field set below.
Unknown/missing fields and incompatible versions refuse. The strict JSON loader
rejects duplicate keys, NUL, nonfinite JSON and documents exceeding 1 MiB.
Integer fields reject booleans. Extents/versions are bounded by 2^24 in this
review profile; B<=16, H<=128, C/R<=4096, 1<=Q<=L. Future broader production
admission requires explicit version/compatibility review, never ignored fields.

| Record | Exact fields | Meaning and validation |
| --- | --- | --- |
| encoding | scalar string | `s8-token-rne127-f32.v1`, `u4-offset8-rne7-f32.v1` or dense-only `fake-s8-float32.v1`; formulas and byte order are in the native contract |
| shape | batch, heads, queries, length, latent, rotary | B,H,Q,L,C,R logical positive extents; U4 requires even C and R |
| paging | kind, page_tokens, physical_pages, table, tokens, write_slots, shared_readonly_prefix_tokens | Dense or paged layout, exact logical-token enumeration 0..L-1, page IDs and append destinations |
| workload | phase, phase_evidence, query_positions, history_lengths, valid_lengths | Per-request phase labels, their provenance and independent causal/extent facts |
| state | update, append_tokens, incoming_version, result_version, allocation_generation, publication, release | Construction/read/functional replacement and lifetime obligations |
| alignment | global_guarantee_bytes, tensor_map, gpu_alignment | Exactly 0, `not-created`, `unestablished` in Q0; a CPU allocation cannot grant GPU/TMA eligibility |
| numerics | scale_dtype, compute_dtype, accum_dtype, scale_floor, rounding, empty_output, empty_lse | Exactly float32/float32/float32, 2^-12, ties-to-even, zero, -inf |
| capabilities | native, tma, inference | Exactly `unsupported-Q0`, `unsupported-Q0`, `not-implemented`; review validation cannot enable implementation |

## Source input and output joins

Dense construction consumes query tensors q[B,H,Q,Dn], qr[B,H,Q,R], float
latent C[B,L,C], float rotary R[B,L,R], uk[H,Dn,C], uv[H,Dv,C] and
allowed[B,H,Q,L]. It returns attention[B,H,Q,Dv], LSE[B,H,Q], and the four
successor cache tensors (latent storage/scale, rotary storage/scale). Fake
quantization retains float storage and receives no compression credit.

Paged read/append source has fourteen ordered inputs: q, qr, latent pool,
latent scale, rotary pool, rotary scale, table, uk, uv, allowed, new latent,
new rotary, write_slots, tokens. Read-only graphs can retain unused signature
inputs. Capture records distinguish placeholders from used operands; do not
infer state updates from unused arguments. Pool shapes are [Nphysical,P,Cstore]
and [Nphysical,P,Rstore], scales [Nphysical,P,1]; the source explicitly reshapes
these to flattened token rows before gather/scatter. S8 Cstore=C, U4 Cstore=C/2
(similarly R). Table is int64[B,ceil(L/P)], tokens int64[L], and append slots
int64[B,A]. Fresh cache results use the same physical extents. Shapes and exact
argument order are retained in `values.json` and both graph censuses.

For request b and token t, the physical token slot is

    page = table[b, floor(t/P)]
    slot = page*P + (t mod P)

All indices are nonnegative int64 with checked arithmetic and range proofs.
Latent/rotary pools and scales use the same slot; scales never use packed-column
indices. Table IDs must be less than Nphysical. The fixture includes permuted
pages, a shared immutable first page and partial last pages. Unused physical
padding is not read; a test poisons its scales with NaNs. A mask does not legalize
an out-of-range access performed before masking.

Page capacity P belongs to the allocator/layout ABI; compute chunk width W
belongs to the selected kernel plan. Neither is inferred from the other.
The fixture's P=4 exercises addressing and tails, not GPU cache-line tuning.
A future adapter must supply base/stride/scale-layout guarantees and prove the
address of each planned transfer. Alignment of the allocation alone does not
prove alignment of every row/page or TMA box. Preferred P is an advisory serving
choice; unsupported P uses a qualified generic path or an explicit refusal.
Q0 neither changes an engine's allocation policy nor creates a tensor map.

## Phase evidence and cache-role inference

`phase` is a B-element vector with labels prefill, decode, speculative-verify
or unknown. `phase_evidence` is exactly `fixture-intent-not-inferred-from-graph`.
The dense Q=1 decode and Q=1 prefill-tail fixtures deliberately have identical
graphs and inputs. Quantized storage, cache role, Q=1, cache length and an append
alone cannot distinguish them. A future detector may use verified producer /
consumer dataflow and serving declarations with provenance and contradiction
checks; ambiguity remains unknown. No source trait annotation is attached to
these fixtures, so Q1 must derive quantization/cache associations or retain
unknown facts. Source quantization arithmetic does not prove persistent lifetime.

`query_positions` is [B,Q], equal to the last Q positions of this bounded L.
`valid_lengths` is [L]*B; `history_lengths` is [L-A]*B for functional append,
and [L]*B for read-only/construction (A=0). History here means cache validity at
graph entry before this graph's update. A read-only graph may receive a cache
already containing the current query tokens. It does not necessarily mean the
request's pre-query history. The allowed mask supplies causal/visibility facts;
labels do not replace it. The all-masked head produces zero output and -inf LSE.
The v1 fixture has uniform Q/L; arbitrary ragged or mixed-query execution is
outside its captured coverage even though different phase labels are allowed.

## Ownership, version and publication

`update` is construction for dense quantization, read-only for paged reads or
functional-append for paged updates. Construction has A=0 and a successor version;
read-only has A=0 and unchanged version; append has 0<A<=L and version+1.
Version and allocation_generation are positive invocation integers. Equality of
integers alone is not an allocation/alias proof; Q3 must bind actual handles,
storage IDs, generation, capacity and readiness. Tensor type identity cannot
establish ownership or storage liveness.

Append writes exactly the last A logical positions. Slots must equal the page
mapping, be globally unique across requests and avoid aliases of retained
logical reads and the shared prefix. Four functional index_copy operations
return fresh nonaliasing pools/scales, preserving all other rows and all inputs.
The fixture is not an in-place writer or a copy-on-write allocator. A future
engine must establish private writable destinations (or perform copy-on-write)
before an in-place selected implementation is legal.

`publication` is after-all-results for construction/update, already-ready for
read-only. `release` is after-all-readers. Bundle publication means the matching
latent, latent-scale, rotary and rotary-scale versions become visible together;
host return in this CPU fixture models only completion, not a device event.
Production stream/event ordering, graph replay, concurrent requests, allocation
reuse, failure/rollback and release callbacks require Q3 tests. TMA would only
move encoded data; Q6 must explicitly represent copy completion, consumer waits,
dequantization work, shared storage reuse and pipeline drain. Q0 promises none
of these device operations or overlap.

## Fallback and acceptance

Source eager/FX/ATen replay is the CPU reference route. Production mock/native
ingestion must refuse these sources before publishing a valid .B. An engine
may choose its own compatible reference only under its external integration;
the compiler cannot reinterpret packed bytes as FP32 cache data or dispatch
them to the existing FP32 G1 family. Q1/Q3/Q5 must establish a synchronous
compatible native fallback before Q6 TMA eligibility. Missing/contradictory
facts disable a candidate; phase unknown never authorizes speculative writes.

Capture/oracle/profiling are outside estimation-only candidate search. Search
continues to read owned snapshots and detached facts; only an eligible selected
plan enters atomic High application and correctness regression. No Q0 serving
record changes a search budget, candidate catalog, native mutation epoch,
COMMENT workaround or production lowering route.

Owner, successor, independent reviewer and approving lead remain **TBD** as
requested. This records Q0 technical evidence; it does not claim independent
transfer acceptance or completion of Q1-Q7.
