# S6-0c Conv Mask Asset Choice

Status: design comparison only. Neither option is approved for mask
materialization or `.ckks_ops.B` publication. The authenticated budget and
its 13-definition/21-context managed-identity joins are recorded in
`FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md`. Stride-two Conv, the v2 ReLU
evaluator identity, and C4 scale/level alignment are separate gates.

## Shared Semantic Contract

For each of the 17 currently supported stride-one Conv contexts, the
existing folded F32 OIHW weight is bound by owner PU, source Conv node/value,
callsite, and folded-weight TCON. A mask group is keyed by an exact signed
rotation and contains the canonical 32,768-slot, little-endian binary64
output-slot vector. For every live OIHW term in the group, its F32 value is
converted exactly to F8 and placed at valid output `(oc,y,x)` positions;
all other slots are positive zero. Overlapping nonzero assignments fail.
The canonical SHA-256 of these bytes is the asset identity, not a path or
context display name. Both options must preserve explicit `ckks.encode`,
its input/result types, scale/level/state, and subsequent rotate, multiply,
rescale, and add operations. Neither option may perform Conv or CKKS state
repair inside encode.

The current model yields 10,386 byte-distinct masks: 2,722,627,584 dense
F8 bytes (2.536 GiB) before plaintext encoding. The largest single Conv
uses 1,143 masks. Its 5,715-step structural serializer probe is 716,715
bytes, below C1's 65,535-step/4 MiB per-event guards, but is **not** a
legal CKKS event. There are four uncovered stride-two contexts.

| Question | A. Chunked external dense masks | B. Typed derived-mask input to `ckks.encode` |
| --- | --- | --- |
| Persisted bytes | Exact 2.536 GiB F8 for this model, plus bounded index/hash metadata. No cross-context byte dedup was found. | Keep authenticated folded F32 payload and compact per-group derivation records; byte size depends on the reviewed record format and is not yet claimed. |
| Materialization | Stream masks in deterministic identity/rotation order, at most 64 masks (16 MiB) per chunk. A single chunked side file plus index can use the existing auxiliary-artifact transaction; `.B` publishes last. | Produce no dense mask side file. A typed, pure derived-mask expression takes the folded-weight value and an immutable geometry/group contract, yields an F8 mask tensor, and is the direct input of explicit `ckks.encode`. |
| Authentication | Verify each of 10,386 256-KiB slices by SHA-256, its offset/length, and the complete index/file digest before use. Raw 32-byte digests alone total 332,352 bytes. | Authenticate folded source bytes, reconstruct the exact F8 mask by the fixed rule, and verify its stored SHA-256 before encoding. Independent producer/provider reconstruction tests must agree for every captured shape and malformed case. |
| Existing substrate | `DSL_IR_EXTERNAL_TENSOR_REFERENCE` carries 64-bit offset/length; the checkpoint publishes registered auxiliary artifacts before `.B`. These are necessary, not proof that every reader/encoder safely streams multi-gigabyte files. | Existing tensor TY remains canonical. Current `ckks.encode.v1` expects a plain tensor input; a derived-mask producer requires a separately reviewed logical operator or typed record. No existing TCON storage kind is redefined. |
| Measurements still required | Disk write/read throughput, peak memory, full-file and per-slice hash time, provider plaintext-encode time, encoded-plaintext size/cache policy, checkpoint failure cleanup, and old-reader behavior on 10,386 references. | Derivation CPU/peak memory, provider encode time, encoded-plaintext cache policy, exact byte-for-byte provider reconstruction, plan/record count and binary size, and old-reader fail-closed behavior. The seven-second host clear rebuild/hash probe is not an encode benchmark. |
| Principal risk | The 2.536 GiB asset cost is intrinsic, even with 16-MiB streaming; 10,386 TCON/URI references may also enlarge mapped metadata. | A provider might silently interpret a compact recipe as Conv, change F32-to-F8/zero-fill semantics, or hide state repair. The logical producer and its verifier must prevent that. |

**Recommendation for review:** pursue B as the likely production contract and
retain A as a bounded, measurable reference/fallback. B is admissible only
if the compiler and an independent provider reconstruction produce identical
authenticated F8 masks, without requiring a provider-specific instruction
in VHO. An executable plan must still list every mask encode, rotation,
multiply, rescale, and addition, with exact value-specific CKKS states.
The compact record may describe plaintext derivation; it may not replace
the Conv circuit or become an implicit runtime kernel.

The minimum prospective addition for B is **one typed, pure derived-mask
logical result or equivalent append-only typed record** connecting a
folded-weight value to geometry, signed rotation, canonical F8 mask digest,
and result tensor TY. The choice of operator versus record, stable name,
physical row, version, reader/printer contract, and whether it can reuse an
existing generic tensor primitive belong to main/common review. No enum,
mapped-image layout, TCON kind, or public ABI v1 is changed by this note.
Until that review, older readers must not be asked to interpret new mask
semantics: a future producer must either use only their understood dense
external-data form or make them fail closed on the new logical contract.

Before either option is selected, run a provider-independent semantic
oracle for the 17 stride-one contexts and benchmark representative 162-,
279-, 567-, and 1,143-mask contexts. The ACE-shaped mock can certify the
adapter boundary and reconstructed bytes, but **cannot measure real CKKS
encoding cost**. Measure disk bytes, hash time, peak RSS, and failure cleanup
with the selected storage implementation; defer actual encode time and
encoded-plaintext size to an identified provider encoder, reporting them
as unknown until then. The four stride-two contexts require their own
reviewed layout/state decision; these measurements cannot be extrapolated
to them.
