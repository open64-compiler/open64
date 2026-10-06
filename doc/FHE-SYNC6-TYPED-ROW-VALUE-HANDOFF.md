# S6-0c Typed External Row Value Handoff

Status: FHE consumer contract request; no common/com implementation or binary
image change is authorized by this note. See
`FHE-SYNC6-CONV-MASK-ASSET-OPTIONS.md` for the selected ACE-style row rule and
`FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md` for the C2-C5 sequence.

## Exact Gap

The existing `DSL_IR_Materialize_External_Tensor_Values` transaction accepts
an existing external tensor and requires its TY to equal the replacement TY.
For the selected Conv path, the authenticated folded weight is rank-4 F32
OIHW, while each transformed feature row is rank-1 F32 of length
`C_out * H * W`. The existing `DSL_CKKS_EXPANSION_OPERAND` can name an
existing value or a preceding CKKS step; it cannot create this typed plain
value. FHE code cannot write WN, ST, TY, TCON, or mapped rows around either
contract.

The diagnostic asset fixture produces one raw little-endian F32 side file
and a per-row index for 17 admitted stride-one contexts. Its index retains
source Conv node/value, owner, exact printed context and callsite, folded
weight TCON/digest, feature-row ordinal, rank-1 shape, byte range, and row
SHA-256. This is test evidence, not a mapped-image association or a backend
Python dependency. Four stride-two contexts are excluded.

## Requested Generic Transaction

Main/common should review a domain-neutral batch transaction for creating
external tensor constants **with a result TY different from the source TY**.
It may extend an existing API only with a separately named, fail-closed
policy; otherwise use a new API. FHE does not prescribe the public spelling.
The complete request needs:

| Input | Contract |
| --- | --- |
| `PU_Info *` and insertion anchor | Exact active owner PU and deterministic native insertion BLOCK; no caller-supplied tree or local-ST ownership. |
| `source_value_id` | Existing, owner-safe external folded tensor reference; source value/TY/TCON and bytes remain immutable. |
| Result TY and tensor TCON | Canonical rank-1 F32 TY and external-data TCON, distinct from rank-4 source TY; exact descriptor/shape/byte-length agreement. |
| Side-file facts | Stable URI/key, byte offset/length, lowercase SHA-256, and source position; no row bytes embedded in `.B`. |
| Provenance | Structured source-value linkage plus a pure transformation identity/ordinal. Whether existing `dsl.converted_from_value_id` is sufficient belongs to common review. |
| Batch result | Stable `DSL_IR_VALUE_ID`, result ST and definition for each created value, suitable as the direct operand of `ckks.encode`. |

Preflight the **entire request array** before mutation: active PU, source
value/ST/TY/owner and external reference, canonical result TY, TCON/side-file
range/dtype/shape/checksum syntax, unique names and insertion anchors, and
every image/REGION/effect invariant. A rejected request leaves WHIRL and all
managed tables unchanged. No call actual is rewritten. Successful creation
uses the existing pure external-data `common.tensor_const.v1` contract and
preserves the source value separately. It performs no Conv geometry, CKKS
encoding, or state repair. Any post-commit failure is terminal for the
checkpoint; the side payload and `.ckks_ops.B` publish only through the
existing all-PU artifact transaction, with `.B` last.

Do not bind context-specific rows into an unspecialized shared callee.
FHE's C5 whole-PU policy first chooses a variant using complete executable
plans. The generic transaction then creates row values in the active chosen
PU; FHE-owned provenance joins each resulting value to its original
source/context/feature-row identity. Equivalent raw bytes alone do not make
two caller contexts equivalent.

## Required Main-Side Tests

1. Positive rank-4 F32 source to rank-1 F32 external rows, including two
   different rows in one atomic batch; source value/TY/TCON remain unchanged.
2. Two caller contexts sharing one source definition do not collide through
   PU-local ST values; owner and insertion anchor are enforced.
3. Wrong owner, rank, dtype, TY/TCON, shape, byte length, URI/checksum,
   duplicate name, stale source, and second-request failure leave native
   WN/ST/value counts unchanged.
4. Mapped `.B` reopen and `ir_b2a -st -src` show source positions, typed row
   constants, external range/checksum, and source linkage. Old artifacts
   without row values reopen unchanged; no WHIRL revision or opcode is added.
5. Existing same-TY caller-actual replacement tests remain unchanged.

## FHE Consumer After Merge

FHE will authenticate exact row bytes and index, register the side file as a
checkpoint auxiliary artifact, create typed values through the reviewed
transaction in the chosen PU variants, and reference only returned value IDs
from explicit `ckks.encode` steps. Then C2 must still prove row-indexed
rotations, multiply-plaintext, accumulation, bias, CKKS state/key transfer,
and the four stride-two contexts before a full `.ckks_ops.B` can publish.
