# Typed External Tensor Value Materialization Contract

Status: FHE-reviewed main/common API implemented and locally certified in
commit `5d749bc3` for SYNC-6 S6-0c. This is not approval of executable CKKS
Conv. The FHE consumer
request is recorded in `FHE-SYNC6-TYPED-ROW-VALUE-HANDOFF.md` on the FHE
branch.

## Boundary And Existing Precedent

`DSL_IR_Materialize_External_Tensor_Values()` creates external-data tensor
constants and can replace caller actuals, but deliberately requires the
source and result to have the same TY. A transformed Conv feature row instead
has a canonical rank-1 F32 TY and an authenticated rank-4 F32 folded-weight
source TY. No caller actual is replaced. Do not weaken the existing API's
same-TY contract or add a permissive flag to it.

The new service belongs in `osprey/common/com/dsl_ir_image.h` and
`dsl_ir_rewrite.cxx`. It constructs a pure `common.tensor_const.v1` value from
already supplied descriptor/TCON/side-file facts. It does not compute Conv
geometry, transform bytes, choose a PU variant, create CKKS state, or write a
side file. A backend consumer uses this generic service, not `DSL_Builder_*`
or raw WN/ST/image edits.

## Proposed Public C API

The following names and fields are the proposed review surface. They are
runtime-only C structs; they add no mapped row or binary WHIRL layout.

```c
typedef UINT64 DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE;

typedef struct {
    const char *name;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE source_handle;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    WN *insert_before;
    SRCPOS source_position;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    const char *transformation_name;
    UINT32 transformation_version;
    UINT32 transformation_ordinal;
} DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX source_owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    const char *transformation_name;
    UINT32 transformation_version;
    UINT32 transformation_ordinal;
} DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE;

extern BOOL DSL_IR_Capture_External_Tensor_Source
    (PU_Info *source_pu_info, DSL_IR_VALUE_ID source_value_id,
     DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE *source_handle);

extern void DSL_IR_Typed_External_Tensor_Value_Request_Init
    (DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *request);

extern BOOL DSL_IR_Materialize_Typed_External_Tensor_Values
    (PU_Info *pu_info,
     const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *requests,
     UINT32 request_count,
     DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *results);

extern BOOL DSL_IR_Image_Get_Typed_External_Tensor_Lineage
    (ST_IDX owner_pu_st, DSL_IR_VALUE_ID value_id,
     DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE *lineage);
```

The request initializer zeros the record. Ordinals are zero-based, including
ordinal zero; `transformation_version` must be nonzero. A transformation name
is a stable, nonempty, printable `domain.operation` identifier, not an opcode
or a free-form source variable name. The FHE producer may use a reviewed
versioned name for its row transform; common/com only validates the syntax
and provenance relationship, not its Conv semantics. Returned WN pointers
are valid only in the selected PU's active memory-pool lifetime. The lineage
query's string is borrowed and must not survive table mutation or reset.

The handle's zero value is invalid. It is a runtime-only, image-generation-
scoped capability, not a mapped ID. Capture deep-copies the validated typed
external reference and owner-qualified source identity into a common/com
registry; the caller never owns or interprets the registry entry.

## Ownership And Preflight

Capture requires `source_pu_info == Current_PU_Info` and validates a live,
pure external-data `common.tensor_const.v1` through the existing typed
external-reference query while its owner-local symtab is active. The registry
deep-copies the owner ST, global value ID, source ST/TY/TCON, URI/range,
checksum, and source position. It rejects local-index ambiguity and is reset
with the DSL image. Capture does not create a result or mutate the source.

Materialization requires `pu_info == Current_PU_Info` and the active local
symtab to belong to `PU_Info_proc_sym(pu_info)`. Each request must present an
owner-qualified source pair matching a live captured handle from the same
image generation. The source owner may equal the result owner (entry stem) or
may be a different PU (called Conv context). Common/com rechecks global
source value/node/owner identity against the captured record but never
dereferences an inactive PU's local ST. A stale handle, a changed source
identity, or a handle captured in another image generation is rejected. The
FHE producer captures caller-owned folded sources before leaving the caller
PU, chooses the whole-PU callee variant, and then creates row values in that
variant. Neither caller actuals nor callee formals are expanded by this API.

`insert_before` must be a statement in a BLOCK owned by the active PU. The
service derives that BLOCK; callers provide neither a BLOCK nor an ST. The
result name must be unique within the PU. The complete request array is
preflighted before any WN, ST, TY, TCON, metadata, or managed-image mutation:

- Every source handle, owner-qualified source value, and insertion anchor is
  current and unambiguous. The result ST and insertion anchor remain strictly
  active-PU-owned even when the source belongs to another PU.
- Every result TY is an existing canonical tensor TY with fully determined
  static shape; it may differ from the source TY. The service does not create
  or mutate a TY.
- Each TCON is existing side-file-dense external data for exactly that result
  TY. Element type, element count, byte length, alignment, side path, and
  range agree with the canonical descriptor and request. The service does not
  create or mutate a TCON.
- URI/key and lowercase SHA-256 syntax are valid; offset plus length does not
  overflow and the range is element-aligned. The external byte digest is
  authenticated by the producer before this call; common/com validates typed
  references and syntax, not side-file contents.
- Name and `(source_owner_pu_st, source_value_id, transformation_name,
  transformation_version, transformation_ordinal)` are not duplicated in the
  active PU or within the batch. Source position is nonzero. No call actual,
  REGION interface, or effect relation is an input or mutation target.

The FHE row consumer additionally requires rank-4 F32 OIHW source and rank-1
F32 result of length `C_out * H * W`; that geometry check belongs to its
semantic gate, not this generic API. A different-TY result is permitted, not
required. No implicit-zero source is admitted in v1.

Before choosing a variant, the FHE producer must prove that each captured
source is the exact call-ABI actual for that Conv context. Variant reuse must
include every embedded row's authenticated content digest and ordered
transformation identity in the complete executable-plan key. Contexts whose
row bytes or ordered transforms differ must select different variants; a
shared source definition or equal shape alone is not enough. Only per-caller
bias values kept as proven formals may be excluded from that key. These are
FHE-side admission rules, not Conv/call-ABI logic in the generic transaction.

## Commit And Mapped Evidence

On success, each request creates one owner-local result ST, one
`STID(MTYPE_M)` definition of a pure `common.tensor_const.v1` external-data
expression, one DSL node, and one constant value. The ST and statement carry
the request source position. The returned value ID, ST, and definition are
stable within the active image and may be used directly as a later
`ckks.encode` operand. Source value, TY, TCON, ST, WN, and side bytes remain
unchanged. No call actual or call-ABI row is rewritten. No inactive local ST
table is read or written during result materialization.

The new node carries its own storage facts and lineage as a fixed 15-attribute
`common.tensor_const.v1` profile. It must not copy the caller's ST metadata:
legacy tensor ST metadata is keyed by local ST_IDX and may collide across PUs.
The node carries `dsl.typed_external_row=1` together with
`DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW`. Storage attributes identify the new
side-file range, checksum, and TCON. Lineage attributes include
`dsl.converted_from_owner_pu_st`, `dsl.converted_from_value_id`,
`dsl.transformation_name`, `dsl.transformation_version`, and
`dsl.transformation_ordinal`. The typed query checks this profile, validates
the result owner and owner-qualified source identity without dereferencing an
inactive local ST, and rejects incomplete or malformed evidence. These are
provenance, not tensor type equivalence. The logical printer exposes the
source and transformation alongside the external range in `ir_b2a -st -src`
without exposing physical `OPR_DSL` encoding.

An invalid request returns `FALSE` with result outputs zeroed and no native
or managed-table change. Batch preflight must include duplicate and
second-request failures. After commit starts, an unexpected invariant or
allocation failure is terminal for the current checkpoint; the API must not
return ordinary failure after publishing a partial in-memory batch. This is
not a general-purpose in-memory undo facility. The existing all-PU artifact
transaction owns side-payload and `.ckks_ops.B` publication, with the `.B`
commit marker last. No final output is published after a terminal failure.

## Compatibility And Certification

No new opcode, TY_KIND, ELF section, mapped row, or WHIRL revision is needed.
The append-only node flag is a capability discriminator: the marker and flag
must occur together, and the old reader rejects a new flagged image rather
than silently accepting unrecognized row semantics. Existing unflagged images
reopen unchanged. Malformed typed-row evidence must fail closed in mapped
validation, the DSL gatekeeper, or the active-PU lineage verifier. Each source
is validated while its own PU's local symtab is active; owner-qualified
global value IDs carry cross-PU provenance. This is structural verification,
not interprocedural optimization. The runtime capture handle is not persisted
or required to reopen the image.

The focused producer test must retain a `.B`, matching
`ir_b2a -st -src` `.T`, and source file. It must prove:

1. Two rank-1 F32 values from one rank-4 F32 external source in one batch;
   source ST/TY/TCON and bytes are unchanged, and both returned definitions
   are usable as direct operand values.
2. Capture a caller-owned source while the caller is active, then create
   values in a specialized callee with colliding local ST indices. The
   callee result owner and caller source owner remain distinct and exact;
   wrong-handle, wrong-owner, and stale-generation cases reject. A second
   caller context may use the same source definition without collapsing its
   own transform ordinal or result value.
   An FHE-side two-context negative must reject variant reuse when their
   authenticated row content or ordered transforms differ, and reject a
   source that is not the context's exact call-ABI actual.
3. Wrong owner, source kind, rank/dtype/TY/TCON/range/shape, malformed URI or
   checksum, duplicate name/transform ordinal, stale anchor, and failure of
   request two leave WN/ST/value counts unchanged.
4. Mapped reopen and `ir_b2a -st -src` show typed row constants, side-file
   range/checksum, source position, and owner-qualified lineage. A wrong or
   missing source owner, value, or transform field fails the structural
   provenance join; prior valid image state is not replaced on rejected load.
5. The existing same-TY caller-actual replacement test and a legacy artifact
   without typed-row lineage remain unchanged.

The FHE consumer accepted the native API and cross-PU capture contract after
review of `5d749bc3`. FHE still owns exact call-ABI actual proof, authenticated
row bytes/digests, ordered transform selection, and variant-key separation.
