# Generated External Tensor Value Contract

Status: proposed main/common contract for FHE SYNC-6 C2 review. No API or
binary-image implementation is approved by this document alone. This is a
separate transaction from the source-derived typed-row API on
`codex/dsl-typed-row-value-contract`.

## Purpose And Ownership

A backend pass sometimes generates a tensor payload from verified geometry,
not from an existing tensor value. The immediate consumer is a rank-1 F32
0/1 packing mask. Common/com owns a generic, source-free external-data value
transaction and its mapped-image evidence. The FHE pass owns mask bytes,
geometry proof, exact variant binding, content digest, and CKKS legality.
Neither the common service nor Python interprets the mask or its packing
algorithm.

The API creates a pure `common.tensor_const.v1` expression in the selected
PU, assigns it to a new owner-local ST, and returns its stable
`DSL_IR_VALUE_ID`, ST, and native STID definition. It does not change a call
actual, formal, source value, REGION interface, or operand of another node.
The result may then be used directly as an operand of `ckks.encode`.

## Proposed Public API

Place declarations in `osprey/common/com/dsl_ir_image.h` and implementation
in `osprey/common/com/dsl_ir_rewrite.cxx`, following the adjacent typed-row
service. Public records use fixed-width IDs and borrowed request strings;
the image makes its own persisted copies.

```c++
typedef struct {
    WN *insert_before;
    const char *name;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum_sha256;
    SRCPOS source_position;
    const char *generation_name;
    UINT32 generation_version;
    const char *geometry_sha256;
    UINT32 stage_ordinal;
    UINT32 diagonal_ordinal;
    const char *variant_key;
} DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT;

BOOL DSL_IR_Materialize_Generated_External_Tensor_Values
    (PU_Info *pu, const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST *requests,
     UINT32 request_count, DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT *results);

BOOL DSL_IR_Image_Get_Generated_External_Tensor_Provenance
    (ST_IDX owner_pu_st, DSL_IR_VALUE_ID value_id,
     DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE *result);

BOOL DSL_IR_Generated_External_Tensor_Validate_PU
    (PU_Info *pu, FILE *diagnostic);
```

`DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE` is a read-only result containing
owner/value identity, generation name/version, geometry digest, stage and
diagonal ordinals, and variant key. The returned strings are borrowed from
the current image and must not survive image reset or mutation. The precise
declaration will follow the existing read-only reference and lineage structs.

## Admission And Transaction

The active `Current_pu`, local symbol table, and `PU_Info` must match. Every
anchor must be an executable statement in that PU's structured BLOCK/REGION
tree; insertion is immediately before that anchor and preserves request
order for equal anchors. Names and the tuple `(generation_name,
generation_version, geometry_sha256, stage_ordinal, diagonal_ordinal,
variant_key)` must be unique within the result owner PU.

The result TY is an existing canonical, fully static TensorDescriptorIR with
external-data memory and side-file placement. The generic service accepts
supported element dtypes and rank rather than hard-coding FHE masks; the
first producer test uses rank-1 F32. The TCON descriptor must equal the
requested TY. Its URI, side path, tensor key, offset, length, and checksum
must agree with the request. Range length must equal exact static tensor
bytes; offset and length obey element alignment, bounds cannot overflow, and
the checksum is lowercase 64-hex SHA-256. The source position must be real.
The producer verifies the actual side-file bytes and digest before calling
this service; common/com checks structured evidence, not external bytes.

Preflight the entire array before any WN, ST, string, TCON, or managed-image
mutation. Clear all outputs on rejection. A rejected second request must
leave all earlier requests unapplied, including names, node/value counts,
STs, WN trees, and REGION rows. After the preflight boundary, commit must not
return a recoverable partial success: an unexpected allocation or invariant
failure is terminal for the enclosing checkpoint. The all-PU artifact
transaction retains responsibility for deleting temporary side files and
withholding the final `.B` marker. This is the existing Open64 terminal-
failure pattern, not a claim of general in-memory rollback after OOM.

## Persistent Evidence And Compatibility

Use a new append-only node capability flag, tentatively
`DSL_IR_NODE_FLAG_GENERATED_EXTERNAL=0x10`, orthogonal to lifecycle flags.
The node has a closed attribute profile with `value_kind=external_data`, the
URI and storage/TCON facts, `dsl.generated_external=1`, and structured
generation/geometry/stage/diagonal/variant fields. It has **no**
`dsl.converted_from_*` attributes and no source-value ID. In particular, it
must not reuse the source-derived typed-row flag or query. Local ST metadata
cannot carry these fields because ST_IDX can collide across PUs.

Mapped-image validation requires the flag and marker together, the exact
profile, valid references, and a pure `common.tensor_const.v1` result. The
active-PU verifier checks owner ST/TY, definition, TCON, side-file facts,
generation identity, and absence of false source lineage. The logical
`ir_b2a -st -src` dump prints the generated provenance and storage range
without exposing physical `OPR_DSL`. Existing unflagged images are unchanged;
the immediately previous reader must reject an image with the new flag.
There is no new ELF section, TY_KIND, opcode, or WHIRL row layout.

## Focused Acceptance

1. Create two generated rank-1 F32 masks in one active PU with distinct
   stage/diagonal ordinals. Return usable value IDs, STs, and STIDs; reopen
   the `.B` in a separate process and retain its matching `-st -src` `.T`.
2. Verify exact side-file range, checksum, TCON, canonical TY, owner,
   source position, and geometry/variant provenance in the query and dump.
   No `converted_from` lineage appears.
3. Reject wrong active PU, wrong TY/TCON, malformed SHA, wrong byte length,
   missing anchor, duplicate name, duplicate provenance tuple, and invalid
   second request with no WN/ST/image/REGION mutation.
4. Reject malformed mapped flag/marker/profile combinations without changing
   the currently loaded image. Confirm the previous reader fails closed and
   legacy ordinary tensor constants still reopen.
5. Rebuild linked producer and `ir_b2a`; run the DSL native syntax/layout
   matrix. Retain `.B`, `.T`, source, commands, and diagnostics on the host.

## Review Decisions Before Code

- Is `geometry_sha256` the canonical digest of a producer-defined geometry
  manifest, and where is its versioned grammar published?
- Must `variant_key` be an opaque stable identifier, or should it be a
  separately validated digest of the variant signature?
- Is one source-free profile enough for both bit-move masks and any future
  generated tensor, or does the producer need an additional generation-kind
  field beyond `generation_name`?
