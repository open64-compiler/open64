/*
 * Backend-free external tensor facts, source capture and active-PU value
 * construction. Borrowed strings and generation handles are process-local;
 * no payload ownership, payload computation or implicit PU activation.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_EXTERNAL_TENSOR_INCLUDED
#define DSL_EXTERNAL_TENSOR_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/*
 * Runtime-only borrowed view of one external tensor constant. The underlying
 * facts remain in the existing DSL value, ST metadata, and canonical TY
 * descriptor tables; this view does not add a mapped-image record. String
 * fields must not be retained across owner-PU table mutation or image reset.
 */
typedef struct {
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    TY_IDX descriptor_ty;
    ST_IDX st;
    TCON_IDX tensor_tcon;
    TY_IDX element_ty;
    INT32 rank;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    const char *dtype;
    const char *logical_shape;
    const char *layout;
} DSL_IR_EXTERNAL_TENSOR_REFERENCE;

typedef enum {
    DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY = 0,
    DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO = 1
} DSL_IR_MATERIALIZE_SOURCE_POLICY;

/*
 * Runtime-only request for materializing converted side-file tensor values.
 * All requests are preflighted before any symbol, WN, or image table changes.
 * source_value_id names an external tensor in the active PU, or an implicit
 * zero only when the explicit alternate source policy allows it. The
 * transaction derives the insertion BLOCK from PU_Info and insert_before. A
 * null call creates an entry-owned value without rewriting a call actual.
 */
typedef struct {
    const char *name;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    DSL_IR_VALUE_ID source_value_id;
    WN *insert_before;
    WN *call;
    UINT32 actual_ordinal;
    DSL_IR_VALUE_ID expected_actual_value_id;
    SRCPOS source_position;
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
    UINT32 source_policy;
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT;

/* Runtime-only proof captured while a source PU's local symtab is active. */
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

/* Capture validated external-constant facts while source_pu_info is the exact
 * Current_PU_Info with its local symbols active. Require nonnull output and a
 * canonical tensor TCON plus lowercase 64-hex checksum; clear output even on
 * rejection. Success appends a copied process-local snapshot and returns a
 * handle usable after switching to a destination PU. Keep shared source rows,
 * types and TCON valid; source/image reset invalidates the generation-qualified
 * handle. No physical/image-row or payload-file mutation; allocation exceptions
 * propagate, with no source ownership transfer. */
extern BOOL DSL_IR_Capture_External_Tensor_Source
                                (PU_Info *source_pu_info,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE
                                     *source_handle);

/* Zero a caller-owned typed conversion request, or ignore NULL. No source
 * capture, string/WHIRL allocation or table mutation occurs; caller must fill
 * the owner/source, descriptor/TCON, insertion and storage/lineage contract
 * before the complete-array materialization transaction can preflight it. */
extern void DSL_IR_Typed_External_Tensor_Value_Request_Init
                                (DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST
                                     *request);

/* Preflight a nonempty borrowed request array for the exact active destination
 * Current_PU_Info, with resident locals, valid existing typed lineage, captured
 * source handles, insertion points, canonical descriptor/TCON/storage facts and
 * unique names/transformation identities. Require request_count result slots.
 * Entry rejection preserves output; after entry checks, results are cleared and
 * subsequent FALSE leaves them cleared without physical/image appends. Commit
 * creates symbols, source-positioned STIDs and typed constant rows/lineage before
 * each insertion point, returning PU-owned WN pointers. Source definitions/calls
 * and canonical TYs are unchanged; no payload computation/file read or checksum-
 * content verification. No final verifier or commit journal: allocation/assertion
 * failures are not recoverable Boolean rollback. */
extern BOOL DSL_IR_Materialize_Typed_External_Tensor_Values
                                (PU_Info *pu_info,
                                 const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT
                                     *results);

/* Query one typed external constant with destination-owner locals active and
 * valid shared core/type/TCON/string state. Require nonnull lineage; clear it
 * before checks, leaving zero output on FALSE. Check the typed marker, storage
 * reference and source owner/value/constant lineage without activating the source
 * PU. Success returns scalar provenance and a borrowed transformation-name
 * string; retain only while its backing state remains valid. No table/physical
 * writes; scratch allocation exceptions propagate. */
extern BOOL DSL_IR_Image_Get_Typed_External_Tensor_Lineage
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE
                                     *lineage);

/* With this PU's local symbols active, scan owned values for bounded local ST
 * indices and typed-row/lineage consistency. Producerless values are skipped
 * after the ST check; any typed/lineage marker requires valid typed external
 * lineage. Require safe shared tables/strings; no PU switch, physical definition
 * proof, tree rewrite or payload-file inspection. FALSE can be silent even with
 * a diagnostic stream; NULL suppresses messages. Allocation exceptions propagate. */
extern BOOL DSL_IR_Typed_External_Tensor_Validate_PU
                                (PU_Info *pu_info, FILE *diagnostic);

/* Source-free external value request; input strings are borrowed for the call. */
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
    const char *geometry_manifest_sha256;
    UINT32 stage_ordinal;
    UINT32 diagonal_ordinal;
    const char *variant_signature_sha256;
} DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX st;
    WN *definition;
} DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT;

typedef struct {
    DSL_IR_VALUE_ID value_id;
    ST_IDX owner_pu_st;
    const char *generation_name;
    UINT32 generation_version;
    const char *geometry_manifest_sha256;
    UINT32 stage_ordinal;
    UINT32 diagonal_ordinal;
    const char *variant_signature_sha256;
} DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE;

/* Zero caller-owned source-free generation request storage, or ignore NULL.
 * Borrowed strings and insertion/type/TCON/provenance fields remain unset;
 * no value, external payload or image row is created by initialization. */
extern void DSL_IR_Generated_External_Tensor_Request_Init
                                (DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST
                                     *request);

/* Clear count-sized caller result slots, then preflight a nonempty borrowed
 * source-free request array for the exact active PU. Boolean rejection precedes
 * physical/image appends but does not preserve prior result-slot contents.
 * Commit creates PU-owned symbols/STIDs and existing generated-constant rows;
 * no payload computation, PU switch or allocation-failure journal is provided. */
extern BOOL DSL_IR_Materialize_Generated_External_Tensor_Values
                                (PU_Info *pu_info,
                                 const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT
                                     *results);

/* Resolve generated provenance with matching owner locals and valid shared
 * core/type/TCON/string tables. Require nonnull output; clear it first and leave
 * it zero on FALSE. Check external reference, exclusive generated marker,
 * generation label/version, digest spellings and bounded stage/diagonal ordinals.
 * TRUE returns scalar IDs and borrowed strings, valid only while backing state
 * remains unchanged. No geometry/payload authentication, PU activation or table
 * writes; scratch allocation exceptions propagate. */
extern BOOL DSL_IR_Image_Get_Generated_External_Tensor_Provenance
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE
                                     *provenance);

/* With selected-PU locals and prevalidated shared core/string state, check
 * generated markers/provenance, unique names and generation identities on owned
 * values with resolvable producers. Missing producers are skipped; this does
 * not replace core/physical-tree validation or authenticate payloads/geometry.
 * No mutation or PU switch; FALSE may be silent even with diagnostics supplied,
 * and scratch allocation exceptions propagate. */
extern BOOL DSL_IR_Generated_External_Tensor_Validate_PU
                                (PU_Info *pu_info, FILE *diagnostic);

/* Zero a caller-owned conversion request and select EXTERNAL_ONLY source
 * policy; NULL is a no-op. Caller still supplies source/value/type/insertion
 * and storage facts for the materializer's full-array preflight. No ownership
 * of input strings, symbol/WHIRL allocation or rollback state is acquired. */
extern void DSL_IR_External_Tensor_Materialization_Request_Init
                                (DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST
                                     *request);

/* Resolve an owner-qualified external tensor constant with its local symbols
 * already active and valid shared core/type/TCON/string state. Require nonnull
 * output; clear it first, leaving zero output on FALSE. Check ST/type/node/
 * descriptor/storage metadata and exact URI agreement, including canonical dense
 * side-file TCON facts when supplied; legacy rows may have no TCON or checksum.
 * Success returns borrowed strings and scalar facts, not payload ownership.
 * No source-PU switch, file read, checksum-content proof or table/physical
 * mutation; scratch allocation exceptions propagate. */
extern BOOL DSL_IR_Image_Get_External_Tensor_Reference
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference);

/*
 * Preflight a complete converted-tensor request array in the exact active PU;
 * derive parent BLOCKs from PU_Info. Boolean preflight rejection changes no
 * physical/image rows or result slots. Commit creates symbols/definitions and
 * selected call actuals; the caller supplies count-sized results and borrowed
 * strings. No payload computation, PU activation or allocation-exception journal.
 */
extern BOOL DSL_IR_Materialize_External_Tensor_Values
                                (PU_Info *pu_info,
                                 const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST
                                     *requests,
                                 UINT32 request_count,
                                 DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT
                                     *results);

#endif /* DSL_EXTERNAL_TENSOR_INCLUDED */
