/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes tensor constant evaluation plus the existing persistent
 * TCON string-carrier envelope and process-local descriptions/cache. Caller
 * phases own optimization controls, PU scope and logical replacement publication.
 * No new envelope, table, mapped-image layout or reader/writer contract is added.
 * Design: doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md, M2/M3 storage and folding.
 */

#ifndef dsl_tensor_fold_INCLUDED
#define dsl_tensor_fold_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "srcpos.h"
#include "symtab_idx.h"
#include "targ_const.h"

/*
 * The envelope below is the existing 128-byte persistent carrier header;
 * creation-info pointers, policies, candidates, results and replacement
 * descriptions are process-local handoffs. Evaluation can intern TCON/string
 * storage, but only the caller publishes WN/ST/managed-image replacements.
 */

#define DSL_TENSOR_FOLD_MAX_RESULTS 2
#define DSL_TENSOR_FOLD_SCALAR_TEXT_SIZE 32
#define DSL_TENSOR_TCON_MAGIC 0x44535443U
#define DSL_TENSOR_TCON_VERSION 1
#define DSL_TENSOR_TCON_ENVELOPE_SIZE 128

typedef enum {
    DSL_TENSOR_TCON_STORAGE_ZERO = 0,
    DSL_TENSOR_TCON_STORAGE_ONE = 1,
    DSL_TENSOR_TCON_STORAGE_SPLAT = 2,
    DSL_TENSOR_TCON_STORAGE_INLINE_DENSE = 3,
    DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE = 4
} DSL_TENSOR_TCON_STORAGE_KIND;

typedef struct {
    UINT64 element_count;
    UINT64 logical_bytes;
    UINT64 byte_offset;
    UINT64 byte_length;
    UINT64 checksum_hi;
    UINT64 checksum_lo;
    INT64 scalar_integer_value;
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_size;
    DSL_TENSOR_TCON_STORAGE_KIND storage_kind;
    UINT32 flags;
    TY_IDX descriptor_ty;
    TCON_IDX scalar_tcon;
    UINT32 element_mtype;
    UINT32 element_size;
    UINT32 required_alignment;
    UINT32 side_path_offset;
    UINT32 side_path_length;
    UINT32 dense_offset;
    UINT32 dense_length;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_TENSOR_TCON_ENVELOPE;

typedef DSL_TENSOR_TCON_ENVELOPE DSL_TENSOR_TCON_RECORD;

typedef char DSL_TENSOR_TCON_ENVELOPE_SIZE_ASSERT
    [(sizeof(DSL_TENSOR_TCON_ENVELOPE) == DSL_TENSOR_TCON_ENVELOPE_SIZE) ? 1 : -1];

typedef struct {
    TY_IDX descriptor_ty;
    TCON_IDX scalar_tcon;
    TYPE_ID element_mtype;
    UINT64 element_count;
    UINT64 logical_bytes;
    UINT32 required_alignment;
    UINT32 element_size;
    INT64 scalar_integer_value;
    const unsigned char *dense_bytes;
    UINT32 dense_bytes_length;
    const char *side_path;
    UINT32 side_path_length;
    UINT64 byte_offset;
    UINT64 byte_length;
    UINT64 checksum_hi;
    UINT64 checksum_lo;
} DSL_TENSOR_TCON_CREATE_INFO;

typedef enum {
    DSL_TENSOR_FOLD_RESULT_NONE = 0,
    DSL_TENSOR_FOLD_RESULT_TCON = 1
} DSL_TENSOR_FOLD_RESULT_KIND;

typedef enum {
    DSL_TENSOR_FOLD_NOT_APPLICABLE = 0,
    DSL_TENSOR_FOLD_SUCCESS = 1,
    DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR = 2,
    DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_OPERATOR = 3,
    DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE = 4,
    DSL_TENSOR_FOLD_REJECT_EFFECTFUL_OPERATOR = 5,
    DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND = 6,
    DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH = 7,
    DSL_TENSOR_FOLD_REJECT_UNRESOLVED_SHAPE = 8,
    DSL_TENSOR_FOLD_REJECT_NUMERIC_POLICY = 9,
    DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET = 10,
    DSL_TENSOR_FOLD_REJECT_WORK_BUDGET = 11,
    DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY = 12,
    DSL_TENSOR_FOLD_REJECT_DIVISION_BY_ZERO = 13,
    DSL_TENSOR_FOLD_REJECT_TARGET_CAPABILITY = 14,
    DSL_TENSOR_FOLD_REJECT_PROFITABILITY = 15,
    DSL_TENSOR_FOLD_INVALID = 16
} DSL_TENSOR_FOLD_STATUS;

typedef enum {
    DSL_TENSOR_FOLD_EVAL_ORDINARY = 0,
    DSL_TENSOR_FOLD_EVAL_PROJECTABLE_DIVREM = 1
} DSL_TENSOR_FOLD_EVALUATOR_KIND;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    DSL_TENSOR_FOLD_EVALUATOR_KIND kind;
    UINT16 max_results;
    UINT32 flags;
} DSL_TENSOR_FOLD_EVALUATOR_ID;

typedef struct {
    UINT64 max_result_elements;
    UINT64 max_inline_bytes;
    UINT64 max_side_file_bytes;
    UINT64 max_evaluator_work;
    BOOL preserve_compact_splats;
} DSL_TENSOR_FOLD_POLICY;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    UINT16 result_count;
    UINT32 operand_count;
    const TCON *operands;
    const TY_IDX *operand_ty;
    const TY_IDX *result_ty;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    UINT32 attribute_count;
    const DSL_TENSOR_FOLD_POLICY *policy;
    UINT32 flags;
} DSL_TENSOR_FOLD_CANDIDATE;

enum {
    DSL_TENSOR_FOLD_CANDIDATE_EFFECTFUL = 0x00000001,
    DSL_TENSOR_FOLD_CANDIDATE_UNRESOLVED_SHAPE = 0x00000002
};

typedef struct {
    DSL_TENSOR_FOLD_RESULT_KIND kind;
    TY_IDX result_ty;
    TCON result;
    UINT32 flags;
} DSL_TENSOR_FOLD_RESULT_SLOT;

typedef struct {
    DSL_TENSOR_FOLD_STATUS status;
    DSL_TENSOR_FOLD_STATUS rejection;
    UINT16 result_count;
    UINT16 rejected_result_index;
    DSL_TENSOR_FOLD_RESULT_SLOT results[DSL_TENSOR_FOLD_MAX_RESULTS];
} DSL_TENSOR_FOLD_OUTPUT;

typedef struct {
    DSL_TENSOR_FOLD_STATUS status;
    UINT16 result_count;
    UINT16 rejected_result_index;
    UINT32 flags;
} DSL_TENSOR_FOLD_MOCK_RESPONSE;

typedef enum {
    DSL_TENSOR_FOLD_VALUE_NONE = 0,
    DSL_TENSOR_FOLD_VALUE_COMPACT_TCON = 1
} DSL_TENSOR_FOLD_VALUE_KIND;

enum {
    DSL_TENSOR_FOLD_VALUE_FLAG_COMPACT = 0x00000001,
    DSL_TENSOR_FOLD_REPLACEMENT_REVISIT_PARENTS = 0x00000001
};

typedef struct {
    DSL_TENSOR_FOLD_VALUE_KIND kind;
    TCON_IDX tcon_idx;
    TCON carrier;
    DSL_TENSOR_TCON_RECORD tensor_record;
    TY_IDX ty;
    ST_IDX st;
    DSL_IR_VALUE_ID dsl_value_id;
    UINT32 flags;
} DSL_TENSOR_FOLD_VALUE;

/*
 * STR_IDX and SRCPOS members are borrowed pass-through identities.  This
 * service does not own or mutate their source tables.
 */
typedef struct {
    TY_IDX result_ty;
    ST_IDX result_st;
    SRCPOS source_position;
    DSL_IR_NODE_ID origin_node_id;
    DSL_IR_VALUE_ID origin_result_value_id;
    STR_IDX result_name;
    STR_IDX metadata;
    STR_IDX lineage;
    UINT32 flags;
} DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT;

typedef struct {
    DSL_TENSOR_FOLD_STATUS status;
    DSL_OPERATOR logical_operator;
    UINT16 version;
    UINT16 result_count;
    TCON_IDX result_tcon_idx;
    TCON result_tcon;
    TY_IDX result_ty;
    ST_IDX result_st;
    TY_IDX descriptor_ty;
    DSL_TENSOR_TCON_STORAGE_KIND result_storage_kind;
    TCON_IDX scalar_tcon;
    INT64 scalar_integer_value;
    char compact_scalar_text[DSL_TENSOR_FOLD_SCALAR_TEXT_SIZE];
    SRCPOS source_position;
    DSL_IR_NODE_ID origin_node_id;
    DSL_IR_VALUE_ID origin_result_value_id;
    STR_IDX result_name;
    STR_IDX metadata;
    STR_IDX lineage;
    UINT32 flags;
} DSL_TENSOR_FOLD_REPLACEMENT;

/* Unless an API says otherwise, callers provide stable, readable count-sized
 * inputs and writable, nonoverlapping outputs in a coherent compiler-table
 * context. None of these services selects a PU or supplies cross-PU authority.
 */

/* Zero a nonnull caller policy and enable compact-splat preservation; zero
 * limits mean unspecified/unbounded, not a measured target budget. Ignore NULL. */
extern void DSL_Tensor_Fold_Default_Policy
                                (DSL_TENSOR_FOLD_POLICY *policy);
/* Disable and clear the process-global test response, without touching TCONs
 * or caller records. Callers serialize mock use and reset it after testing. */
extern void DSL_Tensor_Fold_Reset_Mock_Evaluator (void);
/* Copy and enable a process-global synthetic response after checking status/
 * result-count consistency. FALSE preserves the prior response; rejected index
 * and flags are not validated. Mock SUCCESS supplies no actual folded TCON. */
extern BOOL DSL_Tensor_Fold_Set_Mock_Response
                                (const DSL_TENSOR_FOLD_MOCK_RESPONSE
                                     *response);
/* Read the process-global mock switch without consuming or changing it;
 * TRUE means synthetic response dispatch, not target folding capability. */
extern BOOL DSL_Tensor_Fold_Mock_Evaluator_Enabled (void);
/* Recognize v1 ADD/MUL/DIV/REM (one result) or DIVREM (two). Clear optional
 * output before lookup; NULL probes support. This is an identity inventory,
 * not validation of candidate types, arity, target support or permissions. */
extern BOOL DSL_Tensor_Fold_Get_Evaluator
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_TENSOR_FOLD_EVALUATOR_ID *evaluator);
/* Return one ordinary or two projectable-DIVREM result slots, zero for an
 * unknown kind. No candidate validation, allocation or registry mutation. */
extern UINT16 DSL_Tensor_Fold_Max_Results
                                (DSL_TENSOR_FOLD_EVALUATOR_KIND kind);
/* Borrow a static status spelling, or "unknown" outside the enum range;
 * never free the returned pointer. No record or registry mutation. */
extern const char *DSL_Tensor_Fold_Status_Name
                                (DSL_TENSOR_FOLD_STATUS status);
/* Classify only the defined unsupported-evaluator through profitability
 * status interval as rejection; INVALID and unknown values are not included. */
extern BOOL DSL_Tensor_Fold_Status_Is_Rejection
                                (DSL_TENSOR_FOLD_STATUS status);
/* Preflight a borrowed candidate's arrays/counts, registered pure operator,
 * reviewed evaluator, unresolved/effect flags, result limit and operand-count
 * work budget. Set optional reason to SUCCESS then the first rejection. Does
 * not inspect carrier contents, array elements, descriptor equivalence, exact
 * arity, attribute semantics or every policy field; evaluation checks more. */
extern BOOL DSL_Tensor_Fold_Candidate_Valid
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 DSL_TENSOR_FOLD_STATUS *reason);
/* Clear optional output, then validate and evaluate compact integral v1
 * ADD/MUL/DIV/REM/DIVREM through Targ_WhirlOp, or the enabled synthetic mock.
 * Require stable count-sized arrays, coherent compiler tables and caller-owned
 * phase/master-option legality: this API does not test Enable_WN_Simp.
 * NULL output still evaluates/interns constants; success can add TCON/string
 * state and late rejection can retain unreferenced entries. No WN/ST/image
 * publication or general rollback. Mock slots are not real folded constants. */
extern DSL_TENSOR_FOLD_STATUS Targ_DSL_WhirlOp
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 DSL_TENSOR_FOLD_OUTPUT *output);
/* Zero a nonnull caller-owned value description; ignore NULL. Do not release
 * its former borrowed table identities or reset global constant storage. */
extern void DSL_Tensor_Fold_Value_Init
                                (DSL_TENSOR_FOLD_VALUE *value);
/* Clear nonnull value and initialize optional reason before validating an
 * existing compact ZERO/ONE/SPLAT carrier. A nonzero expected_ty must match
 * exactly. Copy st/dsl_value_id as unverified caller identities; no symbol/PU
 * ownership proof, dense loading, interning or logical IR publication. */
extern BOOL DSL_Tensor_Fold_Identify_Compact_TCON
                                (TCON_IDX tcon_idx,
                                 TY_IDX expected_ty,
                                 ST_IDX st,
                                 DSL_IR_VALUE_ID dsl_value_id,
                                 DSL_TENSOR_FOLD_VALUE *value,
                                 DSL_TENSOR_FOLD_STATUS *reason);
/* Clear a nonnull replacement and set NOT_APPLICABLE; ignore NULL. Referenced
 * TCON/ST/string/metadata storage is neither freed nor rolled back. */
extern void DSL_Tensor_Fold_Replacement_Init
                                (DSL_TENSOR_FOLD_REPLACEMENT *replacement);
/* Describe one compact folded tensor constant for caller-owned publication.
 * Require readable candidate/context/result and matching nonzero context TY;
 * evaluate, find an interned carrier and copy borrowed context identities with
 * REVISIT_PARENTS. Failure clears replacement except status, but previously
 * interned TCONs/strings and cache updates remain. No ST/WN/managed-row rewrite,
 * source-position validation, context-ID ownership proof or exception journal. */
extern DSL_TENSOR_FOLD_STATUS DSL_Tensor_Fold_Describe_Replacement
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 const DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT
                                     *context,
                                 DSL_TENSOR_FOLD_REPLACEMENT *replacement);
/* Clear only the bounded derived lookup cache. Persistent Tcon_Table and
 * TCON character-array entries, existing IDs and borrowed payloads remain;
 * this is not a program/table reset or generation invalidation. */
extern void DSL_Tensor_TCON_Reset (void);
/* Clear the cache and inspect [first,limit) in coherent mapped TCON/type/
 * character tables; zero first becomes one, zero/oversized limit becomes table
 * size. Cache at most 4096 valid carriers, silently skipping others. No row
 * repair, carrier rewrite, full-table certification or returned failure status. */
extern void DSL_Tensor_TCON_Rebuild_Derived_Cache
                                (TCON_IDX first_tcon_idx,
                                 TCON_IDX limit_tcon_idx);
/* Copy a validated tensor carrier envelope to required record. Bad/zero table
 * index or NULL record returns FALSE without clearing it; once decoding starts,
 * record is zeroed even on rejection. Require coherent live backing tables;
 * no interning, payload authentication or independent shape-count proof. */
extern BOOL DSL_Tensor_TCON_Get
                                (TCON_IDX tcon_idx,
                                 DSL_TENSOR_TCON_RECORD *record);
/* Clear nonnull carrier first, then copy a bounded TCON row and validate it.
 * FALSE after copying can leave the invalid raw TCON in output; NULL is not a
 * probe. No cache/table writes or external file access. */
extern BOOL DSL_Tensor_TCON_Get_Carrier
                                (TCON_IDX tcon_idx,
                                 TCON *carrier);
/* Clear required index output, validate a borrowed carrier and find an equal
 * row in the cache then the full TCON table. Successful fallback may cache the
 * row; it does not intern a new constant. Equality requires available dense
 * bytes, not just a matching checksum/path. FALSE leaves index zero. */
extern BOOL DSL_Tensor_TCON_Find_Carrier
                                (const TCON *carrier,
                                 TCON_IDX *tcon_idx);
/* Forward Get with the same required record and asymmetric failure clearing;
 * NULL record is not a Boolean-only probe. No cache/table mutation. */
extern BOOL DSL_Tensor_TCON_Is_Carrier
                                (TCON_IDX tcon_idx,
                                 DSL_TENSOR_TCON_RECORD *record);
/* Validate a borrowed string-TCON envelope against coherent live type/TCON/
 * character tables, copying to optional record (NULL probes validity). Clear
 * record first; reject bad layout/fields/storage. This copies existing mapped
 * carrier bytes, not an object stream. No file I/O, checksum computation,
 * scalar-marker value proof, descriptor shape-count proof or full constructor
 * range revalidation; callers must supply valid character-table identities. */
extern BOOL DSL_Tensor_TCON_Decode_Carrier
                                (const TCON *carrier,
                                 DSL_TENSOR_TCON_RECORD *record);
/* Clear supplied outputs; require both, then borrow a validated carrier's
 * counted path bytes. They are not necessarily NUL-terminated. No file open,
 * path resolution or payload authentication; lifetime follows the backing TCON
 * character storage, not the derived cache; do not retain across storage
 * growth/reset/remapping that invalidates it. FALSE leaves NULL/zero. */
extern BOOL DSL_Tensor_TCON_Get_Side_Path
                                (TCON_IDX tcon_idx,
                                 const char **bytes,
                                 UINT32 *length);
/* Clear supplied outputs; require both, then borrow available embedded dense
 * bytes from an inline or side-file carrier. Never read a side file; absent
 * embedded bytes reject. Lifetime follows backing character storage; do not
 * retain across storage growth/reset/remapping that invalidates it. Digest
 * contents are not authenticated. FALSE leaves NULL/zero. */
extern BOOL DSL_Tensor_TCON_Get_Dense_Bytes
                                (TCON_IDX tcon_idx,
                                 const unsigned char **bytes,
                                 UINT32 *length);
/* Print a validated carrier's compact or dense/side-file facts to a borrowed
 * stream, without a trailing newline, table writes or payload reads. NULL/bad
 * input rejects; later failure may leave partial text. TRUE does not report
 * I/O success or authenticate the displayed checksum. */
extern BOOL DSL_Tensor_TCON_Print
                                (FILE *file,
                                 TCON_IDX tcon_idx);
/* Common creation contract for Zero/One/Splat/Inline_Dense/Side_File_Dense:
 * borrow stable info and readable count-sized input buffers in coherent tables.
 * Check canonical descriptor, supplied geometry/product, scalar type or dense
 * storage rules, but do not prove shape/count or checksum/scalar-value truth.
 * Zero optional index first; carrier changes only on TRUE. Intern copied bytes
 * in the TCON character array, then reuse a bounded cached semantic match or
 * Enter_tcon. Cache hits can choose a different equivalent dense storage form.
 * FALSE may retain string/table allocations; no exception journal or independent
 * Enter_tcon return-index success check. NULL outputs still create constants.
 */

/* Create/intern a ZERO carrier using the common creation contract above.
 * Require scalar_integer_value zero and a matching typed scalar TCON supplied
 * by caller; this marker check does not prove that TCON's actual value is zero. */
extern BOOL DSL_Tensor_TCON_Create_Zero
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Create/intern a ONE carrier using the common creation contract above.
 * Require scalar_integer_value one and a matching typed scalar TCON supplied
 * by caller; this marker check does not prove that TCON's actual value is one. */
extern BOOL DSL_Tensor_TCON_Create_One
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Create/intern a SPLAT carrier using the common creation contract above.
 * Caller ensures the typed scalar TCON and scalar_integer_value describe the
 * same value; this service checks the scalar type, not that correspondence. */
extern BOOL DSL_Tensor_TCON_Create_Splat
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Create a compact exact integral scalar over a canonical static descriptor.
 * Require nonnull index output; zero it first, derive count/bytes/alignment,
 * round-trip through target scalar representation and intern scalar/carrier.
 * Late FALSE may retain scalar/string entries; native failure preserves carrier.
 * The standalone test-stub build instead clears both outputs and always fails.
 * No WN/ST publication, exception rollback or PU selection. */
extern BOOL DSL_Tensor_TCON_Create_Integer_Splat
                                (TY_IDX descriptor_ty,
                                 INT64 scalar_value,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Copy counted dense bytes into an inline carrier using the common creation
 * contract above; require exact logical length and nonzero checksum words.
 * Caller supplies correct target-format bytes and digest; neither is computed
 * or authenticated. No borrowed input buffer survives successful copying. */
extern BOOL DSL_Tensor_TCON_Create_Inline_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Copy a restricted relative path and optional available dense bytes into a
 * side-file carrier using the common creation contract above. Require aligned,
 * nonwrapping byte range of logical length and nonzero checksum words. A
 * positive embedded-byte count requires a readable nonnull buffer from caller.
 * No file I/O or digest authentication; cached byte-proven equality can return
 * an existing inline carrier rather than retain the requested storage location. */
extern BOOL DSL_Tensor_TCON_Create_Side_File_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
/* Hash selected fields of a validated row, or return zero on invalid input.
 * Dense hashing uses type/geometry/checksum metadata, not bytes, path, storage
 * kind or alignment; collisions require Semantic_Equal, not hash-only proof. */
extern UINT64 DSL_Tensor_TCON_Semantic_Hash
                                (TCON_IDX tcon_idx);
/* Compare validated rows without table writes. Same valid index succeeds even
 * without dense bytes; distinct dense rows require matching type/geometry/
 * checksum AND available identical bytes, ignoring path/location/alignment.
 * Compact comparison uses storage kind, exact descriptor and scalar IDs/value;
 * not a cryptographic proof or general tensor numerical equivalence. */
extern BOOL DSL_Tensor_TCON_Semantic_Equal
                                (TCON_IDX left,
                                 TCON_IDX right);
/* Zero required output and return the existing scalar TCON for a bounded
 * ZERO/ONE/SPLAT element. Dense storage rejects even if bytes are embedded;
 * no extraction, interning, file access or output on failure. */
extern BOOL DSL_Tensor_TCON_Get_Element_TCON
                                (TCON_IDX tcon_idx,
                                 UINT64 element_index,
                                 TCON_IDX *element_tcon);

#endif /* dsl_tensor_fold_INCLUDED */
