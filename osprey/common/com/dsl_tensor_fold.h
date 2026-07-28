/*
 * Copyright (C) 2026 Open64 Project
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
 * Shared M0 tensor constant-folding contract.
 *
 * This is a fixed-layout handoff between construction-time WN
 * simplification, the explicit VHO DSL pass, and adapted WOPT.  It declares
 * evaluator identity, bounded input/result records, materialization budgets,
 * and structured rejection reasons only.  It does not allocate tensor TCON
 * storage, publish builder replacements, or change WHIRL mapped-image layout.
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

extern void DSL_Tensor_Fold_Default_Policy
                                (DSL_TENSOR_FOLD_POLICY *policy);
extern void DSL_Tensor_Fold_Reset_Mock_Evaluator (void);
extern BOOL DSL_Tensor_Fold_Set_Mock_Response
                                (const DSL_TENSOR_FOLD_MOCK_RESPONSE
                                     *response);
extern BOOL DSL_Tensor_Fold_Mock_Evaluator_Enabled (void);
extern BOOL DSL_Tensor_Fold_Get_Evaluator
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_TENSOR_FOLD_EVALUATOR_ID *evaluator);
extern UINT16 DSL_Tensor_Fold_Max_Results
                                (DSL_TENSOR_FOLD_EVALUATOR_KIND kind);
extern const char *DSL_Tensor_Fold_Status_Name
                                (DSL_TENSOR_FOLD_STATUS status);
extern BOOL DSL_Tensor_Fold_Status_Is_Rejection
                                (DSL_TENSOR_FOLD_STATUS status);
extern BOOL DSL_Tensor_Fold_Candidate_Valid
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 DSL_TENSOR_FOLD_STATUS *reason);
extern DSL_TENSOR_FOLD_STATUS Targ_DSL_WhirlOp
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 DSL_TENSOR_FOLD_OUTPUT *output);
extern void DSL_Tensor_Fold_Value_Init
                                (DSL_TENSOR_FOLD_VALUE *value);
extern BOOL DSL_Tensor_Fold_Identify_Compact_TCON
                                (TCON_IDX tcon_idx,
                                 TY_IDX expected_ty,
                                 ST_IDX st,
                                 DSL_IR_VALUE_ID dsl_value_id,
                                 DSL_TENSOR_FOLD_VALUE *value,
                                 DSL_TENSOR_FOLD_STATUS *reason);
extern void DSL_Tensor_Fold_Replacement_Init
                                (DSL_TENSOR_FOLD_REPLACEMENT *replacement);
extern DSL_TENSOR_FOLD_STATUS DSL_Tensor_Fold_Describe_Replacement
                                (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
                                 const DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT
                                     *context,
                                 DSL_TENSOR_FOLD_REPLACEMENT *replacement);
extern void DSL_Tensor_TCON_Reset (void);
extern void DSL_Tensor_TCON_Rebuild_Derived_Cache
                                (TCON_IDX first_tcon_idx,
                                 TCON_IDX limit_tcon_idx);
extern BOOL DSL_Tensor_TCON_Get
                                (TCON_IDX tcon_idx,
                                 DSL_TENSOR_TCON_RECORD *record);
extern BOOL DSL_Tensor_TCON_Get_Carrier
                                (TCON_IDX tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Find_Carrier
                                (const TCON *carrier,
                                 TCON_IDX *tcon_idx);
extern BOOL DSL_Tensor_TCON_Is_Carrier
                                (TCON_IDX tcon_idx,
                                 DSL_TENSOR_TCON_RECORD *record);
extern BOOL DSL_Tensor_TCON_Decode_Carrier
                                (const TCON *carrier,
                                 DSL_TENSOR_TCON_RECORD *record);
extern BOOL DSL_Tensor_TCON_Get_Side_Path
                                (TCON_IDX tcon_idx,
                                 const char **bytes,
                                 UINT32 *length);
extern BOOL DSL_Tensor_TCON_Get_Dense_Bytes
                                (TCON_IDX tcon_idx,
                                 const unsigned char **bytes,
                                 UINT32 *length);
extern BOOL DSL_Tensor_TCON_Print
                                (FILE *file,
                                 TCON_IDX tcon_idx);
extern BOOL DSL_Tensor_TCON_Create_Zero
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_One
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Splat
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Integer_Splat
                                (TY_IDX descriptor_ty,
                                 INT64 scalar_value,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Inline_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Side_File_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 TCON_IDX *tcon_idx,
                                 TCON *carrier);
extern UINT64 DSL_Tensor_TCON_Semantic_Hash
                                (TCON_IDX tcon_idx);
extern BOOL DSL_Tensor_TCON_Semantic_Equal
                                (TCON_IDX left,
                                 TCON_IDX right);
extern BOOL DSL_Tensor_TCON_Get_Element_TCON
                                (TCON_IDX tcon_idx,
                                 UINT64 element_index,
                                 TCON_IDX *element_tcon);

#endif /* dsl_tensor_fold_INCLUDED */
