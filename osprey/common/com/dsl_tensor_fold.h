/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_tensor_fold_INCLUDED
#define dsl_tensor_fold_INCLUDED

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
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
#define DSL_TENSOR_TCON_INVALID_ID 0
#define DSL_TENSOR_TCON_RECORD_SIZE 120
#define DSL_TENSOR_TCON_TABLE_CAPACITY 4096

typedef UINT32 DSL_TENSOR_TCON_ID;

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
    DSL_TENSOR_TCON_ID id;
    UINT32 version;
    DSL_TENSOR_TCON_STORAGE_KIND storage_kind;
    UINT32 flags;
    UINT32 required_alignment;
    UINT32 element_size;
    UINT32 carrier_token;
    UINT32 dense_payload_ref;
    UINT32 dense_payload_bytes;
    STR_IDX side_file;
    TY_IDX descriptor_ty;
    TCON_IDX scalar_tcon;
    TYPE_ID element_mtype;
    UINT32 reserved0;
} DSL_TENSOR_TCON_RECORD;

typedef char DSL_TENSOR_TCON_RECORD_SIZE_ASSERT
    [(sizeof(DSL_TENSOR_TCON_RECORD) == DSL_TENSOR_TCON_RECORD_SIZE) ? 1 : -1];

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
    STR_IDX side_file;
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
extern void DSL_Tensor_TCON_Reset (void);
extern UINT32 DSL_Tensor_TCON_Count (void);
extern BOOL DSL_Tensor_TCON_Get
                                (DSL_TENSOR_TCON_ID id,
                                 DSL_TENSOR_TCON_RECORD *record);
extern BOOL DSL_Tensor_TCON_Is_Carrier
                                (const TCON *carrier,
                                 DSL_TENSOR_TCON_ID *id);
extern BOOL DSL_Tensor_TCON_Create_Carrier
                                (DSL_TENSOR_TCON_ID id,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Zero
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 DSL_TENSOR_TCON_ID *id,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_One
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 DSL_TENSOR_TCON_ID *id,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Splat
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 DSL_TENSOR_TCON_ID *id,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Inline_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 DSL_TENSOR_TCON_ID *id,
                                 TCON *carrier);
extern BOOL DSL_Tensor_TCON_Create_Side_File_Dense
                                (const DSL_TENSOR_TCON_CREATE_INFO *info,
                                 DSL_TENSOR_TCON_ID *id,
                                 TCON *carrier);
extern UINT64 DSL_Tensor_TCON_Semantic_Hash
                                (DSL_TENSOR_TCON_ID id);
extern BOOL DSL_Tensor_TCON_Semantic_Equal
                                (DSL_TENSOR_TCON_ID left,
                                 DSL_TENSOR_TCON_ID right);
extern BOOL DSL_Tensor_TCON_Get_Element_TCON
                                (DSL_TENSOR_TCON_ID id,
                                 UINT64 element_index,
                                 TCON_IDX *element_tcon);

#endif /* dsl_tensor_fold_INCLUDED */
