/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "dsl_tensor_fold.h"
#include "strtab.h"

static const char *DSL_tensor_fold_status_name[] = {
    "not_applicable",
    "success",
    "reject_unsupported_evaluator",
    "reject_unsupported_operator",
    "reject_malformed_candidate",
    "reject_effectful_operator",
    "reject_non_constant_operand",
    "reject_descriptor_mismatch",
    "reject_unresolved_shape",
    "reject_numeric_policy",
    "reject_result_budget",
    "reject_work_budget",
    "reject_materialization_policy",
    "reject_division_by_zero",
    "reject_target_capability",
    "reject_profitability",
    "invalid"
};

static BOOL DSL_tensor_fold_mock_enabled = FALSE;
static DSL_TENSOR_FOLD_MOCK_RESPONSE DSL_tensor_fold_mock_response;
static DSL_TENSOR_TCON_RECORD
    DSL_tensor_tcon_records[DSL_TENSOR_TCON_TABLE_CAPACITY];
static UINT32 DSL_tensor_tcon_record_count = 0;
static const char *DSL_tensor_tcon_carrier_prefix =
    "__WHIRL_DSL_TCON__:v1:";

static void
DSL_Tensor_Fold_Clear_Output (DSL_TENSOR_FOLD_OUTPUT *output)
{
    if (output != NULL)
        memset(output, 0, sizeof(*output));
}

static void
DSL_Tensor_Fold_Set_Output_Status
        (DSL_TENSOR_FOLD_OUTPUT *output,
         DSL_TENSOR_FOLD_STATUS status)
{
    if (output == NULL)
        return;

    output->status = status;
    output->rejection = DSL_Tensor_Fold_Status_Is_Rejection(status) ?
                            status : DSL_TENSOR_FOLD_NOT_APPLICABLE;
}

static BOOL
DSL_Tensor_TCON_Alignment_Valid (UINT32 alignment)
{
    return alignment != 0 && (alignment & (alignment - 1)) == 0;
}

static BOOL
DSL_Tensor_TCON_Checksum_Valid (UINT64 checksum_hi, UINT64 checksum_lo)
{
    return checksum_hi != 0 || checksum_lo != 0;
}

static BOOL
DSL_Tensor_TCON_Range_Valid (UINT64 offset, UINT64 length)
{
    return length != 0 && offset + length >= offset;
}

static BOOL
DSL_Tensor_TCON_Mul_Exact (UINT64 left, UINT32 right, UINT64 *result)
{
    if (result != NULL)
        *result = 0;
    if (right != 0 && left > (~0ULL / right))
        return FALSE;
    if (result != NULL)
        *result = left * right;
    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Relative_Path_Valid (STR_IDX path)
{
    const char *text;
    const char *component;
    const char *cursor;

    if (path == STR_IDX_ZERO || path >= STR_Table_Size())
        return FALSE;

    text = Index_To_Str(path);
    if (text == NULL || text[0] == '\0' || text[0] == '/' ||
        text[0] == '\\')
        return FALSE;

    component = text;
    for (cursor = text; ; ++cursor) {
        if (*cursor == '\\')
            return FALSE;
        if (*cursor == '/' || *cursor == '\0') {
            if (cursor == component)
                return FALSE;
            if ((cursor - component == 1 && component[0] == '.') ||
                (cursor - component == 2 &&
                 component[0] == '.' && component[1] == '.'))
                return FALSE;
            if (*cursor == '\0')
                break;
            component = cursor + 1;
        }
    }
    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Common_Info_Valid
        (const DSL_TENSOR_TCON_CREATE_INFO *info)
{
    UINT64 computed_bytes;

    if (info == NULL ||
        info->descriptor_ty == TY_IDX_ZERO ||
        info->element_mtype == MTYPE_UNKNOWN ||
        info->element_size == 0 ||
        info->element_count == 0 ||
        info->logical_bytes == 0 ||
        !DSL_Tensor_TCON_Alignment_Valid(info->required_alignment) ||
        info->required_alignment < info->element_size)
        return FALSE;

    if (!DSL_Tensor_TCON_Mul_Exact(info->element_count,
                                   info->element_size,
                                   &computed_bytes) ||
        computed_bytes != info->logical_bytes)
        return FALSE;

    return TRUE;
}

static UINT64
DSL_Tensor_TCON_Hash_Combine (UINT64 hash, UINT64 value)
{
    hash ^= value;
    hash *= 1099511628211ULL;
    return hash;
}

static BOOL
DSL_Tensor_TCON_Is_Dense
        (DSL_TENSOR_TCON_STORAGE_KIND storage_kind)
{
    return storage_kind == DSL_TENSOR_TCON_STORAGE_INLINE_DENSE ||
           storage_kind == DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE;
}

static BOOL
DSL_Tensor_TCON_Record_Semantic_Equal
        (const DSL_TENSOR_TCON_RECORD *left,
         const DSL_TENSOR_TCON_RECORD *right)
{
    const char *left_bytes;
    const char *right_bytes;

    if (left == NULL || right == NULL)
        return FALSE;

    if (DSL_Tensor_TCON_Is_Dense(left->storage_kind) &&
        DSL_Tensor_TCON_Is_Dense(right->storage_kind)) {
        if (left->descriptor_ty != right->descriptor_ty ||
            left->element_mtype != right->element_mtype ||
            left->element_size != right->element_size ||
            left->element_count != right->element_count ||
            left->logical_bytes != right->logical_bytes ||
            left->checksum_hi != right->checksum_hi ||
            left->checksum_lo != right->checksum_lo)
            return FALSE;

        if (left->dense_payload_ref == 0 ||
            right->dense_payload_ref == 0 ||
            left->dense_payload_bytes != right->dense_payload_bytes)
            return FALSE;

        left_bytes = Index_to_char_array(left->dense_payload_ref);
        right_bytes = Index_to_char_array(right->dense_payload_ref);
        return left_bytes != NULL &&
               right_bytes != NULL &&
               memcmp(left_bytes, right_bytes,
                      left->dense_payload_bytes) == 0;
    }

    return left->storage_kind == right->storage_kind &&
           left->descriptor_ty == right->descriptor_ty &&
           left->element_mtype == right->element_mtype &&
           left->element_size == right->element_size &&
           left->element_count == right->element_count &&
           left->logical_bytes == right->logical_bytes &&
           left->scalar_tcon == right->scalar_tcon &&
           left->scalar_integer_value == right->scalar_integer_value;
}

static UINT64
DSL_Tensor_TCON_Record_Semantic_Hash
        (const DSL_TENSOR_TCON_RECORD *record)
{
    UINT64 hash = 1469598103934665603ULL;

    if (record == NULL)
        return 0;

    if (DSL_Tensor_TCON_Is_Dense(record->storage_kind)) {
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->descriptor_ty);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_mtype);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_size);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_count);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->logical_bytes);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->checksum_hi);
        hash = DSL_Tensor_TCON_Hash_Combine(hash, record->checksum_lo);
        return hash;
    }

    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->storage_kind);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->descriptor_ty);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_mtype);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_size);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->element_count);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->logical_bytes);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->scalar_tcon);
    hash = DSL_Tensor_TCON_Hash_Combine(hash, record->scalar_integer_value);
    return hash;
}

static DSL_TENSOR_TCON_ID
DSL_Tensor_TCON_Find_Equal (const DSL_TENSOR_TCON_RECORD *record)
{
    UINT32 i;

    for (i = 0; i < DSL_tensor_tcon_record_count; ++i) {
        if (DSL_Tensor_TCON_Record_Semantic_Equal
                (&DSL_tensor_tcon_records[i], record))
            return DSL_tensor_tcon_records[i].id;
    }

    return DSL_TENSOR_TCON_INVALID_ID;
}

static BOOL
DSL_Tensor_TCON_Init_Carrier
        (DSL_TENSOR_TCON_ID id,
         DSL_TENSOR_TCON_RECORD *record,
         TCON *carrier)
{
    char token[96];
    UINT32 token_idx;
    UINT32 token_length;

    if (id == DSL_TENSOR_TCON_INVALID_ID)
        return FALSE;

    snprintf(token, sizeof(token), "%s%u", DSL_tensor_tcon_carrier_prefix,
             (unsigned int)id);
    token_length = strlen(token) + 1;
    token_idx = Save_StrN(token, token_length);
    if (token_idx == 0)
        return FALSE;

    if (record != NULL)
        record->carrier_token = token_idx;
    if (carrier != NULL) {
        TCON_clear(*carrier);
        Set_TCON_string_payload(*carrier, MTYPE_STR, token_idx,
                                token_length);
        Set_TCON_dsl_tensor_carrier(*carrier);
    }

    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Create_Record
        (DSL_TENSOR_TCON_STORAGE_KIND storage_kind,
         const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    DSL_TENSOR_TCON_RECORD record;
    DSL_TENSOR_TCON_ID existing_id;

    if (id != NULL)
        *id = DSL_TENSOR_TCON_INVALID_ID;

    if (!DSL_Tensor_TCON_Common_Info_Valid(info))
        return FALSE;

    memset(&record, 0, sizeof(record));
    record.version = 1;
    record.storage_kind = storage_kind;
    record.descriptor_ty = info->descriptor_ty;
    record.scalar_tcon = info->scalar_tcon;
    record.element_mtype = info->element_mtype;
    record.element_size = info->element_size;
    record.scalar_integer_value = info->scalar_integer_value;
    record.element_count = info->element_count;
    record.logical_bytes = info->logical_bytes;
    record.required_alignment = info->required_alignment;
    record.side_file = info->side_file;
    record.byte_offset = info->byte_offset;
    record.byte_length = info->byte_length;
    record.checksum_hi = info->checksum_hi;
    record.checksum_lo = info->checksum_lo;

    switch (storage_kind) {
    case DSL_TENSOR_TCON_STORAGE_ZERO:
        if (info->scalar_tcon == TCON_IDX_ZERO ||
            info->scalar_integer_value != 0)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_ONE:
        if (info->scalar_tcon == TCON_IDX_ZERO ||
            info->scalar_integer_value != 1)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_SPLAT:
        if (info->scalar_tcon == TCON_IDX_ZERO)
            return FALSE;
        if (info->dense_bytes != NULL || info->dense_bytes_length != 0)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_INLINE_DENSE:
        if (info->dense_bytes == NULL ||
            info->dense_bytes_length == 0 ||
            info->dense_bytes_length != info->logical_bytes ||
            !DSL_Tensor_TCON_Checksum_Valid(info->checksum_hi,
                                            info->checksum_lo))
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE:
        if (!DSL_Tensor_TCON_Relative_Path_Valid(info->side_file) ||
            !DSL_Tensor_TCON_Range_Valid(info->byte_offset,
                                         info->byte_length) ||
            info->byte_length != info->logical_bytes ||
            (info->byte_offset % info->required_alignment) != 0 ||
            (info->dense_bytes_length != 0 &&
             info->dense_bytes_length != info->logical_bytes) ||
            !DSL_Tensor_TCON_Checksum_Valid(info->checksum_hi,
                                            info->checksum_lo))
            return FALSE;
        break;
    default:
        return FALSE;
    }

    if (info->dense_bytes != NULL && info->dense_bytes_length != 0) {
        record.dense_payload_ref =
            Save_StrN((const char *)info->dense_bytes,
                      info->dense_bytes_length);
        if (record.dense_payload_ref == 0)
            return FALSE;
        record.dense_payload_bytes = info->dense_bytes_length;
    }

    existing_id = DSL_Tensor_TCON_Find_Equal(&record);
    if (existing_id != DSL_TENSOR_TCON_INVALID_ID) {
        if (id != NULL)
            *id = existing_id;
        return DSL_Tensor_TCON_Create_Carrier(existing_id, carrier);
    }

    if (DSL_tensor_tcon_record_count >= DSL_TENSOR_TCON_TABLE_CAPACITY)
        return FALSE;

    record.id = DSL_tensor_tcon_record_count + 1;
    if (!DSL_Tensor_TCON_Init_Carrier(record.id, &record, carrier))
        return FALSE;

    DSL_tensor_tcon_records[DSL_tensor_tcon_record_count++] = record;
    if (id != NULL)
        *id = record.id;
    return TRUE;
}

void
DSL_Tensor_Fold_Default_Policy (DSL_TENSOR_FOLD_POLICY *policy)
{
    if (policy == NULL)
        return;

    memset(policy, 0, sizeof(*policy));
    policy->preserve_compact_splats = TRUE;
}

void
DSL_Tensor_Fold_Reset_Mock_Evaluator (void)
{
    DSL_tensor_fold_mock_enabled = FALSE;
    memset(&DSL_tensor_fold_mock_response, 0,
           sizeof(DSL_tensor_fold_mock_response));
}

BOOL
DSL_Tensor_Fold_Set_Mock_Response
        (const DSL_TENSOR_FOLD_MOCK_RESPONSE *response)
{
    if (response == NULL ||
        response->result_count > DSL_TENSOR_FOLD_MAX_RESULTS)
        return FALSE;

    switch (response->status) {
    case DSL_TENSOR_FOLD_SUCCESS:
        if (response->result_count == 0)
            return FALSE;
        break;
    case DSL_TENSOR_FOLD_NOT_APPLICABLE:
        if (response->result_count != 0)
            return FALSE;
        break;
    default:
        if (!DSL_Tensor_Fold_Status_Is_Rejection(response->status) ||
            response->result_count != 0)
            return FALSE;
        break;
    }

    DSL_tensor_fold_mock_response = *response;
    DSL_tensor_fold_mock_enabled = TRUE;
    return TRUE;
}

BOOL
DSL_Tensor_Fold_Mock_Evaluator_Enabled (void)
{
    return DSL_tensor_fold_mock_enabled;
}

BOOL
DSL_Tensor_Fold_Get_Evaluator
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_TENSOR_FOLD_EVALUATOR_ID *evaluator)
{
    if (evaluator != NULL)
        memset(evaluator, 0, sizeof(*evaluator));

    if (version != 1)
        return FALSE;

    switch (dsl_operator) {
    case OPR_DSLADD:
    case OPR_DSLMUL:
        if (evaluator != NULL) {
            evaluator->dsl_operator = dsl_operator;
            evaluator->version = version;
            evaluator->kind = DSL_TENSOR_FOLD_EVAL_ORDINARY;
            evaluator->max_results = 1;
        }
        return TRUE;
    default:
        return FALSE;
    }
}

UINT16
DSL_Tensor_Fold_Max_Results (DSL_TENSOR_FOLD_EVALUATOR_KIND kind)
{
    switch (kind) {
    case DSL_TENSOR_FOLD_EVAL_ORDINARY:
        return 1;
    case DSL_TENSOR_FOLD_EVAL_PROJECTABLE_DIVREM:
        return DSL_TENSOR_FOLD_MAX_RESULTS;
    default:
        return 0;
    }
}

const char *
DSL_Tensor_Fold_Status_Name (DSL_TENSOR_FOLD_STATUS status)
{
    UINT32 ordinal = (UINT32)status;

    if (ordinal >= sizeof(DSL_tensor_fold_status_name) /
                   sizeof(DSL_tensor_fold_status_name[0]))
        return "unknown";
    return DSL_tensor_fold_status_name[ordinal];
}

BOOL
DSL_Tensor_Fold_Status_Is_Rejection (DSL_TENSOR_FOLD_STATUS status)
{
    return status >= DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR &&
           status <= DSL_TENSOR_FOLD_REJECT_PROFITABILITY;
}

BOOL
DSL_Tensor_Fold_Candidate_Valid
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_STATUS *reason)
{
    DSL_TENSOR_FOLD_EVALUATOR_ID evaluator;
    DSL_OPERATOR_INFO operator_info;
    const DSL_TENSOR_FOLD_POLICY *policy;

    if (reason != NULL)
        *reason = DSL_TENSOR_FOLD_SUCCESS;

    if (candidate == NULL ||
        candidate->result_count == 0 ||
        candidate->result_count > DSL_TENSOR_FOLD_MAX_RESULTS ||
        candidate->result_ty == NULL ||
        (candidate->operand_count > 0 &&
         (candidate->operands == NULL || candidate->operand_ty == NULL)) ||
        (candidate->attribute_count > 0 &&
         candidate->attributes == NULL)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE;
        return FALSE;
    }

    if (!DSL_Operator_Get_Info_Version(candidate->dsl_operator,
                                       candidate->version, &operator_info)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_OPERATOR;
        return FALSE;
    }

    if (!DSL_Tensor_Fold_Get_Evaluator(candidate->dsl_operator,
                                       candidate->version, &evaluator)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
        return FALSE;
    }

    if (candidate->result_count > evaluator.max_results) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET;
        return FALSE;
    }

    policy = candidate->policy;
    if (policy != NULL &&
        policy->max_evaluator_work != 0 &&
        candidate->operand_count > policy->max_evaluator_work) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_WORK_BUDGET;
        return FALSE;
    }

    return TRUE;
}

static DSL_TENSOR_FOLD_STATUS
DSL_Tensor_Fold_Apply_Mock
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_OUTPUT *output)
{
    const DSL_TENSOR_FOLD_MOCK_RESPONSE *response =
        &DSL_tensor_fold_mock_response;
    UINT32 i;

    if (response->status == DSL_TENSOR_FOLD_SUCCESS &&
        response->result_count > candidate->result_count) {
        DSL_Tensor_Fold_Set_Output_Status
            (output, DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET);
        return DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET;
    }

    if (output != NULL) {
        output->status = response->status;
        output->rejection =
            DSL_Tensor_Fold_Status_Is_Rejection(response->status) ?
                response->status : DSL_TENSOR_FOLD_NOT_APPLICABLE;
        output->result_count = response->result_count;
        output->rejected_result_index = response->rejected_result_index;

        if (response->status == DSL_TENSOR_FOLD_SUCCESS) {
            for (i = 0; i < response->result_count; ++i) {
                output->results[i].kind = DSL_TENSOR_FOLD_RESULT_TCON;
                output->results[i].result_ty = candidate->result_ty[i];
                output->results[i].flags = response->flags;
            }
        }
    }

    return response->status;
}

DSL_TENSOR_FOLD_STATUS
Targ_DSL_WhirlOp
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_OUTPUT *output)
{
    DSL_TENSOR_FOLD_STATUS reason;

    DSL_Tensor_Fold_Clear_Output(output);

    if (!DSL_Tensor_Fold_Candidate_Valid(candidate, &reason)) {
        DSL_Tensor_Fold_Set_Output_Status(output, reason);
        return reason;
    }

    if (DSL_tensor_fold_mock_enabled)
        return DSL_Tensor_Fold_Apply_Mock(candidate, output);

    DSL_Tensor_Fold_Set_Output_Status(output,
                                      DSL_TENSOR_FOLD_NOT_APPLICABLE);
    return DSL_TENSOR_FOLD_NOT_APPLICABLE;
}

void
DSL_Tensor_TCON_Reset (void)
{
    memset(DSL_tensor_tcon_records, 0, sizeof(DSL_tensor_tcon_records));
    DSL_tensor_tcon_record_count = 0;
}

UINT32
DSL_Tensor_TCON_Count (void)
{
    return DSL_tensor_tcon_record_count;
}

BOOL
DSL_Tensor_TCON_Get
        (DSL_TENSOR_TCON_ID id,
         DSL_TENSOR_TCON_RECORD *record)
{
    if (id == DSL_TENSOR_TCON_INVALID_ID ||
        id > DSL_tensor_tcon_record_count ||
        record == NULL)
        return FALSE;

    *record = DSL_tensor_tcon_records[id - 1];
    return TRUE;
}

BOOL
DSL_Tensor_TCON_Is_Carrier
        (const TCON *carrier,
         DSL_TENSOR_TCON_ID *id)
{
    const char *token;
    const char *digits;
    char *end;
    unsigned long parsed_id;

    if (id != NULL)
        *id = DSL_TENSOR_TCON_INVALID_ID;

    if (carrier == NULL ||
        TCON_ty(*carrier) != MTYPE_STR ||
        !TCON_is_dsl_tensor_carrier(*carrier) ||
        TCON_str_idx(*carrier) == 0 ||
        TCON_str_len(*carrier) == 0)
        return FALSE;

    token = Index_to_char_array(TCON_str_idx(*carrier));
    if (token == NULL)
        return FALSE;

    digits = token + strlen(DSL_tensor_tcon_carrier_prefix);
    if (strncmp(token, DSL_tensor_tcon_carrier_prefix,
                strlen(DSL_tensor_tcon_carrier_prefix)) != 0 ||
        digits[0] == '\0')
        return FALSE;

    parsed_id = strtoul(digits, &end, 10);
    if (end == digits || *end != '\0' ||
        parsed_id == DSL_TENSOR_TCON_INVALID_ID ||
        parsed_id > DSL_tensor_tcon_record_count)
        return FALSE;

    if (id != NULL)
        *id = (DSL_TENSOR_TCON_ID)parsed_id;
    return TRUE;
}

BOOL
DSL_Tensor_TCON_Create_Carrier
        (DSL_TENSOR_TCON_ID id,
         TCON *carrier)
{
    DSL_TENSOR_TCON_RECORD record;

    if (!DSL_Tensor_TCON_Get(id, &record))
        return FALSE;
    return DSL_Tensor_TCON_Init_Carrier(id, NULL, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Zero
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_ZERO, info, id, carrier);
}

BOOL
DSL_Tensor_TCON_Create_One
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_ONE, info, id, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Splat
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_SPLAT, info, id, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Inline_Dense
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_INLINE_DENSE, info, id, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Side_File_Dense
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         DSL_TENSOR_TCON_ID *id,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE, info, id, carrier);
}

UINT64
DSL_Tensor_TCON_Semantic_Hash (DSL_TENSOR_TCON_ID id)
{
    DSL_TENSOR_TCON_RECORD record;

    if (!DSL_Tensor_TCON_Get(id, &record))
        return 0;
    return DSL_Tensor_TCON_Record_Semantic_Hash(&record);
}

BOOL
DSL_Tensor_TCON_Semantic_Equal
        (DSL_TENSOR_TCON_ID left,
         DSL_TENSOR_TCON_ID right)
{
    DSL_TENSOR_TCON_RECORD left_record;
    DSL_TENSOR_TCON_RECORD right_record;

    return DSL_Tensor_TCON_Get(left, &left_record) &&
           DSL_Tensor_TCON_Get(right, &right_record) &&
           DSL_Tensor_TCON_Record_Semantic_Equal
               (&left_record, &right_record);
}

BOOL
DSL_Tensor_TCON_Get_Element_TCON
        (DSL_TENSOR_TCON_ID id,
         UINT64 element_index,
         TCON_IDX *element_tcon)
{
    DSL_TENSOR_TCON_RECORD record;

    if (element_tcon != NULL)
        *element_tcon = TCON_IDX_ZERO;

    if (!DSL_Tensor_TCON_Get(id, &record) ||
        element_index >= record.element_count ||
        element_tcon == NULL)
        return FALSE;

    switch (record.storage_kind) {
    case DSL_TENSOR_TCON_STORAGE_ZERO:
    case DSL_TENSOR_TCON_STORAGE_ONE:
    case DSL_TENSOR_TCON_STORAGE_SPLAT:
        *element_tcon = record.scalar_tcon;
        return record.scalar_tcon != TCON_IDX_ZERO;
    default:
        return FALSE;
    }
}
