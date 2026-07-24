/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "dsl_tensor_fold.h"
#include "strtab.h"
#ifdef DSL_TENSOR_FOLD_TEST_STUB
extern UINT32 TCON_Table_Size (void);
extern UINT32 TY_Table_Size (void);
extern BOOL TY_is_tensor_extension (TY_IDX ty);
extern BOOL TY_tensor_is_canonical (TY_IDX ty);
extern TY_IDX TY_tensor_element_ty (TY_IDX ty);
extern TYPE_ID TY_mtype (TY_IDX ty);
extern UINT64 TY_size (TY_IDX ty);
#else
#include "symtab.h"
#endif

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
static TCON_IDX DSL_tensor_tcon_cache[4096];
static UINT32 DSL_tensor_tcon_cache_count = 0;

extern TCON_IDX Enter_tcon (const TCON& tcon);
extern TCON TCON_from_IDX (TCON_IDX tcon_idx);

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
DSL_Tensor_TCON_IDX_Valid (TCON_IDX tcon_idx)
{
    return tcon_idx != TCON_IDX_ZERO && tcon_idx < TCON_Table_Size();
}

static BOOL
DSL_Tensor_TY_IDX_Valid (TY_IDX ty)
{
    return ty != TY_IDX_ZERO && TY_IDX_index(ty) < TY_Table_Size();
}

static BOOL
DSL_Tensor_TCON_Element_Mtype_Valid (UINT32 mtype)
{
    return mtype > MTYPE_UNKNOWN && mtype <= MTYPE_LAST;
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
DSL_Tensor_TCON_Relative_Path_Valid (const char *text, UINT32 length)
{
    UINT32 component_start;
    UINT32 i;

    if (text == NULL || length == 0 || text[0] == '/' || text[0] == '\\')
        return FALSE;

    component_start = 0;
    for (i = 0; i <= length; ++i) {
        char ch = (i == length) ? '\0' : text[i];
        if (i != length && (ch == '\0' || ch == '\\'))
            return FALSE;
        if (ch == '/') {
            if (i == component_start)
                return FALSE;
            if ((i - component_start == 1 &&
                 text[component_start] == '.') ||
                (i - component_start == 2 &&
                 text[component_start] == '.' &&
                 text[component_start + 1] == '.'))
                return FALSE;
            component_start = i + 1;
        }
    }

    if (length == component_start)
        return FALSE;
    if ((length - component_start == 1 && text[component_start] == '.') ||
        (length - component_start == 2 &&
         text[component_start] == '.' && text[component_start + 1] == '.'))
        return FALSE;

    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Common_Info_Valid
        (const DSL_TENSOR_TCON_CREATE_INFO *info)
{
    UINT64 computed_bytes;
    TY_IDX element_ty;

    if (info == NULL ||
        !DSL_Tensor_TY_IDX_Valid(info->descriptor_ty) ||
        !TY_is_tensor_extension(info->descriptor_ty) ||
        !TY_tensor_is_canonical(info->descriptor_ty) ||
        !DSL_Tensor_TCON_Element_Mtype_Valid(info->element_mtype) ||
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

    element_ty = TY_tensor_element_ty(info->descriptor_ty);
    if (!DSL_Tensor_TY_IDX_Valid(element_ty) ||
        TY_mtype(element_ty) != info->element_mtype ||
        TY_size(element_ty) != info->element_size)
        return FALSE;

    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Scalar_Info_Valid
        (TCON_IDX scalar_tcon,
         UINT32 element_mtype)
{
    TCON scalar;

    if (!DSL_Tensor_TCON_IDX_Valid(scalar_tcon))
        return FALSE;

    scalar = TCON_from_IDX(scalar_tcon);
    return TCON_ty(scalar) == element_mtype;
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

static const unsigned char *
DSL_Tensor_TCON_Record_Dense_Bytes
        (const DSL_TENSOR_TCON_RECORD *record,
         const TCON *carrier)
{
    char *payload;

    if (record == NULL || carrier == NULL || record->dense_length == 0)
        return NULL;
    if (record->dense_offset < DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        record->dense_offset + record->dense_length < record->dense_offset ||
        record->dense_offset + record->dense_length > TCON_str_len(*carrier))
        return NULL;

    payload = Index_to_char_array(TCON_str_idx(*carrier));
    if (payload == NULL)
        return NULL;
    return (const unsigned char *)(payload + record->dense_offset);
}

static BOOL
DSL_Tensor_TCON_Record_Semantic_Equal
        (const DSL_TENSOR_TCON_RECORD *left,
         const TCON *left_carrier,
         const DSL_TENSOR_TCON_RECORD *right,
         const TCON *right_carrier)
{
    const unsigned char *left_bytes;
    const unsigned char *right_bytes;

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

        left_bytes = DSL_Tensor_TCON_Record_Dense_Bytes(left, left_carrier);
        right_bytes = DSL_Tensor_TCON_Record_Dense_Bytes(right, right_carrier);
        if (left_bytes == NULL || right_bytes == NULL ||
            left->dense_length != right->dense_length)
            return FALSE;

        return memcmp(left_bytes, right_bytes, left->dense_length) == 0;
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

static TCON_IDX
DSL_Tensor_TCON_Find_Cached_Equal
        (const DSL_TENSOR_TCON_RECORD *record,
         const TCON *carrier)
{
    UINT32 i;

    for (i = 0; i < DSL_tensor_tcon_cache_count; ++i) {
        TCON_IDX cached_idx = DSL_tensor_tcon_cache[i];
        TCON cached_carrier;
        DSL_TENSOR_TCON_RECORD cached_record;

        if (!DSL_Tensor_TCON_IDX_Valid(cached_idx))
            continue;
        cached_carrier = TCON_from_IDX(cached_idx);
        if (DSL_Tensor_TCON_Decode_Carrier(&cached_carrier,
                                           &cached_record) &&
            DSL_Tensor_TCON_Record_Semantic_Equal
                (&cached_record, &cached_carrier, record, carrier))
            return cached_idx;
    }

    return TCON_IDX_ZERO;
}

static void
DSL_Tensor_TCON_Cache (TCON_IDX tcon_idx)
{
    UINT32 i;

    if (tcon_idx == TCON_IDX_ZERO ||
        DSL_tensor_tcon_cache_count >=
            sizeof(DSL_tensor_tcon_cache) / sizeof(DSL_tensor_tcon_cache[0]))
        return;

    for (i = 0; i < DSL_tensor_tcon_cache_count; ++i) {
        if (DSL_tensor_tcon_cache[i] == tcon_idx)
            return;
    }

    DSL_tensor_tcon_cache[DSL_tensor_tcon_cache_count++] = tcon_idx;
}

static BOOL
DSL_Tensor_TCON_Envelope_Offsets_Valid
        (const DSL_TENSOR_TCON_RECORD *record,
         UINT32 payload_length)
{
    if (record == NULL ||
        record->reserved0 != 0 ||
        record->reserved1 != 0 ||
        record->reserved2 != 0 ||
        record->header_size != DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        record->flags != 0 ||
        record->record_size != payload_length ||
        payload_length <= DSL_TENSOR_TCON_ENVELOPE_SIZE)
        return FALSE;

    if (record->record_size == 0)
        return FALSE;

    if (record->side_path_length != 0) {
        if (record->side_path_offset != DSL_TENSOR_TCON_ENVELOPE_SIZE ||
            record->side_path_offset + record->side_path_length <
                record->side_path_offset ||
            record->side_path_offset + record->side_path_length >
                payload_length - 1)
            return FALSE;
    } else if (record->side_path_offset != 0) {
        return FALSE;
    }

    if (record->dense_length != 0) {
        UINT32 expected_dense_offset =
            record->side_path_length == 0 ?
                DSL_TENSOR_TCON_ENVELOPE_SIZE :
                record->side_path_offset + record->side_path_length;
        if (record->dense_offset != expected_dense_offset ||
            record->dense_offset + record->dense_length <
                record->dense_offset ||
            record->dense_offset + record->dense_length > payload_length - 1)
            return FALSE;
    } else if (record->dense_offset != 0) {
        return FALSE;
    }

    if (record->dense_length != 0)
        return record->dense_offset + record->dense_length ==
               payload_length - 1;
    if (record->side_path_length != 0)
        return record->side_path_offset + record->side_path_length ==
               payload_length - 1;
    return payload_length == DSL_TENSOR_TCON_ENVELOPE_SIZE + 1;
}

static BOOL
DSL_Tensor_TCON_Record_Valid
        (const DSL_TENSOR_TCON_RECORD *record,
         const TCON *carrier)
{
    char *payload;
    UINT32 payload_length;
    UINT64 computed_bytes;
    TY_IDX element_ty;

    if (record == NULL ||
        carrier == NULL ||
        TCON_ty(*carrier) != MTYPE_STRING ||
        TCON_str_idx(*carrier) == 0 ||
        TCON_str_len(*carrier) < DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        Index_to_length(TCON_str_idx(*carrier)) != TCON_str_len(*carrier))
        return FALSE;

    payload = Index_to_char_array(TCON_str_idx(*carrier));
    payload_length = TCON_str_len(*carrier);
    if (payload == NULL ||
        memcmp(payload, record, sizeof(*record)) != 0 ||
        payload[payload_length - 1] != '\0')
        return FALSE;

    if (record->magic != DSL_TENSOR_TCON_MAGIC ||
        record->version != DSL_TENSOR_TCON_VERSION ||
        !DSL_Tensor_TCON_Envelope_Offsets_Valid(record, payload_length) ||
        !DSL_Tensor_TY_IDX_Valid(record->descriptor_ty) ||
        !TY_is_tensor_extension(record->descriptor_ty) ||
        !TY_tensor_is_canonical(record->descriptor_ty) ||
        !DSL_Tensor_TCON_Element_Mtype_Valid(record->element_mtype) ||
        record->element_size == 0 ||
        record->element_count == 0 ||
        record->logical_bytes == 0 ||
        !DSL_Tensor_TCON_Alignment_Valid(record->required_alignment) ||
        record->required_alignment < record->element_size)
        return FALSE;

    if (!DSL_Tensor_TCON_Mul_Exact(record->element_count,
                                   record->element_size,
                                   &computed_bytes) ||
        computed_bytes != record->logical_bytes)
        return FALSE;

    element_ty = TY_tensor_element_ty(record->descriptor_ty);
    if (!DSL_Tensor_TY_IDX_Valid(element_ty) ||
        TY_mtype(element_ty) != record->element_mtype ||
        TY_size(element_ty) != record->element_size)
        return FALSE;

    switch (record->storage_kind) {
    case DSL_TENSOR_TCON_STORAGE_ZERO:
        return DSL_Tensor_TCON_Scalar_Info_Valid
                   (record->scalar_tcon, record->element_mtype) &&
               record->scalar_integer_value == 0 &&
               record->side_path_length == 0 &&
               record->side_path_offset == 0 &&
               record->dense_length == 0 &&
               record->dense_offset == 0 &&
               record->byte_offset == 0 &&
               record->byte_length == 0 &&
               record->checksum_hi == 0 &&
               record->checksum_lo == 0;
    case DSL_TENSOR_TCON_STORAGE_ONE:
        return DSL_Tensor_TCON_Scalar_Info_Valid
                   (record->scalar_tcon, record->element_mtype) &&
               record->scalar_integer_value == 1 &&
               record->side_path_length == 0 &&
               record->side_path_offset == 0 &&
               record->dense_length == 0 &&
               record->dense_offset == 0 &&
               record->byte_offset == 0 &&
               record->byte_length == 0 &&
               record->checksum_hi == 0 &&
               record->checksum_lo == 0;
    case DSL_TENSOR_TCON_STORAGE_SPLAT:
        return DSL_Tensor_TCON_Scalar_Info_Valid
                   (record->scalar_tcon, record->element_mtype) &&
               record->side_path_length == 0 &&
               record->side_path_offset == 0 &&
               record->dense_length == 0 &&
               record->dense_offset == 0 &&
               record->byte_offset == 0 &&
               record->byte_length == 0 &&
               record->checksum_hi == 0 &&
               record->checksum_lo == 0;
    case DSL_TENSOR_TCON_STORAGE_INLINE_DENSE:
        return record->scalar_tcon == TCON_IDX_ZERO &&
               record->scalar_integer_value == 0 &&
               record->side_path_length == 0 &&
               record->side_path_offset == 0 &&
               record->byte_offset == 0 &&
               record->byte_length == 0 &&
               record->dense_offset == DSL_TENSOR_TCON_ENVELOPE_SIZE &&
               record->dense_length == record->logical_bytes &&
               DSL_Tensor_TCON_Checksum_Valid(record->checksum_hi,
                                              record->checksum_lo);
    case DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE:
        return record->scalar_tcon == TCON_IDX_ZERO &&
               record->scalar_integer_value == 0 &&
               record->side_path_offset == DSL_TENSOR_TCON_ENVELOPE_SIZE &&
               record->side_path_length != 0 &&
               record->byte_length == record->logical_bytes &&
               (record->byte_offset % record->required_alignment) == 0 &&
               (record->dense_length == 0 ||
                record->dense_length == record->logical_bytes) &&
               DSL_Tensor_TCON_Checksum_Valid(record->checksum_hi,
                                              record->checksum_lo) &&
               DSL_Tensor_TCON_Relative_Path_Valid
                   (payload + record->side_path_offset,
                    record->side_path_length);
    default:
        return FALSE;
    }
}

BOOL
DSL_Tensor_TCON_Decode_Carrier
        (const TCON *carrier,
         DSL_TENSOR_TCON_RECORD *record)
{
    char *payload;
    DSL_TENSOR_TCON_RECORD decoded;

    if (record != NULL)
        memset(record, 0, sizeof(*record));

    if (carrier == NULL ||
        TCON_ty(*carrier) != MTYPE_STRING ||
        TCON_str_idx(*carrier) == 0 ||
        TCON_str_len(*carrier) < DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        Index_to_length(TCON_str_idx(*carrier)) != TCON_str_len(*carrier))
        return FALSE;

    payload = Index_to_char_array(TCON_str_idx(*carrier));
    if (payload == NULL)
        return FALSE;

    memcpy(&decoded, payload, sizeof(decoded));
    if (!DSL_Tensor_TCON_Record_Valid(&decoded, carrier))
        return FALSE;

    if (record != NULL)
        *record = decoded;
    return TRUE;
}

static BOOL
DSL_Tensor_TCON_Create_Record
        (DSL_TENSOR_TCON_STORAGE_KIND storage_kind,
         const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    DSL_TENSOR_TCON_RECORD record;
    TCON built_carrier;
    TCON_IDX existing_idx;
    TCON_IDX entered_idx;
    char *payload;
    UINT32 payload_length;
    UINT32 cursor;

    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;

    if (!DSL_Tensor_TCON_Common_Info_Valid(info))
        return FALSE;

    memset(&record, 0, sizeof(record));
    record.magic = DSL_TENSOR_TCON_MAGIC;
    record.version = DSL_TENSOR_TCON_VERSION;
    record.header_size = DSL_TENSOR_TCON_ENVELOPE_SIZE;
    record.storage_kind = storage_kind;
    record.descriptor_ty = info->descriptor_ty;
    record.scalar_tcon = info->scalar_tcon;
    record.element_mtype = info->element_mtype;
    record.element_size = info->element_size;
    record.scalar_integer_value = info->scalar_integer_value;
    record.element_count = info->element_count;
    record.logical_bytes = info->logical_bytes;
    record.required_alignment = info->required_alignment;
    record.byte_offset = info->byte_offset;
    record.byte_length = info->byte_length;
    record.checksum_hi = info->checksum_hi;
    record.checksum_lo = info->checksum_lo;

    switch (storage_kind) {
    case DSL_TENSOR_TCON_STORAGE_ZERO:
        if (!DSL_Tensor_TCON_Scalar_Info_Valid(info->scalar_tcon,
                                               info->element_mtype) ||
            info->scalar_integer_value != 0 ||
            info->dense_bytes != NULL ||
            info->dense_bytes_length != 0 ||
            info->side_path != NULL ||
            info->side_path_length != 0 ||
            info->byte_offset != 0 ||
            info->byte_length != 0 ||
            info->checksum_hi != 0 ||
            info->checksum_lo != 0)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_ONE:
        if (!DSL_Tensor_TCON_Scalar_Info_Valid(info->scalar_tcon,
                                               info->element_mtype) ||
            info->scalar_integer_value != 1 ||
            info->dense_bytes != NULL ||
            info->dense_bytes_length != 0 ||
            info->side_path != NULL ||
            info->side_path_length != 0 ||
            info->byte_offset != 0 ||
            info->byte_length != 0 ||
            info->checksum_hi != 0 ||
            info->checksum_lo != 0)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_SPLAT:
        if (!DSL_Tensor_TCON_Scalar_Info_Valid(info->scalar_tcon,
                                               info->element_mtype) ||
            info->dense_bytes != NULL ||
            info->dense_bytes_length != 0 ||
            info->side_path != NULL ||
            info->side_path_length != 0 ||
            info->byte_offset != 0 ||
            info->byte_length != 0 ||
            info->checksum_hi != 0 ||
            info->checksum_lo != 0)
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_INLINE_DENSE:
        if (info->dense_bytes == NULL ||
            info->dense_bytes_length == 0 ||
            info->dense_bytes_length != info->logical_bytes ||
            info->scalar_tcon != TCON_IDX_ZERO ||
            info->scalar_integer_value != 0 ||
            info->side_path != NULL ||
            info->side_path_length != 0 ||
            info->byte_offset != 0 ||
            info->byte_length != 0 ||
            !DSL_Tensor_TCON_Checksum_Valid(info->checksum_hi,
                                            info->checksum_lo))
            return FALSE;
        break;
    case DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE:
        if (!DSL_Tensor_TCON_Relative_Path_Valid(info->side_path,
                                                 info->side_path_length) ||
            info->scalar_tcon != TCON_IDX_ZERO ||
            info->scalar_integer_value != 0 ||
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

    payload_length = DSL_TENSOR_TCON_ENVELOPE_SIZE;
    if (storage_kind == DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE) {
        if (payload_length + info->side_path_length < payload_length)
            return FALSE;
        payload_length += info->side_path_length;
    }
    if (info->dense_bytes_length != 0) {
        if (payload_length + info->dense_bytes_length < payload_length)
            return FALSE;
        payload_length += info->dense_bytes_length;
    }
    if (payload_length + 1 < payload_length)
        return FALSE;
    payload_length += 1;
    record.record_size = payload_length;

    payload = (char *)malloc(payload_length);
    if (payload == NULL)
        return FALSE;

    memset(payload, 0, payload_length);
    cursor = DSL_TENSOR_TCON_ENVELOPE_SIZE;
    if (storage_kind == DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE) {
        record.side_path_offset = cursor;
        record.side_path_length = info->side_path_length;
        memcpy(payload + cursor, info->side_path, info->side_path_length);
        cursor += info->side_path_length;
    }
    if (info->dense_bytes_length != 0) {
        record.dense_offset = cursor;
        record.dense_length = info->dense_bytes_length;
        memcpy(payload + cursor, info->dense_bytes,
               info->dense_bytes_length);
        cursor += info->dense_bytes_length;
    }
    memcpy(payload, &record, sizeof(record));

    built_carrier = Host_To_Targ_String(MTYPE_STRING, payload,
                                        payload_length);
    free(payload);
    if (!DSL_Tensor_TCON_Decode_Carrier(&built_carrier, NULL))
        return FALSE;

    existing_idx = DSL_Tensor_TCON_Find_Cached_Equal(&record, &built_carrier);
    if (existing_idx != TCON_IDX_ZERO) {
        if (carrier != NULL)
            *carrier = TCON_from_IDX(existing_idx);
        if (tcon_idx != NULL)
            *tcon_idx = existing_idx;
        return TRUE;
    }

    entered_idx = Enter_tcon(built_carrier);
    if (carrier != NULL)
        *carrier = built_carrier;
    if (tcon_idx != NULL)
        *tcon_idx = entered_idx;
    DSL_Tensor_TCON_Cache(entered_idx);
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
    /*
     * Tensor TCON ownership is persistent in Tcon_Table and the TCON
     * character array.  M2-B has no authoritative runtime table to clear.
     */
    memset(DSL_tensor_tcon_cache, 0, sizeof(DSL_tensor_tcon_cache));
    DSL_tensor_tcon_cache_count = 0;
}

void
DSL_Tensor_TCON_Rebuild_Derived_Cache
        (TCON_IDX first_tcon_idx,
         TCON_IDX limit_tcon_idx)
{
    TCON_IDX idx;
    UINT32 table_size = TCON_Table_Size();

    DSL_Tensor_TCON_Reset();
    if (first_tcon_idx == TCON_IDX_ZERO)
        first_tcon_idx = 1;
    if (limit_tcon_idx == TCON_IDX_ZERO || limit_tcon_idx > table_size)
        limit_tcon_idx = table_size;
    if (first_tcon_idx >= table_size || first_tcon_idx >= limit_tcon_idx)
        return;

    for (idx = first_tcon_idx; idx < limit_tcon_idx; ++idx) {
        TCON carrier = TCON_from_IDX(idx);
        if (DSL_Tensor_TCON_Decode_Carrier(&carrier, NULL))
            DSL_Tensor_TCON_Cache(idx);
    }
}

BOOL
DSL_Tensor_TCON_Get
        (TCON_IDX tcon_idx,
         DSL_TENSOR_TCON_RECORD *record)
{
    TCON carrier;

    if (tcon_idx == TCON_IDX_ZERO || record == NULL)
        return FALSE;
    if (!DSL_Tensor_TCON_IDX_Valid(tcon_idx))
        return FALSE;

    carrier = TCON_from_IDX(tcon_idx);
    return DSL_Tensor_TCON_Decode_Carrier(&carrier, record);
}

BOOL
DSL_Tensor_TCON_Is_Carrier
        (TCON_IDX tcon_idx,
         DSL_TENSOR_TCON_RECORD *record)
{
    return DSL_Tensor_TCON_Get(tcon_idx, record);
}

BOOL
DSL_Tensor_TCON_Get_Side_Path
        (TCON_IDX tcon_idx,
         const char **bytes,
         UINT32 *length)
{
    DSL_TENSOR_TCON_RECORD record;
    TCON carrier;
    char *payload;

    if (bytes != NULL)
        *bytes = NULL;
    if (length != NULL)
        *length = 0;
    if (bytes == NULL || length == NULL ||
        !DSL_Tensor_TCON_IDX_Valid(tcon_idx))
        return FALSE;

    carrier = TCON_from_IDX(tcon_idx);
    if (!DSL_Tensor_TCON_Decode_Carrier(&carrier, &record) ||
        record.side_path_length == 0)
        return FALSE;

    payload = Index_to_char_array(TCON_str_idx(carrier));
    if (payload == NULL)
        return FALSE;

    *bytes = payload + record.side_path_offset;
    *length = record.side_path_length;
    return TRUE;
}

BOOL
DSL_Tensor_TCON_Get_Dense_Bytes
        (TCON_IDX tcon_idx,
         const unsigned char **bytes,
         UINT32 *length)
{
    DSL_TENSOR_TCON_RECORD record;
    TCON carrier;
    const unsigned char *payload;

    if (bytes != NULL)
        *bytes = NULL;
    if (length != NULL)
        *length = 0;
    if (bytes == NULL || length == NULL ||
        !DSL_Tensor_TCON_IDX_Valid(tcon_idx))
        return FALSE;

    carrier = TCON_from_IDX(tcon_idx);
    if (!DSL_Tensor_TCON_Decode_Carrier(&carrier, &record))
        return FALSE;

    payload = DSL_Tensor_TCON_Record_Dense_Bytes(&record, &carrier);
    if (payload == NULL)
        return FALSE;

    *bytes = payload;
    *length = record.dense_length;
    return TRUE;
}

BOOL
DSL_Tensor_TCON_Create_Zero
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_ZERO, info, tcon_idx, carrier);
}

BOOL
DSL_Tensor_TCON_Create_One
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_ONE, info, tcon_idx, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Splat
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_SPLAT, info, tcon_idx, carrier);
}

BOOL
DSL_Tensor_TCON_Create_Inline_Dense
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_INLINE_DENSE, info, tcon_idx,
                carrier);
}

BOOL
DSL_Tensor_TCON_Create_Side_File_Dense
        (const DSL_TENSOR_TCON_CREATE_INFO *info,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
    return DSL_Tensor_TCON_Create_Record
               (DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE, info, tcon_idx,
                carrier);
}

UINT64
DSL_Tensor_TCON_Semantic_Hash (TCON_IDX tcon_idx)
{
    DSL_TENSOR_TCON_RECORD record;

    if (!DSL_Tensor_TCON_Get(tcon_idx, &record))
        return 0;
    return DSL_Tensor_TCON_Record_Semantic_Hash(&record);
}

BOOL
DSL_Tensor_TCON_Semantic_Equal
        (TCON_IDX left,
         TCON_IDX right)
{
    DSL_TENSOR_TCON_RECORD left_record;
    DSL_TENSOR_TCON_RECORD right_record;
    TCON left_carrier;
    TCON right_carrier;

    if (!DSL_Tensor_TCON_IDX_Valid(left) ||
        !DSL_Tensor_TCON_IDX_Valid(right))
        return FALSE;
    if (left == right)
        return DSL_Tensor_TCON_Get(left, &left_record);

    left_carrier = TCON_from_IDX(left);
    right_carrier = TCON_from_IDX(right);
    return DSL_Tensor_TCON_Decode_Carrier(&left_carrier, &left_record) &&
           DSL_Tensor_TCON_Decode_Carrier(&right_carrier, &right_record) &&
           DSL_Tensor_TCON_Record_Semantic_Equal
               (&left_record, &left_carrier, &right_record, &right_carrier);
}

BOOL
DSL_Tensor_TCON_Get_Element_TCON
        (TCON_IDX tcon_idx,
         UINT64 element_index,
         TCON_IDX *element_tcon)
{
    DSL_TENSOR_TCON_RECORD record;

    if (element_tcon != NULL)
        *element_tcon = TCON_IDX_ZERO;

    if (!DSL_Tensor_TCON_Get(tcon_idx, &record) ||
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
