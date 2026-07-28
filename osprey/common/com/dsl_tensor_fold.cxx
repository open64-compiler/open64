/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

#include "opcode.h"
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

#ifndef DSL_TENSOR_FOLD_TEST_STUB
static BOOL
DSL_Tensor_TCON_Static_Element_Count
        (TY_IDX descriptor_ty,
         UINT64 *element_count)
{
    const char *shape =
        TY_tensor_attribute(descriptor_ty, TY_TENSOR_SCHEMA_SHAPE);
    INT32 rank = TY_tensor_rank(descriptor_ty);
    const char *cursor;
    UINT32 dimensions = 0;
    UINT64 count = 1;

    if (element_count != NULL)
        *element_count = 0;
    if (shape == NULL || rank < 0 || element_count == NULL)
        return FALSE;
    cursor = shape;
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor++ != '[')
        return FALSE;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        errno = 0;
        char *end;
        unsigned long long dimension = strtoull(cursor, &end, 10);
        if (errno == ERANGE || end == cursor || dimension == 0 ||
            count > ~0ULL / dimension)
            return FALSE;
        count *= dimension;
        ++dimensions;
        cursor = end;
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor != ']')
            return FALSE;
    }
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor != '\0' || dimensions != (UINT32)rank)
        return FALSE;
    *element_count = count;
    return TRUE;
}
#endif

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
    case OPR_DSLDIV:
    case OPR_DSLREM:
        if (evaluator != NULL) {
            evaluator->dsl_operator = dsl_operator;
            evaluator->version = version;
            evaluator->kind = DSL_TENSOR_FOLD_EVAL_ORDINARY;
            evaluator->max_results = 1;
        }
        return TRUE;
    case OPR_DSLDIVREM:
        if (evaluator != NULL) {
            evaluator->dsl_operator = dsl_operator;
            evaluator->version = version;
            evaluator->kind = DSL_TENSOR_FOLD_EVAL_PROJECTABLE_DIVREM;
            evaluator->max_results = 2;
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

    if ((candidate->flags & DSL_TENSOR_FOLD_CANDIDATE_EFFECTFUL) != 0 ||
        operator_info.effect_model != DSL_EFFECT_MODEL_PURE) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_EFFECTFUL_OPERATOR;
        return FALSE;
    }

    if ((candidate->flags &
         DSL_TENSOR_FOLD_CANDIDATE_UNRESOLVED_SHAPE) != 0) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_UNRESOLVED_SHAPE;
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

static BOOL
DSL_Tensor_Fold_Compact_Record
        (const TCON *carrier,
         DSL_TENSOR_TCON_RECORD *record)
{
    if (!DSL_Tensor_TCON_Decode_Carrier(carrier, record))
        return FALSE;

    return record->storage_kind == DSL_TENSOR_TCON_STORAGE_ZERO ||
           record->storage_kind == DSL_TENSOR_TCON_STORAGE_ONE ||
           record->storage_kind == DSL_TENSOR_TCON_STORAGE_SPLAT;
}

static BOOL
DSL_Tensor_Fold_Create_Compact_Result
        (TY_IDX result_ty,
         TYPE_ID element_mtype,
         const DSL_TENSOR_TCON_RECORD *left,
         const DSL_TENSOR_TCON_RECORD *right,
         const TCON *scalar,
         TCON *carrier)
{
    DSL_TENSOR_TCON_CREATE_INFO result_info;
    TCON_IDX scalar_idx;
    TCON_IDX tensor_idx = TCON_IDX_ZERO;
    INT64 value;
    BOOL created;

    if (left == NULL || right == NULL || scalar == NULL || carrier == NULL)
        return FALSE;
    scalar_idx = Enter_tcon(*scalar);
    if (scalar_idx == TCON_IDX_ZERO)
        return FALSE;

    memset(&result_info, 0, sizeof(result_info));
    result_info.descriptor_ty = result_ty;
    result_info.scalar_tcon = scalar_idx;
    result_info.element_mtype = element_mtype;
    result_info.element_count = left->element_count;
    result_info.logical_bytes = left->logical_bytes;
    result_info.required_alignment =
        left->required_alignment > right->required_alignment ?
            left->required_alignment : right->required_alignment;
    result_info.element_size = left->element_size;
    value = Targ_To_Host(*scalar);
    result_info.scalar_integer_value = value;

    if (value == 0)
        created = DSL_Tensor_TCON_Create_Zero
                      (&result_info, &tensor_idx, carrier);
    else if (value == 1)
        created = DSL_Tensor_TCON_Create_One
                      (&result_info, &tensor_idx, carrier);
    else
        created = DSL_Tensor_TCON_Create_Splat
                      (&result_info, &tensor_idx, carrier);
    return created && tensor_idx != TCON_IDX_ZERO;
}

static DSL_TENSOR_FOLD_STATUS
DSL_Tensor_Fold_Compact_Integer_Binary
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_OUTPUT *output)
{
    DSL_TENSOR_TCON_RECORD left_record;
    DSL_TENSOR_TCON_RECORD right_record;
    OPERATOR whirl_operator[DSL_TENSOR_FOLD_MAX_RESULTS];
    TYPE_ID element_mtype;
    TCON left_scalar;
    TCON right_scalar;
    TCON result_scalar[DSL_TENSOR_FOLD_MAX_RESULTS];
    TCON result_carrier[DSL_TENSOR_FOLD_MAX_RESULTS];
    UINT16 expected_results;
    UINT64 total_result_elements;

    expected_results =
        candidate->dsl_operator == OPR_DSLDIVREM ? 2 : 1;
    if (candidate->operand_count != 2 ||
        candidate->result_count != expected_results)
        return DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE;
    if (!DSL_Tensor_Fold_Compact_Record
             (&candidate->operands[0], &left_record) ||
        !DSL_Tensor_Fold_Compact_Record
             (&candidate->operands[1], &right_record))
        return DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND;
    if (left_record.descriptor_ty != candidate->operand_ty[0] ||
        right_record.descriptor_ty != candidate->operand_ty[1] ||
        left_record.descriptor_ty != right_record.descriptor_ty ||
        left_record.element_mtype != right_record.element_mtype ||
        left_record.element_count != right_record.element_count ||
        left_record.logical_bytes != right_record.logical_bytes)
        return DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH;
    for (UINT16 i = 0; i < expected_results; ++i) {
        if (left_record.descriptor_ty != candidate->result_ty[i])
            return DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH;
    }

    element_mtype = (TYPE_ID)left_record.element_mtype;
    if (!MTYPE_is_integral(element_mtype))
        return DSL_TENSOR_FOLD_REJECT_NUMERIC_POLICY;
    if (candidate->policy != NULL &&
        !candidate->policy->preserve_compact_splats)
        return DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY;
    if (left_record.element_count >
        ~(UINT64)0 / expected_results)
        return DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET;
    total_result_elements =
        left_record.element_count * expected_results;
    if (candidate->policy != NULL &&
        candidate->policy->max_result_elements != 0 &&
        total_result_elements >
            candidate->policy->max_result_elements)
        return DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET;
    if (candidate->policy != NULL &&
        candidate->policy->max_evaluator_work != 0 &&
        total_result_elements >
            candidate->policy->max_evaluator_work)
        return DSL_TENSOR_FOLD_REJECT_WORK_BUDGET;

    switch (candidate->dsl_operator) {
    case OPR_DSLADD:
        whirl_operator[0] = OPR_ADD;
        break;
    case OPR_DSLMUL:
        whirl_operator[0] = OPR_MPY;
        break;
    case OPR_DSLDIV:
        whirl_operator[0] = OPR_DIV;
        break;
    case OPR_DSLREM:
        whirl_operator[0] = OPR_REM;
        break;
    case OPR_DSLDIVREM:
        whirl_operator[0] = OPR_DIV;
        whirl_operator[1] = OPR_REM;
        break;
    default:
        return DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
    }

    left_scalar = TCON_from_IDX(left_record.scalar_tcon);
    right_scalar = TCON_from_IDX(right_record.scalar_tcon);
    if ((candidate->dsl_operator == OPR_DSLDIV ||
         candidate->dsl_operator == OPR_DSLREM ||
         candidate->dsl_operator == OPR_DSLDIVREM) &&
        Targ_To_Host(right_scalar) == 0)
        return DSL_TENSOR_FOLD_REJECT_DIVISION_BY_ZERO;
    if ((candidate->dsl_operator == OPR_DSLDIV ||
         candidate->dsl_operator == OPR_DSLREM ||
         candidate->dsl_operator == OPR_DSLDIVREM) &&
        MTYPE_is_signed(element_mtype) &&
        Targ_To_Host(right_scalar) == -1) {
        UINT32 bit_size = MTYPE_bit_size(element_mtype);
        if (bit_size == 0 || bit_size > 64)
            return DSL_TENSOR_FOLD_REJECT_NUMERIC_POLICY;
        INT64 minimum =
            bit_size == 64 ? (-9223372036854775807LL - 1) :
            -(1LL << (bit_size - 1));
        if (Targ_To_Host(left_scalar) == minimum)
            return DSL_TENSOR_FOLD_REJECT_NUMERIC_POLICY;
    }

    for (UINT16 i = 0; i < expected_results; ++i) {
        BOOL folded = FALSE;
        result_scalar[i] = Targ_WhirlOp
                               (OPCODE_make_op
                                    (whirl_operator[i], element_mtype,
                                     MTYPE_V),
                                left_scalar, right_scalar, &folded);
        if (!folded || TCON_ty(result_scalar[i]) != element_mtype)
            return DSL_TENSOR_FOLD_REJECT_TARGET_CAPABILITY;
    }
    for (UINT16 i = 0; i < expected_results; ++i) {
        if (!DSL_Tensor_Fold_Create_Compact_Result
                 (candidate->result_ty[i], element_mtype,
                  &left_record, &right_record, &result_scalar[i],
                  &result_carrier[i]))
            return DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY;
    }

    if (output != NULL) {
        output->status = DSL_TENSOR_FOLD_SUCCESS;
        output->rejection = DSL_TENSOR_FOLD_NOT_APPLICABLE;
        output->result_count = expected_results;
        for (UINT16 i = 0; i < expected_results; ++i) {
            output->results[i].kind = DSL_TENSOR_FOLD_RESULT_TCON;
            output->results[i].result_ty = candidate->result_ty[i];
            output->results[i].result = result_carrier[i];
        }
    }
    return DSL_TENSOR_FOLD_SUCCESS;
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

    reason = DSL_Tensor_Fold_Compact_Integer_Binary(candidate, output);
    if (reason != DSL_TENSOR_FOLD_SUCCESS)
        DSL_Tensor_Fold_Set_Output_Status(output, reason);
    return reason;
}

void
DSL_Tensor_Fold_Value_Init (DSL_TENSOR_FOLD_VALUE *value)
{
    if (value != NULL)
        memset(value, 0, sizeof(*value));
}

BOOL
DSL_Tensor_Fold_Identify_Compact_TCON
        (TCON_IDX tcon_idx,
         TY_IDX expected_ty,
         ST_IDX st,
         DSL_IR_VALUE_ID dsl_value_id,
         DSL_TENSOR_FOLD_VALUE *value,
         DSL_TENSOR_FOLD_STATUS *reason)
{
    DSL_TENSOR_TCON_RECORD record;
    TCON carrier;

    if (reason != NULL)
        *reason = DSL_TENSOR_FOLD_SUCCESS;
    DSL_Tensor_Fold_Value_Init(value);

    if (value == NULL) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE;
        return FALSE;
    }

    if (!DSL_Tensor_TCON_Get_Carrier(tcon_idx, &carrier) ||
        !DSL_Tensor_TCON_Decode_Carrier(&carrier, &record)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND;
        return FALSE;
    }

    if (record.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO &&
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ONE &&
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND;
        return FALSE;
    }

    if (expected_ty != TY_IDX_ZERO && record.descriptor_ty != expected_ty) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH;
        return FALSE;
    }

    value->kind = DSL_TENSOR_FOLD_VALUE_COMPACT_TCON;
    value->tcon_idx = tcon_idx;
    value->carrier = carrier;
    value->tensor_record = record;
    value->ty = record.descriptor_ty;
    value->st = st;
    value->dsl_value_id = dsl_value_id;
    value->flags = DSL_TENSOR_FOLD_VALUE_FLAG_COMPACT;
    return TRUE;
}

void
DSL_Tensor_Fold_Replacement_Init
        (DSL_TENSOR_FOLD_REPLACEMENT *replacement)
{
    if (replacement != NULL) {
        memset(replacement, 0, sizeof(*replacement));
        replacement->status = DSL_TENSOR_FOLD_NOT_APPLICABLE;
    }
}

static DSL_TENSOR_FOLD_STATUS
DSL_Tensor_Fold_Describe_Failure
        (DSL_TENSOR_FOLD_REPLACEMENT *replacement,
         DSL_TENSOR_FOLD_STATUS status)
{
    DSL_Tensor_Fold_Replacement_Init(replacement);
    if (replacement != NULL)
        replacement->status = status;
    return status;
}

DSL_TENSOR_FOLD_STATUS
DSL_Tensor_Fold_Describe_Replacement
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         const DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT *context,
         DSL_TENSOR_FOLD_REPLACEMENT *replacement)
{
    DSL_TENSOR_FOLD_OUTPUT output;
    DSL_TENSOR_FOLD_STATUS status;
    DSL_TENSOR_TCON_RECORD record;
    TCON_IDX result_tcon_idx;
    TY_IDX result_ty;

    if (replacement == NULL)
        return DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE;
    DSL_Tensor_Fold_Replacement_Init(replacement);

    if (candidate == NULL || context == NULL ||
        candidate->result_count != 1 || candidate->result_ty == NULL)
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement, DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE);
    if (context->result_ty != TY_IDX_ZERO &&
        context->result_ty != candidate->result_ty[0])
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement,
                    DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH);

    status = Targ_DSL_WhirlOp(candidate, &output);
    if (status != DSL_TENSOR_FOLD_SUCCESS)
        return DSL_Tensor_Fold_Describe_Failure(replacement, status);

    if (output.result_count != 1 ||
        output.results[0].kind != DSL_TENSOR_FOLD_RESULT_TCON ||
        output.results[0].result_ty != candidate->result_ty[0])
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement,
                    DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY);

    if (!DSL_Tensor_TCON_Find_Carrier(&output.results[0].result,
                                      &result_tcon_idx) ||
        result_tcon_idx == TCON_IDX_ZERO ||
        !DSL_Tensor_TCON_Get(result_tcon_idx, &record))
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement,
                    DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY);

    if (record.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO &&
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ONE &&
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT)
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement,
                    DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY);

    result_ty = context->result_ty != TY_IDX_ZERO ?
                    context->result_ty : candidate->result_ty[0];
    if (result_ty != record.descriptor_ty ||
        candidate->result_ty[0] != record.descriptor_ty)
        return DSL_Tensor_Fold_Describe_Failure
                   (replacement,
                    DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH);

    replacement->status = DSL_TENSOR_FOLD_SUCCESS;
    replacement->logical_operator = OPR_DSLTENSORCONST;
    replacement->version = 1;
    replacement->result_count = 1;
    replacement->result_tcon_idx = result_tcon_idx;
    replacement->result_tcon = output.results[0].result;
    replacement->result_ty = result_ty;
    replacement->result_st = context->result_st;
    replacement->descriptor_ty = record.descriptor_ty;
    replacement->result_storage_kind = record.storage_kind;
    replacement->scalar_tcon = record.scalar_tcon;
    replacement->scalar_integer_value = record.scalar_integer_value;
    snprintf(replacement->compact_scalar_text,
             sizeof(replacement->compact_scalar_text), "%lld",
             (long long)record.scalar_integer_value);
    replacement->source_position = context->source_position;
    replacement->origin_node_id = context->origin_node_id;
    replacement->origin_result_value_id = context->origin_result_value_id;
    replacement->result_name = context->result_name;
    replacement->metadata = context->metadata;
    replacement->lineage = context->lineage;
    replacement->flags = context->flags |
                         DSL_TENSOR_FOLD_REPLACEMENT_REVISIT_PARENTS;
    return DSL_TENSOR_FOLD_SUCCESS;
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
DSL_Tensor_TCON_Get_Carrier
        (TCON_IDX tcon_idx,
         TCON *carrier)
{
    DSL_TENSOR_TCON_RECORD record;

    if (carrier != NULL)
        TCON_clear(*carrier);
    if (carrier == NULL || !DSL_Tensor_TCON_IDX_Valid(tcon_idx))
        return FALSE;

    *carrier = TCON_from_IDX(tcon_idx);
    return DSL_Tensor_TCON_Decode_Carrier(carrier, &record);
}

BOOL
DSL_Tensor_TCON_Find_Carrier
        (const TCON *carrier,
         TCON_IDX *tcon_idx)
{
    DSL_TENSOR_TCON_RECORD record;
    TCON_IDX found;

    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;
    if (carrier == NULL || tcon_idx == NULL ||
        !DSL_Tensor_TCON_Decode_Carrier(carrier, &record))
        return FALSE;

    found = DSL_Tensor_TCON_Find_Cached_Equal(&record, carrier);
    if (found == TCON_IDX_ZERO) {
        UINT32 table_size = TCON_Table_Size();
        for (TCON_IDX idx = 1; idx < table_size; ++idx) {
            TCON candidate = TCON_from_IDX(idx);
            DSL_TENSOR_TCON_RECORD candidate_record;
            if (DSL_Tensor_TCON_Decode_Carrier
                    (&candidate, &candidate_record) &&
                DSL_Tensor_TCON_Record_Semantic_Equal
                    (&candidate_record, &candidate, &record, carrier)) {
                found = idx;
                DSL_Tensor_TCON_Cache(found);
                break;
            }
        }
    }
    if (found == TCON_IDX_ZERO)
        return FALSE;
    *tcon_idx = found;
    return TRUE;
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

static const char *
DSL_Tensor_TCON_Storage_Name (DSL_TENSOR_TCON_STORAGE_KIND storage_kind)
{
    static const char *storage_name[] = {
        "zero",
        "one",
        "splat",
        "inline_dense",
        "side_file"
    };
    UINT32 index = (UINT32)storage_kind;

    if (index >= sizeof(storage_name) / sizeof(storage_name[0]))
        return "unknown";
    return storage_name[index];
}

BOOL
DSL_Tensor_TCON_Print (FILE *file, TCON_IDX tcon_idx)
{
    DSL_TENSOR_TCON_RECORD record;
    const char *side_path;
    UINT32 side_path_length;

    if (file == NULL || !DSL_Tensor_TCON_Get(tcon_idx, &record))
        return FALSE;

    fprintf(file,
            "tensor_tcon storage=%s descriptor_ty=%u element=%s"
            " elements=%llu bytes=%llu alignment=%u",
            DSL_Tensor_TCON_Storage_Name(record.storage_kind),
            (UINT32)record.descriptor_ty,
            MTYPE_name((TYPE_ID)record.element_mtype),
            (unsigned long long)record.element_count,
            (unsigned long long)record.logical_bytes,
            record.required_alignment);

    switch (record.storage_kind) {
    case DSL_TENSOR_TCON_STORAGE_ZERO:
    case DSL_TENSOR_TCON_STORAGE_ONE:
    case DSL_TENSOR_TCON_STORAGE_SPLAT:
        fprintf(file, " scalar_tcon=%u scalar=%lld",
                (UINT32)record.scalar_tcon,
                (long long)record.scalar_integer_value);
        break;
    case DSL_TENSOR_TCON_STORAGE_INLINE_DENSE:
        fprintf(file,
                " inline_bytes=%u checksum=%016llx%016llx",
                record.dense_length,
                (unsigned long long)record.checksum_hi,
                (unsigned long long)record.checksum_lo);
        break;
    case DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE:
        if (!DSL_Tensor_TCON_Get_Side_Path
                 (tcon_idx, &side_path, &side_path_length))
            return FALSE;
        fputs(" side_file=", file);
        fwrite(side_path, 1, side_path_length, file);
        fprintf(file,
                " byte_offset=%llu byte_length=%llu"
                " available_bytes=%u checksum=%016llx%016llx",
                (unsigned long long)record.byte_offset,
                (unsigned long long)record.byte_length,
                record.dense_length,
                (unsigned long long)record.checksum_hi,
                (unsigned long long)record.checksum_lo);
        break;
    default:
        return FALSE;
    }
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
DSL_Tensor_TCON_Create_Integer_Splat
        (TY_IDX descriptor_ty,
         INT64 scalar_value,
         TCON_IDX *tcon_idx,
         TCON *carrier)
{
#ifdef DSL_TENSOR_FOLD_TEST_STUB
    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;
    if (carrier != NULL)
        memset(carrier, 0, sizeof(*carrier));
    return FALSE;
#else
    DSL_TENSOR_TCON_CREATE_INFO info;
    TY_IDX element_ty;
    TYPE_ID element_mtype;
    UINT64 element_count;
    TCON scalar;

    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;
    if (descriptor_ty == TY_IDX_ZERO || tcon_idx == NULL ||
        !TY_tensor_is_canonical(descriptor_ty) ||
        !DSL_Tensor_TCON_Static_Element_Count
             (descriptor_ty, &element_count))
        return FALSE;

    element_ty = TY_tensor_element_ty(descriptor_ty);
    element_mtype = TY_mtype(element_ty);
    if (!MTYPE_is_integral(element_mtype) || TY_size(element_ty) == 0)
        return FALSE;
    scalar = Host_To_Targ(element_mtype, scalar_value);
    if (Targ_To_Host(scalar) != scalar_value)
        return FALSE;

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = descriptor_ty;
    info.element_mtype = element_mtype;
    info.element_count = element_count;
    info.element_size = TY_size(element_ty);
    if (element_count > ~0ULL / info.element_size)
        return FALSE;
    info.scalar_tcon = Enter_tcon(scalar);
    info.logical_bytes = element_count * info.element_size;
    info.required_alignment = TY_align(descriptor_ty);
    if (info.required_alignment < info.element_size)
        info.required_alignment = info.element_size;
    info.scalar_integer_value = scalar_value;
    if (scalar_value == 0)
        return DSL_Tensor_TCON_Create_Zero
                   (&info, tcon_idx, carrier);
    if (scalar_value == 1)
        return DSL_Tensor_TCON_Create_One
                   (&info, tcon_idx, carrier);
    return DSL_Tensor_TCON_Create_Splat(&info, tcon_idx, carrier);
#endif
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
