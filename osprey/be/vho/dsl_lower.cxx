/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include "dsl_lower.h"
#include "config_dsl.h"
#include "dsl_gatekeeper.h"
#include "dsl_opt.h"
#include "dsl_opcode.h"
#include "dsl_region.h"
#include "errors.h"
#include "wn.h"
#include "wn_map.h"
#include "ir_reader.h"
#include "irbdata.h"
#include "open64_dsl_runtime_abi.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "tracing.h"
#include "wn_util.h"

typedef struct {
    ST_IDX source_st;
    PREG_NUM result_preg;
} VHO_DSL_LOWERED_VALUE;

typedef struct {
    std::string name;
    ST_IDX source_st;
    TY_IDX result_ty;
} VHO_DSL_COMPATIBILITY_VALUE;

typedef struct {
    PU_Info *pu_info;
    FILE *diagnostic;
    VHO_DSL_LOWER_RESULT result;
    std::vector<VHO_DSL_LOWERED_VALUE> lowered_values;
    std::vector<VHO_DSL_COMPATIBILITY_VALUE> compatibility_values;
    ST *runtime_tensor_const;
    ST *runtime_external_tensor;
    ST *runtime_model_input;
    ST *runtime_add;
    ST *runtime_matmul;
    ST *runtime_relu;
    ST *runtime_flatten;
    ST *runtime_residual_add;
    ST *runtime_output_logits;
    ST *runtime_linear;
    ST *runtime_conv2d;
    ST *runtime_batch_norm_infer;
    ST *runtime_max_pool2d;
    ST *runtime_global_avg_pool2d;
    ST *runtime_reshape;
    ST *runtime_transpose;
    ST *runtime_token_embedding;
    ST *runtime_rms_norm;
    ST *runtime_rotary_embedding;
    ST *runtime_attention;
    ST *runtime_swiglu;
} VHO_DSL_LOWER_CONTEXT;

typedef struct {
    UINT32 dtype;
    UINT32 scalar_kind;
    UINT32 bit_width;
    TYPE_ID mtype;
} VHO_DSL_RUNTIME_TYPE;

static BOOL
VHO_DSL_Lower_Report
        (VHO_DSL_LOWER_CONTEXT *context,
         const char *format,
         ...)
{
    if (context->diagnostic != NULL) {
        va_list args;
        va_start(args, format);
        fprintf(context->diagnostic, "DSL lowering: ");
        vfprintf(context->diagnostic, format, args);
        fprintf(context->diagnostic, "\n");
        va_end(args);
    }
    return FALSE;
}

static const char *
VHO_DSL_Str (STR_IDX index)
{
    return index == 0 ? NULL : Index_To_Str(index);
}

static BOOL
VHO_DSL_Runtime_Type
        (const char *name,
         VHO_DSL_RUNTIME_TYPE *runtime_type)
{
    if (name == NULL || runtime_type == NULL)
        return FALSE;

    struct TYPE_ENTRY {
        const char *name;
        UINT32 dtype;
        UINT32 scalar_kind;
        UINT32 bit_width;
        TYPE_ID mtype;
    };
    static const TYPE_ENTRY types[] = {
        { "int8", OPEN64_DSL_DTYPE_I8,
          OPEN64_DSL_SCALAR_SIGNED, 8, MTYPE_I1 },
        { "int16", OPEN64_DSL_DTYPE_I16,
          OPEN64_DSL_SCALAR_SIGNED, 16, MTYPE_I2 },
        { "int32", OPEN64_DSL_DTYPE_I32,
          OPEN64_DSL_SCALAR_SIGNED, 32, MTYPE_I4 },
        { "int64", OPEN64_DSL_DTYPE_I64,
          OPEN64_DSL_SCALAR_SIGNED, 64, MTYPE_I8 },
        { "uint8", OPEN64_DSL_DTYPE_U8,
          OPEN64_DSL_SCALAR_UNSIGNED, 8, MTYPE_U1 },
        { "uint16", OPEN64_DSL_DTYPE_U16,
          OPEN64_DSL_SCALAR_UNSIGNED, 16, MTYPE_U2 },
        { "uint32", OPEN64_DSL_DTYPE_U32,
          OPEN64_DSL_SCALAR_UNSIGNED, 32, MTYPE_U4 },
        { "uint64", OPEN64_DSL_DTYPE_U64,
          OPEN64_DSL_SCALAR_UNSIGNED, 64, MTYPE_U8 },
        { "float16", OPEN64_DSL_DTYPE_F16,
          OPEN64_DSL_SCALAR_FLOAT_BITS, 16, MTYPE_U2 },
        { "bfloat16", OPEN64_DSL_DTYPE_BF16,
          OPEN64_DSL_SCALAR_FLOAT_BITS, 16, MTYPE_U2 },
        { "float32", OPEN64_DSL_DTYPE_F32,
          OPEN64_DSL_SCALAR_FLOAT_BITS, 32, MTYPE_F4 },
        { "float64", OPEN64_DSL_DTYPE_F64,
          OPEN64_DSL_SCALAR_FLOAT_BITS, 64, MTYPE_F8 }
    };

    for (UINT32 i = 0; i < sizeof(types) / sizeof(types[0]); ++i) {
        if (strcmp(name, types[i].name) == 0) {
            runtime_type->dtype = types[i].dtype;
            runtime_type->scalar_kind = types[i].scalar_kind;
            runtime_type->bit_width = types[i].bit_width;
            runtime_type->mtype = types[i].mtype;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
VHO_DSL_Parse_Dimensions
        (const char *text,
         INT32 rank,
         std::vector<INT64> *dimensions)
{
    if (text == NULL || rank < 0 || dimensions == NULL)
        return FALSE;

    dimensions->clear();
    const char *cursor = text;
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

        char *end = NULL;
        INT64 dimension;
        if (*cursor == '?') {
            dimension = -1;
            end = const_cast<char *>(cursor + 1);
        } else {
            dimension = strtoll(cursor, &end, 10);
            if (end == cursor)
                return FALSE;
        }
        dimensions->push_back(dimension);
        cursor = end;

        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        return FALSE;
    }

    while (isspace((unsigned char)*cursor))
        ++cursor;
    return *cursor == '\0' && dimensions->size() == (size_t) rank;
}

static BOOL
VHO_DSL_Runtime_Layout (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_LAYOUT_UNSPECIFIED;
    else if (strcmp(name, "contiguous") == 0 ||
             strcmp(name, "row_major") == 0 ||
             strcmp(name, "NCHW") == 0 || strcmp(name, "OIHW") == 0 ||
             strcmp(name, "OI") == 0 || strcmp(name, "C") == 0)
        *value = OPEN64_DSL_LAYOUT_CONTIGUOUS;
    else if (strcmp(name, "strided") == 0)
        *value = OPEN64_DSL_LAYOUT_STRIDED;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Runtime_Memory (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_MEMORY_UNSPECIFIED;
    else if (strcmp(name, "dense") == 0)
        *value = OPEN64_DSL_MEMORY_UNSPECIFIED;
    else if (strcmp(name, "host") == 0)
        *value = OPEN64_DSL_MEMORY_HOST;
    else if (strcmp(name, "device") == 0)
        *value = OPEN64_DSL_MEMORY_DEVICE;
    else if (strcmp(name, "unified") == 0)
        *value = OPEN64_DSL_MEMORY_UNIFIED;
    else if (strcmp(name, "external_data") == 0)
        *value = OPEN64_DSL_MEMORY_EXTERNAL_DATA;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Runtime_Sharding (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_SHARDING_UNSPECIFIED;
    else if (strcmp(name, "replicated") == 0)
        *value = OPEN64_DSL_SHARDING_REPLICATED;
    else if (strcmp(name, "partitioned") == 0)
        *value = OPEN64_DSL_SHARDING_PARTITIONED;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Runtime_Placement (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_PLACEMENT_UNSPECIFIED;
    else if (strcmp(name, "host") == 0)
        *value = OPEN64_DSL_PLACEMENT_HOST;
    else if (strcmp(name, "device") == 0)
        *value = OPEN64_DSL_PLACEMENT_DEVICE;
    else if (strcmp(name, "side_file") == 0)
        *value = OPEN64_DSL_PLACEMENT_SIDE_FILE;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Runtime_Quantization (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_QUANTIZATION_UNSPECIFIED;
    else if (strcmp(name, "none") == 0)
        *value = OPEN64_DSL_QUANTIZATION_NONE;
    else if (strcmp(name, "affine") == 0)
        *value = OPEN64_DSL_QUANTIZATION_AFFINE;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Runtime_State (const char *name, UINT32 *value)
{
    if (name == NULL || name[0] == '\0')
        *value = OPEN64_DSL_RUNTIME_STATE_UNSPECIFIED;
    else if (strcmp(name, "static") == 0)
        *value = OPEN64_DSL_RUNTIME_STATE_STATIC;
    else if (strcmp(name, "dynamic") == 0)
        *value = OPEN64_DSL_RUNTIME_STATE_DYNAMIC;
    else
        return FALSE;
    return TRUE;
}

static INITV_IDX
VHO_DSL_Append_Integer
        (INITO_IDX inito,
         INITV_IDX previous,
         TYPE_ID mtype,
         INT64 value)
{
    INITV_IDX initv = New_INITV();
    INITV_Init_Integer(initv, mtype, value);
    return Append_INITV(initv, inito, previous);
}

static ST *
VHO_DSL_Create_Descriptor_Symbol
        (TY_IDX tensor_ty,
         BOOL is_constant,
         VHO_DSL_LOWER_CONTEXT *context)
{
    TENSOR_DESCRIPTOR_RECORD descriptor;
    VHO_DSL_RUNTIME_TYPE runtime_type;
    std::vector<INT64> dimensions;
    UINT32 layout;
    UINT32 sharding;
    UINT32 placement;
    UINT32 memory;
    UINT32 quantization;
    UINT32 runtime_state;

    if (!TY_get_tensor_descriptor_record(tensor_ty, &descriptor) ||
        !VHO_DSL_Runtime_Type(VHO_DSL_Str(descriptor.dtype), &runtime_type) ||
        !VHO_DSL_Parse_Dimensions(VHO_DSL_Str(descriptor.logical_shape),
                                  descriptor.rank, &dimensions) ||
        !VHO_DSL_Runtime_Layout(VHO_DSL_Str(descriptor.layout), &layout) ||
        !VHO_DSL_Runtime_Sharding
             (VHO_DSL_Str(descriptor.sharding), &sharding) ||
        !VHO_DSL_Runtime_Placement
             (VHO_DSL_Str(descriptor.placement), &placement) ||
        !VHO_DSL_Runtime_Memory(VHO_DSL_Str(descriptor.memory), &memory) ||
        !VHO_DSL_Runtime_Quantization
             (VHO_DSL_Str(descriptor.quantization), &quantization) ||
        !VHO_DSL_Runtime_State
             (VHO_DSL_Str(descriptor.runtime_state), &runtime_state)) {
        VHO_DSL_Lower_Report
            (context, "cannot materialize complete tensor descriptor for ty=%u",
             TY_IDX_index(tensor_ty));
        return NULL;
    }

    UINT32 total_size = OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE +
                        dimensions.size() * sizeof(INT64);
    TY_IDX blob_ty = Make_Array_Type(MTYPE_U1, 1, total_size);
    ST *blob = Gen_Read_Only_Symbol(blob_ty, ".dsl_descriptor");
    Set_ST_is_initialized(blob);
    INITO_IDX inito = New_INITO(blob);
    INITV_IDX previous = INITV_IDX_ZERO;
    UINT32 flags = OPEN64_DSL_TENSOR_FLAG_NO_ALIAS;
    if (is_constant)
        flags |= OPEN64_DSL_TENSOR_FLAG_CONSTANT;

    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    OPEN64_DSL_RUNTIME_ABI_VERSION);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, total_size);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, flags);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, runtime_type.dtype);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_I4, descriptor.rank);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, dimensions.size());
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    dimensions.empty() ? 0 :
                    OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, 0);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, 0);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, layout);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, sharding);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, placement);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, memory);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, quantization);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, runtime_state);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U4, 0);

    for (UINT32 i = 0; i < dimensions.size(); ++i)
        previous = VHO_DSL_Append_Integer
                       (inito, previous, MTYPE_I8, dimensions[i]);

    if (Get_INITO_Size(inito) != total_size) {
        VHO_DSL_Lower_Report
            (context, "tensor descriptor image has size %u, expected %u",
             Get_INITO_Size(inito), total_size);
        return NULL;
    }
    return blob;
}

static BOOL
VHO_DSL_Payload_Value
        (const char *payload,
         const char *key,
         char *value,
         size_t value_size)
{
    if (payload == NULL || key == NULL || value == NULL || value_size == 0)
        return FALSE;

    size_t key_length = strlen(key);
    const char *cursor = payload;
    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t field_length = end == NULL ? strlen(cursor) :
                              (size_t) (end - cursor);
        if (field_length > key_length &&
            strncmp(cursor, key, key_length) == 0 &&
            cursor[key_length] == '=') {
            size_t length = field_length - key_length - 1;
            if (length >= value_size)
                return FALSE;
            memcpy(value, cursor + key_length + 1, length);
            value[length] = '\0';
            return TRUE;
        }
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    return FALSE;
}

static UINT16
VHO_DSL_Float_To_Half (float value)
{
    union {
        float value;
        UINT32 bits;
    } source;
    source.value = value;

    UINT32 sign = (source.bits >> 16) & 0x8000;
    INT32 exponent = ((source.bits >> 23) & 0xff) - 127 + 15;
    UINT32 mantissa = source.bits & 0x7fffff;
    if (exponent <= 0)
        return (UINT16) sign;
    if (exponent >= 31)
        return (UINT16) (sign | 0x7c00);
    return (UINT16) (sign | ((UINT32) exponent << 10) |
                     ((mantissa + 0x1000) >> 13));
}

static BOOL
VHO_DSL_Scalar_Bits
        (const VHO_DSL_RUNTIME_TYPE *runtime_type,
         const char *text,
         UINT64 *bits)
{
    if (runtime_type == NULL || text == NULL || bits == NULL)
        return FALSE;

    char *end = NULL;
    if (runtime_type->scalar_kind == OPEN64_DSL_SCALAR_SIGNED) {
        INT64 value = strtoll(text, &end, 0);
        *bits = (UINT64) value;
    } else if (runtime_type->scalar_kind == OPEN64_DSL_SCALAR_UNSIGNED) {
        *bits = strtoull(text, &end, 0);
    } else if (runtime_type->dtype == OPEN64_DSL_DTYPE_F64) {
        union {
            double value;
            UINT64 bits;
        } converted;
        converted.value = strtod(text, &end);
        *bits = converted.bits;
    } else {
        union {
            float value;
            UINT32 bits;
        } converted;
        converted.value = (float) strtod(text, &end);
        if (runtime_type->dtype == OPEN64_DSL_DTYPE_F16)
            *bits = VHO_DSL_Float_To_Half(converted.value);
        else if (runtime_type->dtype == OPEN64_DSL_DTYPE_BF16)
            *bits = converted.bits >> 16;
        else
            *bits = converted.bits;
    }
    return end != text && *end == '\0';
}

static ST *
VHO_DSL_Create_Scalar_Symbol
        (const DSL_OPCODE_ANNOTATION *annotation,
         TY_IDX tensor_ty,
         VHO_DSL_LOWER_CONTEXT *context)
{
    TENSOR_DESCRIPTOR_RECORD descriptor;
    VHO_DSL_RUNTIME_TYPE runtime_type;
    char value[128];
    UINT64 bits;

    if (!TY_get_tensor_descriptor_record(tensor_ty, &descriptor) ||
        !VHO_DSL_Runtime_Type(VHO_DSL_Str(descriptor.dtype), &runtime_type) ||
        !VHO_DSL_Payload_Value(annotation->payload, "value",
                               value, sizeof(value)) ||
        !VHO_DSL_Scalar_Bits(&runtime_type, value, &bits)) {
        VHO_DSL_Lower_Report
            (context, "cannot materialize tensor constant scalar payload");
        return NULL;
    }

    TY_IDX blob_ty = Make_Array_Type
                         (MTYPE_U1, 1,
                          sizeof(OPEN64_DSL_SCALAR_VALUE_V1));
    ST *blob = Gen_Read_Only_Symbol(blob_ty, ".dsl_scalar");
    Set_ST_is_initialized(blob);
    INITO_IDX inito = New_INITO(blob);
    INITV_IDX previous = INITV_IDX_ZERO;
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, runtime_type.scalar_kind);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, runtime_type.bit_width);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U8, bits);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U8, 0);

    if (Get_INITO_Size(inito) != sizeof(OPEN64_DSL_SCALAR_VALUE_V1)) {
        VHO_DSL_Lower_Report
            (context, "tensor scalar image has unexpected size");
        return NULL;
    }
    return blob;
}

static PREG_NUM
VHO_DSL_Find_Result_Preg
        (ST_IDX source_st,
         const VHO_DSL_LOWER_CONTEXT *context)
{
    for (UINT32 i = 0; i < context->lowered_values.size(); ++i) {
        if (context->lowered_values[i].source_st == source_st)
            return context->lowered_values[i].result_preg;
    }
    return 0;
}

static const VHO_DSL_COMPATIBILITY_VALUE *
VHO_DSL_Find_Compatibility_Value
        (const char *name,
         const VHO_DSL_LOWER_CONTEXT *context)
{
    if (name == NULL)
        return NULL;
    for (UINT32 i = 0; i < context->compatibility_values.size(); ++i) {
        if (context->compatibility_values[i].name == name)
            return &context->compatibility_values[i];
    }
    return NULL;
}

static TY_IDX
VHO_DSL_Create_Compatibility_Tensor_Type
        (const char *name,
         const char *dtype,
         INT32 rank,
         const char *shape,
         VHO_DSL_LOWER_CONTEXT *context)
{
    VHO_DSL_RUNTIME_TYPE runtime_type;
    std::vector<INT64> dimensions;
    if (!VHO_DSL_Runtime_Type(dtype, &runtime_type) ||
        !VHO_DSL_Parse_Dimensions(shape, rank, &dimensions)) {
        VHO_DSL_Lower_Report
            (context, "compatibility tensor has invalid dtype, rank, or shape");
        return TY_IDX_ZERO;
    }

    char type_name[160];
    char rank_value[32];
    snprintf(type_name, sizeof(type_name), "%s.compatibility_type",
             name == NULL ? "dsl" : name);
    snprintf(rank_value, sizeof(rank_value), "%d", rank);
    TY_IDX ty = TY_Create_Tensor_Type
                    (type_name, MTYPE_To_TY(runtime_type.mtype), rank);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_KIND, "tensor");
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_DTYPE, dtype);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_RANK, rank_value);
    TY_tensor_bind_attribute(ty, TY_TENSOR_SCHEMA_SHAPE, shape);
    return ty;
}

static TY_IDX
VHO_DSL_Create_Compatibility_Matmul_Type
        (TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         const char *name,
         VHO_DSL_LOWER_CONTEXT *context)
{
    TENSOR_DESCRIPTOR_RECORD kid0_descriptor;
    TENSOR_DESCRIPTOR_RECORD kid1_descriptor;
    std::vector<INT64> kid0_shape;
    std::vector<INT64> kid1_shape;
    if (!TY_get_tensor_descriptor_record(kid0_ty, &kid0_descriptor) ||
        !TY_get_tensor_descriptor_record(kid1_ty, &kid1_descriptor) ||
        kid0_descriptor.rank != 2 || kid1_descriptor.rank != 2 ||
        !VHO_DSL_Parse_Dimensions
             (VHO_DSL_Str(kid0_descriptor.logical_shape), 2, &kid0_shape) ||
        !VHO_DSL_Parse_Dimensions
             (VHO_DSL_Str(kid1_descriptor.logical_shape), 2, &kid1_shape) ||
        kid0_shape[1] != kid1_shape[0]) {
        VHO_DSL_Lower_Report
            (context, "compatibility matmul has incompatible rank-2 shapes");
        return TY_IDX_ZERO;
    }

    char shape[96];
    snprintf(shape, sizeof(shape), "[%lld,%lld]",
             (long long)kid0_shape[0], (long long)kid1_shape[1]);
    TY_IDX result_ty = VHO_DSL_Create_Compatibility_Tensor_Type
                           (name, VHO_DSL_Str(kid0_descriptor.dtype), 2,
                            shape, context);
    if (result_ty == TY_IDX_ZERO)
        return TY_IDX_ZERO;

    const TY_TENSOR_SCHEMA_KEY representation_keys[] = {
        TY_TENSOR_SCHEMA_TRAITS,
        TY_TENSOR_SCHEMA_LAYOUT,
        TY_TENSOR_SCHEMA_SHARDING,
        TY_TENSOR_SCHEMA_PLACEMENT,
        TY_TENSOR_SCHEMA_MEMORY,
        TY_TENSOR_SCHEMA_QUANTIZATION,
        TY_TENSOR_SCHEMA_RUNTIME_STATE,
        TY_TENSOR_SCHEMA_LINEAGE
    };
    for (UINT32 i = 0;
         i < sizeof(representation_keys) / sizeof(representation_keys[0]);
         ++i) {
        const char *value = TY_tensor_attribute
                                (kid0_ty, representation_keys[i]);
        if (value != NULL)
            TY_tensor_bind_attribute(result_ty, representation_keys[i], value);
    }
    return result_ty;
}

static ST_IDX
VHO_DSL_Create_Compatibility_Result
        (const char *name,
         TY_IDX result_ty)
{
    ST *result = New_ST();
    ST_Init(result, Save_Str(name), CLASS_VAR, SCLASS_AUTO,
            EXPORT_LOCAL, result_ty);
    Set_ST_is_temp_var(result);
    ST_tensor_bind_attribute
        (ST_st_idx(result),
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "true");
    return ST_st_idx(result);
}

static ST *
VHO_DSL_Runtime_Function
        (DSL_OPERATOR dsl_operator,
         BOOL external_tensor,
         VHO_DSL_LOWER_CONTEXT *context)
{
    ST **slot = NULL;
    const char *name = NULL;
    switch (dsl_operator) {
    case OPR_DSLTENSORCONST:
        slot = external_tensor ? &context->runtime_external_tensor :
               &context->runtime_tensor_const;
        name = external_tensor ? "__open64_dsl_external_tensor_v1" :
               "__open64_dsl_tensor_const_v1";
        break;
    case OPR_DSLMODELINPUT:
        slot = &context->runtime_model_input;
        name = "__open64_dsl_model_input_v1";
        break;
    case OPR_DSLADD:
        slot = &context->runtime_add;
        name = "__open64_dsl_add_v1";
        break;
    case OPR_DSLMATMUL:
        slot = &context->runtime_matmul;
        name = "__open64_dsl_matmul_v1";
        break;
    case OPR_DSLRELU:
        slot = &context->runtime_relu;
        name = "__open64_dsl_relu_v1";
        break;
    case OPR_DSLFLATTEN:
        slot = &context->runtime_flatten;
        name = "__open64_dsl_flatten_v1";
        break;
    case OPR_DSLRESIDUALADD:
        slot = &context->runtime_residual_add;
        name = "__open64_dsl_residual_add_v1";
        break;
    case OPR_DSLOUTPUTLOGITS:
        slot = &context->runtime_output_logits;
        name = "__open64_dsl_output_logits_v1";
        break;
    case OPR_DSLLINEAR:
        slot = &context->runtime_linear;
        name = "__open64_dsl_linear_v1";
        break;
    case OPR_DSLCONV2D:
        slot = &context->runtime_conv2d;
        name = "__open64_dsl_conv2d_v1";
        break;
    case OPR_DSLBATCHNORMINFER:
        slot = &context->runtime_batch_norm_infer;
        name = "__open64_dsl_batch_norm_infer_v1";
        break;
    case OPR_DSLMAXPOOL2D:
        slot = &context->runtime_max_pool2d;
        name = "__open64_dsl_max_pool2d_v1";
        break;
    case OPR_DSLGLOBALAVGPOOL2D:
        slot = &context->runtime_global_avg_pool2d;
        name = "__open64_dsl_global_avg_pool2d_v1";
        break;
    case OPR_DSLRESHAPE:
        slot = &context->runtime_reshape;
        name = "__open64_dsl_reshape_v1";
        break;
    case OPR_DSLTRANSPOSE:
        slot = &context->runtime_transpose;
        name = "__open64_dsl_transpose_v1";
        break;
    case OPR_DSLTOKENEMBEDDING:
        slot = &context->runtime_token_embedding;
        name = "__open64_dsl_token_embedding_v1";
        break;
    case OPR_DSLRMSNORM:
        slot = &context->runtime_rms_norm;
        name = "__open64_dsl_rms_norm_v1";
        break;
    case OPR_DSLROTARYEMBEDDING:
        slot = &context->runtime_rotary_embedding;
        name = "__open64_dsl_rotary_embedding_v1";
        break;
    case OPR_DSLATTENTION:
        slot = &context->runtime_attention;
        name = "__open64_dsl_attention_v1";
        break;
    case OPR_DSLSWIGLU:
        slot = &context->runtime_swiglu;
        name = "__open64_dsl_swiglu_v1";
        break;
    default:
        return NULL;
    }

    if (*slot == NULL) {
        TY_IDX function_ty = Make_Function_Type
                                 (MTYPE_To_TY(Pointer_Mtype));
        *slot = Gen_Intrinsic_Function(function_ty, name);
    }
    return *slot;
}

static WN *
VHO_DSL_Pointer_Parm (WN *value)
{
    return WN_CreateParm(Pointer_Mtype, value,
                         MTYPE_To_TY(Pointer_Mtype), WN_PARM_BY_VALUE);
}

static WN *
VHO_DSL_U4_Parm (UINT32 value)
{
    return WN_CreateParm(MTYPE_U4, WN_Intconst(MTYPE_U4, value),
                         MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
}

static WN *
VHO_DSL_I4_Parm (INT32 value)
{
    return WN_CreateParm(MTYPE_I4, WN_Intconst(MTYPE_I4, value),
                         MTYPE_To_TY(MTYPE_I4), WN_PARM_BY_VALUE);
}

static WN *
VHO_DSL_U8_Parm (UINT64 value)
{
    return WN_CreateParm(MTYPE_U8, WN_Intconst(MTYPE_U8, value),
                         MTYPE_To_TY(MTYPE_U8), WN_PARM_BY_VALUE);
}

static BOOL
VHO_DSL_Payload_U4
        (const char *payload,
         const char *name,
         UINT32 *value)
{
    char text[32];
    char *end = NULL;
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    if (!isdigit((unsigned char)text[0]))
        return FALSE;
    unsigned long parsed = strtoul(text, &end, 10);
    if (end == text || *end != '\0' || parsed > UINT32_MAX)
        return FALSE;
    *value = (UINT32)parsed;
    return TRUE;
}

static BOOL
VHO_DSL_Payload_I4
        (const char *payload,
         const char *name,
         INT32 *value)
{
    char text[32];
    char *end = NULL;
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    long parsed = strtol(text, &end, 10);
    if (end == text || *end != '\0' || parsed < INT32_MIN ||
        parsed > INT32_MAX)
        return FALSE;
    *value = (INT32)parsed;
    return TRUE;
}

static BOOL
VHO_DSL_Payload_Pair
        (const char *payload,
         const char *name,
         BOOL allow_zero,
         UINT32 values[2])
{
    char text[64];
    char *end = NULL;
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    if (!isdigit((unsigned char)text[0]))
        return FALSE;
    unsigned long first = strtoul(text, &end, 10);
    if (end == text || *end != ',' || first > UINT32_MAX)
        return FALSE;
    const char *second_text = end + 1;
    if (!isdigit((unsigned char)second_text[0]))
        return FALSE;
    unsigned long second = strtoul(second_text, &end, 10);
    if (end == second_text || *end != '\0' || second > UINT32_MAX ||
        (!allow_zero && (first == 0 || second == 0)))
        return FALSE;
    values[0] = (UINT32)first;
    values[1] = (UINT32)second;
    return TRUE;
}

static BOOL
VHO_DSL_Payload_Bool
        (const char *payload,
         const char *name,
         BOOL *value)
{
    char text[16];
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    if (strcmp(text, "true") == 0)
        *value = TRUE;
    else if (strcmp(text, "false") == 0)
        *value = FALSE;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Payload_Operator_Layout
        (const char *payload,
         const char *name,
         UINT32 *layout)
{
    char text[16];
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    if (strcmp(text, "NCHW") == 0)
        *layout = OPEN64_DSL_OPERATOR_LAYOUT_NCHW;
    else if (strcmp(text, "OIHW") == 0)
        *layout = OPEN64_DSL_OPERATOR_LAYOUT_OIHW;
    else if (strcmp(text, "OI") == 0)
        *layout = OPEN64_DSL_OPERATOR_LAYOUT_OI;
    else
        return FALSE;
    return TRUE;
}

static BOOL
VHO_DSL_Payload_Double_Bits
        (const char *payload,
         const char *name,
         UINT64 *bits)
{
    char text[64];
    char *end = NULL;
    union {
        double value;
        UINT64 bits;
    } parsed;
    if (!VHO_DSL_Payload_Value
             (payload, name, text, sizeof(text)))
        return FALSE;
    parsed.value = strtod(text, &end);
    if (end == text || *end != '\0' ||
        !(parsed.value > 0.0 && parsed.value < 1.0))
        return FALSE;
    *bits = parsed.bits;
    return TRUE;
}

static BOOL
VHO_DSL_Build_Runtime_Call
        (WN *native,
         TY_IDX result_ty,
         DSL_OPERATOR dsl_operator,
         VHO_DSL_LOWER_CONTEXT *context,
         WN **call_result)
{
    DSL_OPCODE_ANNOTATION annotation;
    char value_kind[32];
    BOOL external_tensor;
    if (!DSL_WN_Get_Opcode_Annotation(native, &annotation))
        return FALSE;
    external_tensor = dsl_operator == OPR_DSLTENSORCONST &&
        VHO_DSL_Payload_Value(annotation.payload, "value_kind",
                              value_kind, sizeof(value_kind)) &&
        strcmp(value_kind, "external_data") == 0;
    ST *descriptor = VHO_DSL_Create_Descriptor_Symbol
                         (result_ty, dsl_operator == OPR_DSLTENSORCONST,
                          context);
    ST *function = VHO_DSL_Runtime_Function
                       (dsl_operator, external_tensor, context);
    if (descriptor == NULL || function == NULL)
        return FALSE;

    UINT32 operand_count = WN_kid_count(native);
    UINT32 parameter_count = 0;
    if (dsl_operator == OPR_DSLTENSORCONST ||
        dsl_operator == OPR_DSLMODELINPUT)
        parameter_count = 2;
    else if (dsl_operator == OPR_DSLADD ||
             dsl_operator == OPR_DSLMATMUL)
        parameter_count = 4;
    else if (dsl_operator == OPR_DSLRELU)
        parameter_count = 2;
    else if (dsl_operator == OPR_DSLFLATTEN)
        parameter_count = 4;
    else if (dsl_operator == OPR_DSLRESIDUALADD ||
             dsl_operator == OPR_DSLOUTPUTLOGITS)
        parameter_count = 3;
    else if (dsl_operator == OPR_DSLLINEAR)
        parameter_count = 6;
    else if (dsl_operator == OPR_DSLCONV2D)
        parameter_count = 16;
    else if (dsl_operator == OPR_DSLBATCHNORMINFER)
        parameter_count = 9;
    else if (dsl_operator == OPR_DSLMAXPOOL2D)
        parameter_count = 11;
    else if (dsl_operator == OPR_DSLGLOBALAVGPOOL2D)
        parameter_count = 5;
    else if (dsl_operator == OPR_DSLRESHAPE)
        parameter_count = 2;
    else if (dsl_operator == OPR_DSLTRANSPOSE)
        parameter_count = 3;
    else if (dsl_operator == OPR_DSLTOKENEMBEDDING)
        parameter_count = 3;
    else if (dsl_operator == OPR_DSLRMSNORM)
        parameter_count = 5;
    else if (dsl_operator == OPR_DSLROTARYEMBEDDING)
        parameter_count = 4;
    else if (dsl_operator == OPR_DSLATTENTION)
        parameter_count = 7;
    else if (dsl_operator == OPR_DSLSWIGLU)
        parameter_count = 3;
    else
        return FALSE;

    WN *call = WN_Call(Pointer_Mtype, MTYPE_V, parameter_count, function);
    WN_Set_Call_Default_Flags(call);
    WN_Set_Call_Does_Mem_Alloc(call);
    UINT32 parameter = 0;

    if (dsl_operator != OPR_DSLTENSORCONST &&
        dsl_operator != OPR_DSLMODELINPUT) {
        for (UINT32 kid = 0; kid < operand_count; ++kid) {
            WN *operand = WN_kid(native, kid);
            if (WN_operator(operand) != OPR_LDID)
                return FALSE;
            PREG_NUM preg = VHO_DSL_Find_Result_Preg
                                (WN_st_idx(operand), context);
            if (preg == 0) {
                VHO_DSL_Lower_Report
                    (context,
                     "%s kid%u is not defined by an earlier DSL value",
                     DSL_OPERATOR_name(dsl_operator), kid);
                return FALSE;
            }
            WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                            (WN_LdidPreg(Pointer_Mtype, preg));
        }
    }

    if (dsl_operator == OPR_DSLLINEAR && annotation.version == 3)
        WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                        (WN_Intconst(Pointer_Mtype, 0));

    WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                    (WN_Lda(Pointer_Mtype, 0, descriptor));
    if (dsl_operator == OPR_DSLTENSORCONST) {
        if (external_tensor) {
            char storage_uri[1024];
            if (!VHO_DSL_Payload_Value(annotation.payload, "value",
                                       storage_uri, sizeof(storage_uri)))
                return FALSE;
            WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                            (WN_LdaString
                                                 (storage_uri, 0,
                                                  strlen(storage_uri) + 1));
        } else {
            ST *scalar = VHO_DSL_Create_Scalar_Symbol
                             (&annotation, result_ty, context);
            if (scalar == NULL)
                return FALSE;
            WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                            (WN_Lda(Pointer_Mtype, 0, scalar));
        }
    } else if (dsl_operator == OPR_DSLMODELINPUT) {
        char ordinal_text[32];
        char *end = NULL;
        if (!VHO_DSL_Payload_Value(annotation.payload, "attr.input_ordinal",
                                   ordinal_text, sizeof(ordinal_text)))
            return FALSE;
        unsigned long ordinal = strtoul(ordinal_text, &end, 10);
        if (end == ordinal_text || *end != '\0' || ordinal > UINT32_MAX)
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm((UINT32)ordinal);
    } else if (dsl_operator == OPR_DSLADD) {
        char rule[32];
        UINT32 broadcast_rule = OPEN64_DSL_BROADCAST_NONE;
        if (VHO_DSL_Payload_Value(annotation.payload, "attr.broadcast_rule",
                                  rule, sizeof(rule)) &&
            strcmp(rule, "numpy") == 0)
            broadcast_rule = OPEN64_DSL_BROADCAST_NUMPY;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(broadcast_rule);
    } else if (dsl_operator == OPR_DSLMATMUL) {
        UINT32 flags = OPEN64_DSL_MATMUL_FLAG_NONE;
        char transpose[16];
        if (VHO_DSL_Payload_Value(annotation.payload, "attr.transpose_kid0",
                                  transpose, sizeof(transpose)) &&
            strcmp(transpose, "true") == 0)
            flags |= OPEN64_DSL_MATMUL_FLAG_TRANSPOSE_KID0;
        if (VHO_DSL_Payload_Value(annotation.payload, "attr.transpose_kid1",
                                  transpose, sizeof(transpose)) &&
            strcmp(transpose, "true") == 0)
            flags |= OPEN64_DSL_MATMUL_FLAG_TRANSPOSE_KID1;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(flags);
    } else if (dsl_operator == OPR_DSLFLATTEN) {
        char start_text[32];
        char end_text[32];
        char *start_end = NULL;
        char *end_end = NULL;
        if (!VHO_DSL_Payload_Value(annotation.payload, "attr.start_dim",
                                   start_text, sizeof(start_text)) ||
            !VHO_DSL_Payload_Value(annotation.payload, "attr.end_dim",
                                   end_text, sizeof(end_text)))
            return FALSE;
        long start_dim = strtol(start_text, &start_end, 10);
        long end_dim = strtol(end_text, &end_end, 10);
        INT32 rank = TY_tensor_rank(WN_ty(WN_kid(native, 0)));
        if (start_end == start_text || *start_end != '\0' ||
            end_end == end_text || *end_end != '\0' ||
            start_dim < INT32_MIN || start_dim > INT32_MAX ||
            end_dim < INT32_MIN || end_dim > INT32_MAX || rank <= 0)
            return FALSE;
        if (start_dim < 0)
            start_dim += rank;
        if (end_dim < 0)
            end_dim += rank;
        if (start_dim < 0 || end_dim < start_dim || end_dim >= rank)
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_I4_Parm((INT32)start_dim);
        WN_kid(call, parameter++) = VHO_DSL_I4_Parm((INT32)end_dim);
    } else if (dsl_operator == OPR_DSLOUTPUTLOGITS) {
        char semantic[32];
        if (!VHO_DSL_Payload_Value(annotation.payload, "attr.semantic",
                                   semantic, sizeof(semantic)))
            return FALSE;
        UINT32 output_semantic;
        if (strcmp(semantic, "logits") == 0)
            output_semantic = OPEN64_DSL_OUTPUT_SEMANTIC_LOGITS;
        else if (annotation.version == 3 &&
                 strcmp(semantic, "token_logits") == 0)
            output_semantic = OPEN64_DSL_OUTPUT_SEMANTIC_TOKEN_LOGITS;
        else
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(output_semantic);
    } else if (dsl_operator == OPR_DSLLINEAR) {
        BOOL has_bias;
        BOOL transpose_input;
        BOOL transpose_weight;
        UINT32 weight_layout;
        if (!VHO_DSL_Payload_Bool
                 (annotation.payload, "attr.has_bias", &has_bias) ||
            !VHO_DSL_Payload_Bool
                 (annotation.payload, "attr.transpose_input",
                  &transpose_input) ||
            !VHO_DSL_Payload_Bool
                 (annotation.payload, "attr.transpose_weight",
                  &transpose_weight) ||
            !VHO_DSL_Payload_Operator_Layout
                 (annotation.payload, "attr.weight_layout", &weight_layout))
            return FALSE;
        UINT32 flags = OPEN64_DSL_LINEAR_FLAG_NONE;
        if (has_bias)
            flags |= OPEN64_DSL_LINEAR_FLAG_HAS_BIAS;
        if (transpose_input)
            flags |= OPEN64_DSL_LINEAR_FLAG_TRANSPOSE_INPUT;
        if (transpose_weight)
            flags |= OPEN64_DSL_LINEAR_FLAG_TRANSPOSE_WEIGHT;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(flags);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(weight_layout);
    } else if (dsl_operator == OPR_DSLCONV2D) {
        UINT32 kernel[2];
        UINT32 stride[2];
        UINT32 padding[2];
        UINT32 dilation[2];
        UINT32 groups;
        UINT32 input_layout;
        UINT32 weight_layout;
        UINT32 output_layout;
        if (!VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.kernel_shape", FALSE, kernel) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.stride", FALSE, stride) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.padding", TRUE, padding) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.dilation", FALSE, dilation) ||
            !VHO_DSL_Payload_U4
                 (annotation.payload, "attr.groups", &groups) || groups == 0 ||
            !VHO_DSL_Payload_Operator_Layout
                 (annotation.payload, "attr.input_layout", &input_layout) ||
            !VHO_DSL_Payload_Operator_Layout
                 (annotation.payload, "attr.weight_layout", &weight_layout) ||
            !VHO_DSL_Payload_Operator_Layout
                 (annotation.payload, "attr.output_layout", &output_layout))
            return FALSE;
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(kernel[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(stride[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(padding[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(dilation[i]);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(groups);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(input_layout);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(weight_layout);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(output_layout);
    } else if (dsl_operator == OPR_DSLBATCHNORMINFER) {
        BOOL training;
        UINT64 epsilon_bits;
        UINT32 input_layout;
        INT32 channel_axis;
        if (!VHO_DSL_Payload_Double_Bits
                 (annotation.payload, "attr.epsilon", &epsilon_bits) ||
            !VHO_DSL_Payload_Bool
                 (annotation.payload, "attr.training", &training) || training ||
            !VHO_DSL_Payload_Operator_Layout
                 (annotation.payload, "attr.input_layout", &input_layout) ||
            !VHO_DSL_Payload_I4
                 (annotation.payload, "attr.channel_axis", &channel_axis))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U8_Parm(epsilon_bits);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(input_layout);
        WN_kid(call, parameter++) = VHO_DSL_I4_Parm(channel_axis);
    } else if (dsl_operator == OPR_DSLMAXPOOL2D) {
        UINT32 kernel[2];
        UINT32 stride[2];
        UINT32 padding[2];
        UINT32 dilation[2];
        BOOL ceil_mode;
        if (!VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.kernel_shape", FALSE, kernel) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.stride", FALSE, stride) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.padding", TRUE, padding) ||
            !VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.dilation", FALSE, dilation) ||
            !VHO_DSL_Payload_Bool
                 (annotation.payload, "attr.ceil_mode", &ceil_mode))
            return FALSE;
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(kernel[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(stride[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(padding[i]);
        for (UINT32 i = 0; i < 2; ++i)
            WN_kid(call, parameter++) = VHO_DSL_U4_Parm(dilation[i]);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(ceil_mode ? 1 : 0);
    } else if (dsl_operator == OPR_DSLGLOBALAVGPOOL2D) {
        UINT32 output_size[2];
        char reduction_axes[32];
        if (!VHO_DSL_Payload_Pair
                 (annotation.payload, "attr.output_size", FALSE,
                  output_size) ||
            !VHO_DSL_Payload_Value
                 (annotation.payload, "attr.reduction_axes", reduction_axes,
                  sizeof(reduction_axes)) ||
            strcmp(reduction_axes, "spatial") != 0)
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(output_size[0]);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(output_size[1]);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm
                                        (OPEN64_DSL_REDUCTION_AXES_SPATIAL);
    } else if (dsl_operator == OPR_DSLTRANSPOSE) {
        char permutation[256];
        if (!VHO_DSL_Payload_Value
                 (annotation.payload, "attr.permutation", permutation,
                  sizeof(permutation)))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                        (WN_LdaString
                                             (permutation, 0,
                                              strlen(permutation) + 1));
    } else if (dsl_operator == OPR_DSLRMSNORM) {
        UINT64 epsilon_bits;
        INT32 axis;
        if (!VHO_DSL_Payload_Double_Bits
                 (annotation.payload, "attr.epsilon", &epsilon_bits) ||
            !VHO_DSL_Payload_I4(annotation.payload, "attr.axis", &axis))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U8_Parm(epsilon_bits);
        WN_kid(call, parameter++) = VHO_DSL_I4_Parm(axis);
    } else if (dsl_operator == OPR_DSLATTENTION) {
        UINT32 query_heads;
        UINT32 kv_heads;
        UINT32 head_dim;
        if (!VHO_DSL_Payload_U4
                 (annotation.payload, "attr.query_heads", &query_heads) ||
            !VHO_DSL_Payload_U4
                 (annotation.payload, "attr.kv_heads", &kv_heads) ||
            !VHO_DSL_Payload_U4
                 (annotation.payload, "attr.head_dim", &head_dim))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(query_heads);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(kv_heads);
        WN_kid(call, parameter++) = VHO_DSL_U4_Parm(head_dim);
    }

    if (parameter != parameter_count)
        return FALSE;
    *call_result = call;
    return TRUE;
}

static BOOL VHO_DSL_Lower_Definition
                                (WN *block,
                                 WN *statement,
                                 VHO_DSL_LOWER_CONTEXT *context);

static BOOL
VHO_DSL_Normalize_Compatibility
        (WN *block,
         WN *carrier,
         VHO_DSL_LOWER_CONTEXT *context)
{
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPCODE_ANNOTATION annotation;
    if (!DSL_WN_Verify_Opcode_Carrier(carrier, context->diagnostic) ||
        !DSL_WN_Get_Logical_Opcode
             (carrier, &logical_opcode, context->diagnostic) ||
        !DSL_WN_Get_Opcode_Annotation(carrier, &annotation) ||
        logical_opcode.effective_version != 1) {
        ++context->result.malformed_node_count;
        return VHO_DSL_Lower_Report
                   (context, "compatibility carrier is malformed or unsupported");
    }

    DSL_OPERATOR dsl_operator = logical_opcode.dsl_operator;
    if (dsl_operator != OPR_DSLTENSORCONST &&
        dsl_operator != OPR_DSLADD &&
        dsl_operator != OPR_DSLMATMUL) {
        ++context->result.unsupported_node_count;
        return VHO_DSL_Lower_Report
                   (context, "%s.v%u has no compatibility migration route",
                    DSL_OPERATOR_name(dsl_operator),
                    logical_opcode.effective_version);
    }

    char result_name[160];
    TY_IDX result_ty = TY_IDX_ZERO;
    WN *operands[2] = { NULL, NULL };
    UINT32 operand_count = 0;

    if (dsl_operator == OPR_DSLTENSORCONST) {
        char dtype[32];
        char rank_text[32];
        char shape[256];
        if (!VHO_DSL_Payload_Value(annotation.payload, "name",
                                   result_name, sizeof(result_name)) ||
            !VHO_DSL_Payload_Value(annotation.payload, "dtype",
                                   dtype, sizeof(dtype)) ||
            !VHO_DSL_Payload_Value(annotation.payload, "rank",
                                   rank_text, sizeof(rank_text)) ||
            !VHO_DSL_Payload_Value(annotation.payload, "shape",
                                   shape, sizeof(shape))) {
            ++context->result.malformed_node_count;
            return VHO_DSL_Lower_Report
                       (context,
                        "compatibility tensor_const payload is incomplete");
        }
        char *rank_end = NULL;
        long rank = strtol(rank_text, &rank_end, 10);
        if (rank_end == rank_text || *rank_end != '\0' || rank < 0 ||
            rank > INT32_MAX) {
            ++context->result.malformed_node_count;
            return VHO_DSL_Lower_Report
                       (context, "compatibility tensor rank is invalid");
        }
        result_ty = VHO_DSL_Create_Compatibility_Tensor_Type
                        (result_name, dtype, (INT32)rank, shape, context);
    } else {
        char kid_name[2][160];
        const VHO_DSL_COMPATIBILITY_VALUE *kids[2];
        for (UINT32 kid = 0; kid < 2; ++kid) {
            char key[16];
            snprintf(key, sizeof(key), "kid%u", kid);
            if (!VHO_DSL_Payload_Value(annotation.payload, key,
                                       kid_name[kid], sizeof(kid_name[kid])) ||
                (kids[kid] = VHO_DSL_Find_Compatibility_Value
                                 (kid_name[kid], context)) == NULL) {
                ++context->result.malformed_node_count;
                return VHO_DSL_Lower_Report
                           (context,
                            "%s kid%u has no earlier compatibility value",
                            DSL_OPERATOR_name(dsl_operator), kid);
            }
            operands[kid] = WN_CreateLdid
                                (OPR_LDID, MTYPE_M, MTYPE_M, 0,
                                 kids[kid]->source_st, kids[kid]->result_ty);
        }
        operand_count = 2;
        snprintf(result_name, sizeof(result_name), "dsl_compat_result_%u",
                 (UINT32)context->compatibility_values.size() + 1);
        result_ty = dsl_operator == OPR_DSLADD ? kids[0]->result_ty :
            VHO_DSL_Create_Compatibility_Matmul_Type
                (kids[0]->result_ty, kids[1]->result_ty,
                 result_name, context);
    }

    if (result_ty == TY_IDX_ZERO) {
        ++context->result.malformed_node_count;
        return FALSE;
    }
    ST_IDX result_st = VHO_DSL_Create_Compatibility_Result
                           (result_name, result_ty);
    WN *native = DSL_WN_Create_Native
                     (dsl_operator, 1, annotation.payload,
                      operands, operand_count);
    if (native == NULL) {
        ++context->result.malformed_node_count;
        return VHO_DSL_Lower_Report
                   (context, "could not normalize compatibility carrier");
    }
    WN *definition = WN_CreateStid
                         (OPR_STID, MTYPE_V, MTYPE_M, 0,
                          result_st, result_ty, native);
    WN_Set_Linenum(definition, WN_Get_Linenum(carrier));
    WN_INSERT_BlockBefore(block, carrier, definition);
    WN *removed = WN_EXTRACT_FromBlock(block, carrier);
    WN_DELETE_Tree(removed);

    VHO_DSL_COMPATIBILITY_VALUE value;
    value.name = result_name;
    value.source_st = result_st;
    value.result_ty = result_ty;
    context->compatibility_values.push_back(value);
    ++context->result.compatibility_node_count;
    return VHO_DSL_Lower_Definition(block, definition, context);
}

static BOOL
VHO_DSL_Lower_Definition
        (WN *block,
         WN *statement,
         VHO_DSL_LOWER_CONTEXT *context)
{
    WN *native = WN_kid0(statement);
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPERATOR_INFO info;
    ++context->result.native_node_count;

    if (!DSL_WN_Get_Logical_Opcode
             (native, &logical_opcode, context->diagnostic) ||
        !DSL_Operator_Get_Info_Version
             (logical_opcode.dsl_operator,
              logical_opcode.effective_version, &info)) {
        ++context->result.malformed_node_count;
        return VHO_DSL_Lower_Report
                   (context, "native node has no supported logical operator");
    }
    if (info.lowering_model != DSL_LOWERING_MODEL_RUNTIME_CALL) {
        ++context->result.unsupported_node_count;
        return VHO_DSL_Lower_Report
                   (context, "%s.v%u has no executable VHO lowering route "
                    "(registered model: %s)",
                    DSL_OPERATOR_name(logical_opcode.dsl_operator),
                    logical_opcode.effective_version,
                    DSL_Lowering_Model_Name(info.lowering_model));
    }

    ST_IDX result_st = WN_st_idx(statement);
    if (VHO_DSL_Find_Result_Preg(result_st, context) != 0) {
        ++context->result.malformed_node_count;
        return VHO_DSL_Lower_Report
                   (context, "DSL result symbol has multiple definitions");
    }

    WN *call = NULL;
    if (!VHO_DSL_Build_Runtime_Call
             (native, WN_ty(statement), logical_opcode.dsl_operator,
              context, &call)) {
        ++context->result.malformed_node_count;
        return FALSE;
    }

    const char *preg_name = ST_name(St_Table[result_st]);
    PREG_NUM preg = Create_Preg(Pointer_Mtype, preg_name);
    WN *capture = WN_StidPreg
                      (Pointer_Mtype, preg,
                       WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg,
                               MTYPE_To_TY(Pointer_Mtype)));
    WN *projection = DSL_WN_Create_Opcode_Comment_Projection(native);
    SRCPOS source_position = WN_Get_Linenum(statement);
    WN_Set_Linenum(call, source_position);
    WN_Set_Linenum(capture, source_position);
    if (projection != NULL) {
        WN_Set_Linenum(projection, source_position);
        WN_INSERT_BlockBefore(block, statement, projection);
    }
    WN_INSERT_BlockBefore(block, statement, call);
    WN_INSERT_BlockBefore(block, statement, capture);

    VHO_DSL_LOWERED_VALUE lowered;
    lowered.source_st = result_st;
    lowered.result_preg = preg;
    context->lowered_values.push_back(lowered);

    WN *removed = WN_EXTRACT_FromBlock(block, statement);
    WN_DELETE_Tree(removed);
    ++context->result.lowered_node_count;
    return TRUE;
}

static BOOL
VHO_DSL_Lower_Tree
        (WN *wn,
         VHO_DSL_LOWER_CONTEXT *context)
{
    if (wn == NULL)
        return TRUE;

    if (WN_operator(wn) == OPR_BLOCK) {
        BOOL valid = TRUE;
        WN *statement = WN_first(wn);
        while (statement != NULL) {
            WN *next = WN_next(statement);
            if (WN_operator(statement) == OPR_STID &&
                WN_kid_count(statement) == 1 &&
                DSL_WN_Is_Native(WN_kid0(statement))) {
                if (!VHO_DSL_Lower_Definition(wn, statement, context))
                    valid = FALSE;
            } else if (!DSL_WN_Is_Native(statement) &&
                       DSL_WN_Has_Opcode(statement)) {
                if (!VHO_DSL_Normalize_Compatibility
                         (wn, statement, context))
                    valid = FALSE;
            } else if (WN_operator(statement) == OPR_REGION &&
                       DSL_Region_Is_Managed_WN
                           (context->pu_info, statement)) {
                if (!VHO_DSL_Lower_Tree(statement, context)) {
                    valid = FALSE;
                } else if (!DSL_Region_Consume_WN
                                (context->pu_info, statement)) {
                    valid = FALSE;
                    ++context->result.malformed_node_count;
                    VHO_DSL_Lower_Report
                        (context,
                         "could not consume lowered managed REGION record");
                } else {
                    WN *body = WN_region_body(statement);
                    for (WN *child = WN_first(body); child != NULL; ) {
                        WN *next_child = WN_next(child);
                        WN_EXTRACT_FromBlock(body, child);
                        WN_INSERT_BlockBefore(wn, statement, child);
                        child = next_child;
                    }
                    WN *removed = WN_EXTRACT_FromBlock(wn, statement);
                    WN_DELETE_Tree(removed);
                }
            } else if (!VHO_DSL_Lower_Tree(statement, context)) {
                valid = FALSE;
            }
            statement = next;
        }
        return valid;
    }

    if (DSL_WN_Is_Native(wn)) {
        ++context->result.native_node_count;
        ++context->result.malformed_node_count;
        return VHO_DSL_Lower_Report
                   (context,
                    "native DSL expression is not a defining tensor STID");
    }

    BOOL valid = TRUE;
    for (INT kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (!VHO_DSL_Lower_Tree(WN_kid(wn, kid), context))
            valid = FALSE;
    }
    return valid;
}

static void
VHO_DSL_Count_Executable_Carriers
        (WN *wn,
         UINT32 *count)
{
    if (wn == NULL)
        return;
    if (DSL_WN_Is_Native(wn) ||
        (WN_operator(wn) != OPR_COMMENT && DSL_WN_Has_Opcode(wn)))
        ++*count;

    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement))
            VHO_DSL_Count_Executable_Carriers(statement, count);
        return;
    }
    for (INT kid = 0; kid < WN_kid_count(wn); ++kid)
        VHO_DSL_Count_Executable_Carriers(WN_kid(wn, kid), count);
}

BOOL
VHO_DSL_Lowered_Tree_Is_Canonical
        (WN *tree,
         FILE *diagnostic,
         UINT32 *remaining_carrier_count)
{
    UINT32 count = 0;
    VHO_DSL_Count_Executable_Carriers(tree, &count);
    if (remaining_carrier_count != NULL)
        *remaining_carrier_count = count;
    if (count != 0 && diagnostic != NULL)
        fprintf(diagnostic,
                "DSL lowering: %u executable DSL carrier(s) remain before "
                "standard VHO\n", count);
    return count == 0;
}

BOOL
VHO_DSL_Lower_Verified_Program_Unit
        (struct pu_info *pu_info,
         WN *tree,
         FILE *diagnostic,
         VHO_DSL_LOWER_RESULT *result)
{
    VHO_DSL_LOWER_CONTEXT context;
    memset(&context.result, 0, sizeof(context.result));
    context.pu_info = pu_info;
    context.diagnostic = diagnostic;
    context.runtime_tensor_const = NULL;
    context.runtime_external_tensor = NULL;
    context.runtime_model_input = NULL;
    context.runtime_add = NULL;
    context.runtime_matmul = NULL;
    context.runtime_relu = NULL;
    context.runtime_flatten = NULL;
    context.runtime_residual_add = NULL;
    context.runtime_output_logits = NULL;
    context.runtime_linear = NULL;
    context.runtime_conv2d = NULL;
    context.runtime_batch_norm_infer = NULL;
    context.runtime_max_pool2d = NULL;
    context.runtime_global_avg_pool2d = NULL;
    context.runtime_reshape = NULL;
    context.runtime_transpose = NULL;
    context.runtime_token_embedding = NULL;
    context.runtime_rms_norm = NULL;
    context.runtime_rotary_embedding = NULL;
    context.runtime_attention = NULL;
    context.runtime_swiglu = NULL;

    BOOL valid = pu_info != NULL && tree != NULL;
    if (valid)
        valid = VHO_DSL_Lower_Tree(tree, &context);
    else
        VHO_DSL_Lower_Report
            (&context, "program unit or WHIRL tree is missing");

    if (!VHO_DSL_Lowered_Tree_Is_Canonical
             (tree, diagnostic,
              &context.result.remaining_executable_carrier_count))
        valid = FALSE;

    if (result != NULL)
        *result = context.result;
    return valid;
}

WN *
VHO_DSL_Lower_Driver
        (struct pu_info *pu_info,
         WN *tree)
{
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    BOOL gatekeeper_valid = DSL_Gatekeeper_Verify_PU
                                (pu_info, stderr, &gatekeeper_result);
    FmtAssert(gatekeeper_valid,
              ("DSL gatekeeper rejected Very High Level WHIRL"));

    VHO_DSL_OPT_RESULT opt_result;
    BOOL opt_valid = VHO_DSL_Optimize_Program_Unit
                         (pu_info, &tree, stderr, &opt_result);
    FmtAssert(opt_valid,
              ("DSL VHO optimization pipeline failed"));

    if (opt_result.executed_stage_count != 0) {
        gatekeeper_valid = DSL_Gatekeeper_Verify_PU
                               (pu_info, stderr, &gatekeeper_result);
        FmtAssert(gatekeeper_valid,
                  ("DSL VHO optimization produced invalid WHIRL"));
    }

    VHO_DSL_LOWER_RESULT lower_result;
    BOOL lower_valid = VHO_DSL_Lower_Verified_Program_Unit
                           (pu_info, tree, stderr, &lower_result);
    FmtAssert(lower_valid,
              ("DSL lowering did not produce canonical WHIRL"));
    if (VHO_DSL_Dump_After_Lowering) {
        fprintf(TFile,
                "\n\n========== WHIRL after VHO DSL Lowering =========="
                "\n");
        fdump_tree(TFile, tree);
        fflush(TFile);
    }
    return tree;
}
