/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned DSL lowering for the active PU: validate semantic/runtime
 * contracts, materialize descriptors and effectful calls, and apply selected
 * physical/GEMM plans transactionally where the caller supplies a tree slot.
 * CUDA plans become ordinary high-level WHIRL device bodies; the backend
 * driver owns their program-boundary publication and whirl2c only emits the
 * selected semantics. This file does not perform IPA or change WHIRL binary
 * layouts. Direct lowering may mutate its tree; copied-tree entry points
 * commit only after canonical verification succeeds.
 * Controlling designs: doc/WHIRL-DSL-INFRASTRUCTURE.md,
 * doc/AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md,
 * doc/AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md,
 * doc/AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md,
 * doc/AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md,
 * doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md,
 * doc/AI-COMPILER-OPTIMIZATION-OCCUPANCY.md,
 * doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md, and
 * doc/AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md.
 */

#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <string>
#include <vector>

#include "dsl_lower.h"
#include "config.h"
#include "config_dsl.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "dsl_opt.h"
#include "dsl_opcode.h"
#include "dsl_region.h"
#include "dsl_shape_refine.h"
#include "dsl_tensor_analysis_opt.h"
#include "dsl_tensor_evolution_opt.h"
#include "dsl_tensor_locality_opt.h"
#include "dsl_fetch_pipeline_opt.h"
#include "dsl_tile_candidate_opt.h"
#include "errors.h"
#include "wn.h"
#include "wn_simp.h"
#include "wn_map.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "irbdata.h"
#include "open64_dsl_runtime_abi.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "tracing.h"
#include "wn_util.h"
#include "dwarf_DST_mem.h"

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
    ST *runtime_matmul_physical;
    ST *runtime_matmul_generated;
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
    ST *runtime_rotary_embedding_v2;
    ST *runtime_attention;
    ST *runtime_attention_v2;
    ST *runtime_swiglu;
    ST *runtime_scatter;
    const DSL_PHYSICAL_PLAN_ANALYSIS *physical_plan;
    const DSL_PHYSICAL_PLAN_IR *physical_plan_ir;
    const DSL_TILE_PLAN_RECORD *generated_tile;
    UINT32 generated_entry_ordinal;
} VHO_DSL_LOWER_CONTEXT;

typedef char VHO_DSL_Runtime_Cublaslt_Provider_Matches
    [(INT)OPEN64_DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ==
     (INT)DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ? 1 : -1];
typedef char VHO_DSL_Runtime_Hopper_Profile_Matches
    [(INT)OPEN64_DSL_TARGET_PROFILE_NVIDIA_HOPPER ==
     (INT)DSL_TARGET_PROFILE_NVIDIA_HOPPER ? 1 : -1];
typedef char VHO_DSL_Runtime_Provider_Schedule_Matches
    [(INT)OPEN64_DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED ==
     (INT)DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED ? 1 : -1];

typedef struct {
    UINT32 dtype;
    UINT32 scalar_kind;
    UINT32 bit_width;
    TYPE_ID mtype;
} VHO_DSL_RUNTIME_TYPE;

/* Record a lowering failure in the active context and optional diagnostic;
 * return FALSE so callers preserve their own rollback boundary. */
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

/* Borrow a process-lifetime string-table entry; zero denotes absent metadata. */
static const char *
VHO_DSL_Str (STR_IDX index)
{
    return index == 0 ? NULL : Index_To_Str(index);
}

/* Translate a supported semantic dtype to the runtime scalar ABI; reject
 * unknown names without inventing a precision conversion. */
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

/* Parse a descriptor shape into caller-owned dimensions, preserving unknown
 * extents as -1; FALSE leaves only a partial scratch vector, never committed IR. */
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

/* Map supported descriptor layouts to stable runtime ABI tags; unknown
 * layouts fail rather than silently using contiguous storage. */
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

/* Translate a descriptor memory-space name for runtime dispatch; unsupported
 * spaces remain a lowering failure, not a placement decision. */
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

/* Encode a supported sharding contract for the runtime; reject unsupported
 * distributions without altering the compiler placement plan. */
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

/* Encode a supported target placement for the runtime boundary; no device
 * discovery or cross-PU ownership inference is performed here. */
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

/* Validate the descriptor quantization spelling against runtime ABI tags;
 * this mapping never authorizes lossy arithmetic. */
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

/* Encode supported runtime-state semantics; unknown state contracts fail
 * before descriptor emission. */
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

/* Append one typed integer to a caller-owned initializer chain; the caller
 * owns the containing symbol and commit decision. */
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

/* Materialize the already-selected physical-provider ABI as an immutable
 * constant in the active compilation tables; do not reselect its provider. */
static ST *
VHO_DSL_Create_Physical_Plan_Symbol
        (const DSL_PHYSICAL_IMPLEMENTATION_RECORD *implementation,
         VHO_DSL_LOWER_CONTEXT *context)
{
    if (implementation == NULL ||
        implementation->provider !=
            DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ||
        implementation->capability_id == 0 ||
        implementation->fallback_implementation_id == 0 ||
        implementation->identity == 0)
        return NULL;

    TY_IDX blob_ty = Make_Array_Type
                         (MTYPE_U1, 1,
                          OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE);
    Set_TY_align(blob_ty, 8);
    ST *blob = Gen_Read_Only_Symbol(blob_ty, ".dsl_physical_plan");
    Set_ST_is_initialized(blob);
    INITO_IDX inito = New_INITO(blob);
    INITV_IDX previous = INITV_IDX_ZERO;
    UINT32 flags = OPEN64_DSL_PHYSICAL_PLAN_FLAG_ALLOW_FALLBACK;
    if ((implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING) != 0)
        flags |= OPEN64_DSL_PHYSICAL_PLAN_FLAG_SEMANTICS_PRESERVING;

    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    OPEN64_DSL_RUNTIME_ABI_VERSION);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    implementation->provider);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    implementation->capability_id);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    implementation->target_profile_id);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    implementation->schedule_kind);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4, flags);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U8,
                    implementation->identity);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U8, 0);
    if (Get_INITO_Size(inito) != OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE) {
        VHO_DSL_Lower_Report
            (context, "physical plan image has size %u, expected %u",
             Get_INITO_Size(inito), OPEN64_DSL_PHYSICAL_PLAN_V1_SIZE);
        return NULL;
    }
    return blob;
}

/* Emit a pointer-free, fixed-size plan for the generated matmul runtime. */
static ST *
VHO_DSL_Create_Generated_Tile_Symbol
        (const DSL_TILE_PLAN_RECORD *tile,
         VHO_DSL_LOWER_CONTEXT *context)
{
    if (tile == NULL || tile->shared_bytes_per_stage > UINT32_MAX)
        return NULL;
    const BOOL bound_entry = context->generated_entry_ordinal != 0;
    const UINT32 plan_size = bound_entry ?
        OPEN64_DSL_GENERATED_MATMUL_PLAN_V2_SIZE :
        OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE;
    TY_IDX blob_ty = Make_Array_Type
                         (MTYPE_U1, 1, plan_size);
    Set_TY_align(blob_ty, 8);
    ST *blob = Gen_Read_Only_Symbol(blob_ty, ".dsl_generated_matmul_plan");
    Set_ST_is_initialized(blob);
    INITO_IDX inito = New_INITO(blob);
    INITV_IDX previous = INITV_IDX_ZERO;
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    bound_entry ? 2 : OPEN64_DSL_RUNTIME_ABI_VERSION);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U2,
                    plan_size);
    previous = VHO_DSL_Append_Integer
                   (inito, previous, MTYPE_U4,
                    plan_size);
    const UINT32 fields[] = {
        OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED,
        tile->target_profile_id,
        tile->optimization_plan_id,
        tile->id,
        tile->cta_m, tile->cta_n, tile->cta_k,
        tile->thread_m, tile->thread_n,
        (UINT32)tile->shared_bytes_per_stage,
        tile->threads_per_cta,
        OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT
    };
    for (UINT32 index = 0; index < sizeof(fields) / sizeof(fields[0]); ++index)
        previous = VHO_DSL_Append_Integer
                       (inito, previous, MTYPE_U4, fields[index]);
    previous = VHO_DSL_Append_Integer(inito, previous, MTYPE_U8, 0);
    if (bound_entry)
        previous = VHO_DSL_Append_Integer
                       (inito, previous, MTYPE_U8,
                        context->generated_entry_ordinal);
    if (Get_INITO_Size(inito) != plan_size) {
        VHO_DSL_Lower_Report
            (context, "generated matmul plan image has size %u",
             Get_INITO_Size(inito));
        return NULL;
    }
    return blob;
}

/* Validate a tensor descriptor and materialize its runtime ABI constant;
 * malformed shape, dtype or layout facts fail without an executable call. */
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

/* Read one named operator-payload field into caller scratch storage; absent
 * or malformed fields are reported to the lowering caller. */
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

/* Encode an FP32 value as FP16 scalar bits for an explicitly requested
 * runtime scalar representation; this is not a GEMM narrowing policy. */
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

/* Convert a validated scalar payload into its declared ABI bit encoding;
 * reject unsupported types rather than changing the tensor numerical contract. */
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

/* Materialize a typed immutable scalar ABI object from semantic payload;
 * failures propagate before its consuming runtime call is inserted. */
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

/* Look up a result already lowered in this PU; the returned entry is
 * borrowed and no value is created on a miss. */
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

/* Find a normalized compatibility value in the active lowering context;
 * no symbol or descriptor is synthesized by this lookup. */
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

/* Create a tensor type for the legacy carrier from explicit payload facts;
 * reject incomplete contracts rather than guessing storage or dtype. */
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

/* Derive the legacy matmul result type from operand contracts; require
 * compatible contraction dimensions before normalizing its result. */
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

/* Create and register a PU-local normalized result for a legacy carrier;
 * the surrounding normalization owns executable-tree mutation. */
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

/* Lazily bind the runtime entry for a supported logical operator in the
 * active context; return no binding for an unsupported runtime contract. */
static ST *
VHO_DSL_Runtime_Function
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         BOOL external_tensor,
         BOOL physical_matmul,
         BOOL generated_matmul,
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
        slot = generated_matmul ? &context->runtime_matmul_generated :
               physical_matmul ? &context->runtime_matmul_physical :
                                 &context->runtime_matmul;
        name = generated_matmul ?
                   (context->generated_entry_ordinal == 0 ?
                    "__open64_dsl_matmul_generated_v1" :
                    "__open64_dsl_matmul_generated_v2") :
               physical_matmul ? "__open64_dsl_matmul_physical_v1" :
                                 "__open64_dsl_matmul_v1";
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
        if (version == 1) {
            slot = &context->runtime_rotary_embedding;
            name = "__open64_dsl_rotary_embedding_v1";
        } else if (version == 2) {
            slot = &context->runtime_rotary_embedding_v2;
            name = "__open64_dsl_rotary_embedding_v2";
        }
        break;
    case OPR_DSLATTENTION:
        if (version == 1) {
            slot = &context->runtime_attention;
            name = "__open64_dsl_attention_v1";
        } else if (version == 2) {
            slot = &context->runtime_attention_v2;
            name = "__open64_dsl_attention_v2";
        }
        break;
    case OPR_DSLSWIGLU:
        slot = &context->runtime_swiglu;
        name = "__open64_dsl_swiglu_v1";
        break;
    case OPR_DSLSCATTER:
        slot = &context->runtime_scatter;
        name = "__open64_dsl_scatter_v1";
        break;
    default:
        return NULL;
    }

    if (slot == NULL)
        return NULL;
    if (*slot == NULL) {
        TY_IDX function_ty = Make_Function_Type
                                 (MTYPE_To_TY(Pointer_Mtype));
        *slot = Gen_Intrinsic_Function(function_ty, name);
    }
    return *slot;
}

/* Wrap a newly owned address expression as a by-value pointer argument;
 * ownership of the expression transfers to the returned WHIRL parameter. */
static WN *
VHO_DSL_Pointer_Parm (WN *value)
{
    return WN_CreateParm(Pointer_Mtype, value,
                         MTYPE_To_TY(Pointer_Mtype), WN_PARM_BY_VALUE);
}

/* Build a by-reference state argument with read/write effects matching its
 * semantic contract; the runtime call must remain conservatively effectful. */
static WN *
VHO_DSL_State_Parm
        (const DSL_STATE_OBJECT_RECORD *state,
         DSL_STATE_EFFECT_KIND effect_kind)
{
    UINT32 flags = WN_PARM_BY_REFERENCE | WN_PARM_PASSED_NOT_SAVED;
    if (effect_kind == DSL_STATE_EFFECT_READ)
        flags |= WN_PARM_READ_ONLY;
    else
        flags |= WN_PARM_OUT;
    return WN_CreateParm
               (Pointer_Mtype,
                WN_Lda(Pointer_Mtype, 0, &St_Table[state->st]),
                MTYPE_To_TY(Pointer_Mtype), flags);
}

/* Find the semantic image node defining a result symbol; zero means no
 * matching definition and does not authorize an executable rewrite. */
static DSL_IR_NODE_ID
VHO_DSL_Image_Node_For_Result (ST_IDX result_st)
{
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Count(); ++i) {
        DSL_IR_VALUE_RECORD value;
        if (DSL_IR_Image_Get_Value(i, &value) && value.st == result_st)
            return value.producer_node_id;
    }
    return DSL_IR_NODE_INVALID_ID;
}

/* Create a by-value unsigned 32-bit runtime ABI constant argument. */
static WN *
VHO_DSL_U4_Parm (UINT32 value)
{
    return WN_CreateParm(MTYPE_U4, WN_Intconst(MTYPE_U4, value),
                         MTYPE_To_TY(MTYPE_U4), WN_PARM_BY_VALUE);
}

/* Create a by-value signed 32-bit runtime ABI constant argument. */
static WN *
VHO_DSL_I4_Parm (INT32 value)
{
    return WN_CreateParm(MTYPE_I4, WN_Intconst(MTYPE_I4, value),
                         MTYPE_To_TY(MTYPE_I4), WN_PARM_BY_VALUE);
}

/* Create a by-value unsigned 64-bit runtime ABI constant argument. */
static WN *
VHO_DSL_U8_Parm (UINT64 value)
{
    return WN_CreateParm(MTYPE_U8, WN_Intconst(MTYPE_U8, value),
                         MTYPE_To_TY(MTYPE_U8), WN_PARM_BY_VALUE);
}

/* Decode a payload integer within unsigned ABI range; absent or invalid
 * values fail instead of being truncated into a runtime argument. */
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

/* Decode a payload integer within signed ABI range before call emission. */
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

/* Decode a two-dimensional operator attribute into caller-owned values;
 * reject malformed arity or unsupported values before lowering. */
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

/* Decode an operator boolean under the accepted payload syntax; report
 * malformed values to the enclosing runtime-call builder. */
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

/* Resolve a supported operator-specific layout for runtime ABI emission;
 * do not make a new profitability or tensor-placement choice. */
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

/* Read a floating-point payload as its double-width ABI bit pattern;
 * reject malformed numeric text before constructing the runtime call. */
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

/* Lower a validated definition to an effectful runtime call using PU-local
 * operand bindings and any preselected provider; failures propagate without
 * installing the call. The enclosing tree engine owns replacement/rollback. */
static BOOL
VHO_DSL_Build_Runtime_Call
        (WN *native,
         TY_IDX result_ty,
         DSL_OPERATOR dsl_operator,
         DSL_IR_NODE_ID image_node_id,
         VHO_DSL_LOWER_CONTEXT *context,
         WN **call_result)
{
    DSL_OPCODE_ANNOTATION annotation;
    DSL_PHYSICAL_SITE_RECORD physical_site;
    DSL_PHYSICAL_IMPLEMENTATION_RECORD physical_implementation;
    char value_kind[32];
    BOOL external_tensor;
    BOOL physical_matmul = FALSE;
    BOOL generated_matmul = FALSE;
    if (!DSL_WN_Get_Opcode_Annotation(native, &annotation))
        return FALSE;
    external_tensor = dsl_operator == OPR_DSLTENSORCONST &&
        VHO_DSL_Payload_Value(annotation.payload, "value_kind",
                              value_kind, sizeof(value_kind)) &&
        strcmp(value_kind, "external_data") == 0;
    if (dsl_operator == OPR_DSLMATMUL && context->physical_plan != NULL &&
        DSL_physical_plan_ir_find_selected
            (context->physical_plan_ir, image_node_id, &physical_site,
             &physical_implementation))
        physical_matmul = physical_implementation.provider ==
                          DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT;
    if (dsl_operator == OPR_DSLMATMUL && context->generated_tile != NULL) {
        if (result_ty != context->generated_tile->source_descriptor_ty)
            return VHO_DSL_Lower_Report
                       (context, "generated tile result descriptor mismatch");
        generated_matmul = TRUE;
    }
    ST *descriptor = VHO_DSL_Create_Descriptor_Symbol
                         (result_ty, dsl_operator == OPR_DSLTENSORCONST,
                          context);
    ST *function = VHO_DSL_Runtime_Function
                       (dsl_operator, annotation.version, external_tensor,
                        physical_matmul, generated_matmul, context);
    if (descriptor == NULL || function == NULL)
        return FALSE;

    UINT32 operand_count = WN_kid_count(native);
    UINT32 parameter_count = 0;
    if (dsl_operator == OPR_DSLTENSORCONST ||
        dsl_operator == OPR_DSLMODELINPUT)
        parameter_count = 2;
    else if (dsl_operator == OPR_DSLADD ||
             dsl_operator == OPR_DSLMATMUL)
        parameter_count = (physical_matmul || generated_matmul) ? 5 : 4;
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
        parameter_count = annotation.version == 2 ? 5 : 4;
    else if (dsl_operator == OPR_DSLATTENTION)
        parameter_count = 7;
    else if (dsl_operator == OPR_DSLSWIGLU)
        parameter_count = 3;
    else if (dsl_operator == OPR_DSLSCATTER)
        parameter_count = 5;
    else
        return FALSE;

    UINT32 state_effect_count = 0;
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect))
            return FALSE;
        if (effect.owner_node_id == image_node_id)
            ++state_effect_count;
    }
    parameter_count += state_effect_count;

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
        if (physical_matmul || generated_matmul) {
            ST *physical = generated_matmul ?
                VHO_DSL_Create_Generated_Tile_Symbol
                    (context->generated_tile, context) :
                VHO_DSL_Create_Physical_Plan_Symbol
                    (&physical_implementation, context);
            if (physical == NULL)
                return FALSE;
            WN_kid(call, parameter++) = VHO_DSL_Pointer_Parm
                                            (WN_Lda(Pointer_Mtype, 0,
                                                    physical));
        }
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
    } else if (dsl_operator == OPR_DSLSCATTER) {
        INT32 axis;
        if (!VHO_DSL_Payload_I4(annotation.payload, "attr.axis", &axis))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_I4_Parm(axis);
    }

    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        DSL_STATE_OBJECT_RECORD state;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect))
            return FALSE;
        if (effect.owner_node_id != image_node_id)
            continue;
        if (!DSL_Effect_Image_Get_State_Object
                 (effect.state_object_id, &state))
            return FALSE;
        WN_kid(call, parameter++) = VHO_DSL_State_Parm
                                        (&state,
                                         (DSL_STATE_EFFECT_KIND)
                                             effect.effect_kind);
        if (effect.effect_kind == DSL_STATE_EFFECT_READ)
            ++context->result.state_read_count;
        else
            ++context->result.state_modify_count;
    }

    if (parameter != parameter_count)
        return FALSE;
    if (physical_matmul || generated_matmul)
        ++context->result.physical_provider_lowered_count;
    *call_result = call;
    return TRUE;
}

/* Forward contract for compatibility normalization: lower one definition in
 * the caller-owned working block; transactional isolation belongs to its caller. */
static BOOL VHO_DSL_Lower_Definition
                                (WN *block,
                                 WN *statement,
                                 VHO_DSL_LOWER_CONTEXT *context);

/* Normalize a legacy DSL carrier into explicit semantic result/operand
 * bindings for this PU; reject unsupported carriers before runtime lowering. */
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

/* Replace one validated definition with runtime call/result WHIRL and
 * register its result binding; the caller owns transactional tree isolation. */
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
    DSL_IR_NODE_ID image_node_id = VHO_DSL_Image_Node_For_Result(result_st);
    if (!VHO_DSL_Build_Runtime_Call
             (native, WN_ty(statement), logical_opcode.dsl_operator,
              image_node_id, context, &call)) {
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

/* Visit the active tree in execution order, lower supported definitions and
 * recurse within the PU; failures may leave this working tree partially
 * changed, so transactional callers must discard it rather than commit. */
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

/* Count remaining executable DSL carriers recursively without mutating
 * the tree; retained comments are evidence, not executable operations. */
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

/* Verify that lowering left no executable semantic carrier; return a
 * diagnostic/count without repairing or mutating an invalid tree. */
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

/* Validate selected provider/site bindings against the active PU before
 * executable mutation; no plan selection or cross-PU traversal is allowed. */
static BOOL
VHO_DSL_Preflight_Physical_Plan
        (struct pu_info *pu_info,
         const DSL_PHYSICAL_PLAN_ANALYSIS *physical_plan,
         VHO_DSL_LOWER_CONTEXT *context)
{
    if (physical_plan == NULL)
        return TRUE;
    if (!VHO_DSL_Physical_Plan_Verify
             (physical_plan, context->diagnostic))
        return VHO_DSL_Lower_Report
                   (context, "selected physical plan is invalid");
    const DSL_PHYSICAL_PLAN_IR *physical_ir =
        VHO_DSL_Physical_Plan_Get_IR(physical_plan);
    if (physical_ir == NULL)
        return VHO_DSL_Lower_Report
                   (context, "selected physical plan IR is missing");
    context->physical_plan_ir = physical_ir;

    UINT32 site_count = DSL_physical_plan_ir_site_count(physical_ir);
    for (UINT32 id = 1; id <= site_count; ++id) {
        DSL_PHYSICAL_SITE_RECORD site;
        DSL_PHYSICAL_IMPLEMENTATION_RECORD implementation;
        DSL_PROVIDER_CAPABILITY_RECORD capability;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        if (!DSL_physical_plan_ir_get_site(physical_ir, id, &site) ||
            site.owner_pu_st != PU_Info_proc_sym(pu_info) ||
            !DSL_physical_plan_ir_get_implementation
                 (physical_ir, site.selected_implementation_id,
                  &implementation) ||
            implementation.site_id != site.id ||
            implementation.legality != DSL_OPT_LEGALITY_PROVEN ||
            (implementation.flags &
             DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) == 0 ||
            (implementation.flags &
             DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING) == 0 ||
            !DSL_provider_capability_get
                 (implementation.capability_id, &capability) ||
            !DSL_IR_Image_Get_Node(site.semantic_node_id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &descriptor) ||
            descriptor.logical_operator != capability.logical_operator ||
            descriptor.version != capability.operator_version)
            return VHO_DSL_Lower_Report
                       (context, "physical site %u is incomplete or stale", id);

        for (UINT32 prior = 1; prior < id; ++prior) {
            DSL_PHYSICAL_SITE_RECORD prior_site;
            if (!DSL_physical_plan_ir_get_site
                     (physical_ir, prior, &prior_site) ||
                prior_site.semantic_node_id == site.semantic_node_id)
                return VHO_DSL_Lower_Report
                           (context,
                            "physical site %u duplicates a semantic node", id);
        }

        if (implementation.provider ==
            DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT) {
            if (implementation.implementation_kind !=
                    DSL_PHYSICAL_IMPLEMENTATION_BASELINE_DIRECT ||
                implementation.schedule_kind != DSL_PHYSICAL_SCHEDULE_DIRECT ||
                implementation.fallback_implementation_id != 0)
                return VHO_DSL_Lower_Report
                           (context, "physical site %u has an invalid baseline",
                            id);
        } else if (implementation.provider ==
                   DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT) {
            DSL_PHYSICAL_IMPLEMENTATION_RECORD fallback;
            if (capability.provider !=
                    DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ||
                capability.implementation_kind !=
                    DSL_PHYSICAL_IMPLEMENTATION_LIBRARY ||
                capability.logical_operator != OPR_DSLMATMUL ||
                capability.operator_version != 1 ||
                capability.schedule_kind !=
                    DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED ||
                (capability.flags &
                 (DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
                  DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED)) !=
                    (DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME |
                     DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED) ||
                implementation.implementation_kind !=
                    DSL_PHYSICAL_IMPLEMENTATION_LIBRARY ||
                implementation.schedule_kind !=
                    DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED ||
                (implementation.flags &
                 DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE) == 0 ||
                implementation.fallback_implementation_id !=
                    site.baseline_implementation_id ||
                !DSL_physical_plan_ir_get_implementation
                     (physical_ir, implementation.fallback_implementation_id,
                      &fallback) ||
                fallback.provider != DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT)
                return VHO_DSL_Lower_Report
                           (context,
                            "physical site %u has no reviewed matmul provider "
                            "and direct fallback contract", id);
            ++context->result.physical_provider_site_count;
        } else {
            return VHO_DSL_Lower_Report
                       (context,
                        "physical site %u selects unsupported provider %s",
                        id,
                        DSL_physical_provider_name(implementation.provider));
        }
    }
    context->result.physical_plan_site_count = site_count;
    return TRUE;
}

/* Prove generated signed-I4 loop and address arithmetic stays in range,
 * including predicated edge expressions; reject unsafe fixed shapes. */
static BOOL
VHO_DSL_CUDA_I4_Geometry_Safe (const DSL_TILE_PLAN_RECORD &tile)
{
    return VHO_DSL_cuda_i4_geometry_safe(tile);
}

/* Check that the selected exact/predicated policy agrees with all three
 * problem extents; no edge policy is chosen during source emission. */
static BOOL
VHO_DSL_CUDA_Edge_Policy_Consistent (const DSL_TILE_PLAN_RECORD &tile)
{
    if (tile.cta_m == 0 || tile.cta_n == 0 || tile.cta_k == 0)
        return FALSE;
    const BOOL has_tail = tile.problem_m % tile.cta_m != 0 ||
                          tile.problem_n % tile.cta_n != 0 ||
                          tile.problem_k % tile.cta_k != 0;
    return tile.edge_policy ==
        (has_tail ? DSL_TILE_EDGE_PREDICATED : DSL_TILE_EDGE_EXACT);
}

/* Preflight the active PU and selected plan, lower its working tree, then
 * verify canonical form and site coverage. The caller decides whether this
 * working tree is direct mutation or a copy that can be discarded on failure. */
static BOOL
VHO_DSL_Lower_Verified_Engine
        (struct pu_info *pu_info,
         WN *tree,
         const DSL_PHYSICAL_PLAN_ANALYSIS *physical_plan,
         const DSL_TILE_PLAN_RECORD *generated_tile,
         UINT32 generated_entry_ordinal,
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
    context.runtime_matmul_physical = NULL;
    context.runtime_matmul_generated = NULL;
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
    context.runtime_rotary_embedding_v2 = NULL;
    context.runtime_attention = NULL;
    context.runtime_attention_v2 = NULL;
    context.runtime_swiglu = NULL;
    context.runtime_scatter = NULL;
    context.physical_plan = physical_plan;
    context.physical_plan_ir = NULL;
    context.generated_tile = generated_tile;
    context.generated_entry_ordinal = generated_entry_ordinal;

    BOOL valid = pu_info != NULL && tree != NULL &&
                 !(physical_plan != NULL && generated_tile != NULL);
    BOOL preflight_valid = FALSE;
    if (!valid) {
        VHO_DSL_Lower_Report
            (&context, "program unit or WHIRL tree is missing");
    } else {
        preflight_valid = VHO_DSL_Preflight_Physical_Plan
                              (pu_info, physical_plan, &context);
        if (preflight_valid && generated_tile != NULL) {
            const DSL_TILE_PLAN_RECORD &tile = *generated_tile;
            preflight_valid = tile.id != 0 &&
                tile.optimization_plan_id != 0 &&
                tile.legality == DSL_OPT_LEGALITY_PROVEN &&
                tile.rejection_reason == DSL_OPT_REJECT_NONE &&
                (tile.flags & DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING) != 0 &&
                tile.family != DSL_TILE_FAMILY_BASELINE &&
                tile.target_profile_id == DSL_TARGET_PROFILE_NVIDIA_HOPPER &&
                tile.source_descriptor_ty != TY_IDX_ZERO &&
                TY_is_tensor_extension(tile.source_descriptor_ty) &&
                TY_tensor_rank(tile.source_descriptor_ty) == 2 &&
                TY_mtype(TY_tensor_element_ty(tile.source_descriptor_ty)) ==
                    MTYPE_F4 &&
                tile.problem_m != 0 && tile.problem_n != 0 &&
                tile.problem_k != 0 &&
                VHO_DSL_CUDA_I4_Geometry_Safe(tile) &&
                tile.cta_m != 0 && tile.cta_n != 0 && tile.cta_k != 0 &&
                tile.thread_m != 0 && tile.thread_n != 0 &&
                tile.cta_m % tile.thread_m == 0 &&
                tile.cta_n % tile.thread_n == 0 &&
                tile.threads_per_cta ==
                    (tile.cta_m / tile.thread_m) *
                    (tile.cta_n / tile.thread_n) &&
                tile.threads_per_cta <= 1024 &&
                tile.shared_bytes_per_stage <= UINT32_MAX &&
                tile.a_shared_layout <=
                    DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED &&
                tile.b_shared_layout <= DSL_TILE_B_LAYOUT_XOR_COLUMNS &&
                (tile.b_shared_layout != DSL_TILE_B_LAYOUT_XOR_COLUMNS ||
                 (tile.cta_n >= 32 && tile.cta_n <= 1024 &&
                  (tile.cta_n & (tile.cta_n - 1)) == 0)) &&
                tile.a_shared_stride ==
                    (tile.a_shared_layout ==
                         DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                     tile.cta_m + 1 : tile.cta_k) &&
                tile.shared_bytes_per_stage ==
                    4ULL * tile.cta_k *
                    ((UINT64)(tile.a_shared_layout ==
                          DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                          tile.cta_m + 1 : tile.cta_m) + tile.cta_n) &&
                tile.shared_bytes_per_stage <= tile.shared_capacity_bytes &&
                VHO_DSL_CUDA_Edge_Policy_Consistent(tile) &&
                tile.instruction_family == DSL_TILE_INSTRUCTION_VECTOR_FMA;
            if (!preflight_valid)
                VHO_DSL_Lower_Report
                    (&context, "generated tile failed lowering preflight");
            else
                context.result.physical_plan_site_count =
                    context.result.physical_provider_site_count = 1;
        }
        valid = preflight_valid;
    }
    if (preflight_valid) {
        valid = VHO_DSL_Lower_Tree(tree, &context);
        if (!VHO_DSL_Lowered_Tree_Is_Canonical
                 (tree, diagnostic,
                  &context.result.remaining_executable_carrier_count))
            valid = FALSE;
        if (context.result.physical_provider_lowered_count !=
            context.result.physical_provider_site_count) {
            VHO_DSL_Lower_Report
                (&context,
                 "lowered %u of %u selected physical provider site(s)",
                 context.result.physical_provider_lowered_count,
                 context.result.physical_provider_site_count);
            valid = FALSE;
        }
    }

    if (result != NULL)
        *result = context.result;
    return valid;
}

/* Preflight and lower a copied PU tree using the selected physical plan;
 * install the copy only after canonical verification, leaving the original
 * tree binding intact on failure. */
BOOL
VHO_DSL_Lower_Verified_Program_Unit_With_Physical_Plan
        (struct pu_info *pu_info,
         WN **tree,
         const DSL_PHYSICAL_PLAN_ANALYSIS *physical_plan,
         FILE *diagnostic,
         VHO_DSL_LOWER_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (pu_info == NULL || tree == NULL || *tree == NULL ||
        physical_plan == NULL)
        return FALSE;
    WN *working_tree = WN_COPY_Tree(*tree);
    VHO_DSL_LOWER_RESULT working_result;
    memset(&working_result, 0, sizeof(working_result));
    BOOL valid = working_tree != NULL &&
                 VHO_DSL_Lower_Verified_Engine
                     (pu_info, working_tree, physical_plan, NULL, 0,
                      diagnostic,
                      &working_result);
    if (!valid) {
        if (working_tree != NULL)
            WN_DELETE_Tree(working_tree);
        if (result != NULL)
            *result = working_result;
        return FALSE;
    }
    *tree = working_tree;
    Set_PU_Info_tree_ptr(pu_info, working_tree);
    if (result != NULL)
        *result = working_result;
    return TRUE;
}

/* A selected tile is applied only after the copied PU lowers completely. */
BOOL
VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile
        (struct pu_info *pu_info,
         WN **tree,
         const DSL_TILE_PLAN_RECORD *tile,
         FILE *diagnostic,
         VHO_DSL_LOWER_RESULT *result)
{
    return VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile_Bound
               (pu_info, tree, tile, 0, diagnostic, result);
}

/* Apply the selected generated tile to an isolated tree copy with its device
 * ordinal; commit only after canonical/site verification. Failure preserves
 * the caller tree binding; temporary table allocations remain pool-owned. */
BOOL
VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile_Bound
        (struct pu_info *pu_info,
         WN **tree,
         const DSL_TILE_PLAN_RECORD *tile,
         UINT32 device_entry_ordinal,
         FILE *diagnostic,
         VHO_DSL_LOWER_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (pu_info == NULL || tree == NULL || *tree == NULL || tile == NULL)
        return FALSE;
    WN *working_tree = WN_COPY_Tree(*tree);
    VHO_DSL_LOWER_RESULT working_result;
    memset(&working_result, 0, sizeof(working_result));
    BOOL valid = working_tree != NULL &&
                 VHO_DSL_Lower_Verified_Engine
                     (pu_info, working_tree, NULL, tile,
                      device_entry_ordinal, diagnostic,
                      &working_result);
    if (!valid) {
        if (working_tree != NULL)
            WN_DELETE_Tree(working_tree);
        if (result != NULL)
            *result = working_result;
        return FALSE;
    }
    *tree = working_tree;
    Set_PU_Info_tree_ptr(pu_info, working_tree);
    if (result != NULL)
        *result = working_result;
    return TRUE;
}

/* Lower a gatekeeper-verified active PU directly to runtime WHIRL; the
 * caller owns the tree and must not assume copied-tree rollback on failure. */
BOOL
VHO_DSL_Lower_Verified_Program_Unit
        (struct pu_info *pu_info,
         WN *tree,
         FILE *diagnostic,
         VHO_DSL_LOWER_RESULT *result)
{
    return VHO_DSL_Lower_Verified_Engine
               (pu_info, tree, NULL, NULL, 0, diagnostic, result);
}

/* Capture PU-local straight-line definition/use order for tile analysis;
 * reject unsupported control flow rather than claiming a whole-program view. */
static DSL_TENSOR_CONTROL_SNAPSHOT *
VHO_DSL_CUDA_Linear_Snapshot (PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
    if (WN_operator(tree) != OPR_FUNC_ENTRY ||
        WN_operator(WN_func_body(tree)) != OPR_BLOCK) {
        fprintf(diagnostic, "DSL CUDA snapshot: unexpected root operator %u\n",
                (UINT32)WN_operator(tree));
        return NULL;
    }
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        DSL_tensor_control_snapshot_create(pu_info, diagnostic);
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = 1;
    block.reverse_postorder = 1;
    if (snapshot == NULL ||
        !DSL_tensor_control_snapshot_add_block(snapshot, &block, diagnostic)) {
        DSL_tensor_control_snapshot_destroy(snapshot);
        return NULL;
    }
    UINT32 order = 0;
    for (WN *statement = WN_first(WN_func_body(tree)); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_RETURN)
            continue;
        if (WN_operator(statement) != OPR_STID ||
            WN_kid_count(statement) != 1 ||
            !DSL_WN_Is_Native(WN_kid0(statement))) {
            fprintf(diagnostic,
                    "DSL CUDA snapshot: unsupported statement operator %u\n",
                    (UINT32)WN_operator(statement));
            DSL_tensor_control_snapshot_destroy(snapshot);
            return NULL;
        }
        DSL_IR_VALUE_RECORD value;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_LOGICAL_OPCODE logical;
        ST_IDX result_st = WN_st_idx(statement);
        if (!DSL_IR_Image_Find_PU_Value
                 (result_st, ST_name(WN_st(statement)),
                  ST_name(St_Table[PU_Info_proc_sym(pu_info)]), &value) ||
            value.st != result_st || value.ty != WN_ty(statement) ||
            value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
            !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
            (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
            node.result_value_id != value.id ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &descriptor) ||
            !DSL_WN_Get_Logical_Opcode
                 (WN_kid0(statement), &logical, NULL) ||
            descriptor.logical_operator != logical.dsl_operator ||
            descriptor.version != logical.source_version ||
            node.operand_count != (UINT32)WN_kid_count(WN_kid0(statement)) ||
            (node.payload == STR_IDX_ZERO && logical.payload[0] != '\0') ||
            (node.payload != STR_IDX_ZERO &&
             strcmp(Index_To_Str(node.payload), logical.payload) != 0)) {
            fprintf(diagnostic,
                    "DSL CUDA snapshot: unresolved native definition at %u\n",
                    order + 1);
            DSL_tensor_control_snapshot_destroy(snapshot);
            return NULL;
        }
        DSL_TENSOR_CONTROL_POSITION position;
        memset(&position, 0, sizeof(position));
        position.node_id = value.producer_node_id;
        position.block_id = 1;
        position.reverse_postorder = 1;
        position.statement_order = ++order;
        if (!DSL_tensor_control_snapshot_add_position
                 (snapshot, &position, diagnostic)) {
            DSL_tensor_control_snapshot_destroy(snapshot);
            return NULL;
        }
    }
    if (order == 0 || !DSL_tensor_control_snapshot_seal(snapshot, diagnostic)) {
        DSL_tensor_control_snapshot_destroy(snapshot);
        return NULL;
    }
    return snapshot;
}

/* Build the selected reduction tile as canonical, detached high-level WHIRL.
 * It remains PU-local planning evidence until the driver owns a device PU. */
static WN *
VHO_DSL_CUDA_Build_Selected_K_Loop (const DSL_TILE_PLAN_RECORD &tile)
{
    if (tile.problem_k == 0 || tile.problem_k > INT_MAX ||
        tile.cta_k == 0 || tile.cta_k > INT_MAX - tile.problem_k)
        return NULL;
    const TYPE_ID mtype = MTYPE_I4;
    PREG_NUM base = Create_Preg(mtype, "ai_k_tile_base");
    WN *index = WN_CreateIdname(base, MTYPE_To_PREG(mtype));
    WN *start = WN_StidPreg(mtype, base, WN_Intconst(mtype, 0));
    WN *end = WN_LT(mtype, WN_LdidPreg(mtype, base),
                    WN_Intconst(mtype, tile.problem_k));
    WN *step = WN_StidPreg
                   (mtype, base,
                    WN_Add(mtype, WN_LdidPreg(mtype, base),
                           WN_Intconst(mtype, tile.cta_k)));
    return WN_CreateDO(index, start, end, step, WN_CreateBlock(), NULL);
}

/* Reject a stale or malformed loop before its bounds drive CUDA emission. */
static BOOL
VHO_DSL_CUDA_Verify_Selected_K_Loop
        (const WN *loop, const DSL_TILE_PLAN_RECORD &tile)
{
    return loop != NULL && WN_operator(loop) == OPR_DO_LOOP &&
           WN_operator(WN_index(loop)) == OPR_IDNAME &&
           WN_operator(WN_start(loop)) == OPR_STID &&
           WN_st_idx(WN_index(loop)) == WN_st_idx(WN_start(loop)) &&
           WN_idname_offset(WN_index(loop)) ==
               WN_offset(WN_start(loop)) &&
           WN_operator(WN_kid0(WN_start(loop))) == OPR_INTCONST &&
           WN_const_val(WN_kid0(WN_start(loop))) == 0 &&
           WN_operator(WN_end(loop)) == OPR_LT &&
           WN_operator(WN_kid0(WN_end(loop))) == OPR_LDID &&
           WN_st_idx(WN_kid0(WN_end(loop))) ==
               WN_st_idx(WN_index(loop)) &&
           WN_offset(WN_kid0(WN_end(loop))) ==
               WN_idname_offset(WN_index(loop)) &&
           WN_operator(WN_kid1(WN_end(loop))) == OPR_INTCONST &&
           (UINT64)WN_const_val(WN_kid1(WN_end(loop))) == tile.problem_k &&
           WN_operator(WN_step(loop)) == OPR_STID &&
           WN_st_idx(WN_step(loop)) == WN_st_idx(WN_index(loop)) &&
           WN_offset(WN_step(loop)) == WN_idname_offset(WN_index(loop)) &&
           WN_operator(WN_kid0(WN_step(loop))) == OPR_ADD &&
           WN_operator(WN_kid0(WN_kid0(WN_step(loop)))) == OPR_LDID &&
           WN_st_idx(WN_kid0(WN_kid0(WN_step(loop)))) ==
               WN_st_idx(WN_index(loop)) &&
           WN_offset(WN_kid0(WN_kid0(WN_step(loop)))) ==
               WN_idname_offset(WN_index(loop)) &&
           WN_operator(WN_kid1(WN_kid0(WN_step(loop)))) == OPR_INTCONST &&
           (UINT64)WN_const_val
               (WN_kid1(WN_kid0(WN_step(loop)))) == tile.cta_k &&
           WN_operator(WN_do_body(loop)) == OPR_BLOCK &&
           WN_first(WN_do_body(loop)) == NULL;
}

struct VHO_DSL_CUDA_WH_CONTEXT {
    TY_IDX float_pointer_ty;
    ST *a, *b, *c, *as, *bs, *acc;
    ST *tid, *local_row, *local_col, *row0, *col0;
    ST *base, *offset, *q, *i, *j, *av;
    ST *thread_idx, *block_idx, *barrier;
    ST *slot, *ready, *a_map, *b_map;
    ST *ready_init, *proxy_publish, *acquire, *issue, *wait, *invalidate;
    const DSL_TILE_PLAN_RECORD *tile;
    const DSL_FETCH_PLAN_RECORD *fetch;
};

/* Preflight the selected tile/fetch pair for this executable lowering: one
 * synchronous slot or exact compact two-slot TMA with barrier capacity.
 * Unsupported engines/layouts/resources fail without constructing a device PU. */
static BOOL
VHO_DSL_CUDA_Pipeline_Valid (const DSL_TILE_PLAN_RECORD &tile,
                             const DSL_FETCH_PLAN_RECORD &fetch)
{
    return VHO_DSL_fetch_pipeline_can_lower(tile, fetch);
}

/* Create an automatic symbol in the currently owned device scope; callers
 * must supply its exact type and attach any CUDA address-space attributes. */
static ST *
VHO_DSL_CUDA_New_Local (const char *name, TY_IDX ty)
{
    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, ty);
    return st;
}

/* Create a fresh signed-I4 load of a device-local scalar; no tree node is
 * shared with another parent. */
static WN *
VHO_DSL_CUDA_I4_Load (ST *st)
{
    return WN_Ldid(MTYPE_I4, 0, ST_st_idx(st), ST_type(st));
}

/* Create a device-local signed-I4 assignment, taking ownership of its value
 * tree; the schedule builder owns placement and ordering. */
static WN *
VHO_DSL_CUDA_I4_Store (ST *st, WN *value)
{
    return WN_Stid(MTYPE_I4, 0, st, ST_type(st), value);
}

/* Create a fresh signed-I4 constant for a previously range-checked plan. */
static WN *
VHO_DSL_CUDA_I4 (INT32 value)
{
    return WN_Intconst(MTYPE_I4, value);
}

/* Build the conjunction of only the needed upper-bound predicates; consume
 * supplied expression trees and return NULL when neither axis needs a guard. */
static WN *
VHO_DSL_CUDA_Bounds2 (WN *first, INT32 first_extent,
                      WN *second, INT32 second_extent)
{
    if (first == NULL)
        return second == NULL ? NULL :
            WN_LT(MTYPE_I4, second, VHO_DSL_CUDA_I4(second_extent));
    WN *first_test =
        WN_LT(MTYPE_I4, first, VHO_DSL_CUDA_I4(first_extent));
    return second == NULL ? first_test :
        WN_CAND(first_test,
                WN_LT(MTYPE_I4, second,
                      VHO_DSL_CUDA_I4(second_extent)));
}

/* Build an F32 element address from a formal pointer or local array; consume
 * the index tree. Geometry legality is established before body construction. */
static WN *
VHO_DSL_CUDA_Index (ST *array, WN *index)
{
    WN *base = ST_sclass(array) == SCLASS_FORMAL ?
        WN_Ldid(Pointer_Mtype, 0, ST_st_idx(array), ST_type(array)) :
        WN_Lda(Pointer_Mtype, 0, array);
    WN *bytes = WN_Mpy
        (Pointer_Mtype, WN_Cvt(MTYPE_I4, Pointer_Mtype, index),
         WN_Intconst(Pointer_Mtype, sizeof(float)));
    return WN_Add(Pointer_Mtype, base, bytes);
}

/* Create a typed F32 memory read at a selected physical element address;
 * consume its index without choosing layout or placement. */
static WN *
VHO_DSL_CUDA_F4_Load (const VHO_DSL_CUDA_WH_CONTEXT &context,
                      ST *array, WN *index)
{
    return WN_Iload(MTYPE_F4, 0, MTYPE_To_TY(MTYPE_F4),
                    VHO_DSL_CUDA_Index(array, index));
}

/* Create a typed F32 store at a selected physical element address; consume
 * index/value trees, with ordering supplied by the schedule builder. */
static WN *
VHO_DSL_CUDA_F4_Store (const VHO_DSL_CUDA_WH_CONTEXT &context,
                       ST *array, WN *index, WN *value)
{
    return WN_Istore(MTYPE_F4, 0, context.float_pointer_ty,
                     VHO_DSL_CUDA_Index(array, index), value);
}

/* Construct a high-level DO_LOOP around an owned body and selected limits;
 * every index/load/step is a fresh node to preserve strict-tree ownership. */
static WN *
VHO_DSL_CUDA_Do (ST *index, WN *first, WN *limit, INT32 step, WN *body)
{
    return WN_CreateDO
        (WN_CreateIdname(0, index), VHO_DSL_CUDA_I4_Store(index, first),
         WN_LT(MTYPE_I4, VHO_DSL_CUDA_I4_Load(index), limit),
         VHO_DSL_CUDA_I4_Store
             (index, WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(index),
                            VHO_DSL_CUDA_I4(step))),
         body, NULL);
}

/* Read one CUDA execution-coordinate field through its recorded symbol/type;
 * field identity and offsets must match the dim3 declaration. */
static WN *
VHO_DSL_CUDA_Builtin_Dim (ST *symbol, INT32 offset, UINT field_id)
{
    return WN_Cvt(MTYPE_U4, MTYPE_I4,
                  WN_Ldid(MTYPE_U4, offset, ST_st_idx(symbol),
                          ST_type(symbol), field_id));
}

/* Reuse or declare one global CUDA coordinate identity, so multiple device
 * PUs cannot rename the builtin. This lookup is not cross-PU analysis. */
static ST *
VHO_DSL_CUDA_Builtin_Symbol (const char *name, TY_IDX dim_ty)
{
    const ST *existing;
    ST_IDX index;
    FOREACH_SYMBOL(GLOBAL_SYMTAB, existing, index) {
        if (ST_class(existing) == CLASS_VAR && strcmp(ST_name(existing), name) == 0) {
            FmtAssert(ST_sclass(existing) == SCLASS_EXTERN &&
                      TY_kind(ST_type(existing)) == KIND_STRUCT &&
                      TY_size(ST_type(existing)) == TY_size(dim_ty),
                      ("CUDA coordinate builtin has an incompatible declaration"));
            return &St_Table[ST_st_idx(existing)];
        }
    }
    ST *st = New_ST(GLOBAL_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_EXTERN,
            EXPORT_PREEMPTIBLE, dim_ty);
    return st;
}

/* Construct the three-U4 coordinate type used by threadIdx/blockIdx reads;
 * retain explicit field offsets for mapped-image and CUDA emission. */
static TY_IDX
VHO_DSL_CUDA_Dim_Type ()
{
    TY_IDX ty_idx;
    TY &ty = New_TY(ty_idx);
    TY_Init(ty, 12, KIND_STRUCT, MTYPE_M, Save_Str("open64_cuda_dim3"));
    Set_TY_align(ty_idx, 4);
    const char *names[] = {"x", "y", "z"};
    for (UINT32 coordinate = 0; coordinate < 3; ++coordinate) {
        FLD_HANDLE field = New_FLD();
        if (coordinate == 0)
            Set_TY_fld(ty, field);
        FLD_Init(field, Save_Str(names[coordinate]),
                 MTYPE_To_TY(MTYPE_U4), coordinate * 4);
        if (coordinate == 2)
            Set_FLD_last_field(field);
    }
    return ty_idx;
}

/* Declare the uniform CTA barrier builtin; calls remain effectful and the
 * schedule builder must prove convergence before inserting them. */
static ST *
VHO_DSL_CUDA_Barrier_Symbol ()
{
    TY_IDX type = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, type, GLOBAL_SYMTAB + 1);
    Set_PU_c_lang(pu);
    ST *st = New_ST(GLOBAL_SYMTAB);
    ST_Init(st, Save_Str("__syncthreads"), CLASS_FUNC, SCLASS_EXTERN,
            EXPORT_PREEMPTIBLE, type);
    Set_ST_pu(st, pu_idx);
    return st;
}

/* Create an effectful CTA barrier call; caller placement supplies the
 * publication or release invariant, not this leaf constructor. */
static WN *
VHO_DSL_CUDA_Barrier (ST *barrier)
{
    return WN_Call(MTYPE_V, MTYPE_V, 0, ST_st_idx(barrier));
}

/* Declare a typed device-operation ABI with no purity assumptions; this is
 * target-mechanism binding, never a scheduling or profitability decision. */
static ST *
VHO_DSL_CUDA_Operation (const char *name, UINT32 count,
                        const TY_IDX *parameters)
{
    const ST *existing;
    ST_IDX index;
    FOREACH_SYMBOL(GLOBAL_SYMTAB, existing, index) {
        if (ST_class(existing) != CLASS_FUNC || strcmp(ST_name(existing), name) != 0)
            continue;
        FmtAssert(ST_device_symbol_kind(*existing) == DEVICE_SYMBOL_FUNCTION,
                  ("CUDA operation name collides with a non-device function"));
        TYLIST_IDX types = TY_tylist(ST_pu_type(existing));
        FmtAssert(TYLIST_type(Tylist_Table[types]) == MTYPE_To_TY(MTYPE_V),
                  ("CUDA pipeline operation has incompatible return type"));
        for (UINT32 i = 0; i < count; ++i)
            FmtAssert(TYLIST_type(Tylist_Table[types + i + 1]) == parameters[i],
                      ("CUDA pipeline operation has incompatible parameter type"));
        FmtAssert(TYLIST_type(Tylist_Table[types + count + 1]) == TY_IDX_ZERO,
                  ("CUDA pipeline operation has incompatible arity"));
        return &St_Table[ST_st_idx(existing)];
    }
    TY_IDX type;
    TY &ty = New_TY(type);
    TY_Init(ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_has_prototype(type);
    TYLIST_IDX list;
    Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(ty, list);
    for (UINT32 i = 0; i < count; ++i)
        Set_TYLIST_type(New_TYLIST(list), parameters[i]);
    Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, type, GLOBAL_SYMTAB + 1);
    Set_PU_c_lang(pu);
    ST *st = New_ST(GLOBAL_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_FUNC, SCLASS_EXTERN,
            EXPORT_PREEMPTIBLE, type);
    Set_ST_pu(st, pu_idx);
    Set_ST_device_symbol_kind(*st, DEVICE_SYMBOL_FUNCTION);
    return st;
}

/* Create an effectful typed device call, transferring argument-tree
 * ownership to its parameters; the caller supplies the complete operation ABI. */
static WN *
VHO_DSL_CUDA_Operation_Call (ST *operation, const std::vector<WN *> &args)
{
    WN *call = WN_Call(MTYPE_V, MTYPE_V, args.size(), ST_st_idx(operation));
    TYLIST_IDX types = TY_tylist(ST_pu_type(operation)) + 1;
    for (UINT32 i = 0; i < args.size(); ++i)
        WN_kid(call, i) = WN_CreateParm
            (WN_rtype(args[i]), args[i], Tylist_Table[types + i], WN_PARM_BY_VALUE);
    return call;
}

/* Build the eight-byte ready-barrier address for a proven slot index;
 * consume the slot expression and preserve the selected shared-storage base. */
static WN *
VHO_DSL_CUDA_Ready_Address (const VHO_DSL_CUDA_WH_CONTEXT &c, WN *slot)
{
    return WN_Add(Pointer_Mtype, WN_Lda(Pointer_Mtype, 0, c.ready),
                   WN_Mpy(Pointer_Mtype,
                          WN_Cvt(MTYPE_I4, Pointer_Mtype, slot),
                          WN_Intconst(Pointer_Mtype, 8)));
}

/* Guard an owned producer block by elected thread zero and an optional
 * refill predicate; uniform consumer barriers must stay outside this branch. */
static WN *
VHO_DSL_CUDA_Producer_Only (const VHO_DSL_CUDA_WH_CONTEXT &c, WN *body,
                           WN *extra = NULL)
{
    WN *test = WN_EQ(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.tid),
                      VHO_DSL_CUDA_I4(0));
    if (extra != NULL) test = WN_CAND(test, extra);
    return WN_CreateIf(test, body, WN_CreateBlock());
}

/* Build acquire/expect-bytes followed by A/B issues into one selected slot;
 * consume the tile-base expression. The enclosing prologue/refill must prove
 * that the slot is free or CTA-released before entering this block. */
static WN *
VHO_DSL_CUDA_TMA_Issue_Pair (const VHO_DSL_CUDA_WH_CONTEXT &c, WN *base)
{
    WN *body = WN_CreateBlock();
    std::vector<WN *> args;
    args.push_back(VHO_DSL_CUDA_Ready_Address(c, VHO_DSL_CUDA_I4_Load(c.slot)));
    args.push_back(VHO_DSL_CUDA_I4(c.tile->shared_bytes_per_stage));
    WN_INSERT_BlockLast(body, VHO_DSL_CUDA_Operation_Call(c.acquire, args));
    for (UINT32 operand = 0; operand < 2; ++operand) {
        args.clear();
        args.push_back(VHO_DSL_CUDA_Index
            (operand == 0 ? c.as : c.bs,
             WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.slot),
                    VHO_DSL_CUDA_I4(c.tile->cta_k *
                        (operand == 0 ? c.tile->cta_m : c.tile->cta_n)))));
        ST *map = operand == 0 ? c.a_map : c.b_map;
        args.push_back(WN_Ldid(Pointer_Mtype, 0, ST_st_idx(map), ST_type(map)));
        args.push_back(operand == 0 ? WN_COPY_Tree(base) :
                                                   VHO_DSL_CUDA_I4_Load(c.col0));
        args.push_back(operand == 0 ? VHO_DSL_CUDA_I4_Load(c.row0) :
                                                   WN_COPY_Tree(base));
        args.push_back(VHO_DSL_CUDA_Ready_Address(c, VHO_DSL_CUDA_I4_Load(c.slot)));
        WN_INSERT_BlockLast(body, VHO_DSL_CUDA_Operation_Call(c.issue, args));
    }
    WN_DELETE_Tree(base);
    return body;
}

/* Collect direct calls in structured lexical order for the pipeline proof;
 * this read-only walk does not reorder or infer effects. */
static void
VHO_DSL_CUDA_Collect_Calls (WN *tree, std::vector<WN *> *calls)
{
    if (tree == NULL) return;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt))
            VHO_DSL_CUDA_Collect_Calls(stmt, calls);
    } else {
        if (WN_operator(tree) == OPR_CALL) calls->push_back(tree);
        for (INT32 i = 0; i < WN_kid_count(tree); ++i)
            VHO_DSL_CUDA_Collect_Calls(WN_kid(tree, i), calls);
    }
}

/* Prove canonical selected bounds/index/step; a different generation extent
 * must not be accepted merely because its operation sequence looks similar. */
static BOOL
VHO_DSL_CUDA_Verify_Pipeline_Loop (WN *loop, ST *index, INT32 limit, INT32 step)
{
    if (loop == NULL || WN_operator(loop) != OPR_DO_LOOP ||
        WN_operator(WN_start(loop)) != OPR_STID || WN_st(WN_start(loop)) != index ||
        WN_operator(WN_kid0(WN_start(loop))) != OPR_INTCONST ||
        WN_const_val(WN_kid0(WN_start(loop))) != 0 ||
        WN_operator(WN_step(loop)) != OPR_STID || WN_st(WN_step(loop)) != index)
        return FALSE;
    WN *end = WN_LT(MTYPE_I4, VHO_DSL_CUDA_I4_Load(index), VHO_DSL_CUDA_I4(limit));
    WN *increment = WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(index), VHO_DSL_CUDA_I4(step));
    const BOOL valid = WN_Simp_Compare_Trees(WN_end(loop), end) == 0 &&
        WN_Simp_Compare_Trees(WN_kid0(WN_step(loop)), increment) == 0;
    WN_DELETE_Tree(end);
    WN_DELETE_Tree(increment);
    return valid;
}

/* Check constructed WHIRL publication, wait/compute/release/refill order
 * and slot/parity expressions against the fixed selected schedule. Reject
 * malformed ordering before the driver may publish the device image. */
static BOOL
VHO_DSL_CUDA_Verify_Pipeline_Order
        (const VHO_DSL_CUDA_WH_CONTEXT &c, WN *body, WN *k_loop,
         WN *compute, WN *prologue, WN *refill)
{
    std::vector<WN *> calls;
    VHO_DSL_CUDA_Collect_Calls(body, &calls);
    if (!VHO_DSL_CUDA_Verify_Pipeline_Loop
            (k_loop, c.base, c.tile->problem_k, c.tile->cta_k) ||
        !VHO_DSL_CUDA_Verify_Pipeline_Loop(compute, c.q, c.tile->cta_k, 1))
        return FALSE;
    if ((c.fetch->flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0)
        return calls.empty() && c.as == NULL && c.bs == NULL && c.ready == NULL &&
            WN_first(WN_do_body(k_loop)) == compute && WN_next(compute) == NULL &&
            WN_operator(WN_first(WN_do_body(compute))) == OPR_IF;
    if (c.ready == NULL)
        return calls.size() == 2 && WN_st(calls[0]) == c.barrier &&
               WN_st(calls[1]) == c.barrier &&
               WN_prev(compute) == calls[0] && WN_next(compute) == calls[1] &&
               WN_first(WN_do_body(k_loop)) != compute;
    ST *expected[] = {c.ready_init, c.ready_init, c.proxy_publish, c.barrier,
                      c.acquire, c.issue, c.issue, c.wait, c.barrier,
                      c.acquire, c.issue, c.issue, c.invalidate, c.invalidate};
    if (calls.size() != sizeof(expected) / sizeof(expected[0])) return FALSE;
    for (UINT32 i = 0; i < calls.size(); ++i)
        if (WN_st(calls[i]) != expected[i]) return FALSE;
    WN *slot = WN_first(WN_do_body(k_loop));
    WN *expected_slot = WN_Binary(OPR_REM, MTYPE_I4,
        WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
               VHO_DSL_CUDA_I4(c.tile->cta_k)), VHO_DSL_CUDA_I4(2));
    WN *expected_phase = WN_Binary(OPR_REM, MTYPE_I4,
        WN_Div(MTYPE_I4,
            WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                   VHO_DSL_CUDA_I4(c.tile->cta_k)), VHO_DSL_CUDA_I4(2)),
        VHO_DSL_CUDA_I4(2));
    WN *expected_guard = WN_CAND(
        WN_EQ(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.tid), VHO_DSL_CUDA_I4(0)),
        WN_LT(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
              VHO_DSL_CUDA_I4((INT32)c.tile->problem_k - 2 * (INT32)c.tile->cta_k)));
    const BOOL valid = WN_operator(slot) == OPR_STID && WN_st(slot) == c.slot &&
        WN_Simp_Compare_Trees(WN_kid0(slot), expected_slot) == 0 &&
        WN_next(slot) == calls[7] && WN_next(calls[7]) == compute &&
        WN_next(compute) == calls[8] && WN_next(calls[8]) == refill &&
        WN_next(refill) == NULL &&
        WN_Simp_Compare_Trees(WN_kid0(WN_kid(calls[7], 1)), expected_phase) == 0 &&
        WN_Simp_Compare_Trees(WN_if_test(refill), expected_guard) == 0 &&
        WN_prev(k_loop) == prologue &&
        VHO_DSL_CUDA_Verify_Pipeline_Loop
            (WN_first(WN_then(prologue)), c.slot,
             std::min((UINT64)2, c.tile->problem_k / c.tile->cta_k), 1);
    WN_DELETE_Tree(expected_slot);
    WN_DELETE_Tree(expected_phase);
    WN_DELETE_Tree(expected_guard);
    return valid;
}

/* Lower the selected compact/XOR B representation into a fresh address
 * expression; copy repeated column subtrees to preserve strict WHIRL trees. */
static WN *
VHO_DSL_CUDA_B_Shared_Index (const DSL_TILE_PLAN_RECORD &tile,
                             WN *row, WN *col)
{
    if (tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS)
        col = WN_Binary(OPR_BXOR, MTYPE_I4, col,
                       WN_Div(MTYPE_I4, WN_COPY_Tree(col),
                              VHO_DSL_CUDA_I4(32)));
    return WN_Add(MTYPE_I4,
                   WN_Mpy(MTYPE_I4, row, VHO_DSL_CUDA_I4(tile.cta_n)), col);
}

/* Construct D's register-tiled global-load loop in the active device PU.
 * A is reused across j, B is loaded per i/j, and guards precede every
 * potentially invalid load. Caller preflights the plan and owns the returned
 * pool tree; no shared symbol, barrier, or async call is constructed here. */
static WN *
VHO_DSL_cuda_build_direct_loop (VHO_DSL_CUDA_WH_CONTEXT &c)
{
    const DSL_TILE_PLAN_RECORD &tile = *c.tile;
    WN *q_body = WN_CreateBlock(), *i_body = WN_CreateBlock(), *j_body = WN_CreateBlock();
    WN *row = WN_Add(MTYPE_I4,
        WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.row0), VHO_DSL_CUDA_I4_Load(c.local_row)),
        VHO_DSL_CUDA_I4_Load(c.i));
    WN *col = WN_Add(MTYPE_I4,
        WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.col0), VHO_DSL_CUDA_I4_Load(c.local_col)),
        VHO_DSL_CUDA_I4_Load(c.j));
    WN *q = WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base), VHO_DSL_CUDA_I4_Load(c.q));
    WN *a_index = WN_Add(MTYPE_I4,
        WN_Mpy(MTYPE_I4, WN_COPY_Tree(row), VHO_DSL_CUDA_I4(tile.problem_k)), WN_COPY_Tree(q));
    WN *a_then = WN_CreateBlock(), *a_else = WN_CreateBlock();
    WN_INSERT_BlockLast(a_then, WN_Stid(MTYPE_F4, 0, c.av, ST_type(c.av),
                                       VHO_DSL_CUDA_F4_Load(c, c.a, a_index)));
    WN_INSERT_BlockLast(a_else, WN_Stid(MTYPE_F4, 0, c.av, ST_type(c.av), WN_Floatconst(MTYPE_F4, 0)));
    WN_INSERT_BlockLast(i_body, WN_CreateIf(WN_LT(MTYPE_I4, WN_COPY_Tree(row),
        VHO_DSL_CUDA_I4(tile.problem_m)), a_then, a_else));
    WN *b_index = WN_Add(MTYPE_I4,
        WN_Mpy(MTYPE_I4, WN_COPY_Tree(q), VHO_DSL_CUDA_I4(tile.problem_n)), WN_COPY_Tree(col));
    WN *acc_index = WN_Add(MTYPE_I4,
        WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.i), VHO_DSL_CUDA_I4(tile.thread_n)),
        VHO_DSL_CUDA_I4_Load(c.j));
    WN *sum = WN_Add(MTYPE_F4, VHO_DSL_CUDA_F4_Load(c, c.acc, WN_COPY_Tree(acc_index)),
        WN_Mpy(MTYPE_F4, WN_Ldid(MTYPE_F4, 0, ST_st_idx(c.av), ST_type(c.av)),
            VHO_DSL_CUDA_F4_Load(c, c.b, b_index)));
    WN *update = WN_CreateBlock();
    WN_INSERT_BlockLast(update, VHO_DSL_CUDA_F4_Store(c, c.acc, acc_index, sum));
    WN_INSERT_BlockLast(j_body, WN_CreateIf(VHO_DSL_CUDA_Bounds2(row, tile.problem_m,
        col, tile.problem_n), update, WN_CreateBlock()));
    WN_INSERT_BlockLast(i_body, VHO_DSL_CUDA_Do(c.j, VHO_DSL_CUDA_I4(0),
        VHO_DSL_CUDA_I4(tile.thread_n), 1, j_body));
    WN_INSERT_BlockLast(q_body, VHO_DSL_CUDA_Do(c.i, VHO_DSL_CUDA_I4(0),
        VHO_DSL_CUDA_I4(tile.thread_m), 1, i_body));
    WN *guard = WN_CreateBlock();
    WN_INSERT_BlockLast(guard, WN_CreateIf(WN_LT(MTYPE_I4, q, VHO_DSL_CUDA_I4(tile.problem_k)),
        q_body, WN_CreateBlock()));
    return VHO_DSL_CUDA_Do(c.q, VHO_DSL_CUDA_I4(0), VHO_DSL_CUDA_I4(tile.cta_k), 1, guard);
}

/* Construct device control flow from the selected tile/fetch pair, including
 * slot storage, predication, synchronization and epilogue. Assert structural
 * schedule proof before returning; no independent emitter policy is allowed. */
static WN *
VHO_DSL_CUDA_Build_Body (VHO_DSL_CUDA_WH_CONTEXT &c)
{
    const DSL_TILE_PLAN_RECORD &tile = *c.tile;
    const INT32 bm = tile.cta_m, bn = tile.cta_n, bk = tile.cta_k;
    const INT32 tm = tile.thread_m, tn = tile.thread_n;
    const INT32 threads = tile.threads_per_cta;
    const INT32 m = tile.problem_m, k = tile.problem_k, n = tile.problem_n;
    const BOOL guard_m = tile.problem_m % tile.cta_m != 0;
    const BOOL guard_n = tile.problem_n % tile.cta_n != 0;
    const BOOL guard_k = tile.problem_k % tile.cta_k != 0;
    WN *body = WN_CreateBlock();
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_I4_Store
                   (c.tid, VHO_DSL_CUDA_Builtin_Dim(c.thread_idx, 0, 1)));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_I4_Store
                   (c.local_row,
                    WN_Mpy(MTYPE_I4,
                           WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.tid),
                                  VHO_DSL_CUDA_I4(bn / tn)),
                           VHO_DSL_CUDA_I4(tm))));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_I4_Store
                   (c.local_col,
                    WN_Mpy(MTYPE_I4,
                           WN_Binary(OPR_REM, MTYPE_I4,
                                     VHO_DSL_CUDA_I4_Load(c.tid),
                                     VHO_DSL_CUDA_I4(bn / tn)),
                           VHO_DSL_CUDA_I4(tn))));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_I4_Store
                   (c.row0, WN_Mpy(MTYPE_I4,
                                    VHO_DSL_CUDA_Builtin_Dim(c.block_idx, 4, 2),
                                    VHO_DSL_CUDA_I4(bm))));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_I4_Store
                   (c.col0, WN_Mpy(MTYPE_I4,
                                    VHO_DSL_CUDA_Builtin_Dim(c.block_idx, 0, 1),
                                    VHO_DSL_CUDA_I4(bn))));

    WN *initialize = WN_CreateBlock();
    WN_INSERT_BlockLast
        (initialize, VHO_DSL_CUDA_F4_Store
                         (c, c.acc, VHO_DSL_CUDA_I4_Load(c.offset),
                          WN_Floatconst(MTYPE_F4, 0.0)));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_Do(c.offset, VHO_DSL_CUDA_I4(0),
                               VHO_DSL_CUDA_I4(tm * tn), 1, initialize));

    WN *k_body = WN_CreateBlock();
    const BOOL direct = (c.fetch->flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0;
    WN *compute = NULL, *prologue = NULL, *refill = NULL;
    if (!direct) {
        WN *a_body = WN_CreateBlock();
        WN *a_row = WN_Add
            (MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.row0),
             WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                    VHO_DSL_CUDA_I4(bk)));
        WN *a_col = WN_Add
            (MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
             WN_Binary(OPR_REM, MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                       VHO_DSL_CUDA_I4(bk)));
        WN *a_index = WN_Add(MTYPE_I4, WN_Mpy(MTYPE_I4, a_row,
                                              VHO_DSL_CUDA_I4(k)), a_col);
        WN *a_smem_index = tile.a_shared_layout ==
                DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
            WN_Add(MTYPE_I4,
                   WN_Mpy(MTYPE_I4,
                          WN_Binary(OPR_REM, MTYPE_I4,
                                    VHO_DSL_CUDA_I4_Load(c.offset),
                                    VHO_DSL_CUDA_I4(bk)),
                          VHO_DSL_CUDA_I4(bm + 1)),
                   WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                          VHO_DSL_CUDA_I4(bk))) :
            VHO_DSL_CUDA_I4_Load(c.offset);
        WN *a_store = VHO_DSL_CUDA_F4_Store
            (c, c.as, a_smem_index,
             VHO_DSL_CUDA_F4_Load(c, c.a, a_index));
        if (guard_m || guard_k) {
            WN *valid = VHO_DSL_CUDA_Bounds2
                (guard_m ?
                     WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.row0),
                            WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                                   VHO_DSL_CUDA_I4(bk))) : NULL, m,
                 guard_k ?
                     WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                            WN_Binary(OPR_REM, MTYPE_I4,
                                      VHO_DSL_CUDA_I4_Load(c.offset),
                                      VHO_DSL_CUDA_I4(bk))) : NULL, k);
            WN *then_block = WN_CreateBlock();
            WN *else_block = WN_CreateBlock();
            WN_INSERT_BlockLast(then_block, a_store);
            WN_INSERT_BlockLast
                (else_block, VHO_DSL_CUDA_F4_Store
                                 (c, c.as,
                                  tile.a_shared_layout ==
                                      DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                                  WN_Add(MTYPE_I4,
                                         WN_Mpy(MTYPE_I4,
                                                WN_Binary(OPR_REM, MTYPE_I4,
                                                    VHO_DSL_CUDA_I4_Load(c.offset),
                                                    VHO_DSL_CUDA_I4(bk)),
                                                VHO_DSL_CUDA_I4(bm + 1)),
                                         WN_Div(MTYPE_I4,
                                                VHO_DSL_CUDA_I4_Load(c.offset),
                                                VHO_DSL_CUDA_I4(bk))) :
                                  VHO_DSL_CUDA_I4_Load(c.offset),
                                  WN_Floatconst(MTYPE_F4, 0.0)));
            WN_INSERT_BlockLast
                (a_body, WN_CreateIf(valid, then_block, else_block));
        } else {
            WN_INSERT_BlockLast(a_body, a_store);
        }
        WN_INSERT_BlockLast
            (k_body, VHO_DSL_CUDA_Do
                         (c.offset, VHO_DSL_CUDA_I4_Load(c.tid),
                          VHO_DSL_CUDA_I4(bm * bk), threads, a_body));

        WN *b_body = WN_CreateBlock();
        WN *b_row = WN_Add
            (MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
             WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                    VHO_DSL_CUDA_I4(bn)));
        WN *b_col = WN_Add
            (MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.col0),
             WN_Binary(OPR_REM, MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                       VHO_DSL_CUDA_I4(bn)));
        WN *b_index = WN_Add(MTYPE_I4, WN_Mpy(MTYPE_I4, b_row,
                                              VHO_DSL_CUDA_I4(n)), b_col);
        WN *b_store = VHO_DSL_CUDA_F4_Store
            (c, c.bs, VHO_DSL_CUDA_B_Shared_Index
                (tile, WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                              VHO_DSL_CUDA_I4(bn)),
                 WN_Binary(OPR_REM, MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                           VHO_DSL_CUDA_I4(bn))),
             VHO_DSL_CUDA_F4_Load(c, c.b, b_index));
        if (guard_k || guard_n) {
            WN *valid = VHO_DSL_CUDA_Bounds2
                (guard_k ?
                     WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                            WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.offset),
                                   VHO_DSL_CUDA_I4(bn))) : NULL, k,
                 guard_n ?
                     WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.col0),
                            WN_Binary(OPR_REM, MTYPE_I4,
                                      VHO_DSL_CUDA_I4_Load(c.offset),
                                      VHO_DSL_CUDA_I4(bn))) : NULL, n);
            WN *then_block = WN_CreateBlock();
            WN *else_block = WN_CreateBlock();
            WN_INSERT_BlockLast(then_block, b_store);
            WN_INSERT_BlockLast
                (else_block, VHO_DSL_CUDA_F4_Store
                                 (c, c.bs, VHO_DSL_CUDA_B_Shared_Index
                                     (tile, WN_Div(MTYPE_I4,
                                         VHO_DSL_CUDA_I4_Load(c.offset),
                                         VHO_DSL_CUDA_I4(bn)),
                                      WN_Binary(OPR_REM, MTYPE_I4,
                                         VHO_DSL_CUDA_I4_Load(c.offset),
                                         VHO_DSL_CUDA_I4(bn))),
                                  WN_Floatconst(MTYPE_F4, 0.0)));
            WN_INSERT_BlockLast
                (b_body, WN_CreateIf(valid, then_block, else_block));
        } else {
            WN_INSERT_BlockLast(b_body, b_store);
        }
        WN_INSERT_BlockLast
            (k_body, VHO_DSL_CUDA_Do
                         (c.offset, VHO_DSL_CUDA_I4_Load(c.tid),
                          VHO_DSL_CUDA_I4(bk * bn), threads, b_body));
        WN_INSERT_BlockLast(k_body, VHO_DSL_CUDA_Barrier(c.barrier));

        WN *j_body = WN_CreateBlock();
        WN *acc_index = WN_Add
            (MTYPE_I4,
             WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.i), VHO_DSL_CUDA_I4(tn)),
             VHO_DSL_CUDA_I4_Load(c.j));
        WN *as_row = WN_Add(MTYPE_I4,
                             VHO_DSL_CUDA_I4_Load(c.local_row),
                             VHO_DSL_CUDA_I4_Load(c.i));
        WN *as_index = tile.a_shared_layout ==
                DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
            WN_Add(MTYPE_I4,
                   WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.q),
                          VHO_DSL_CUDA_I4(bm + 1)), as_row) :
            WN_Add(MTYPE_I4,
                   WN_Mpy(MTYPE_I4, as_row, VHO_DSL_CUDA_I4(bk)),
                   VHO_DSL_CUDA_I4_Load(c.q));
        if (c.ready != NULL)
            as_index = WN_Add(MTYPE_I4, as_index,
                WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.slot),
                       VHO_DSL_CUDA_I4(bm * bk)));
        WN *bs_index = VHO_DSL_CUDA_B_Shared_Index
            (tile, VHO_DSL_CUDA_I4_Load(c.q),
             WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.local_col),
                    VHO_DSL_CUDA_I4_Load(c.j)));
        if (c.ready != NULL)
            bs_index = WN_Add(MTYPE_I4, bs_index,
                WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.slot),
                       VHO_DSL_CUDA_I4(bk * bn)));
        WN *sum = WN_Add
            (MTYPE_F4,
             VHO_DSL_CUDA_F4_Load
                 (c, c.acc,
                  WN_Add(MTYPE_I4,
                         WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.i),
                                VHO_DSL_CUDA_I4(tn)),
                         VHO_DSL_CUDA_I4_Load(c.j))),
             WN_Mpy(MTYPE_F4,
                    WN_Ldid(MTYPE_F4, 0, ST_st_idx(c.av), ST_type(c.av)),
                    VHO_DSL_CUDA_F4_Load(c, c.bs, bs_index)));
        WN_INSERT_BlockLast(j_body,
            VHO_DSL_CUDA_F4_Store(c, c.acc, acc_index, sum));
        WN *i_body = WN_CreateBlock();
        WN_INSERT_BlockLast
            (i_body, WN_Stid(MTYPE_F4, 0, c.av, ST_type(c.av),
                             VHO_DSL_CUDA_F4_Load(c, c.as, as_index)));
        WN_INSERT_BlockLast
            (i_body, VHO_DSL_CUDA_Do(c.j, VHO_DSL_CUDA_I4(0),
                                     VHO_DSL_CUDA_I4(tn), 1, j_body));
        WN *q_body = WN_CreateBlock();
        WN_INSERT_BlockLast
            (q_body, VHO_DSL_CUDA_Do(c.i, VHO_DSL_CUDA_I4(0),
                                     VHO_DSL_CUDA_I4(tm), 1, i_body));
        compute = VHO_DSL_CUDA_Do(c.q, VHO_DSL_CUDA_I4(0),
                                      VHO_DSL_CUDA_I4(bk), 1, q_body);
        WN_INSERT_BlockLast(k_body, compute);
        WN_INSERT_BlockLast(k_body, VHO_DSL_CUDA_Barrier(c.barrier));
        if (c.ready != NULL) {
            WN_EXTRACT_FromBlock(k_body, compute);
            WN_DELETE_Tree(k_body);
            WN *init = WN_CreateBlock();
            for (UINT32 slot = 0; slot < 2; ++slot) {
                std::vector<WN *> args;
                args.push_back(VHO_DSL_CUDA_Ready_Address(c, VHO_DSL_CUDA_I4(slot)));
                WN_INSERT_BlockLast(init, VHO_DSL_CUDA_Operation_Call(c.ready_init, args));
            }
            std::vector<WN *> args;
            WN_INSERT_BlockLast(init, VHO_DSL_CUDA_Operation_Call(c.proxy_publish, args));
            WN_INSERT_BlockLast(body, VHO_DSL_CUDA_Producer_Only(c, init));
            WN_INSERT_BlockLast(body, VHO_DSL_CUDA_Barrier(c.barrier));
            WN *prime = WN_CreateBlock();
            WN_INSERT_BlockLast(prime,
                VHO_DSL_CUDA_Do(c.slot, VHO_DSL_CUDA_I4(0),
                    VHO_DSL_CUDA_I4(std::min(2, k / bk)), 1,
                    VHO_DSL_CUDA_TMA_Issue_Pair(c,
                        WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.slot),
                               VHO_DSL_CUDA_I4(bk)))));
            prologue = VHO_DSL_CUDA_Producer_Only(c, prime);
            WN_INSERT_BlockLast(body, prologue);
            k_body = WN_CreateBlock();
            WN_INSERT_BlockLast(k_body, VHO_DSL_CUDA_I4_Store(c.slot,
                WN_Binary(OPR_REM, MTYPE_I4,
                    WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                           VHO_DSL_CUDA_I4(bk)), VHO_DSL_CUDA_I4(2))));
            args.push_back(VHO_DSL_CUDA_Ready_Address(c, VHO_DSL_CUDA_I4_Load(c.slot)));
            args.push_back(WN_Binary(OPR_REM, MTYPE_I4,
                WN_Div(MTYPE_I4,
                    WN_Div(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                           VHO_DSL_CUDA_I4(bk)), VHO_DSL_CUDA_I4(2)),
                VHO_DSL_CUDA_I4(2)));
            WN_INSERT_BlockLast(k_body, VHO_DSL_CUDA_Operation_Call(c.wait, args));
            WN_INSERT_BlockLast(k_body, compute);
            WN_INSERT_BlockLast(k_body, VHO_DSL_CUDA_Barrier(c.barrier));
            refill = VHO_DSL_CUDA_Producer_Only(c,
                VHO_DSL_CUDA_TMA_Issue_Pair(c,
                    WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                           VHO_DSL_CUDA_I4(2 * bk))),
                WN_LT(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.base),
                      VHO_DSL_CUDA_I4(k - 2 * bk)));
            WN_INSERT_BlockLast(k_body, refill);
        }
    } else {
        compute = VHO_DSL_cuda_build_direct_loop(c);
        WN_INSERT_BlockLast(k_body, compute);
    }
    WN *k_loop = VHO_DSL_CUDA_Do(c.base, VHO_DSL_CUDA_I4(0),
                                  VHO_DSL_CUDA_I4(k), bk, k_body);
    WN_INSERT_BlockLast(body, k_loop);
    if (c.ready != NULL) {
        WN *drain = WN_CreateBlock();
        for (UINT32 slot = 0; slot < 2; ++slot) {
            std::vector<WN *> args;
            args.push_back(VHO_DSL_CUDA_Ready_Address(c, VHO_DSL_CUDA_I4(slot)));
            WN_INSERT_BlockLast(drain, VHO_DSL_CUDA_Operation_Call(c.invalidate, args));
        }
        WN_INSERT_BlockLast(body, VHO_DSL_CUDA_Producer_Only(c, drain));
    }

    WN *store_j = WN_CreateBlock();
    WN *c_row = WN_Add
        (MTYPE_I4,
         WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.row0),
                VHO_DSL_CUDA_I4_Load(c.local_row)),
         VHO_DSL_CUDA_I4_Load(c.i));
    WN *c_col = WN_Add
        (MTYPE_I4,
         WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.col0),
                VHO_DSL_CUDA_I4_Load(c.local_col)),
         VHO_DSL_CUDA_I4_Load(c.j));
    WN *c_index = WN_Add(MTYPE_I4, WN_Mpy(MTYPE_I4, c_row,
                                          VHO_DSL_CUDA_I4(n)), c_col);
    WN *result = VHO_DSL_CUDA_F4_Load
        (c, c.acc,
         WN_Add(MTYPE_I4,
                WN_Mpy(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.i),
                       VHO_DSL_CUDA_I4(tn)),
                VHO_DSL_CUDA_I4_Load(c.j)));
    WN *c_store = VHO_DSL_CUDA_F4_Store(c, c.c, c_index, result);
    if (guard_m || guard_n) {
        WN *valid = VHO_DSL_CUDA_Bounds2
            (guard_m ?
                 WN_Add(MTYPE_I4,
                        WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.row0),
                               VHO_DSL_CUDA_I4_Load(c.local_row)),
                        VHO_DSL_CUDA_I4_Load(c.i)) : NULL, m,
             guard_n ?
                 WN_Add(MTYPE_I4,
                        WN_Add(MTYPE_I4, VHO_DSL_CUDA_I4_Load(c.col0),
                               VHO_DSL_CUDA_I4_Load(c.local_col)),
                        VHO_DSL_CUDA_I4_Load(c.j)) : NULL, n);
        WN *then_block = WN_CreateBlock();
        WN_INSERT_BlockLast(then_block, c_store);
        WN_INSERT_BlockLast
            (store_j, WN_CreateIf(valid, then_block, WN_CreateBlock()));
    } else {
        WN_INSERT_BlockLast(store_j, c_store);
    }
    WN *store_i = WN_CreateBlock();
    WN_INSERT_BlockLast
        (store_i, VHO_DSL_CUDA_Do(c.j, VHO_DSL_CUDA_I4(0),
                                  VHO_DSL_CUDA_I4(tn), 1, store_j));
    WN_INSERT_BlockLast
        (body, VHO_DSL_CUDA_Do(c.i, VHO_DSL_CUDA_I4(0),
                               VHO_DSL_CUDA_I4(tm), 1, store_i));
    FmtAssert(VHO_DSL_CUDA_Verify_Pipeline_Order
                  (c, body, k_loop, compute, prologue, refill),
              ("CUDA P8 WHIRL acquire/issue/wait/release proof failed"));
    return body;
}

/* Under backend-driver ownership, preflight the selected schedule and build
 * one device PU with its own scope/map; restore the caller context before
 * returning. No final image is published by this construction service. */
static BOOL
VHO_DSL_CUDA_Build_Selected_Whirl
        (const DSL_TILE_PLAN_RECORD &tile, const DSL_FETCH_PLAN_RECORD &fetch,
         UINT32 ordinal,
         PU_Info **device_pu,
         FILE *diagnostic)
{
    if (device_pu == NULL || !VHO_DSL_CUDA_Pipeline_Valid(tile, fetch) ||
        !VHO_DSL_CUDA_I4_Geometry_Safe(tile) ||
        tile.thread_m == 0 || tile.thread_n == 0 ||
        !VHO_DSL_CUDA_Edge_Policy_Consistent(tile) ||
        tile.cta_m % tile.thread_m != 0 ||
        tile.cta_n % tile.thread_n != 0 ||
        (tile.cta_m / tile.thread_m) *
        (tile.cta_n / tile.thread_n) != tile.threads_per_cta ||
        tile.threads_per_cta > 1024) {
        fprintf(diagnostic,
                "DSL CUDA WHIRL requires a consistent synchronous "
                "edge policy and tile geometry\n");
        return FALSE;
    }
    const SYMTAB_IDX level = CURRENT_SYMTAB;
    const BOOL tma = fetch.engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC;
    const UINT32 parameter_count = tma ? 5 : 3;
    SCOPE saved_scope = Scope_tab[level];
    PU *saved_pu = Current_pu;
    WN_MAP_TAB *saved_map = Current_Map_Tab;
    PU_Info *saved_info = Current_PU_Info;

    TY_IDX function_ty_idx;
    TY &function_ty = New_TY(function_ty_idx);
    TY_Init(function_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty_idx, 1);
    Set_TY_has_prototype(function_ty_idx);
    TY_IDX float_pointer_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(function_ty, tylist_idx);
    TY_IDX void_pointer_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
    for (UINT32 ordinal = 0; ordinal != parameter_count; ++ordinal)
        Set_TYLIST_type(New_TYLIST(tylist_idx),
                        ordinal < 3 ? float_pointer_ty : void_pointer_ty);
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);

    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, function_ty_idx, GLOBAL_SYMTAB + 1);
    Set_PU_c_lang(pu);
    Set_PU_no_inline(pu);
    char kernel_name[64];
    if (ordinal == 0)
        snprintf(kernel_name, sizeof(kernel_name), "ai_matmul_tile_f32");
    else
        snprintf(kernel_name, sizeof(kernel_name),
                 "ai_matmul_tile_f32_%u", ordinal);
    ST *kernel = New_ST(GLOBAL_SYMTAB);
    ST_Init(kernel, Save_Str(kernel_name), CLASS_FUNC,
            SCLASS_TEXT, EXPORT_PREEMPTIBLE, function_ty_idx);
    Set_ST_pu(kernel, pu_idx);
    Set_ST_device_symbol_kind(*kernel, DEVICE_SYMBOL_KERNEL);

    New_Scope(GLOBAL_SYMTAB + 1, MEM_pu_nz_pool_ptr, TRUE);
    Scope_tab[CURRENT_SYMTAB].st = kernel;
    VHO_DSL_CUDA_WH_CONTEXT c;
    memset(&c, 0, sizeof(c));
    c.tile = &tile;
    c.fetch = &fetch;
    c.float_pointer_ty = float_pointer_ty;
    const char *formal_names[] = {"a", "b", "c", "a_map", "b_map"};
    ST *formals[5];
    for (UINT32 ordinal = 0; ordinal != parameter_count; ++ordinal) {
        formals[ordinal] = New_ST(CURRENT_SYMTAB);
        ST_Init(formals[ordinal], Save_Str(formal_names[ordinal]), CLASS_VAR,
                SCLASS_FORMAL, EXPORT_LOCAL,
                ordinal < 3 ? float_pointer_ty : void_pointer_ty);
        Set_ST_is_value_parm(formals[ordinal]);
    }
    c.a = formals[0]; c.b = formals[1]; c.c = formals[2];
    const BOOL direct = (fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0;
    if (!direct) {
        c.as = VHO_DSL_CUDA_New_Local
            ("as", Make_Array_Type(MTYPE_F4, 1,
                                    fetch.stage_count * tile.cta_k *
                                    (tile.a_shared_layout ==
                                         DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                                         tile.a_shared_stride : tile.cta_m)));
        c.bs = VHO_DSL_CUDA_New_Local
            ("bs", Make_Array_Type(MTYPE_F4, 1,
                                    fetch.stage_count * tile.cta_k * tile.cta_n));
        Set_ST_device_symbol_kind(*c.as, DEVICE_SYMBOL_SHARED);
        Set_ST_device_symbol_kind(*c.bs, DEVICE_SYMBOL_SHARED);
    }
    c.acc = VHO_DSL_CUDA_New_Local
        ("acc", Make_Array_Type(MTYPE_F4, 1,
                                tile.thread_m * tile.thread_n));
    c.tid = VHO_DSL_CUDA_New_Local("tid", MTYPE_To_TY(MTYPE_I4));
    c.local_row = VHO_DSL_CUDA_New_Local("local_row", MTYPE_To_TY(MTYPE_I4));
    c.local_col = VHO_DSL_CUDA_New_Local("local_col", MTYPE_To_TY(MTYPE_I4));
    c.row0 = VHO_DSL_CUDA_New_Local("row0", MTYPE_To_TY(MTYPE_I4));
    c.col0 = VHO_DSL_CUDA_New_Local("col0", MTYPE_To_TY(MTYPE_I4));
    c.base = VHO_DSL_CUDA_New_Local("base", MTYPE_To_TY(MTYPE_I4));
    c.offset = VHO_DSL_CUDA_New_Local("offset", MTYPE_To_TY(MTYPE_I4));
    c.q = VHO_DSL_CUDA_New_Local("q", MTYPE_To_TY(MTYPE_I4));
    c.i = VHO_DSL_CUDA_New_Local("i", MTYPE_To_TY(MTYPE_I4));
    c.j = VHO_DSL_CUDA_New_Local("j", MTYPE_To_TY(MTYPE_I4));
    c.av = VHO_DSL_CUDA_New_Local("av", MTYPE_To_TY(MTYPE_F4));
    TY_IDX dim_ty = VHO_DSL_CUDA_Dim_Type();
    c.thread_idx = VHO_DSL_CUDA_Builtin_Symbol("threadIdx", dim_ty);
    c.block_idx = VHO_DSL_CUDA_Builtin_Symbol("blockIdx", dim_ty);
    c.barrier = direct ? NULL : VHO_DSL_CUDA_Barrier_Symbol();
    if (tma) {
        c.a_map = formals[3]; c.b_map = formals[4];
        c.slot = VHO_DSL_CUDA_New_Local("slot", MTYPE_To_TY(MTYPE_I4));
        c.ready = VHO_DSL_CUDA_New_Local("ready", Make_Array_Type(MTYPE_U8, 1, 2));
        Set_ST_device_symbol_kind(*c.ready, DEVICE_SYMBOL_SHARED);
        ST *arrays[] = {c.as, c.bs};
        for (UINT32 i = 0; i < 2; ++i) {
            TY_IDX aligned = ST_type(arrays[i]);
            Set_TY_align(aligned, 128);
            Set_TY_is_user_align(aligned);
            Set_ST_type(arrays[i], aligned);
        }
        TY_IDX one[] = {void_pointer_ty};
        TY_IDX two[] = {void_pointer_ty, MTYPE_To_TY(MTYPE_I4)};
        TY_IDX five[] = {void_pointer_ty, void_pointer_ty,
                         MTYPE_To_TY(MTYPE_I4), MTYPE_To_TY(MTYPE_I4), void_pointer_ty};
        c.ready_init = VHO_DSL_CUDA_Operation("__open64_cuda_barrier_init", 1, one);
        c.proxy_publish = VHO_DSL_CUDA_Operation("__open64_cuda_proxy_publish", 0, NULL);
        c.acquire = VHO_DSL_CUDA_Operation("__open64_cuda_tma_acquire", 2, two);
        c.issue = VHO_DSL_CUDA_Operation("__open64_cuda_tma_issue", 5, five);
        c.wait = VHO_DSL_CUDA_Operation("__open64_cuda_tma_wait", 2, two);
        c.invalidate = VHO_DSL_CUDA_Operation("__open64_cuda_barrier_invalidate", 1, one);
    }

    WN *entry = WN_CreateEntry(parameter_count, kernel, VHO_DSL_CUDA_Build_Body(c),
                               NULL, NULL);
    for (UINT32 ordinal = 0; ordinal != parameter_count; ++ordinal)
        WN_kid(entry, ordinal) = WN_CreateIdname(0, formals[ordinal]);
    PU_Info *info = TYPE_MEM_POOL_ALLOC(PU_Info, MEM_pu_nz_pool_ptr);
    PU_Info_init(info);
    Set_PU_Info_tree_ptr(info, entry);
    PU_Info_proc_sym(info) = ST_st_idx(kernel);
    Set_PU_Info_symtab_ptr(info, NULL);
    Set_PU_Info_state(info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_flags(info, PU_IS_COMPILER_GENERATED);
    PU_Info_maptab(info) = WN_MAP_TAB_Create(MEM_pu_nz_pool_ptr);
    PU_Info_pu_dst(info) = DST_INVALID_IDX;
    PU_Info_cu_dst(info) = DST_INVALID_IDX;
    Current_pu = &pu;
    Current_Map_Tab = PU_Info_maptab(info);
    Current_PU_Info = info;
    Save_Local_Symtab(CURRENT_SYMTAB, info);
    *device_pu = info;
    Scope_tab[level] = saved_scope;
    Current_scope = level;
    Current_pu = saved_pu;
    Current_Map_Tab = saved_map;
    Current_PU_Info = saved_info;
    return TRUE;
}

struct VHO_DSL_CUDA_PENDING_KERNEL {
    DSL_TILE_PLAN_RECORD tile;
    DSL_FETCH_PLAN_RECORD fetch;
    ST_IDX owner;
};

static std::vector<VHO_DSL_CUDA_PENDING_KERNEL> VHO_DSL_CUDA_Pending_Kernels;

/* Validate and copy selected numeric tile/fetch records into the driver
 * handoff registry while the analysis PU is active; do not retain analysis
 * pointers or construct another PU from this per-PU phase. */
static BOOL
VHO_DSL_CUDA_Stage_Selected_Whirl
        (PU_Info *pu_info, const DSL_TILE_PLAN_RECORD &tile,
         const DSL_FETCH_PLAN_RECORD &fetch,
         UINT32 *ordinal, FILE *diagnostic)
{
    if (ordinal == NULL || !VHO_DSL_CUDA_Pipeline_Valid(tile, fetch) ||
        VHO_DSL_CUDA_Pending_Kernels.size() >= UINT32_MAX) {
        fprintf(diagnostic, "DSL CUDA device entry registry is full\n");
        return FALSE;
    }
    *ordinal = (UINT32)VHO_DSL_CUDA_Pending_Kernels.size();
    VHO_DSL_CUDA_PENDING_KERNEL pending;
    pending.tile = tile;
    pending.fetch = fetch;
    pending.owner = PU_Info_proc_sym(pu_info);
    VHO_DSL_CUDA_Pending_Kernels.push_back(pending);
    return TRUE;
}

/* Return the number of staged driver handoffs without changing ownership
 * or consuming any selected plan. */
UINT32
VHO_DSL_CUDA_Program_Kernel_Count(void)
{
    return (UINT32)VHO_DSL_CUDA_Pending_Kernels.size();
}

/* Build one staged device PU for the backend driver and return its binding;
 * reject invalid ordinals/outputs before publication. The driver owns the
 * resulting PU lifetime and atomic program-image commit. */
BOOL
VHO_DSL_CUDA_Take_Program_Kernel
        (UINT32 ordinal, PU_Info **device_pu, UINT32 *plan_id,
         UINT32 *tile_id, ST_IDX *owner, DSL_FETCH_PLAN_RECORD *fetch,
         FILE *diagnostic)
{
    if (device_pu == NULL || plan_id == NULL || tile_id == NULL ||
        owner == NULL || fetch == NULL || ordinal >= VHO_DSL_CUDA_Pending_Kernels.size()) {
        fprintf(diagnostic,
                "DSL CUDA WHIRL output requested without a selected "
                "device PU\n");
        return FALSE;
    }
    const VHO_DSL_CUDA_PENDING_KERNEL &pending =
        VHO_DSL_CUDA_Pending_Kernels[ordinal];
    if (!VHO_DSL_CUDA_Build_Selected_Whirl
            (pending.tile, pending.fetch, ordinal, device_pu, diagnostic))
        return FALSE;
    *plan_id = pending.tile.optimization_plan_id;
    *tile_id = pending.tile.id;
    *owner = pending.owner;
    *fetch = pending.fetch;
    return TRUE;
}

/* Retain the synchronous CUDA template as a comparison baseline for the
 * independently materialized device PU. */
static BOOL
VHO_DSL_CUDA_Write_Selected_Tile
        (const char *path, const DSL_TILE_PLAN_RECORD &tile,
         const DSL_OPT_COST_RECORD &cost, BOOL tma_fallback,
         FILE *diagnostic)
{
    WN *k_loop = VHO_DSL_CUDA_Build_Selected_K_Loop(tile);
    if (!VHO_DSL_CUDA_Verify_Selected_K_Loop(k_loop, tile)) {
        if (k_loop != NULL)
            WN_DELETE_Tree(k_loop);
        fprintf(diagnostic, "DSL CUDA selected K loop is not canonical WHIRL\n");
        return FALSE;
    }
    int fd = open(path, O_WRONLY | O_CREAT | O_EXCL, 0644);
    if (fd < 0) {
        WN_DELETE_Tree(k_loop);
        fprintf(diagnostic, "DSL CUDA output cannot create %s\n", path);
        return FALSE;
    }
    FILE *file = fdopen(fd, "w");
    if (file == NULL) {
        close(fd);
        remove(path);
        WN_DELETE_Tree(k_loop);
        return FALSE;
    }
    if (VHO_DSL_AI_Opt_Trace && Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)) {
        fprintf(TFile, "AI_OPT 7 K_LOOP %u %u %u 0 %llu %u "
                       "detached_plan_only\n",
                tile.site_id, tile.optimization_plan_id, tile.id,
                (unsigned long long)WN_const_val(WN_kid1(WN_end(k_loop))),
                (UINT32)WN_const_val(WN_kid1(WN_kid0(WN_step(k_loop)))));
        fprintf(TFile, "========== AI selected K-tile WHIRL plan loop =========="
                       "\nplan=%u tile=%u scope=detached_plan_only\n",
                tile.optimization_plan_id, tile.id);
        fdump_tree(TFile, k_loop);
        fflush(TFile);
    }
    fprintf(file,
            "// Open64 cost-selected CUDA tile source; runtime binding is opt-in.\n"
            "// Numerical/runtime verification and target resource calibration pending.\n"
            "// tile_plan=%u optimization_plan=%u candidate=%u family=%s\n"
            "// legality=proven cost=%llu unit=relative confidence=low\n"
            "// shared_ab_bytes_per_stage=%llu threads_per_cta=%u\n"
            "// a_smem_layout=%s a_stride=%u\n"
            "// b_smem_layout=%s\n"
            "// whirl_k_loop=DO_LOOP scope=detached_plan_only start=0 end=%llu step=%u\n"
            "#include <cuda_runtime.h>\n"
            "#define AI_TILE_PLAN_ID %u\n"
            "#define AI_OPTIMIZATION_PLAN_ID %u\n"
            "#define M %llu\n#define N %llu\n#define K %llu\n"
            "#define BM %u\n#define BN %u\n#define BK %u\n"
            "#define TM %u\n#define TN %u\n#define A_STRIDE %u\n"
            "#define AI_A_SHARED_ELEMS %u\n"
            "#define AI_B_SMEM_XOR_COLUMNS %u\n"
            "#define THREADS ((BM / TM) * (BN / TN))\n",
            tile.id, tile.optimization_plan_id, tile.candidate_id,
            DSL_tile_family_name(tile.family),
            (unsigned long long)cost.total,
            (unsigned long long)tile.shared_bytes_per_stage,
            tile.threads_per_cta,
            tile.a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                "transposed_padded" : "row_major", tile.a_shared_stride,
            tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
                "xor_columns" : "row_major",
            (unsigned long long)WN_const_val(WN_kid1(WN_end(k_loop))),
            (UINT32)WN_const_val(WN_kid1(WN_kid0(WN_step(k_loop)))),
            tile.id, tile.optimization_plan_id,
            (unsigned long long)tile.problem_m,
            (unsigned long long)tile.problem_n,
            (unsigned long long)tile.problem_k,
            tile.cta_m, tile.cta_n, tile.cta_k,
            tile.thread_m, tile.thread_n, tile.a_shared_stride,
            tile.cta_k * (tile.a_shared_layout ==
                DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                tile.cta_m + 1 : tile.cta_m),
            tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ? 1 : 0);
    fprintf(file,
        "extern \"C\" __global__ void %s(\n",
        tma_fallback ? "ai_matmul_tile_f32_sync" : "ai_matmul_tile_f32");
    fputs(
        "    const float *__restrict__ a, const float *__restrict__ b,\n"
        "    float *__restrict__ c) {\n"
        "  __shared__ float bs[BK][BN];\n"
        "  const int tid = threadIdx.x;\n"
        "  const int local_row = (tid / (BN / TN)) * TM;\n"
        "  const int local_col = (tid % (BN / TN)) * TN;\n"
        "  const int row0 = blockIdx.y * BM;\n"
        "  const int col0 = blockIdx.x * BN;\n"
        "  float acc[TM][TN] = {};\n", file);
    fputs(tile.a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
          "  __shared__ float as[BK][A_STRIDE];\n" :
          "  __shared__ float as[BM][BK];\n", file);
    fprintf(file, "  for (int base = 0; base < %lld; base += %lld) {\n",
            (long long)WN_const_val(WN_kid1(WN_end(k_loop))),
            (long long)WN_const_val(WN_kid1(WN_kid0(WN_step(k_loop)))));
    fputs(
        "    for (int offset = tid; offset < BM * BK; offset += THREADS) {\n"
        "      const int r = offset / BK, q = offset % BK;\n"
        "      const float av = row0+r < M && base+q < K\n"
        "          ? a[(row0+r)*K + base+q] : 0.0f;\n", file);
    fputs(tile.a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
          "      as[q][r] = av;\n" : "      as[r][q] = av;\n", file);
    fputs(
        "    }\n"
        "    for (int offset = tid; offset < BK * BN; offset += THREADS) {\n"
        "      const int q = offset / BN, col = offset % BN;\n", file);
    fputs(tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
        "      bs[q][col ^ (col / 32)] = base+q < K && col0+col < N\n" :
        "      bs[q][col] = base+q < K && col0+col < N\n", file);
    fputs(
        "          ? b[(base+q)*N + col0+col] : 0.0f;\n"
        "    }\n"
        "    __syncthreads();\n"
        "    for (int q = 0; q < BK; ++q)\n"
        "      for (int i = 0; i < TM; ++i) {\n", file);
    fputs(tile.a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
          "        const float av = as[q][local_row+i];\n" :
          "        const float av = as[local_row+i][q];\n", file);
    fputs(
        "        for (int j = 0; j < TN; ++j)\n", file);
    fputs(tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
        "          acc[i][j] += av * bs[q][(local_col+j) ^ ((local_col+j)/32)];\n" :
        "          acc[i][j] += av * bs[q][local_col+j];\n", file);
    fputs(
        "      }\n"
        "    __syncthreads();\n"
        "  }\n"
        "  for (int i = 0; i < TM; ++i)\n"
        "    for (int j = 0; j < TN; ++j)\n"
        "      if (row0+local_row+i < M && col0+local_col+j < N)\n"
        "        c[(row0+local_row+i)*N + col0+local_col+j] = acc[i][j];\n"
        "}\n", file);
    BOOL wrote = !ferror(file);
    BOOL closed = fclose(file) == 0;
    BOOL valid = wrote && closed;
    WN_DELETE_Tree(k_loop);
    if (!valid)
        remove(path);
    return valid;
}

/* Append the retained legacy TMA parity-source path after revalidating its
 * selected fetch contract; this is not WHIRL application or GPU acceptance. */
static BOOL
VHO_DSL_CUDA_Append_TMA_Pipeline
        (const char *path, const DSL_TILE_PLAN_RECORD &tile,
         const DSL_FETCH_PLAN_RECORD &fetch, FILE *diagnostic)
{
    if (tile.cta_m == 0 || tile.cta_n == 0 || tile.cta_k == 0 ||
        tile.cta_m > 256 || tile.cta_n > 256 || tile.cta_k > 256 ||
        tile.edge_policy != DSL_TILE_EDGE_EXACT ||
        tile.instruction_family != DSL_TILE_INSTRUCTION_VECTOR_FMA ||
        tile.problem_m % tile.cta_m != 0 ||
        tile.problem_n % tile.cta_n != 0 ||
        tile.problem_k % tile.cta_k != 0 ||
        tile.problem_n % 4 != 0 || tile.problem_k % 4 != 0 ||
        fetch.engine != DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ||
        fetch.legality != DSL_OPT_LEGALITY_PROVEN ||
        fetch.stage_count < 2 || fetch.stage_count > 3 ||
        fetch.buffering_bytes !=
            tile.shared_bytes_per_stage * fetch.stage_count ||
        fetch.buffering_bytes > tile.shared_capacity_bytes) {
        fprintf(diagnostic, "DSL CUDA TMA plan failed lowering legality\n");
        return FALSE;
    }
    FILE *file = fopen(path, "a");
    if (file == NULL)
        return FALSE;
    fprintf(file,
            "\n// P8 selected plan=%u engine=tma_2d stages=%u "
            "prefetch_distance=%u\n"
            "// movement_cost=relative raw=%llu hidden=%llu unhidden=%llu\n"
            "// Requires CUDA tensor-map support and sm_90; GPU verification pending.\n"
            "#include <cuda.h>\n#include <cuda/barrier>\n"
            "#include <cuda/ptx>\n#include <stdint.h>\n"
            "#define P8_STAGES %u\n"
            "#define P8_STAGE_BYTES ((BM*BK + BK*BN)*sizeof(float))\n"
            "#define P8_SMEM_BYTES (P8_STAGES*P8_STAGE_BYTES)\n",
            fetch.optimization_plan_id, fetch.stage_count,
            fetch.prefetch_distance,
            (unsigned long long)fetch.raw_movement_cost,
            (unsigned long long)fetch.hidden_movement_cost,
            (unsigned long long)fetch.unhidden_movement_cost,
            fetch.stage_count);
    fputs(
        "using ai_barrier = cuda::barrier<cuda::thread_scope_block>;\n"
        "namespace ai_ptx = cuda::ptx;\n"
        "__device__ __forceinline__ void ai_tma_issue(\n"
        "    int slot, int k_tile, int row0, int col0,\n"
        "    const CUtensorMap *a_map, const CUtensorMap *b_map,\n"
        "    float (*as)[BM][BK], float (*bs)[BK][BN], ai_barrier *ready) {\n"
        "  int32_t a_coord[2] = {k_tile*BK, row0};\n"
        "  int32_t b_coord[2] = {col0, k_tile*BK};\n"
        "  ai_ptx::cp_async_bulk_tensor(\n"
        "      ai_ptx::space_shared, ai_ptx::space_global,\n"
        "      &as[slot], a_map, a_coord,\n"
        "      cuda::device::barrier_native_handle(ready[slot]));\n"
        "  ai_ptx::cp_async_bulk_tensor(\n"
        "      ai_ptx::space_shared, ai_ptx::space_global,\n"
        "      &bs[slot], b_map, b_coord,\n"
        "      cuda::device::barrier_native_handle(ready[slot]));\n"
        "  (void)cuda::device::barrier_arrive_tx(\n"
        "      ready[slot], 1, P8_STAGE_BYTES);\n"
        "}\n"
        "extern \"C\" __global__ void ai_matmul_tile_f32(\n"
        "    const __grid_constant__ CUtensorMap a_map,\n"
        "    const __grid_constant__ CUtensorMap b_map, float *c) {\n"
        "  extern __shared__ __align__(128) unsigned char storage[];\n"
        "  float (*as)[BM][BK] =\n"
        "      reinterpret_cast<float (*)[BM][BK]>(storage);\n"
        "  float (*bs)[BK][BN] = reinterpret_cast<float (*)[BK][BN]>(\n"
        "      storage + P8_STAGES*BM*BK*sizeof(float));\n"
        "  #pragma nv_diag_suppress static_var_with_dynamic_init\n"
        "  __shared__ ai_barrier ready[P8_STAGES];\n"
        "  const int tid = threadIdx.x;\n"
        "  const int local_row = (tid / (BN / TN)) * TM;\n"
        "  const int local_col = (tid % (BN / TN)) * TN;\n"
        "  const int row0 = blockIdx.y * BM;\n"
        "  const int col0 = blockIdx.x * BN;\n"
        "  const int k_tiles = K / BK;\n"
        "  float acc[TM][TN] = {};\n"
        "  if (tid == 0) {\n"
        "    for (int s = 0; s < P8_STAGES; ++s) init(&ready[s], 1);\n"
        "  }\n"
        "  __syncthreads();\n"
        "  if (tid == 0) {\n"
        "    for (int s = 0; s < P8_STAGES && s < k_tiles; ++s)\n"
        "      ai_tma_issue(s, s, row0, col0, &a_map, &b_map, as, bs, ready);\n"
        "  }\n"
        "  for (int kt = 0; kt < k_tiles; ++kt) {\n"
        "    const int slot = kt % P8_STAGES;\n"
        "    const int parity = (kt / P8_STAGES) & 1;\n"
        "    while (!ai_ptx::mbarrier_try_wait_parity(\n"
        "        ai_ptx::sem_acquire, ai_ptx::scope_cta,\n"
        "        cuda::device::barrier_native_handle(ready[slot]), parity)) {}\n"
        "    for (int q = 0; q < BK; ++q)\n"
        "      for (int i = 0; i < TM; ++i) {\n"
        "        const float av = as[slot][local_row+i][q];\n"
        "        for (int j = 0; j < TN; ++j)\n"
        "          acc[i][j] += av * bs[slot][q][local_col+j];\n"
        "      }\n"
        "    __syncthreads();\n"
        "    if (tid == 0 && kt + P8_STAGES < k_tiles)\n"
        "      ai_tma_issue(slot, kt + P8_STAGES, row0, col0,\n"
        "                   &a_map, &b_map, as, bs, ready);\n"
        "  }\n"
        "  if (tid == 0) {\n"
        "    for (int s = 0; s < P8_STAGES; ++s) ready[s].~ai_barrier();\n"
        "  }\n"
        "  for (int i = 0; i < TM; ++i)\n"
        "    for (int j = 0; j < TN; ++j)\n"
        "      c[(row0+local_row+i)*N + col0+local_col+j] = acc[i][j];\n"
        "}\n"
        "extern \"C\" cudaError_t ai_matmul_tile_f32_launch_ex(\n"
        "    const float *a, const float *b, float *c, cudaStream_t stream,\n"
        "    bool *used_tma) {\n"
        "  if (used_tma) *used_tma = false;\n"
        "  if (!a || !b || !c) return cudaErrorInvalidDevicePointer;\n"
        "  const dim3 grid(N/BN, M/BM);\n"
        "  int device = 0, optin_bytes = 0;\n"
        "  cudaDeviceProp prop{};\n"
        "  cudaError_t status = cudaGetDevice(&device);\n"
        "  if (status != cudaSuccess) return status;\n"
        "  status = cudaGetDeviceProperties(&prop, device);\n"
        "  if (status != cudaSuccess) return status;\n"
        "  status = cudaDeviceGetAttribute(\n"
        "      &optin_bytes, cudaDevAttrMaxSharedMemoryPerBlockOptin, device);\n"
        "  if (status != cudaSuccess) return status;\n"
        "  if (prop.major < 9 || optin_bytes < P8_SMEM_BYTES + 128 ||\n"
        "      (reinterpret_cast<uintptr_t>(a) & 15) ||\n"
        "      (reinterpret_cast<uintptr_t>(b) & 15)) {\n"
        "    ai_matmul_tile_f32_sync<<<grid, THREADS, 0, stream>>>(a, b, c);\n"
        "    return cudaGetLastError();\n"
        "  }\n"
        "  const uint64_t a_dims[2] = {K, M}, b_dims[2] = {N, K};\n"
        "  const uint64_t a_stride[1] = {K*sizeof(float)};\n"
        "  const uint64_t b_stride[1] = {N*sizeof(float)};\n"
        "  const uint32_t a_box[2] = {BK, BM}, b_box[2] = {BN, BK};\n"
        "  const uint32_t element_stride[2] = {1, 1};\n"
        "  CUtensorMap a_map{}, b_map{};\n"
        "  if (cuInit(0) != CUDA_SUCCESS ||\n"
        "      cuTensorMapEncodeTiled(\n"
        "          &a_map, CU_TENSOR_MAP_DATA_TYPE_FLOAT32, 2,\n"
        "          const_cast<float *>(a), a_dims, a_stride, a_box,\n"
        "          element_stride, CU_TENSOR_MAP_INTERLEAVE_NONE,\n"
        "          CU_TENSOR_MAP_SWIZZLE_NONE, CU_TENSOR_MAP_L2_PROMOTION_NONE,\n"
        "          CU_TENSOR_MAP_FLOAT_OOB_FILL_NONE) != CUDA_SUCCESS ||\n"
        "      cuTensorMapEncodeTiled(\n"
        "          &b_map, CU_TENSOR_MAP_DATA_TYPE_FLOAT32, 2,\n"
        "          const_cast<float *>(b), b_dims, b_stride, b_box,\n"
        "          element_stride, CU_TENSOR_MAP_INTERLEAVE_NONE,\n"
        "          CU_TENSOR_MAP_SWIZZLE_NONE, CU_TENSOR_MAP_L2_PROMOTION_NONE,\n"
        "          CU_TENSOR_MAP_FLOAT_OOB_FILL_NONE) != CUDA_SUCCESS) {\n"
        "    ai_matmul_tile_f32_sync<<<grid, THREADS, 0, stream>>>(a, b, c);\n"
        "    return cudaGetLastError();\n"
        "  }\n"
        "  status = cudaFuncSetAttribute(\n"
        "      ai_matmul_tile_f32, cudaFuncAttributeMaxDynamicSharedMemorySize,\n"
        "      P8_SMEM_BYTES);\n"
        "  if (status != cudaSuccess) {\n"
        "    ai_matmul_tile_f32_sync<<<grid, THREADS, 0, stream>>>(a, b, c);\n"
        "    return cudaGetLastError();\n"
        "  }\n"
        "  ai_matmul_tile_f32<<<grid, THREADS, P8_SMEM_BYTES, stream>>>(\n"
        "      a_map, b_map, c);\n"
        "  status = cudaGetLastError();\n"
        "  if (status == cudaSuccess && used_tma) *used_tma = true;\n"
        "  return status;\n"
        "}\n"
        "extern \"C\" cudaError_t ai_matmul_tile_f32_launch(\n"
        "    const float *a, const float *b, float *c, cudaStream_t stream) {\n"
        "  return ai_matmul_tile_f32_launch_ex(a, b, c, stream, nullptr);\n"
        "}\n", file);
    BOOL wrote = !ferror(file);
    BOOL closed = fclose(file) == 0;
    BOOL valid = wrote && closed;
    if (!valid) {
        remove(path);
        fprintf(diagnostic, "DSL CUDA TMA source write failed: %s\n", path);
    }
    return valid;
}

/* Emit active-PU semantic/locality evidence under the AI trace option;
 * malformed facts fail rather than generating an untrustworthy report. */
static BOOL
VHO_DSL_CUDA_Trace_Tensor_Facts
        (const DSL_TENSOR_ANALYSIS *tensor,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality)
{
    if (!VHO_DSL_AI_Opt_Trace ||
        !Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG))
        return TRUE;
    for (UINT32 id = 1; id <= DSL_tensor_analysis_fact_count(tensor); ++id) {
        DSL_TENSOR_FACT_RECORD fact;
        TENSOR_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_tensor_analysis_get_fact(tensor, id, &fact) ||
            !TY_get_tensor_descriptor_record(fact.descriptor_ty,
                                             &descriptor) ||
            !DSL_IR_Image_Get_Value(fact.value_id, &value))
            return FALSE;
        const char *shape = VHO_DSL_Str(descriptor.logical_shape);
        if (shape == NULL || shape[0] == '\0' ||
            strpbrk(shape, " \t\r\n") != NULL)
            return FALSE;
        UINT32 source_line = value.st != ST_IDX_ZERO ?
                             ST_Line(St_Table[value.st]) : 0;
        fprintf(TFile,
                "AI_OPT 7 SEMANTIC %u %u %s %d %s %s %s %s %u %u\n",
                fact.id, fact.value_id,
                DSL_tensor_value_role_name(fact.value_role), fact.rank,
                shape, TY_mtype(fact.element_ty) == MTYPE_F4 ? "f32" : "other",
                DSL_tensor_ownership_name(fact.ownership),
                DSL_tensor_reuse_role_name(fact.reuse_role),
                fact.use_count, source_line);
    }
    for (UINT32 id = 1; id <= DSL_tensor_locality_fact_count(locality);
         ++id) {
        DSL_TENSOR_LOCALITY_FACT_RECORD fact;
        if (!DSL_tensor_locality_get_fact(locality, id, &fact))
            return FALSE;
        fprintf(TFile,
                "AI_OPT 7 LOCALITY %u %u %s %s %s %s %s %s %s "
                "%llu %llu %llu %llu %llu %u %u\n",
                fact.id, fact.value_id,
                DSL_tensor_size_state_name(fact.size_state),
                DSL_tensor_lifetime_state_name(fact.lifetime_state),
                DSL_tensor_distance_state_name(fact.reuse_distance_state),
                DSL_tensor_alias_state_name(fact.alias_state),
                DSL_tensor_access_pattern_name(fact.access_pattern),
                DSL_tensor_residency_benefit_name(fact.residency_benefit),
                DSL_tensor_critical_path_state_name(fact.critical_path_state),
                (unsigned long long)fact.object_bytes,
                (unsigned long long)fact.reuse_distance_statements,
                (unsigned long long)fact.working_set_bytes,
                (unsigned long long)fact.estimated_read_bytes,
                (unsigned long long)fact.estimated_write_bytes,
                fact.producer_path_position, fact.last_use_path_position);
    }
    fflush(TFile);
    return !ferror(TFile);
}

/* Resolve driver-selected hardware through the common architecture table and
 * clone explicit timing overrides. No global table mutation or PU traversal.
 * See doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md. */
static BOOL
VHO_DSL_cuda_get_hardware(DSL_HARDWARE_DESCRIPTION_RECORD *hardware)
{
    DSL_HARDWARE_DESCRIPTION_RECORD selected;
    if (!DSL_hardware_description_find_arch(VHO_DSL_GPU_Arch ? VHO_DSL_GPU_Arch : "sm_90", &selected))
        return FALSE;
    VHO_DSL_FETCH_PIPELINE_CONTROL control;
    VHO_DSL_Fetch_Pipeline_Control_Init(&control);
    control.target_profile_id = selected.target_profile_id;
    control.hardware_arch_id = selected.arch_id;
    control.memory_latency_set = VHO_DSL_GEMM_Memory_Latency_Set;
    control.memory_latency = VHO_DSL_GEMM_Memory_Latency;
    return VHO_DSL_fetch_hardware_description(control, hardware);
}

/* Enumerate bounded tile/schedule pairs without mutating executable WHIRL.
 * Keep the cheapest proven synchronous pair as fallback; trace all examined
 * pairs and movement resources, including rejected staged siblings. P7
 * topology legality does not imply movement feasibility; rebuild only the
 * verified winner. See doc/AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md. */
static BOOL
VHO_DSL_cuda_select_joint_plan
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_LOCALITY_ANALYSIS *locality, DSL_TILE_ANALYSIS *tiles,
         const DSL_TILE_SITE_RECORD &site, const DSL_HARDWARE_DESCRIPTION_RECORD &hardware,
         DSL_TILE_PLAN_ID *winner,
         UINT32 *candidate_count,
         FILE *diagnostic)
{
    const DSL_TILE_PLAN_IR *ir = VHO_DSL_Tile_Get_IR(tiles);
    UINT64 best = ~(UINT64)0, fallback_cost = ~(UINT64)0;
    UINT32 count = 0, chosen = 0, fallback = 0;
    const BOOL trace = VHO_DSL_AI_Opt_Trace && Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG);
    const BOOL tma_allowed = VHO_DSL_CUDA_TMA_Prefetch && !VHO_DSL_CUDA_Tile_Apply;
    const BOOL readiness = VHO_DSL_GEMM_Cost_Model == NULL ||
        strcmp(VHO_DSL_GEMM_Cost_Model, "readiness") == 0;
    DSL_IR_VALUE_RECORD value;
    const UINT32 source_line = DSL_IR_Image_Get_Value(site.semantic_value_id, &value) &&
        value.st != ST_IDX_ZERO ? ST_Line(St_Table[value.st]) : 0;
    *winner = 0;
    *candidate_count = 0;
    if (trace)
        fprintf(TFile, "AI_OPT 7 JOINT_MODE joint 48 %u %u %s low model=%u direct=%u\n",
                site.tile_plan_count, tma_allowed, readiness ? "cycles" : "relative",
                readiness ? 6 : 3, !VHO_DSL_CUDA_Tile_Apply);
    if (trace && readiness) {
        fprintf(TFile, "AI_OPT 7 HW_DESC %s %s %u %u %s %s %u\n", hardware.architecture,
            hardware.table_name, hardware.target_profile_id, hardware.revision,
            hardware.unit, hardware.provenance, hardware.latency_override);
        const DSL_HARDWARE_PERFORMANCE_RECORD &p = hardware.performance;
        fprintf(TFile, "AI_OPT 7 HW_PERF %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
            (unsigned long long)p.sms, (unsigned long long)p.warp_lanes,
            (unsigned long long)p.memory_bytes_per_cycle, (unsigned long long)p.scalar_ops_per_cycle,
            (unsigned long long)p.local_issues_per_cycle, (unsigned long long)p.copy_elements_per_cycle,
            (unsigned long long)p.latency_hiding_warps, (unsigned long long)p.demand_latency,
            (unsigned long long)p.staged_latency, (unsigned long long)p.tma_latency,
            (unsigned long long)p.store_latency, (unsigned long long)p.publication_cycles,
            (unsigned long long)p.wait_cycles, (unsigned long long)p.release_cycles,
            (unsigned long long)p.operand_issue_cycles);
        const DSL_HARDWARE_RESOURCE_RECORD &r = hardware.resources;
        fprintf(TFile, "AI_OPT 7 HW_RESOURCE 1 mixed_estimated %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",
            (unsigned long long)r.max_blocks, (unsigned long long)r.max_warps,
            (unsigned long long)r.max_threads, (unsigned long long)r.max_threads_per_cta,
            (unsigned long long)r.registers_per_sm, (unsigned long long)r.max_registers_per_thread,
            (unsigned long long)r.register_quantum, (unsigned long long)r.register_warp_quantum,
            (unsigned long long)r.shared_capacity, (unsigned long long)r.shared_budget,
            (unsigned long long)r.shared_quantum, (unsigned long long)r.shared_reserved);
    }
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        DSL_TILE_PLAN_RECORD tile;
        if (!DSL_tile_plan_ir_get_plan(ir, site.first_tile_plan_id + i, &tile))
            return FALSE;
        if (tile.family == DSL_TILE_FAMILY_BASELINE || tile.legality != DSL_OPT_LEGALITY_PROVEN)
            continue;
        VHO_DSL_FETCH_PIPELINE_CONTROL control;
        VHO_DSL_Fetch_Pipeline_Control_Init(&control);
        control.select_plans = 0;
        control.joint_cost = 1;
        control.readiness_cost = readiness;
        control.hardware_arch_id = hardware.arch_id;
        control.target_profile_id = hardware.target_profile_id;
        control.memory_latency = VHO_DSL_GEMM_Memory_Latency;
        control.memory_latency_set = VHO_DSL_GEMM_Memory_Latency_Set;
        control.enable_direct = !VHO_DSL_CUDA_Tile_Apply;
        control.tile_plan_id = tile.id;
        control.enable_vector = control.enable_async_copy = 0;
        control.enable_multidimensional_async = tma_allowed;
        control.maximum_buffer_stages = 2;
        DSL_FETCH_PIPELINE_ANALYSIS *fetch = VHO_DSL_Fetch_Pipeline_Create
            (pu, graph, locality, NULL, tiles, &control, diagnostic);
        if (fetch == NULL)
            return FALSE;
        BOOL valid = VHO_DSL_Fetch_Pipeline_Build(fetch, diagnostic);
        const DSL_FETCH_PIPELINE_IR *fetch_ir = VHO_DSL_Fetch_Pipeline_Get_IR(fetch);
        const DSL_OPT_PLAN_CONTEXT *context = VHO_DSL_Fetch_Pipeline_Get_Plan_Context(fetch, 1);
        for (UINT32 p = 1; valid && p <= DSL_fetch_pipeline_ir_plan_count(fetch_ir); ++p) {
            DSL_FETCH_PLAN_RECORD pipeline;
            DSL_OPT_PLAN_RECORD plan;
            DSL_OPT_COST_RECORD cost;
            valid = ++count <= 48 && DSL_fetch_pipeline_ir_get_plan(fetch_ir, p, &pipeline) &&
                DSL_opt_plan_get_plan(context, pipeline.optimization_plan_id, &plan) &&
                DSL_opt_plan_get_cost(context, plan.cost_id, &cost);
            if (!valid)
                break;
            if (trace)
                fprintf(TFile, "AI_OPT 7 JOINT_CAND %u %u %u %u %s %s %s "
                    "%llu %llu %llu %llu %llu %llu %llu %llu %llu %s %u\n",
                    count, tile.id, tile.optimization_plan_id, pipeline.optimization_plan_id,
                    DSL_memory_movement_name(pipeline.engine),
                    DSL_opt_legality_name(plan.legality),
                    DSL_opt_rejection_reason_name(plan.rejection_reason),
                    (unsigned long long)cost.terms[DSL_OPT_COST_COMPUTE].amount,
                    (unsigned long long)cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN].amount,
                    (unsigned long long)cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN].amount,
                    (unsigned long long)cost.terms[DSL_OPT_COST_SYNC].amount,
                    (unsigned long long)cost.terms[DSL_OPT_COST_LAUNCH].amount,
                    (unsigned long long)cost.terms[DSL_OPT_COST_RUNTIME_SELECTION].amount,
                    (unsigned long long)cost.total,
                    (unsigned long long)pipeline.buffering_bytes,
                    (unsigned long long)tile.shared_capacity_bytes,
                    (pipeline.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "D" :
                        pipeline.engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ? "T2" : "S1", source_line);
            if (trace) {
                const DSL_FETCH_RESOURCE_RECORD &resource = pipeline.resources;
                fprintf(TFile, "AI_OPT 7 JOINT_RESOURCE %u %u %u %llu %llu %u %u %u\n",
                    count, resource.register_bytes_per_thread, resource.register_capacity_per_thread,
                    (unsigned long long)resource.shared_bytes,
                    (unsigned long long)resource.shared_limit_bytes,
                    resource.threads_per_cta, resource.thread_limit, source_line);
            }
            if (trace && readiness) {
                DSL_GEMM_READINESS_INPUT input;
                DSL_GEMM_READINESS_RESULT result;
                if (!VHO_DSL_fetch_readiness_cost(tile, pipeline, hardware, &input, &result)) {
                    valid = FALSE;
                    break;
                }
                fprintf(TFile, "AI_OPT 7 JOINT_READINESS %u %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %u\n",
                    count, (unsigned long long)input.ctas, (unsigned long long)input.steps,
                    (unsigned long long)input.consumer, (unsigned long long)input.input_bytes,
                    (unsigned long long)input.output_bytes, (unsigned long long)input.copy,
                    (unsigned long long)input.warps, (unsigned long long)result.delivery,
                    (unsigned long long)result.window, (unsigned long long)result.wait,
                    (unsigned long long)result.waves, (unsigned long long)result.total, source_line);
                DSL_GEMM_OCCUPANCY_RESULT occupancy;
                DSL_GEMM_READINESS_RESULT legacy;
                const DSL_FETCH_RESOURCE_RECORD &r = pipeline.resources;
                if (!DSL_gemm_occupancy(hardware.resources, input.hardware.warp_lanes,
                        input.hardware.sms, r.threads_per_cta, r.register_bytes_per_thread,
                        r.shared_bytes, input.ctas, &occupancy) ||
                    !DSL_readiness_evaluate(input, &legacy)) { valid = FALSE; break; }
                fprintf(TFile, "AI_OPT 7 JOINT_OCCUPANCY %u %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %u\n",
                    count, (unsigned long long)occupancy.registers, (unsigned long long)occupancy.shared,
                    (unsigned long long)occupancy.limits[0], (unsigned long long)occupancy.limits[1],
                    (unsigned long long)occupancy.limits[2], (unsigned long long)occupancy.limits[3],
                    (unsigned long long)occupancy.limits[4], (unsigned long long)occupancy.resident,
                    (unsigned long long)occupancy.warps, (unsigned long long)occupancy.limiting,
                    (unsigned long long)occupancy.full, (unsigned long long)occupancy.tail,
                    (unsigned long long)occupancy.unused, (unsigned long long)occupancy.tail_sms,
                    (unsigned long long)legacy.total, source_line);
            }
            if (trace && (pipeline.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0) {
                UINT64 sectors[2];
                if (!VHO_DSL_direct_gmem_sectors(tile, sectors)) { valid = FALSE; break; }
                fprintf(TFile, "AI_OPT 7 DIRECT_ACCESS %u %llu %llu %llu cache=unknown\n",
                    count, (unsigned long long)sectors[0], (unsigned long long)sectors[1],
                    (unsigned long long)tile.gmem_c_sector_bytes);
            }
            if (plan.legality != DSL_OPT_LEGALITY_PROVEN || !cost.complete)
                continue;
            if (cost.total < best) {
                best = cost.total;
                *winner = tile.id;
                chosen = count;
            }
            if (pipeline.engine == DSL_MEMORY_MOVEMENT_DEMAND &&
                (pipeline.flags & DSL_FETCH_PLAN_FLAG_DIRECT) == 0 && cost.total < fallback_cost) {
                fallback_cost = cost.total;
                fallback = count;
            }
        }
        VHO_DSL_Fetch_Pipeline_Destroy(fetch);
        if (!valid)
            return FALSE;
    }
    if (chosen == 0 || fallback == 0)
        return FALSE;
    *candidate_count = count;
    if (trace)
        fprintf(TFile, "AI_OPT 7 JOINT_SELECT %u %u %u "
                "selection_reason=lowest_complete_joint_cost\n", chosen, fallback, count);
    return TRUE;
}

/* Build PU-local analyses, legality/cost-select tile and fetch plans, and
 * copy their completed records to the caller before destroying analyses.
 * Emit legacy source only when requested; device construction is driver-owned. */
static BOOL
VHO_DSL_CUDA_Emit_Selected_Tile
        (PU_Info *pu_info, WN *tree, const char *path,
         DSL_OPT_PLAN_ID *emitted_plan_id,
         DSL_OPT_PLAN_ID *emitted_fetch_plan_id,
         UINT32 *tile_candidate_count, UINT32 *fetch_candidate_count,
         UINT32 *joint_candidate_count,
         DSL_TILE_PLAN_RECORD *emitted_tile,
         DSL_FETCH_PLAN_RECORD *emitted_fetch,
         FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_GRAPH *graph = NULL;
    DSL_TENSOR_ANALYSIS *tensor = NULL;
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot = NULL;
    DSL_TENSOR_LOCALITY_ANALYSIS *locality = NULL;
    DSL_TILE_ANALYSIS *analysis = NULL;
    DSL_FETCH_PIPELINE_ANALYSIS *fetch_analysis = NULL;
    const DSL_TILE_PLAN_IR *ir = NULL;
    const DSL_FETCH_PIPELINE_IR *fetch_ir = NULL;
    const DSL_OPT_PLAN_CONTEXT *plans = NULL;
    const DSL_OPT_PLAN_CONTEXT *fetch_plans = NULL;
    DSL_TILE_SITE_RECORD site;
    DSL_FETCH_SITE_RECORD fetch_site;
    DSL_FETCH_PLAN_RECORD selected_fetch;
    DSL_OPT_PLAN_RECORD selected;
    DSL_OPT_COST_RECORD cost;
    VHO_DSL_TILE_CONTROL control;
    VHO_DSL_FETCH_PIPELINE_CONTROL fetch_control;
    DSL_HARDWARE_DESCRIPTION_RECORD hardware;
    BOOL valid = FALSE;
    const BOOL joint = VHO_DSL_GEMM_Plan_Search != NULL &&
                       strcmp(VHO_DSL_GEMM_Plan_Search, "joint") == 0;
    DSL_TILE_PLAN_ID joint_tile = 0;
    if (!VHO_DSL_cuda_get_hardware(&hardware)) return FALSE;
    if (emitted_plan_id != NULL)
        *emitted_plan_id = DSL_OPT_PLAN_INVALID_ID;
    if (emitted_fetch_plan_id != NULL)
        *emitted_fetch_plan_id = DSL_OPT_PLAN_INVALID_ID;
    if (tile_candidate_count != NULL)
        *tile_candidate_count = 0;
    if (fetch_candidate_count != NULL)
        *fetch_candidate_count = 0;
    *joint_candidate_count = 0;
    if (emitted_tile != NULL)
        memset(emitted_tile, 0, sizeof(*emitted_tile));
    if (emitted_fetch != NULL)
        memset(emitted_fetch, 0, sizeof(*emitted_fetch));
    memset(&selected_fetch, 0, sizeof(selected_fetch));
    if ((snapshot = VHO_DSL_CUDA_Linear_Snapshot
                        (pu_info, tree, diagnostic)) == NULL) {
        fprintf(diagnostic,
                "DSL CUDA output requires a straight-line native tensor PU\n");
        goto done;
    }
    graph = DSL_tensor_evolution_create(pu_info, diagnostic);
    if (graph == NULL ||
        !VHO_DSL_Tensor_Evolution_Build_Semantic_Roots(graph, diagnostic))
        goto done;
    tensor = DSL_tensor_analysis_create(pu_info, graph, diagnostic);
    if (tensor == NULL || !VHO_DSL_Tensor_Analysis_Build(tensor, diagnostic))
        goto done;
    locality = DSL_tensor_locality_create
                   (pu_info, tensor, snapshot, diagnostic);
    if (locality == NULL ||
        !VHO_DSL_Tensor_Locality_Build(locality, diagnostic))
        goto done;
    VHO_DSL_Tile_Control_Init(&control);
    control.target_profile_id = hardware.target_profile_id;
    if (joint) {
        control.select_plans = 0;
        control.defer_movement_resources = 1;
    }
    analysis = VHO_DSL_Tile_Create
                   (pu_info, graph, tensor, locality, NULL,
                    &control, diagnostic);
    if (analysis == NULL || !VHO_DSL_Tile_Build(analysis, diagnostic) ||
        !VHO_DSL_Tile_Verify(analysis, diagnostic))
        goto done;
    ir = VHO_DSL_Tile_Get_IR(analysis);
    if (DSL_tile_plan_ir_site_count(ir) != 1 ||
        !DSL_tile_plan_ir_get_site(ir, 1, &site)) {
        fprintf(diagnostic, "DSL CUDA output requires exactly one matmul site\n");
        goto done;
    }
    if (!VHO_DSL_CUDA_Trace_Tensor_Facts(tensor, locality)) {
        fprintf(diagnostic, "DSL CUDA output cannot trace tensor facts\n");
        goto done;
    }
    if (tile_candidate_count != NULL)
        *tile_candidate_count = site.tile_plan_count;
    plans = VHO_DSL_Tile_Get_Plan_Context(analysis, site.id);
    if (joint) {
        if (!VHO_DSL_cuda_select_joint_plan(pu_info, graph, locality, analysis,
                                           site, hardware, &joint_tile, joint_candidate_count, diagnostic) ||
            !VHO_DSL_tile_finalize_joint_selection(analysis, site.id, joint_tile, diagnostic))
            goto done;
        ir = VHO_DSL_Tile_Get_IR(analysis);
        if (!DSL_tile_plan_ir_get_site(ir, 1, &site))
            goto done;
    }
    if (plans == NULL || site.selected_plan_id == site.baseline_plan_id ||
        !DSL_opt_plan_get_plan(plans, site.selected_plan_id, &selected) ||
        !DSL_opt_plan_get_cost(plans, selected.cost_id, &cost) ||
        !cost.complete || selected.legality != DSL_OPT_LEGALITY_PROVEN) {
        fprintf(diagnostic,
                "DSL CUDA output has no cost-selected legal tile\n");
        goto done;
    }
    if (VHO_DSL_CUDA_TMA_Prefetch || VHO_DSL_CUDA_Whirl_Output != NULL) {
        VHO_DSL_Fetch_Pipeline_Control_Init(&fetch_control);
        fetch_control.target_profile_id = hardware.target_profile_id;
        fetch_control.hardware_arch_id = hardware.arch_id;
        fetch_control.focus_value_id = site.semantic_value_id;
        if (joint) {
            fetch_control.joint_cost = 1;
            fetch_control.readiness_cost = VHO_DSL_GEMM_Cost_Model == NULL ||
                strcmp(VHO_DSL_GEMM_Cost_Model, "readiness") == 0;
            fetch_control.memory_latency = VHO_DSL_GEMM_Memory_Latency;
            fetch_control.memory_latency_set = VHO_DSL_GEMM_Memory_Latency_Set;
            fetch_control.enable_direct = !VHO_DSL_CUDA_Tile_Apply;
            fetch_control.tile_plan_id = joint_tile;
            fetch_control.enable_vector = fetch_control.enable_async_copy = 0;
            fetch_control.enable_multidimensional_async =
                VHO_DSL_CUDA_TMA_Prefetch && !VHO_DSL_CUDA_Tile_Apply;
        }
        fetch_control.maximum_buffer_stages = VHO_DSL_CUDA_TMA_Prefetch ? 2 : 1;
        fetch_control.prefetch_distance_hint = VHO_DSL_CUDA_TMA_Prefetch ? 1 : 0;
        if (!VHO_DSL_CUDA_TMA_Prefetch) {
            fetch_control.enable_async_copy = 0;
            fetch_control.enable_multidimensional_async = 0;
        }
        fetch_analysis = VHO_DSL_Fetch_Pipeline_Create
            (pu_info, graph, locality, NULL, analysis, &fetch_control,
             diagnostic);
        if (fetch_analysis == NULL ||
            !VHO_DSL_Fetch_Pipeline_Build(fetch_analysis, diagnostic) ||
            !VHO_DSL_Fetch_Pipeline_Verify(fetch_analysis, diagnostic))
            goto done;
        fetch_ir = VHO_DSL_Fetch_Pipeline_Get_IR(fetch_analysis);
        if (DSL_fetch_pipeline_ir_site_count(fetch_ir) != 1 ||
            !DSL_fetch_pipeline_ir_get_site(fetch_ir, 1, &fetch_site) ||
            fetch_site.selected_tile_plan_id == 0)
            goto done;
        if (fetch_candidate_count != NULL)
            *fetch_candidate_count = fetch_site.pipeline_plan_count;
        fetch_plans = VHO_DSL_Fetch_Pipeline_Get_Plan_Context
            (fetch_analysis, fetch_site.id);
        if (fetch_plans == NULL)
            goto done;
        for (UINT32 i = 0; i < fetch_site.pipeline_plan_count; ++i) {
            DSL_FETCH_PLAN_RECORD candidate;
            if (!DSL_fetch_pipeline_ir_get_plan
                    (fetch_ir, fetch_site.first_pipeline_plan_id + i,
                     &candidate))
                goto done;
            if (VHO_DSL_AI_Opt_Trace &&
                Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)) {
                fprintf(TFile,
                    "AI_OPT 7 FETCH_CAND %u %u %u %s %s %s %u %u "
                    "%llu %llu %llu %llu",
                    fetch_site.id, candidate.id,
                    candidate.optimization_plan_id,
                    DSL_memory_movement_name(candidate.engine),
                    DSL_opt_legality_name(candidate.legality),
                    DSL_opt_rejection_reason_name(candidate.rejection_reason),
                    candidate.stage_count, candidate.prefetch_distance,
                    (unsigned long long)candidate.buffering_bytes,
                    (unsigned long long)candidate.raw_movement_cost,
                    (unsigned long long)candidate.hidden_movement_cost,
                    (unsigned long long)candidate.unhidden_movement_cost);
                if (joint)
                    fprintf(TFile, " %s",
                        (candidate.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "D" :
                        candidate.engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ? "T2" : "S1");
                fputc('\n', TFile);
            }
            if (candidate.optimization_plan_id == fetch_site.selected_plan_id)
                selected_fetch = candidate;
        }
        if (selected_fetch.id == 0 ||
            (!joint && VHO_DSL_CUDA_TMA_Prefetch && selected_fetch.engine !=
                DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC) ||
            selected_fetch.legality != DSL_OPT_LEGALITY_PROVEN) {
            fprintf(diagnostic,
                    "DSL CUDA TMA requested but P8 selected no legal TMA plan\n");
            goto done;
        }
        if (joint) {
            DSL_OPT_PLAN_RECORD whole_plan;
            if (!DSL_opt_plan_get_plan(fetch_plans, selected_fetch.optimization_plan_id, &whole_plan) ||
                !DSL_opt_plan_get_cost(fetch_plans, whole_plan.cost_id, &cost) || !cost.complete)
                goto done;
        }
        if (VHO_DSL_AI_Opt_Trace &&
            Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)) {
            fprintf(TFile,
                "AI_OPT 7 FETCH_SELECT %u %u %u %s "
                "selection_reason=%s\n",
                fetch_site.id, fetch_site.baseline_plan_id,
                fetch_site.selected_plan_id,
                DSL_memory_movement_name(selected_fetch.engine),
                joint ? "lowest_complete_joint_cost" : "lowest_complete_relative_cost");
            fflush(TFile);
        }
    }
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        DSL_TILE_PLAN_RECORD tile;
        DSL_TENSOR_FACT_RECORD fact;
        if (!DSL_tile_plan_ir_get_plan
                 (ir, site.first_tile_plan_id + i, &tile))
            goto done;
        if (tile.optimization_plan_id != site.selected_plan_id)
            continue;
        if (!DSL_tensor_analysis_find_fact
                 (tensor, site.semantic_value_id, &fact) ||
            TY_mtype(fact.element_ty) != MTYPE_F4 ||
            tile.problem_m > INT_MAX || tile.problem_n > INT_MAX ||
            tile.problem_k > INT_MAX || tile.cta_m == 0 ||
            tile.cta_n == 0 || tile.cta_k == 0 ||
            tile.thread_m == 0 || tile.thread_n == 0 ||
            tile.threads_per_cta == 0 ||
            tile.threads_per_cta > 1024) {
            fprintf(diagnostic,
                    "DSL CUDA output selected tile exceeds F32 kernel contract\n");
            goto done;
        }
        if (VHO_DSL_CUDA_Whirl_Output != NULL &&
            !VHO_DSL_CUDA_Pipeline_Valid(tile, selected_fetch)) {
            fprintf(diagnostic, "DSL CUDA P8 selected schedule failed legality\n");
            goto done;
        }
        const BOOL tma = selected_fetch.engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC;
        valid = path == NULL || VHO_DSL_CUDA_Write_Selected_Tile
                    (path, tile, cost, tma,
                     diagnostic);
        if (valid && path != NULL && tma)
            valid = VHO_DSL_CUDA_Append_TMA_Pipeline
                        (path, tile, selected_fetch, diagnostic);
        if (valid && emitted_plan_id != NULL)
            *emitted_plan_id = selected.id;
        if (valid && emitted_tile != NULL)
            *emitted_tile = tile;
        if (valid && emitted_fetch != NULL)
            *emitted_fetch = selected_fetch;
        if (valid && emitted_fetch_plan_id != NULL &&
            selected_fetch.id != 0)
            *emitted_fetch_plan_id = selected_fetch.optimization_plan_id;
        break;
    }
done:
    VHO_DSL_Fetch_Pipeline_Destroy(fetch_analysis);
    VHO_DSL_Tile_Destroy(analysis);
    DSL_tensor_locality_destroy(locality);
    DSL_tensor_control_snapshot_destroy(snapshot);
    DSL_tensor_analysis_destroy(tensor);
    DSL_tensor_evolution_destroy(graph);
    return valid;
}

/* Report each phase decision or non-execution reason for this PU; distinguish
 * host rewrite, staged device pipeline and source-only analysis evidence. */
static void
VHO_DSL_AI_Trace_Phases (PU_Info *pu_info,
                         DSL_OPT_PLAN_ID tile_plan_id,
                         DSL_OPT_PLAN_ID fetch_plan_id,
                         UINT32 tile_candidates, UINT32 fetch_candidates,
                         UINT32 joint_candidates,
                         BOOL tile_applied, BOOL direct)
{
    if (!VHO_DSL_AI_Opt_Trace ||
        !Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG))
        return;
    const BOOL tile = tile_plan_id != DSL_OPT_PLAN_INVALID_ID;
    const BOOL fetch = fetch_plan_id != DSL_OPT_PLAN_INVALID_ID;
    const char *no_tile_reason =
        VHO_DSL_CUDA_Tile_Output != NULL && !VHO_DSL_CUDA_Legacy_Emit ?
        "cuda_legacy_emit_off" : "cuda_tile_output_unset";
    const char *pu_name = ST_name(St_Table[PU_Info_proc_sym(pu_info)]);
    fprintf(TFile, "AI_OPT 7 CONTEXT %s matmul_f32_source\n", pu_name);
    for (UINT32 phase = 0; phase != 12; ++phase) {
        const char *status = "disabled_by_policy";
        const char *reason = "phase_not_in_matmul_source_slice";
        UINT32 regions = 0, candidates = 0, selected = 0;
        if (phase == 0 || phase == 1) {
            status = tile ? "analyzed_no_rewrite" : "disabled_by_option";
            reason = tile ? "analysis_for_tile_source" : no_tile_reason;
            regions = tile ? 1 : 0;
        } else if (phase == 2 || phase == 4 || phase == 5) {
            status = "not_applicable";
            reason = phase == 2 ? "single_matmul_no_fusion" :
                     "single_device_no_distribution";
        } else if (phase == 7) {
            status = tile_applied ? "rewritten" :
                     tile ? "analyzed_no_rewrite" : "disabled_by_option";
            reason = tile_applied ? "generated_runtime_call" :
                     tile ? "source_sidecar_no_whirl_rewrite" :
                     no_tile_reason;
            regions = tile ? 1 : 0;
            candidates = tile ? tile_candidates : 0;
            selected = tile ? tile_plan_id : 0;
        } else if (phase == 8) {
            status = fetch && VHO_DSL_CUDA_Whirl_Output != NULL ? "rewritten" :
                     fetch ? "analyzed_no_rewrite" : "disabled_by_option";
            reason = fetch && VHO_DSL_CUDA_Whirl_Output != NULL ?
                     (direct ? "device_direct_loads" : "device_pipeline") :
                     fetch ? "source_sidecar_no_whirl_rewrite" :
                     (tile ? "cuda_tma_prefetch_off" : no_tile_reason);
            regions = fetch ? 1 : 0;
            candidates = fetch ? fetch_candidates : 0;
            selected = fetch ? fetch_plan_id : 0;
        } else if (phase == 9) {
            status = tile_applied ? "rewritten" :
                     tile ? "analyzed_no_rewrite" : "disabled_by_option";
            reason = tile_applied ? "generated_runtime_call" :
                     tile ? "source_sidecar_no_whirl_rewrite" :
                     no_tile_reason;
            regions = tile ? 1 : 0;
            if (joint_candidates != 0) {
                candidates = joint_candidates;
                reason = tile_applied ? "joint_generated_runtime_call" : "joint_selected_device_plan";
            }
            selected = tile ? tile_plan_id : 0;
        } else if (phase == 10) {
            reason = "runtime_variants_not_in_matmul_slice";
        } else if (phase == 11) {
            status = "not_applicable";
            reason = "no_profile_join_at_compile_time";
        }
        fprintf(TFile, "AI_OPT 7 PHASE matmul_f32_source %u %s %s "
                       "%u %u %u %u\n", phase, status, reason,
                regions, candidates, selected,
                (tile_applied && (phase == 7 || phase == 9)) ||
                (phase == 8 && fetch && VHO_DSL_CUDA_Whirl_Output != NULL) ? 1U : 0U);
    }
    fflush(TFile);
}

/* Recognize the bounded one-matmul PU admitted by generated-provider
 * application; do not enlarge scope or infer fusion across call boundaries. */
static BOOL
VHO_DSL_AI_Is_Standalone_Matmul (WN *tree)
{
    if (WN_operator(tree) != OPR_FUNC_ENTRY ||
        WN_operator(WN_func_body(tree)) != OPR_BLOCK)
        return FALSE;
    UINT32 matmul_count = 0;
    for (WN *statement = WN_first(WN_func_body(tree)); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_RETURN)
            continue;
        if (WN_operator(statement) != OPR_STID ||
            WN_kid_count(statement) != 1)
            return FALSE;
        DSL_LOGICAL_OPCODE logical;
        if (!DSL_WN_Get_Logical_Opcode(WN_kid0(statement), &logical, NULL))
            return FALSE;
        if (logical.dsl_operator == OPR_DSLMATMUL)
            ++matmul_count;
        else if (logical.dsl_operator != OPR_DSLTENSORCONST &&
                 logical.dsl_operator != OPR_DSLMODELINPUT)
            return FALSE;
    }
    return matmul_count == 1;
}

/* Own the active-PU VHO sequence: validate/refine, select, stage device plans,
 * lower host semantics and emit controlled trace evidence. Device-image
 * construction/publication stays in the backend driver; failed gates abort
 * rather than silently accepting an incomplete optimized program. */
WN *
VHO_DSL_Lower_Driver
        (struct pu_info *pu_info,
         WN *tree)
{
    FmtAssert(VHO_DSL_GEMM_Plan_Search == NULL ||
              strcmp(VHO_DSL_GEMM_Plan_Search, "baseline") == 0 ||
              strcmp(VHO_DSL_GEMM_Plan_Search, "joint") == 0,
              ("gemm_plan_search requires baseline or joint"));
    const BOOL joint = VHO_DSL_GEMM_Plan_Search != NULL &&
                       strcmp(VHO_DSL_GEMM_Plan_Search, "joint") == 0;
    DSL_HARDWARE_DESCRIPTION_RECORD hardware;
    FmtAssert(VHO_DSL_cuda_get_hardware(&hardware),
              ("gpu_arch requires a supported exact sm_* hardware description"));
    FmtAssert((!VHO_DSL_CUDA_Whirl_Output && !VHO_DSL_CUDA_Tile_Output) ||
              hardware.target_profile_id == DSL_TARGET_PROFILE_NVIDIA_HOPPER,
              ("gpu_arch hardware description is planning-only for this CUDA lowering"));
    FmtAssert(VHO_DSL_GEMM_Cost_Model == NULL ||
              strcmp(VHO_DSL_GEMM_Cost_Model, "relative") == 0 ||
              strcmp(VHO_DSL_GEMM_Cost_Model, "readiness") == 0,
              ("gemm_cost_model requires relative or readiness"));
    FmtAssert(!joint || (Opt_Level >= 3 && VHO_DSL_CUDA_Whirl_Output != NULL),
              ("gemm_plan_search=joint requires -O3 and cuda_whirl_output"));
    FmtAssert(!joint || !VHO_DSL_CUDA_Legacy_Emit || VHO_DSL_CUDA_Tile_Output == NULL,
              ("D/S1/T2 joint search uses WHIRL-derived CUDA; disable cuda_legacy_emit"));
    BOOL shape_current = VHO_DSL_Shape_Refinement_Is_Current
                             (pu_info, tree, stderr);
    FmtAssert(shape_current,
              ("DSL lowering received stale tensor shape state"));

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    BOOL gatekeeper_valid = DSL_Gatekeeper_Verify_PU
                                (pu_info, stderr, &gatekeeper_result);
    FmtAssert(gatekeeper_valid,
              ("DSL gatekeeper rejected Very High Level WHIRL"));

    BOOL shape_invalidated =
        VHO_DSL_Opt_Enabled_Stages_Invalidate_Shape();
    if (shape_invalidated) {
        BOOL invalidated = VHO_DSL_Shape_Refinement_Invalidate
                               (pu_info, tree,
                                VHO_DSL_SHAPE_TRIGGER_VHO_DSL_OPTIMIZATION,
                                stderr);
        FmtAssert(invalidated,
                  ("could not invalidate DSL tensor shape state"));
    }

    VHO_DSL_OPT_RESULT opt_result;
    BOOL opt_valid = VHO_DSL_Opt_Register_Default_Passes() &&
                     VHO_DSL_Optimize_Program_Unit
                         (pu_info, &tree, stderr, &opt_result);
    FmtAssert(opt_valid,
              ("DSL VHO optimization pipeline failed"));

    if (opt_result.executed_shape_invalidating_stage_count != 0) {
        tree = VHO_DSL_Shape_Refine_Driver(pu_info, tree);
        Set_PU_Info_tree_ptr(pu_info, tree);
    }

    if (opt_result.executed_stage_count != 0) {
        gatekeeper_valid = DSL_Gatekeeper_Verify_PU
                               (pu_info, stderr, &gatekeeper_result);
        FmtAssert(gatekeeper_valid,
                  ("DSL VHO optimization produced invalid WHIRL"));
    }

    shape_current = VHO_DSL_Shape_Refinement_Is_Current
                        (pu_info, tree, stderr);
    FmtAssert(shape_current,
              ("DSL lowering received stale tensor shape state after "
               "optimization"));
    const BOOL standalone_matmul =
        VHO_DSL_AI_Is_Standalone_Matmul(tree);

    DSL_OPT_PLAN_ID emitted_tile_plan_id = DSL_OPT_PLAN_INVALID_ID;
    DSL_OPT_PLAN_ID emitted_fetch_plan_id = DSL_OPT_PLAN_INVALID_ID;
    UINT32 tile_candidate_count = 0, fetch_candidate_count = 0, joint_candidate_count = 0;
    DSL_TILE_PLAN_RECORD emitted_tile;
    DSL_FETCH_PLAN_RECORD emitted_fetch;
    UINT32 device_entry_ordinal = 0;
    memset(&emitted_tile, 0, sizeof(emitted_tile));
    if (VHO_DSL_CUDA_Tile_Apply)
        FmtAssert(Opt_Level >= 3 && standalone_matmul &&
                  ((VHO_DSL_CUDA_Tile_Output != NULL &&
                    VHO_DSL_CUDA_Legacy_Emit) ||
                   VHO_DSL_CUDA_Whirl_Output != NULL) &&
                  (!VHO_DSL_CUDA_TMA_Prefetch || joint),
                  ("M2 generated tile requires -O3, one matmul, a CUDA "
                   "output, and synchronous movement"));
    if ((VHO_DSL_CUDA_Tile_Output != NULL && VHO_DSL_CUDA_Legacy_Emit) ||
        VHO_DSL_CUDA_Whirl_Output != NULL) {
        BOOL emitted = VHO_DSL_CUDA_Emit_Selected_Tile
                           (pu_info, tree,
                            VHO_DSL_CUDA_Legacy_Emit ?
                                VHO_DSL_CUDA_Tile_Output : NULL,
                            &emitted_tile_plan_id, &emitted_fetch_plan_id,
                            &tile_candidate_count, &fetch_candidate_count,
                            &joint_candidate_count,
                            &emitted_tile,
                            &emitted_fetch,
                            stderr);
        FmtAssert(emitted, ("DSL CUDA tile plan selection failed"));
    }
    if (VHO_DSL_CUDA_Whirl_Output != NULL) {
        FmtAssert(emitted_tile_plan_id != DSL_OPT_PLAN_INVALID_ID,
                  ("CUDA WHIRL image requires a selected tile"));
        FmtAssert(VHO_DSL_CUDA_Stage_Selected_Whirl
                      (pu_info, emitted_tile, emitted_fetch,
                       &device_entry_ordinal, stderr),
                  ("DSL CUDA selected tile staging failed"));
    }

    VHO_DSL_LOWER_RESULT lower_result;
    BOOL lower_valid = VHO_DSL_CUDA_Tile_Apply ?
        VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile_Bound
            (pu_info, &tree, &emitted_tile, device_entry_ordinal,
             stderr, &lower_result) :
        VHO_DSL_Lower_Verified_Program_Unit
            (pu_info, tree, stderr, &lower_result);
    FmtAssert(lower_valid,
              ("DSL lowering did not produce canonical WHIRL"));
    if (VHO_DSL_AI_Opt_Trace &&
        emitted_tile_plan_id != DSL_OPT_PLAN_INVALID_ID &&
        Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG)) {
        fprintf(TFile, "AI_OPT 7 TILE_ACTION 1 %u %s %s "
                       "dsl_to_runtime %u\n",
                emitted_tile_plan_id,
                VHO_DSL_CUDA_Legacy_Emit &&
                VHO_DSL_CUDA_Tile_Output != NULL ?
                    "source_sidecar" : "device_whirl_sidecar",
                VHO_DSL_CUDA_Tile_Apply ? "generated_runtime_call" : "none",
                lower_result.lowered_node_count);
        if (VHO_DSL_CUDA_Whirl_Output != NULL) {
            const BOOL guard_m =
                emitted_tile.problem_m % emitted_tile.cta_m != 0;
            const BOOL guard_n =
                emitted_tile.problem_n % emitted_tile.cta_n != 0;
            const BOOL guard_k =
                emitted_tile.problem_k % emitted_tile.cta_k != 0;
            fprintf(TFile, "AI_OPT 7 EDGE_PLAN 1 %u %s %s %s %s "
                           "barriers=%s\n",
                    emitted_tile_plan_id,
                    DSL_tile_edge_policy_name(emitted_tile.edge_policy),
                    (emitted_fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "a=guarded_load" :
                        guard_m || guard_k ? "a=zero_fill" : "a=direct",
                    (emitted_fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "b=guarded_load" :
                        guard_k || guard_n ? "b=zero_fill" : "b=direct",
                    guard_m || guard_n ? "c=guarded_store" : "c=direct",
                    (emitted_fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "none" : "uniform");
        }
        if (emitted_fetch_plan_id != DSL_OPT_PLAN_INVALID_ID)
            fprintf(TFile,
                    "AI_OPT 7 FETCH_ACTION 1 %u %s %s %s\n",
                    emitted_fetch_plan_id,
                    VHO_DSL_CUDA_Whirl_Output != NULL ? "device_whirl_sidecar" : "source_sidecar",
                    (emitted_fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0 ? "direct_scalar" :
                    emitted_fetch.engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ?
                        "tma_2d" : "cooperative_scalar",
                    VHO_DSL_CUDA_Whirl_Output != NULL ?
                        "whirl_rewrite=device_pipeline" : "whirl_rewrite=none");
        fflush(TFile);
    }
    if (standalone_matmul)
        VHO_DSL_AI_Trace_Phases(pu_info, emitted_tile_plan_id,
                                emitted_fetch_plan_id, tile_candidate_count,
                                fetch_candidate_count,
                                joint_candidate_count,
                                VHO_DSL_CUDA_Tile_Apply,
                                emitted_fetch_plan_id != 0 &&
                                    (emitted_fetch.flags & DSL_FETCH_PLAN_FLAG_DIRECT) != 0);
    if (VHO_DSL_Dump_After_Lowering) {
        fprintf(TFile,
                "\n\n========== WHIRL after VHO DSL Lowering =========="
                "\n");
        fdump_tree(TFile, tree);
        fflush(TFile);
    }
    return tree;
}
