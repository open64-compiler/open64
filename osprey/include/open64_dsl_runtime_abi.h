/*
 * Copyright (C) 2026 Open64 Project
 *
 * Target-independent runtime ABI for lowered DSL tensor operations.
 */

#ifndef open64_dsl_runtime_abi_INCLUDED
#define open64_dsl_runtime_abi_INCLUDED

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OPEN64_DSL_RUNTIME_ABI_VERSION 1
#define OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE 64

typedef void *OPEN64_DSL_TENSOR_HANDLE;

/* Append-only numeric contracts.  Zero always means unspecified. */
typedef enum {
    OPEN64_DSL_DTYPE_UNSPECIFIED = 0,
    OPEN64_DSL_DTYPE_I8 = 1,
    OPEN64_DSL_DTYPE_I16 = 2,
    OPEN64_DSL_DTYPE_I32 = 3,
    OPEN64_DSL_DTYPE_I64 = 4,
    OPEN64_DSL_DTYPE_U8 = 5,
    OPEN64_DSL_DTYPE_U16 = 6,
    OPEN64_DSL_DTYPE_U32 = 7,
    OPEN64_DSL_DTYPE_U64 = 8,
    OPEN64_DSL_DTYPE_F16 = 9,
    OPEN64_DSL_DTYPE_BF16 = 10,
    OPEN64_DSL_DTYPE_F32 = 11,
    OPEN64_DSL_DTYPE_F64 = 12
} OPEN64_DSL_RUNTIME_DTYPE;

typedef enum {
    OPEN64_DSL_LAYOUT_UNSPECIFIED = 0,
    OPEN64_DSL_LAYOUT_CONTIGUOUS = 1,
    OPEN64_DSL_LAYOUT_STRIDED = 2
} OPEN64_DSL_RUNTIME_LAYOUT;

typedef enum {
    OPEN64_DSL_MEMORY_UNSPECIFIED = 0,
    OPEN64_DSL_MEMORY_HOST = 1,
    OPEN64_DSL_MEMORY_DEVICE = 2,
    OPEN64_DSL_MEMORY_UNIFIED = 3,
    OPEN64_DSL_MEMORY_EXTERNAL_DATA = 4
} OPEN64_DSL_RUNTIME_MEMORY;

typedef enum {
    OPEN64_DSL_SHARDING_UNSPECIFIED = 0,
    OPEN64_DSL_SHARDING_REPLICATED = 1,
    OPEN64_DSL_SHARDING_PARTITIONED = 2
} OPEN64_DSL_RUNTIME_SHARDING;

typedef enum {
    OPEN64_DSL_PLACEMENT_UNSPECIFIED = 0,
    OPEN64_DSL_PLACEMENT_HOST = 1,
    OPEN64_DSL_PLACEMENT_DEVICE = 2,
    OPEN64_DSL_PLACEMENT_SIDE_FILE = 3
} OPEN64_DSL_RUNTIME_PLACEMENT;

typedef enum {
    OPEN64_DSL_QUANTIZATION_UNSPECIFIED = 0,
    OPEN64_DSL_QUANTIZATION_NONE = 1,
    OPEN64_DSL_QUANTIZATION_AFFINE = 2
} OPEN64_DSL_RUNTIME_QUANTIZATION;

typedef enum {
    OPEN64_DSL_RUNTIME_STATE_UNSPECIFIED = 0,
    OPEN64_DSL_RUNTIME_STATE_STATIC = 1,
    OPEN64_DSL_RUNTIME_STATE_DYNAMIC = 2
} OPEN64_DSL_RUNTIME_STATE;

typedef enum {
    OPEN64_DSL_TENSOR_FLAG_NONE = 0,
    OPEN64_DSL_TENSOR_FLAG_NO_ALIAS = 1u << 0,
    OPEN64_DSL_TENSOR_FLAG_CONSTANT = 1u << 1,
    OPEN64_DSL_TENSOR_FLAG_HAS_STRIDES = 1u << 2
} OPEN64_DSL_RUNTIME_TENSOR_FLAGS;

typedef enum {
    OPEN64_DSL_BROADCAST_NONE = 0,
    OPEN64_DSL_BROADCAST_NUMPY = 1
} OPEN64_DSL_RUNTIME_BROADCAST_RULE;

typedef enum {
    OPEN64_DSL_MATMUL_FLAG_NONE = 0,
    OPEN64_DSL_MATMUL_FLAG_TRANSPOSE_KID0 = 1u << 0,
    OPEN64_DSL_MATMUL_FLAG_TRANSPOSE_KID1 = 1u << 1
} OPEN64_DSL_RUNTIME_MATMUL_FLAGS;

typedef enum {
    OPEN64_DSL_OUTPUT_SEMANTIC_UNSPECIFIED = 0,
    OPEN64_DSL_OUTPUT_SEMANTIC_LOGITS = 1
} OPEN64_DSL_RUNTIME_OUTPUT_SEMANTIC;

typedef enum {
    OPEN64_DSL_OPERATOR_LAYOUT_UNSPECIFIED = 0,
    OPEN64_DSL_OPERATOR_LAYOUT_NCHW = 1,
    OPEN64_DSL_OPERATOR_LAYOUT_OIHW = 2,
    OPEN64_DSL_OPERATOR_LAYOUT_OI = 3
} OPEN64_DSL_RUNTIME_OPERATOR_LAYOUT;

typedef enum {
    OPEN64_DSL_LINEAR_FLAG_NONE = 0,
    OPEN64_DSL_LINEAR_FLAG_HAS_BIAS = 1u << 0,
    OPEN64_DSL_LINEAR_FLAG_TRANSPOSE_INPUT = 1u << 1,
    OPEN64_DSL_LINEAR_FLAG_TRANSPOSE_WEIGHT = 1u << 2
} OPEN64_DSL_RUNTIME_LINEAR_FLAGS;

typedef enum {
    OPEN64_DSL_REDUCTION_AXES_UNSPECIFIED = 0,
    OPEN64_DSL_REDUCTION_AXES_SPATIAL = 1
} OPEN64_DSL_RUNTIME_REDUCTION_AXES;

/*
 * Fixed header followed by optional signed 64-bit shape and stride arrays.
 * Offsets are byte offsets from the beginning of this descriptor blob.  A
 * zero offset denotes an absent array.  No process-local pointers occur in the
 * descriptor image.
 */
typedef struct {
    uint16_t abi_version;
    uint16_t header_size;
    uint32_t total_size;
    uint32_t flags;
    uint32_t dtype;
    int32_t rank;
    uint32_t shape_count;
    uint32_t shape_offset;
    uint32_t stride_count;
    uint32_t stride_offset;
    uint32_t layout;
    uint32_t sharding;
    uint32_t placement;
    uint32_t memory;
    uint32_t quantization;
    uint32_t runtime_state;
    uint32_t reserved;
} OPEN64_DSL_TENSOR_DESCRIPTOR_V1;

typedef enum {
    OPEN64_DSL_SCALAR_UNSPECIFIED = 0,
    OPEN64_DSL_SCALAR_SIGNED = 1,
    OPEN64_DSL_SCALAR_UNSIGNED = 2,
    OPEN64_DSL_SCALAR_FLOAT_BITS = 3
} OPEN64_DSL_RUNTIME_SCALAR_KIND;

typedef struct {
    uint32_t kind;
    uint32_t bit_width;
    uint64_t bits_lo;
    uint64_t bits_hi;
} OPEN64_DSL_SCALAR_VALUE_V1;

/*
 * Each successful call returns a distinct owning handle.  A NULL handle is a
 * runtime failure.  The compiler stores successful handles in distinct PREGs;
 * the runtime owns allocation, placement, and eventual reclamation policy.
 */
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_tensor_const_v1
                                (const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 const OPEN64_DSL_SCALAR_VALUE_V1 *value);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_external_tensor_v1
                                (const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 const char *storage_uri);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_model_input_v1
                                (const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t input_ordinal);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_add_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE kid1,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t broadcast_rule);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_matmul_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE kid1,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t flags);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_relu_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_flatten_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 int32_t start_dim,
                                 int32_t end_dim);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_residual_add_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE kid1,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_output_logits_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t semantic);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_linear_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE weight,
                                 OPEN64_DSL_TENSOR_HANDLE bias,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t flags,
                                 uint32_t weight_layout);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_conv2d_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE weight,
                                 OPEN64_DSL_TENSOR_HANDLE bias,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t kernel_h,
                                 uint32_t kernel_w,
                                 uint32_t stride_h,
                                 uint32_t stride_w,
                                 uint32_t padding_h,
                                 uint32_t padding_w,
                                 uint32_t dilation_h,
                                 uint32_t dilation_w,
                                 uint32_t groups,
                                 uint32_t input_layout,
                                 uint32_t weight_layout,
                                 uint32_t output_layout);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_batch_norm_infer_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 OPEN64_DSL_TENSOR_HANDLE scale,
                                 OPEN64_DSL_TENSOR_HANDLE bias,
                                 OPEN64_DSL_TENSOR_HANDLE running_mean,
                                 OPEN64_DSL_TENSOR_HANDLE running_variance,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint64_t epsilon_bits,
                                 uint32_t input_layout,
                                 int32_t channel_axis);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_max_pool2d_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t kernel_h,
                                 uint32_t kernel_w,
                                 uint32_t stride_h,
                                 uint32_t stride_w,
                                 uint32_t padding_h,
                                 uint32_t padding_w,
                                 uint32_t dilation_h,
                                 uint32_t dilation_w,
                                 uint32_t ceil_mode);
OPEN64_DSL_TENSOR_HANDLE __open64_dsl_global_avg_pool2d_v1
                                (OPEN64_DSL_TENSOR_HANDLE kid0,
                                 const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result,
                                 uint32_t output_h,
                                 uint32_t output_w,
                                 uint32_t reduction_axes);

#ifdef __cplusplus
}
#endif

#endif /* open64_dsl_runtime_abi_INCLUDED */
