/*
 * Compile-time contract checks for the first lowered DSL runtime ABI.
 */

#include "open64_dsl_runtime_abi.h"

typedef char DSL_runtime_descriptor_size_must_be_64
    [sizeof(OPEN64_DSL_TENSOR_DESCRIPTOR_V1) == 64 ? 1 : -1];
typedef char DSL_runtime_scalar_size_must_be_24
    [sizeof(OPEN64_DSL_SCALAR_VALUE_V1) == 24 ? 1 : -1];
typedef char DSL_runtime_abi_version_must_be_1
    [OPEN64_DSL_RUNTIME_ABI_VERSION == 1 ? 1 : -1];
typedef char DSL_runtime_logits_semantic_must_be_1
    [OPEN64_DSL_OUTPUT_SEMANTIC_LOGITS == 1 ? 1 : -1];
typedef char DSL_runtime_token_logits_semantic_must_be_2
    [OPEN64_DSL_OUTPUT_SEMANTIC_TOKEN_LOGITS == 2 ? 1 : -1];
typedef char DSL_runtime_operator_layout_nchw_must_be_1
    [OPEN64_DSL_OPERATOR_LAYOUT_NCHW == 1 ? 1 : -1];
typedef char DSL_runtime_operator_layout_oihw_must_be_2
    [OPEN64_DSL_OPERATOR_LAYOUT_OIHW == 2 ? 1 : -1];
typedef char DSL_runtime_operator_layout_oi_must_be_3
    [OPEN64_DSL_OPERATOR_LAYOUT_OI == 3 ? 1 : -1];
typedef char DSL_runtime_linear_transpose_weight_must_be_4
    [OPEN64_DSL_LINEAR_FLAG_TRANSPOSE_WEIGHT == 4 ? 1 : -1];
typedef char DSL_runtime_spatial_reduction_must_be_1
    [OPEN64_DSL_REDUCTION_AXES_SPATIAL == 1 ? 1 : -1];

int
main(void)
{
    OPEN64_DSL_TENSOR_HANDLE (*rotary_v2)
        (OPEN64_DSL_TENSOR_HANDLE, OPEN64_DSL_TENSOR_HANDLE,
         OPEN64_DSL_TENSOR_HANDLE, OPEN64_DSL_TENSOR_HANDLE,
         const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *) =
            &__open64_dsl_rotary_embedding_v2;
    OPEN64_DSL_TENSOR_HANDLE (*attention_v2)
        (OPEN64_DSL_TENSOR_HANDLE, OPEN64_DSL_TENSOR_HANDLE,
         OPEN64_DSL_TENSOR_HANDLE,
         const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *, uint32_t, uint32_t,
         uint32_t, OPEN64_DSL_STATE_HANDLE *, OPEN64_DSL_STATE_HANDLE *) =
            &__open64_dsl_attention_v2;
    OPEN64_DSL_TENSOR_DESCRIPTOR_V1 descriptor;
    descriptor.abi_version = OPEN64_DSL_RUNTIME_ABI_VERSION;
    descriptor.header_size = OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE;
    descriptor.flags = OPEN64_DSL_TENSOR_FLAG_NO_ALIAS;
    descriptor.dtype = OPEN64_DSL_DTYPE_I32;
    descriptor.layout = OPEN64_DSL_LAYOUT_CONTIGUOUS;
    descriptor.sharding = OPEN64_DSL_SHARDING_REPLICATED;
    descriptor.placement = OPEN64_DSL_PLACEMENT_HOST;
    descriptor.memory = OPEN64_DSL_MEMORY_HOST;
    descriptor.quantization = OPEN64_DSL_QUANTIZATION_NONE;
    descriptor.runtime_state = OPEN64_DSL_RUNTIME_STATE_STATIC;
    return descriptor.header_size == sizeof(descriptor) &&
           rotary_v2 != 0 && attention_v2 != 0 ? 0 : 1;
}
