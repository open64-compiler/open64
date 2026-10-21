/* Link-only stand-in for the compiler-produced PU in the host runtime test. */

#include "open64_dsl_runtime_abi.h"

#include <stdint.h>

typedef struct {
    OPEN64_DSL_TENSOR_DESCRIPTOR_V1 header;
    int64_t shape[2];
} M0_DESCRIPTOR;

static const M0_DESCRIPTOR matrix = {
    {
        OPEN64_DSL_RUNTIME_ABI_VERSION,
        OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE,
        sizeof(M0_DESCRIPTOR),
        OPEN64_DSL_TENSOR_FLAG_CONSTANT,
        OPEN64_DSL_DTYPE_F32,
        2,
        2,
        OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE,
        0,
        0,
        OPEN64_DSL_LAYOUT_CONTIGUOUS,
        OPEN64_DSL_SHARDING_UNSPECIFIED,
        OPEN64_DSL_PLACEMENT_HOST,
        OPEN64_DSL_MEMORY_HOST,
        OPEN64_DSL_QUANTIZATION_NONE,
        OPEN64_DSL_RUNTIME_STATE_STATIC,
        0,
    },
    {128, 128},
};

static const OPEN64_DSL_SCALAR_VALUE_V1 one = {
    OPEN64_DSL_SCALAR_FLOAT_BITS, 32, 0x3f800000, 0,
};

void
ai_matmul_m0(void)
{
    OPEN64_DSL_TENSOR_HANDLE lhs =
        __open64_dsl_tensor_const_v1(&matrix.header, &one);
    OPEN64_DSL_TENSOR_HANDLE rhs =
        __open64_dsl_tensor_const_v1(&matrix.header, &one);
    (void)__open64_dsl_matmul_v1(lhs, rhs, &matrix.header,
                                OPEN64_DSL_MATMUL_FLAG_NONE);
}
