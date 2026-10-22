#include "generated_runtime.h"
#include "runtime.h"

#include <assert.h>
#include <stdint.h>

typedef struct {
    OPEN64_DSL_TENSOR_DESCRIPTOR_V1 header;
    int64_t shape[2];
} TEST_DESCRIPTOR;

static const TEST_DESCRIPTOR matrix = {
    {
        OPEN64_DSL_RUNTIME_ABI_VERSION,
        OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE,
        sizeof(TEST_DESCRIPTOR),
        OPEN64_DSL_TENSOR_FLAG_CONSTANT,
        OPEN64_DSL_DTYPE_F32,
        2, 2, OPEN64_DSL_TENSOR_DESCRIPTOR_V1_SIZE, 0, 0,
        OPEN64_DSL_LAYOUT_CONTIGUOUS,
        OPEN64_DSL_SHARDING_UNSPECIFIED,
        OPEN64_DSL_PLACEMENT_HOST,
        OPEN64_DSL_MEMORY_HOST,
        OPEN64_DSL_QUANTIZATION_NONE,
        OPEN64_DSL_RUNTIME_STATE_STATIC,
        0
    },
    {128, 128}
};
static const OPEN64_DSL_SCALAR_VALUE_V1 one = {
    OPEN64_DSL_SCALAR_FLOAT_BITS, 32, 0x3f800000, 0
};
static const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 plan = {
    OPEN64_DSL_RUNTIME_ABI_VERSION,
    OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE,
    OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE,
    OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED,
    OPEN64_DSL_TARGET_PROFILE_NVIDIA_HOPPER,
    3, 3, 128, 128, 32, 8, 8, 32768, 256,
    OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
    0
};

static int mock_fails;
static int launch_count;

int
ai_matmul_m2_launch(const float *a, const float *b, float *c,
                    uint32_t m, uint32_t n, uint32_t k,
                    const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *selected)
{
    uint32_t i;
    assert(a != 0 && b != 0 && c != 0 && selected->tile_plan_id == 3);
    assert(m == 128 && n == 128 && k == 128);
    ++launch_count;
    if (mock_fails)
        return 1;
    for (i = 0; i < m * n; ++i)
        c[i] = 128.0f;
    return 0;
}

static void
run_case(int fail, int expected_path)
{
    OPEN64_DSL_TENSOR_HANDLE a, b, c;
    ai_matmul_m0_reset();
    mock_fails = fail;
    launch_count = 0;
    a = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    b = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    assert(a != 0 && b != 0);
    c = __open64_dsl_matmul_generated_v1(a, b, &matrix.header,
                                         OPEN64_DSL_MATMUL_FLAG_NONE, &plan);
    assert(c != 0 && c != a && c != b);
    assert(launch_count == 1);
    assert(ai_matmul_m0_last_generated_path() == expected_path);
    assert(ai_matmul_m0_validate(128, 128, 128.0f));
}

int
main(void)
{
    OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 invalid = plan;
    OPEN64_DSL_TENSOR_HANDLE a, b;
    run_case(0, 1);
    run_case(1, 2);

    ai_matmul_m0_reset();
    a = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    b = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    launch_count = 0;
    invalid.shared_bytes_per_stage = 1;
    assert(__open64_dsl_matmul_generated_v1(a, b, &matrix.header, 0,
                                             &invalid) == 0);
    assert(launch_count == 0);
    assert(ai_matmul_m0_last_generated_path() == 0);

    ai_matmul_m0_reset();
    a = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    b = __open64_dsl_tensor_const_v1(&matrix.header, &one);
    invalid = plan;
    invalid.fallback_provider = OPEN64_DSL_PHYSICAL_PROVIDER_UNSPECIFIED;
    mock_fails = 1;
    assert(__open64_dsl_matmul_generated_v1(a, b, &matrix.header, 0,
                                             &invalid) == 0);
    assert(ai_matmul_m0_last_generated_path() == 0);
    ai_matmul_m0_reset();
    return 0;
}
