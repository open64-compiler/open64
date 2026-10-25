/* Host-only binding test; this is not a CUDA provider implementation. */

#include "open64_dsl_runtime_abi.h"
#include "runtime.h"

#include <stdint.h>
#include <stdio.h>

_Static_assert(sizeof(OPEN64_DSL_GENERATED_MATMUL_PLAN_V1) == 64,
               "generated matmul v1 ABI size changed");
_Static_assert(sizeof(OPEN64_DSL_GENERATED_MATMUL_PLAN_V2) == 72,
               "generated matmul v2 ABI size changed");

extern void ai_matmul_duplicate_0(void);
extern void ai_matmul_duplicate_1(void);

static uint64_t seen_entry_ordinal;

#ifndef AI_EXPECT_PLAN
#error "compiler-selected plan ID is required"
#endif

int
ai_matmul_m2_launch(const float *a, const float *b, float *c,
                    uint32_t m, uint32_t n, uint32_t k,
                    const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan)
{
    uint32_t index;
    if (a == 0 || b == 0 || c == 0 || plan == 0 ||
        m != 128 || n != 128 || k != 128 ||
        plan->optimization_plan_id != AI_EXPECT_PLAN ||
        plan->tile_plan_id != AI_EXPECT_PLAN)
        return 1;
    for (index = 0; index != m * n; ++index)
        c[index] = 128.0f;
    return 0;
}

OPEN64_DSL_TENSOR_HANDLE
__open64_dsl_matmul_generated_v2
        (OPEN64_DSL_TENSOR_HANDLE kid0, OPEN64_DSL_TENSOR_HANDLE kid1,
         const OPEN64_DSL_TENSOR_DESCRIPTOR_V1 *result, uint32_t flags,
         const OPEN64_DSL_GENERATED_MATMUL_PLAN_V2 *plan)
{
    OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 base;
    if (plan == 0 || plan->base.abi_version != 2 ||
        plan->base.header_size != OPEN64_DSL_GENERATED_MATMUL_PLAN_V2_SIZE ||
        plan->base.total_size != OPEN64_DSL_GENERATED_MATMUL_PLAN_V2_SIZE ||
        plan->base.reserved != 0 || plan->device_entry_ordinal != 1)
        return 0;
    seen_entry_ordinal = plan->device_entry_ordinal;
    base = plan->base;
    base.abi_version = OPEN64_DSL_RUNTIME_ABI_VERSION;
    base.header_size = OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE;
    base.total_size = OPEN64_DSL_GENERATED_MATMUL_PLAN_V1_SIZE;
    return __open64_dsl_matmul_generated_v1
               (kid0, kid1, result, flags, &base);
}

int
main(void)
{
    ai_matmul_duplicate_0();
    if (seen_entry_ordinal != 0 ||
        ai_matmul_m0_last_generated_path() != 1 ||
        !ai_matmul_m0_validate(128, 128, 128.0f)) {
        fprintf(stderr, "first host PU binding failed: %s\n",
                ai_matmul_m0_error());
        return 1;
    }
    ai_matmul_m0_reset();
    ai_matmul_duplicate_1();
    if (seen_entry_ordinal != 1 ||
        ai_matmul_m0_last_generated_path() != 1 ||
        !ai_matmul_m0_validate(128, 128, 128.0f)) {
        fprintf(stderr, "second host PU binding failed: %s\n",
                ai_matmul_m0_error());
        return 1;
    }
    ai_matmul_m0_reset();
    puts("both host PUs used their versioned generated-plan binding");
    return 0;
}
