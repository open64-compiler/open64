#include "generated_runtime.h"
#include "runtime.h"

#include <stdint.h>
#include <stdio.h>

extern void ai_matmul_m0(void);

#if !defined(AI_EXPECT_PLAN) || !defined(AI_EXPECT_SHARED)
#error "compiler-selected plan and shared bytes are required"
#endif

int
ai_matmul_m2_launch(const float *a, const float *b, float *c,
                    uint32_t m, uint32_t n, uint32_t k,
                    const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan)
{
    uint32_t i;
    if (a == 0 || b == 0 || c == 0 || m != 128 || n != 128 || k != 128 ||
        plan == 0 || plan->provider != OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED ||
        plan->target_profile != OPEN64_DSL_TARGET_PROFILE_NVIDIA_HOPPER ||
        plan->optimization_plan_id != AI_EXPECT_PLAN ||
        plan->tile_plan_id != AI_EXPECT_PLAN ||
        plan->cta_m != 128 || plan->cta_n != 128 || plan->cta_k != 32 ||
        plan->thread_m != 8 || plan->thread_n != 8 ||
        plan->shared_bytes_per_stage != AI_EXPECT_SHARED ||
        plan->threads_per_cta != 256 ||
        plan->fallback_provider != OPEN64_DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT)
        return 1;
    for (i = 0; i < m * n; ++i)
        c[i] = 128.0f;
    return 0;
}

int
main(void)
{
    ai_matmul_m0();
    if (ai_matmul_m0_last_generated_path() != 1 ||
        !ai_matmul_m0_validate(128, 128, 128.0f)) {
        fprintf(stderr, "compiled PU ABI smoke failed: %s\n",
                ai_matmul_m0_error());
        ai_matmul_m0_reset();
        return 1;
    }
    ai_matmul_m0_reset();
    puts("compiled M2 PU reached generated provider with expected plan");
    return 0;
}
