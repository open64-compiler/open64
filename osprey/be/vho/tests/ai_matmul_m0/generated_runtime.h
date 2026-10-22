#ifndef AI_MATMUL_GENERATED_RUNTIME_H
#define AI_MATMUL_GENERATED_RUNTIME_H

#include "open64_dsl_runtime_abi.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Zero means the selected CUDA kernel completed and copied its result home. */
int ai_matmul_m2_launch(const float *a, const float *b, float *c,
                        uint32_t m, uint32_t n, uint32_t k,
                        const OPEN64_DSL_GENERATED_MATMUL_PLAN_V1 *plan);

#ifdef __cplusplus
}
#endif

#endif
