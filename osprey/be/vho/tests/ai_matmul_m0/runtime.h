#ifndef AI_MATMUL_M0_RUNTIME_H
#define AI_MATMUL_M0_RUNTIME_H

#include <stdint.h>

void ai_matmul_m0_reset(void);
int ai_matmul_m0_validate(uint32_t rows, uint32_t cols, float expected);
const char *ai_matmul_m0_error(void);
/* 0: no generated call, 1: generated CUDA path, 2: direct CPU fallback. */
int ai_matmul_m0_last_generated_path(void);

#endif
