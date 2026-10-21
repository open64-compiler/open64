#ifndef AI_MATMUL_M0_RUNTIME_H
#define AI_MATMUL_M0_RUNTIME_H

#include <stdint.h>

void ai_matmul_m0_reset(void);
int ai_matmul_m0_validate(uint32_t rows, uint32_t cols, float expected);
const char *ai_matmul_m0_error(void);

#endif
