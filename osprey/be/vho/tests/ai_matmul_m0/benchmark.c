#define _POSIX_C_SOURCE 200809L

#include "runtime.h"

#include <stdint.h>
#include <stdio.h>
#include <time.h>

extern void ai_matmul_m0(void);

static uint64_t
elapsed_ns(const struct timespec *start, const struct timespec *end)
{
    return (uint64_t)(end->tv_sec - start->tv_sec) * 1000000000ULL +
           (uint64_t)(end->tv_nsec - start->tv_nsec);
}

int
main(void)
{
    unsigned sample;
    struct timespec start, end;
    for (sample = 0; sample < 35; ++sample) {
        ai_matmul_m0_reset();
        if (clock_gettime(CLOCK_MONOTONIC, &start) != 0)
            return 2;
        ai_matmul_m0();
        if (clock_gettime(CLOCK_MONOTONIC, &end) != 0)
            return 2;
        if (!ai_matmul_m0_validate(128, 128, 128.0f)) {
            fprintf(stderr, "sample %u: %s\n", sample, ai_matmul_m0_error());
            return 1;
        }
        if (sample == 5)
            puts("sample,elapsed_ns");
        if (sample >= 5)
            printf("%u,%llu\n", sample - 5,
                   (unsigned long long)elapsed_ns(&start, &end));
    }
    ai_matmul_m0_reset();
    return 0;
}
