#define _POSIX_C_SOURCE 200809L

#include "runtime.h"

#include <stdint.h>
#include <stdio.h>
#include <time.h>

extern void ai_matmul_m0(void);

static uint64_t
time_ns(void)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0)
        return 0;
    return (uint64_t)now.tv_sec * 1000000000ull + (uint64_t)now.tv_nsec;
}

int
main(void)
{
    int sample;
    puts("sample,elapsed_ns");
    for (sample = 0; sample < 30; ++sample) {
        uint64_t start, stop;
        ai_matmul_m0_reset();
        start = time_ns();
        ai_matmul_m0();
        stop = time_ns();
        if (start == 0 || stop <= start ||
            ai_matmul_m0_last_generated_path() != 1 ||
            !ai_matmul_m0_validate(128, 128, 128.0f)) {
            fprintf(stderr, "sample=%d path=%d error=%s\n", sample,
                    ai_matmul_m0_last_generated_path(), ai_matmul_m0_error());
            ai_matmul_m0_reset();
            return 1;
        }
        printf("%d,%llu\n", sample,
               (unsigned long long)(stop - start));
    }
    ai_matmul_m0_reset();
    return 0;
}
