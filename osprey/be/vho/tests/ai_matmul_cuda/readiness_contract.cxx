/* Copyright (C) 2026 Open64 Project */
/* Standalone oracle for VHO's pure analytical schedule, no WHIRL or runtime
 * dependency. See doc/AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md. */
#include "dsl_gemm_readiness.h"
#include <assert.h>
#include <stdio.h>

/* Literal schedule oracle deliberately does not use max-plus matrices. */
static uint64_t Literal(const DSL_GEMM_READINESS_INPUT &in)
{
    uint64_t total = 0;
    for (uint64_t first = 0; first < in.ctas; first += 4) {
        uint64_t active = std::min(UINT64_C(4), in.ctas - first);
        uint64_t service = (in.input_bytes * active + 31) / 32;
        uint64_t slots[2] = {0, 0}, engine = 0;
        unsigned preload = std::min(in.steps, UINT64_C(2));
        for (unsigned j = 0; j < preload; ++j) {
            engine = std::max(engine, UINT64_C(2) * (j + 1)) + service;
            slots[j] = engine + in.latency;
        }
        uint64_t clock = 2 * preload + 8;
        for (uint64_t j = 0; j < in.steps; ++j) {
            clock = std::max(clock, slots[j % 2]) + 8 + in.consumer + 8;
            if (j + 2 < in.steps) {
                clock += 2;
                engine = std::max(engine, clock) + service;
                slots[j % 2] = engine + in.latency;
            }
        }
        total += clock + (in.output_bytes * active + 31) / 32 + in.latency;
    }
    return total;
}

/* Check prologue, recurrence, drain, bandwidth, waves, and fail-closed input. */
int main()
{
    unsigned cases = 0;
    for (unsigned steps = 1; steps <= 65; ++steps)
        for (unsigned ctas = 1; ctas <= 9; ++ctas)
            for (unsigned profile = 0; profile < 4; ++profile) {
                DSL_GEMM_READINESS_INPUT in = {};
                in.ctas = ctas; in.steps = steps; in.consumer = profile ? 37 : 1;
                in.input_bytes = profile == 2 ? 8192 : 32;
                in.output_bytes = 128; in.warps = 8;
                in.latency = profile * 100; in.kind = 2;
                DSL_GEMM_READINESS_RESULT result;
                assert(DSL_readiness_evaluate(in, &result));
                assert(result.total == Literal(in));
                assert(result.total == result.compute + result.memory + result.sync + result.issue);
                ++cases;
            }
    DSL_GEMM_READINESS_INPUT in = {};
    in.ctas = 1; in.steps = 1000000000; in.consumer = 32;
    in.input_bytes = 512; in.output_bytes = 32; in.warps = 8;
    in.latency = 400; in.kind = 2;
    DSL_GEMM_READINESS_RESULT result;
    assert(DSL_readiness_evaluate(in, &result));
    for (unsigned kind = 0; kind < 2; ++kind) {
        in.steps = 3; in.kind = kind; in.copy = kind ? 4 : 0;
        assert(DSL_readiness_evaluate(in, &result));
        assert(result.wait == 150);
        assert(result.sync == (kind ? 48 : 0));
        assert(result.memory == (16 + 50 + in.copy) * 3 + 401);
    }
    DSL_GEMM_READINESS_RESULT original = result;
    in.ctas = 0;
    assert(!DSL_readiness_evaluate(in, &result));
    assert(memcmp(&original, &result, sizeof(result)) == 0);
    in.ctas = UINT64_MAX; in.input_bytes = UINT64_MAX;
    assert(!DSL_readiness_evaluate(in, &result));
    assert(memcmp(&original, &result, sizeof(result)) == 0);
    printf("readiness contract: %u literal T2 oracles, D/S1, billion-step and invalid/overflow checks passed\n", cases);
    return 0;
}
