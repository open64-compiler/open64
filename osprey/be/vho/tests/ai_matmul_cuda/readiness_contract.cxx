/* Copyright (C) 2026 Open64 Project */
/* Standalone oracle for VHO's pure analytical schedule, no WHIRL or runtime
 * dependency. See doc/AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md. */
#include "dsl_gemm_readiness.h"
#include <assert.h>
#include <stdio.h>

/* Explicit synthetic fixture, not a compiler fallback or hardware lookup. */
static DSL_HARDWARE_PERFORMANCE_RECORD Test_hardware()
{
    DSL_HARDWARE_PERFORMANCE_RECORD hw =
        { 4, 32, 32, 128, 4, 128, 16, 100, 100, 100, 400, 8, 8, 8, 1 };
    return hw;
}

/* Literal schedule oracle deliberately does not use max-plus matrices. */
static uint64_t Literal(const DSL_GEMM_READINESS_INPUT &in)
{
    uint64_t total = 0;
    const DSL_HARDWARE_PERFORMANCE_RECORD &hw = in.hardware;
    uint64_t pair_issue = 2 * hw.operand_issue_cycles;
    for (uint64_t first = 0; first < in.ctas; first += hw.sms) {
        uint64_t active = std::min(hw.sms, in.ctas - first);
        uint64_t service = DSL_readiness_ceil(in.input_bytes * active, hw.memory_bytes_per_cycle);
        uint64_t slots[2] = {0, 0}, engine = 0;
        unsigned preload = std::min(in.steps, UINT64_C(2));
        for (unsigned j = 0; j < preload; ++j) {
            engine = std::max(engine, pair_issue * (j + 1)) + service;
            slots[j] = engine + in.latency;
        }
        uint64_t clock = pair_issue * preload + hw.publication_cycles;
        for (uint64_t j = 0; j < in.steps; ++j) {
            clock = std::max(clock, slots[j % 2]) + hw.wait_cycles + in.consumer + hw.release_cycles;
            if (j + 2 < in.steps) {
                clock += pair_issue;
                engine = std::max(engine, clock) + service;
                slots[j % 2] = engine + in.latency;
            }
        }
        total += clock + DSL_readiness_ceil(in.output_bytes * active, hw.memory_bytes_per_cycle) + hw.store_latency;
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
                in.hardware = Test_hardware();
                in.hardware.sms = profile + 1;
                in.hardware.memory_bytes_per_cycle = 16 * (profile + 1);
                in.hardware.publication_cycles = profile;
                in.hardware.wait_cycles = profile * 3;
                in.hardware.release_cycles = profile * 5;
                in.hardware.operand_issue_cycles = profile;
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
    in.hardware = Test_hardware();
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
    in.hardware.memory_bytes_per_cycle = 0;
    assert(!DSL_readiness_evaluate(in, &result));
    assert(memcmp(&original, &result, sizeof(result)) == 0);
    in.hardware = Test_hardware();
    in.ctas = 0;
    assert(!DSL_readiness_evaluate(in, &result));
    assert(memcmp(&original, &result, sizeof(result)) == 0);
    in.ctas = UINT64_MAX; in.input_bytes = UINT64_MAX;
    assert(!DSL_readiness_evaluate(in, &result));
    assert(memcmp(&original, &result, sizeof(result)) == 0);
    printf("readiness contract: %u literal T2 oracles, D/S1, billion-step and invalid/overflow checks passed\n", cases);
    return 0;
}
