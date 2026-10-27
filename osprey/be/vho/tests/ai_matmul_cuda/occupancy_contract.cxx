/* Copyright (C) 2026 Open64 Project */
/* Independent literal occupancy-round oracle; no WHIRL/runtime dependency.
 * Design: doc/AI-COMPILER-OPTIMIZATION-OCCUPANCY.md. */
#include "dsl_gemm_occupancy.h"
#include <assert.h>
#include <stdio.h>

/* Oracle division uses small synthetic fixtures, never production arithmetic. */
static uint64_t Ceil(uint64_t a, uint64_t b) { return (a + b - 1) / b; }

/* Literal service schedule does not call the production wave/matrix helpers. */
static uint64_t Literal(const DSL_GEMM_READINESS_INPUT &in, uint64_t resident)
{
    const DSL_HARDWARE_PERFORMANCE_RECORD &h = in.hardware;
    uint64_t total = 0, capacity = h.sms * resident;
    for (uint64_t first = 0; first < in.ctas; first += capacity) {
        uint64_t active = std::min(capacity, in.ctas - first);
        uint64_t q = Ceil(active, h.sms), consumer = in.consumer * q;
        uint64_t service = Ceil(in.input_bytes * active, h.memory_bytes_per_cycle);
        uint64_t barriers = (h.wait_cycles + h.release_cycles) * q;
        uint64_t issue = 2 * h.operand_issue_cycles * q, clock = 0;
        if (in.kind != 2) {
            uint64_t startup = Ceil(in.latency, std::min(in.warps * q, h.latency_hiding_warps));
            clock = in.steps * (consumer + service + startup + in.copy * q + (in.kind ? barriers : 0));
        } else {
            uint64_t ready[2] = {0, 0}, engine = 0;
            uint64_t preload = std::min(in.steps, UINT64_C(2));
            for (uint64_t j = 0; j < preload; ++j) {
                engine = std::max(engine, issue * (j + 1)) + service;
                ready[j] = engine + in.latency;
            }
            clock = issue * preload + h.publication_cycles * q;
            for (uint64_t j = 0; j < in.steps; ++j) {
                clock = std::max(clock, ready[j % 2]) + consumer + barriers;
                if (j + 2 < in.steps) {
                    clock += issue;
                    engine = std::max(engine, clock) + service;
                    ready[j % 2] = engine + in.latency;
                }
            }
        }
        total += clock + Ceil(in.output_bytes * active, h.memory_bytes_per_cycle) + h.store_latency;
    }
    return total;
}

/* Cover each limiter, ties, partial warps, rounding cliffs and failure atomicity,
 * then compare all three production schedules with independent literal loops. */
int main()
{
    DSL_HARDWARE_RESOURCE_RECORD hardware = {32, 64, 2048, 1024, 65536, 255,
        256, 4, 233472, 65536, 128, 1024};
    DSL_GEMM_OCCUPANCY_RESULT result;
    assert(DSL_gemm_occupancy(hardware, 32, 4, 33, 33, 1, 129, &result));
    assert(result.registers == 2048 && result.shared == 1152);
    assert(result.resident == 32 && result.warps == 64 && result.limiting == 11);
    assert(result.full == 1 && result.tail == 1 && result.unused == 127 && result.tail_sms == 1);
    for (unsigned limiter = 0; limiter < 5; ++limiter) {
        DSL_HARDWARE_RESOURCE_RECORD h = hardware;
        h.max_blocks = 100; h.max_warps = 100; h.max_threads = 10000;
        h.registers_per_sm = 1000000; h.shared_capacity = h.shared_budget = 1000000;
        if (limiter == 0) h.max_blocks = 1;
        if (limiter == 1) h.max_warps = 1;
        if (limiter == 2) h.max_threads = 32;
        if (limiter == 3) h.registers_per_sm = 1024;
        if (limiter == 4) h.shared_budget = 1024;
        assert(DSL_gemm_occupancy(h, 32, 4, 32, 4, 0, 5, &result));
        assert(result.resident == 1 && result.limiting == (UINT64_C(1) << limiter));
    }
    DSL_GEMM_OCCUPANCY_RESULT original = result;
    for (unsigned bad = 0; bad < 7; ++bad) {
        DSL_HARDWARE_RESOURCE_RECORD h = hardware;
        if (bad == 0) h.register_quantum = 0;
        if (bad == 1) h.shared_budget = h.shared_capacity + 1;
        if (bad == 2) h.shared_reserved = UINT64_MAX;
        if (bad == 3) h.registers_per_sm = 1;
        bool valid = DSL_gemm_occupancy(h, 32, bad == 4 ? UINT64_MAX : 4,
            bad == 5 ? 1025 : 32, bad == 6 ? 1024 : 4, 0, 5, &result);
        if (bad == 3 || bad == 5 || bad == 6) {
            assert(valid && !result.resident);
            result = original;
        } else {
            assert(!valid && memcmp(&original, &result, sizeof(result)) == 0);
        }
    }
    unsigned cases = 0;
    for (unsigned kind = 0; kind < 3; ++kind)
        for (unsigned steps = 1; steps <= 5; ++steps)
            for (unsigned ctas = 1; ctas <= 41; ++ctas)
                for (unsigned resident = 1; resident <= 3; ++resident)
                    for (unsigned sms = 1; sms <= 3; ++sms) {
                        DSL_GEMM_READINESS_INPUT in = {};
                        in.hardware = {sms, 32, 17, 128, 4, 128, 16, 100, 100, 100, 31, 3, 5, 7, 2};
                        in.ctas = ctas; in.steps = steps; in.consumer = 23;
                        in.input_bytes = 127; in.output_bytes = 65; in.copy = kind == 1 ? 3 : 0;
                        in.warps = 4; in.latency = 107; in.kind = kind;
                        DSL_GEMM_READINESS_RESULT actual, legacy;
                        assert(DSL_gemm_occupancy_cost(in, resident, &actual));
                        assert(actual.total == Literal(in, resident));
                        assert(actual.compute == Ceil(ctas, sms) * steps * in.consumer);
                        assert(actual.total == actual.compute + actual.memory + actual.sync + actual.issue);
                        if (resident == 1) {
                            assert(DSL_readiness_evaluate(in, &legacy));
                            assert(memcmp(&actual, &legacy, sizeof(actual)) == 0);
                        }
                        ++cases;
                    }
    DSL_GEMM_READINESS_INPUT bad = {};
    DSL_GEMM_READINESS_RESULT untouched = {}, saved = untouched;
    assert(!DSL_gemm_occupancy_cost(bad, 1, &untouched));
    assert(memcmp(&saved, &untouched, sizeof(saved)) == 0);
    for (unsigned fault = 0; fault < 5; ++fault) {
        bad = {};
        bad.hardware = {4, 32, 17, 128, 4, 128, 16, 100, 100, 100, 31, 3, 5, 7, 2};
        bad.ctas = 33; bad.steps = 3; bad.consumer = 23;
        bad.input_bytes = 127; bad.output_bytes = 65; bad.warps = 4; bad.kind = 2;
        if (fault == 0) bad.hardware.memory_bytes_per_cycle = 0;
        if (fault == 1) bad.input_bytes = UINT64_MAX;
        if (fault == 2) bad.consumer = UINT64_MAX;
        if (fault == 3) bad.hardware.publication_cycles = UINT64_MAX;
        assert(!DSL_gemm_occupancy_cost(bad, fault == 4 ? UINT64_MAX : 3, &untouched));
        assert(memcmp(&saved, &untouched, sizeof(saved)) == 0);
    }
    bad = {};
    bad.hardware = {4, 32, 17, 128, 4, 128, 16, 100, 100, 100, 31, 3, 5, 7, 2};
    bad.ctas = 33; bad.steps = 1000000000; bad.consumer = 23;
    bad.input_bytes = 127; bad.output_bytes = 65; bad.warps = 4; bad.kind = 2;
    assert(DSL_gemm_occupancy_cost(bad, 3, &untouched));
    printf("occupancy contract: %u literal D/S1/T2 oracles; rounding, five limiters, tails, billion-K and atomic failure passed\n", cases);
    return 0;
}
