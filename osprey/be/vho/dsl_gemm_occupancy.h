/* Copyright (C) 2026 Open64 Project */
/* PU-local logical occupancy estimate; no native allocation or WHIRL mutation.
 * Controlling design: doc/AI-COMPILER-OPTIMIZATION-OCCUPANCY.md. */
#ifndef dsl_gemm_occupancy_INCLUDED
#define dsl_gemm_occupancy_INCLUDED
#include "dsl_gemm_readiness.h"

struct DSL_GEMM_OCCUPANCY_RESULT {
    uint64_t registers, shared, limits[5], resident, warps, limiting;
    uint64_t full, tail, unused, tail_sms;
};

/* Estimate rounded capacity from logical demand; never mutate output on
 * unknown or overflow. Zero residency is a valid rejection ledger, never
 * authorization to select. Limits are block/warp/thread/reg/SMEM. */
static inline bool DSL_gemm_occupancy(const DSL_HARDWARE_RESOURCE_RECORD &r,
    uint64_t lanes, uint64_t sms, uint64_t threads, uint64_t register_bytes,
    uint64_t shared_bytes, uint64_t ctas, DSL_GEMM_OCCUPANCY_RESULT *out)
{
    if (!out || !DSL_hardware_resources_valid(r) || !lanes || !sms || !threads ||
        !register_bytes || !ctas) return false;
    DSL_GEMM_OCCUPANCY_RESULT o = {};
    uint64_t warps = DSL_readiness_ceil(threads, lanes), allocated_warps;
    uint64_t registers = DSL_readiness_ceil(register_bytes, 4), temporary, capacity;
    bool thread_register_limit = registers > r.max_registers_per_thread;
    if (!DSL_readiness_mul(registers, lanes, &temporary) ||
        !DSL_readiness_mul(DSL_readiness_ceil(temporary, r.register_quantum), r.register_quantum, &registers) ||
        !DSL_readiness_mul(DSL_readiness_ceil(warps, r.register_warp_quantum), r.register_warp_quantum, &allocated_warps) ||
        !DSL_readiness_mul(registers, allocated_warps, &o.registers) ||
        !DSL_readiness_add(shared_bytes, r.shared_reserved, &temporary) ||
        !DSL_readiness_mul(DSL_readiness_ceil(temporary, r.shared_quantum), r.shared_quantum, &o.shared)) return false;
    o.limits[0] = r.max_blocks;
    o.limits[1] = r.max_warps / warps;
    o.limits[2] = threads > r.max_threads_per_cta ? 0 : r.max_threads / threads;
    o.limits[3] = thread_register_limit ? 0 : r.registers_per_sm / o.registers;
    o.limits[4] = o.shared ? r.shared_budget / o.shared : r.max_blocks;
    o.resident = *std::min_element(o.limits, o.limits + 5);
    if (!DSL_readiness_mul(o.resident, warps, &o.warps) ||
        !DSL_readiness_mul(sms, o.resident, &capacity)) return false;
    for (unsigned i = 0; i < 5; ++i)
        if (o.limits[i] == o.resident) o.limiting |= UINT64_C(1) << i;
    o.full = capacity ? ctas / capacity : 0;
    o.tail = capacity ? ctas % capacity : 0;
    o.unused = o.tail ? capacity - o.tail : 0;
    o.tail_sms = std::min(o.tail, sms);
    *out = o;
    return true;
}

/* Evaluate lockstep residency rounds without multiplying shared throughput.
 * CTA work/issue/sync share each SM; traffic shares one device bandwidth.
 * This is an estimate, not a bound on the unknown native CUDA schedule. */
static inline bool DSL_gemm_occupancy_cost(const DSL_GEMM_READINESS_INPUT &in,
    uint64_t resident, DSL_GEMM_READINESS_RESULT *out)
{
    if (!out || !resident || !in.ctas || !in.steps || !in.consumer || !in.warps ||
        !in.input_bytes || !in.output_bytes || in.kind > 2 ||
        !DSL_hardware_performance_valid(in.hardware)) return false;
    uint64_t capacity;
    if (!DSL_readiness_mul(in.hardware.sms, resident, &capacity)) return false;
    DSL_GEMM_READINESS_RESULT result = {};
    for (unsigned tail = 0; tail < 2; ++tail) {
        uint64_t active = tail ? in.ctas % capacity : capacity;
        uint64_t rounds = tail ? (active != 0) : in.ctas / capacity;
        if (!rounds) continue;
        uint64_t q = DSL_readiness_ceil(active, in.hardware.sms);
        DSL_GEMM_READINESS_INPUT group = in;
        if (!DSL_readiness_mul(in.consumer, q, &group.consumer) ||
            !DSL_readiness_mul(in.copy, q, &group.copy) ||
            !DSL_readiness_mul(in.warps, q, &group.warps) ||
            !DSL_readiness_mul(in.hardware.wait_cycles, q, &group.hardware.wait_cycles) ||
            !DSL_readiness_mul(in.hardware.release_cycles, q, &group.hardware.release_cycles) ||
            !DSL_readiness_mul(in.hardware.operand_issue_cycles, q, &group.hardware.operand_issue_cycles) ||
            !DSL_readiness_mul(in.hardware.publication_cycles, q, &group.hardware.publication_cycles)) return false;
        DSL_GEMM_READINESS_RESULT wave;
        if (!DSL_readiness_wave(group, active, &wave)) return false;
        uint64_t *dest[] = {&result.compute, &result.memory, &result.sync,
            &result.issue, &result.total, &result.wait};
        const uint64_t source[] = {wave.compute, wave.memory, wave.sync,
            wave.issue, wave.total, wave.wait};
        for (unsigned i = 0; i < 6; ++i) {
            uint64_t amount;
            if (!DSL_readiness_mul(source[i], rounds, &amount) ||
                !DSL_readiness_add(*dest[i], amount, dest[i])) return false;
        }
        result.delivery = std::max(result.delivery, wave.delivery);
        result.window = std::max(result.window, wave.window);
        if (!DSL_readiness_add(result.waves, rounds, &result.waves)) return false;
    }
    *out = result;
    return true;
}
#endif
