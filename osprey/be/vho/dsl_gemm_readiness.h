/* Copyright (C) 2026 Open64 Project */
/* VHO-only analytical schedule arithmetic, not an instruction simulator or
 * persisted WHIRL contract. All hardware rates/times are caller-supplied from
 * doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md. */
#ifndef dsl_gemm_readiness_INCLUDED
#define dsl_gemm_readiness_INCLUDED

#include <algorithm>
#include <stdint.h>
#include <string.h>
#include "dsl_hardware_performance.h"

struct DSL_GEMM_READINESS_INPUT {
    uint64_t ctas, steps, consumer, input_bytes, output_bytes, copy;
    uint64_t warps, latency;
    DSL_HARDWARE_PERFORMANCE_RECORD hardware;
    unsigned kind; // 0 D, 1 S1, 2 T2
};

struct DSL_GEMM_READINESS_RESULT {
    uint64_t compute, memory, sync, issue, total;
    uint64_t delivery, window, wait, waves;
};

/* Checked arithmetic reserves UINT64_MAX as the absent max-plus edge. */
static inline bool DSL_readiness_add(uint64_t a, uint64_t b, uint64_t *out)
{
    if (a >= UINT64_MAX - b) return false;
    *out = a + b;
    return true;
}

/* Reject overflow rather than permit a wrapped cost to win selection. */
static inline bool DSL_readiness_mul(uint64_t a, uint64_t b, uint64_t *out)
{
    if (b && a > (UINT64_MAX - 1) / b) return false;
    *out = a * b;
    return true;
}

/* Positive-divisor ceiling division does not overflow its numerator. */
static inline uint64_t DSL_readiness_ceil(uint64_t a, uint64_t b)
{
    return a / b + (a % b != 0);
}

struct DSL_GEMM_READINESS_MATRIX { uint64_t edge[4][4]; };

/* Compose constant-size max-plus schedule transfers; absent edges stay absent.
 * Caller owns both inputs; output is assigned only after successful checks. */
static inline bool DSL_readiness_compose(const DSL_GEMM_READINESS_MATRIX &a,
    const DSL_GEMM_READINESS_MATRIX &b, DSL_GEMM_READINESS_MATRIX *out)
{
    DSL_GEMM_READINESS_MATRIX result;
    memset(&result, 0xff, sizeof(result));
    for (unsigned i = 0; i < 4; ++i)
        for (unsigned j = 0; j < 4; ++j)
            for (unsigned k = 0; k < 4; ++k) {
                if (a.edge[i][k] == UINT64_MAX || b.edge[k][j] == UINT64_MAX) continue;
                uint64_t value;
                if (!DSL_readiness_add(a.edge[i][k], b.edge[k][j], &value)) return false;
                if (result.edge[i][j] == UINT64_MAX || value > result.edge[i][j])
                    result.edge[i][j] = value;
            }
    *out = result;
    return true;
}

/* Apply repeated slot-release/refill boundaries in O(log steps), not O(K).
 * State is [consumer clock, next ready, following ready, transfer service end]. */
static inline bool DSL_readiness_repeat(DSL_GEMM_READINESS_MATRIX matrix,
    uint64_t count, uint64_t state[4])
{
    while (count) {
        if (count & 1) {
            uint64_t next[4] = {0, 0, 0, 0};
            for (unsigned i = 0; i < 4; ++i)
                for (unsigned j = 0; j < 4; ++j) {
                    if (matrix.edge[i][j] == UINT64_MAX) continue;
                    uint64_t value;
                    if (!DSL_readiness_add(matrix.edge[i][j], state[j], &value)) return false;
                    next[i] = std::max(next[i], value);
                }
            memcpy(state, next, sizeof(next));
        }
        count >>= 1;
        if (count && !DSL_readiness_compose(matrix, matrix, &matrix)) return false;
    }
    return true;
}

/* Evaluate one wave using the real T2 preload/refill/drain dependency pattern.
 * Service is serialized aggregate wave traffic; A/B issue costs come from
 * the hardware description. D/S1 use an explicit inter-warp latency
 * allowance; no arbitrary hidden-traffic discount is applied. */
static inline bool DSL_readiness_wave(const DSL_GEMM_READINESS_INPUT &in,
    uint64_t active, DSL_GEMM_READINESS_RESULT *out)
{
    DSL_GEMM_READINESS_RESULT r = {};
    const DSL_HARDWARE_PERFORMANCE_RECORD &hw = in.hardware;
    uint64_t bytes, service, body, clock, output;
    uint64_t barriers, pair_issue;
    if (!DSL_readiness_add(hw.wait_cycles, hw.release_cycles, &barriers) ||
        !DSL_readiness_mul(hw.operand_issue_cycles, 2, &pair_issue)) return false;
    if (!DSL_readiness_mul(in.input_bytes, active, &bytes)) return false;
    service = DSL_readiness_ceil(bytes, hw.memory_bytes_per_cycle);
    if (!DSL_readiness_add(service, in.latency, &r.delivery) ||
        !DSL_readiness_mul(in.consumer, in.steps, &r.compute) ||
        !DSL_readiness_mul(in.output_bytes, active, &bytes) ||
        !DSL_readiness_add(DSL_readiness_ceil(bytes, hw.memory_bytes_per_cycle), hw.store_latency, &output)) return false;
    if (in.kind != 2) {
        uint64_t exposed = DSL_readiness_ceil(in.latency, std::min(in.warps, hw.latency_hiding_warps));
        if (!DSL_readiness_add(service, exposed, &body) ||
            !DSL_readiness_add(body, in.copy, &body) ||
            !DSL_readiness_mul(body, in.steps, &r.memory) ||
            !DSL_readiness_mul(exposed, in.steps, &r.wait) ||
            !DSL_readiness_mul(in.kind == 1 ? barriers : 0, in.steps, &r.sync)) return false;
    } else {
        // The issuing warp is also a consumer: refill blocks its next consume.
        uint64_t count = std::min(in.steps, UINT64_C(2));
        uint64_t state[4] = {0, 0, 0, 0};
        if (!DSL_readiness_mul(count, pair_issue, &state[0]) ||
            !DSL_readiness_add(state[0], hw.publication_cycles, &state[0])) return false;
        for (uint64_t j = 0; j < count; ++j) {
            uint64_t issued;
            if (!DSL_readiness_mul(j + 1, pair_issue, &issued) ||
                !DSL_readiness_add(std::max(state[3], issued), service, &state[3]) ||
                !DSL_readiness_add(state[3], in.latency, &state[j + 1])) return false;
        }
        if (!DSL_readiness_add(in.consumer, barriers, &body) ||
            !DSL_readiness_add(body, pair_issue, &body)) return false;
        if (in.steps > 2) {
            DSL_GEMM_READINESS_MATRIX matrix;
            memset(&matrix, 0xff, sizeof(matrix));
            matrix.edge[0][0] = matrix.edge[0][1] = body;
            matrix.edge[1][2] = 0;
            uint64_t refill;
            if (!DSL_readiness_add(body, service, &refill) ||
                !DSL_readiness_add(refill, in.latency, &matrix.edge[2][0])) return false;
            matrix.edge[2][1] = matrix.edge[2][0];
            matrix.edge[2][3] = r.delivery;
            matrix.edge[3][0] = matrix.edge[3][1] = refill;
            matrix.edge[3][3] = service;
            if (!DSL_readiness_repeat(matrix, in.steps - 2, state)) return false;
        }
        for (uint64_t j = 0; j < count; ++j) {
            if (!DSL_readiness_add(std::max(state[0], state[j + 1]), in.consumer, &state[0]) ||
                !DSL_readiness_add(state[0], barriers, &state[0])) return false;
        }
        clock = state[0];
        if (!DSL_readiness_mul(in.steps, barriers, &r.sync) ||
            !DSL_readiness_add(r.sync, hw.publication_cycles, &r.sync) ||
            !DSL_readiness_mul(in.steps, pair_issue, &r.issue) ||
            !DSL_readiness_add(r.compute, r.sync, &body) ||
            !DSL_readiness_add(body, r.issue, &body) || clock < body) return false;
        r.memory = r.wait = clock - body;
        if (in.steps > 1 && !DSL_readiness_add(in.consumer, barriers, &r.window)) return false;
    }
    if (!DSL_readiness_add(r.memory, output, &r.memory) ||
        !DSL_readiness_add(r.compute, r.memory, &r.total) ||
        !DSL_readiness_add(r.total, r.sync, &r.total) ||
        !DSL_readiness_add(r.total, r.issue, &r.total)) return false;
    *out = r;
    return true;
}

/* Cost sequential grid waves under the supplied hardware description,
 * one CTA/SM. Invalid/overflowing input leaves the caller's result untouched. */
static inline bool DSL_readiness_evaluate(const DSL_GEMM_READINESS_INPUT &in,
    DSL_GEMM_READINESS_RESULT *out)
{
    if (!out || !in.ctas || !in.steps || !in.consumer || !in.input_bytes ||
        !in.output_bytes || !in.warps || in.kind > 2 ||
        !DSL_hardware_performance_valid(in.hardware)) return false;
    DSL_GEMM_READINESS_RESULT result = {};
    for (unsigned tail = 0; tail < 2; ++tail) {
        uint64_t active = tail ? in.ctas % in.hardware.sms : in.hardware.sms;
        uint64_t waves = tail ? (active != 0) : in.ctas / in.hardware.sms;
        if (!waves) continue;
        DSL_GEMM_READINESS_RESULT wave;
        if (!DSL_readiness_wave(in, active, &wave)) return false;
        uint64_t *dest[] = {&result.compute, &result.memory, &result.sync,
                           &result.issue, &result.total, &result.wait};
        const uint64_t source[] = {wave.compute, wave.memory, wave.sync,
                                   wave.issue, wave.total, wave.wait};
        for (unsigned i = 0; i < 6; ++i) {
            uint64_t term;
            if (!DSL_readiness_mul(source[i], waves, &term) ||
                !DSL_readiness_add(*dest[i], term, dest[i])) return false;
        }
        result.delivery = std::max(result.delivery, wave.delivery);
        result.window = std::max(result.window, wave.window);
        result.waves += waves;
    }
    *out = result;
    return true;
}
#endif
