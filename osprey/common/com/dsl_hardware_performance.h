/* Copyright (C) 2026 Open64 Project */
/* Policy-free runtime Hardware Description IR performance payload. No WHIRL
 * image layout or cost selection. Design:
 * doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md. */
#ifndef dsl_hardware_performance_INCLUDED
#define dsl_hardware_performance_INCLUDED

#include <stdint.h>

typedef struct {
    uint64_t sms;
    uint64_t warp_lanes;
    uint64_t memory_bytes_per_cycle;
    uint64_t scalar_ops_per_cycle;
    uint64_t local_issues_per_cycle;
    uint64_t copy_elements_per_cycle;
    uint64_t latency_hiding_warps;
    uint64_t demand_latency;
    uint64_t staged_latency;
    uint64_t tma_latency;
    uint64_t store_latency;
    uint64_t publication_cycles;
    uint64_t wait_cycles;
    uint64_t release_cycles;
    uint64_t operand_issue_cycles;
} DSL_HARDWARE_PERFORMANCE_RECORD;

/* Runtime-only capacity envelope, not a persisted/native allocation promise.
 * Mixed fact/estimate provenance is defined by
 * doc/AI-COMPILER-OPTIMIZATION-OCCUPANCY.md. */
typedef struct {
    uint64_t max_blocks, max_warps, max_threads, max_threads_per_cta;
    uint64_t registers_per_sm, max_registers_per_thread;
    uint64_t register_quantum, register_warp_quantum;
    uint64_t shared_capacity, shared_budget, shared_quantum, shared_reserved;
} DSL_HARDWARE_RESOURCE_RECORD;

/* Structural checks only; callers own occupancy estimation and policy. */
static inline bool DSL_hardware_resources_valid(const DSL_HARDWARE_RESOURCE_RECORD &r)
{
    return r.max_blocks && r.max_warps && r.max_threads && r.max_threads_per_cta &&
        r.registers_per_sm && r.max_registers_per_thread && r.register_quantum &&
        r.register_warp_quantum && r.shared_capacity && r.shared_budget &&
        r.shared_budget <= r.shared_capacity && r.shared_quantum;
}

/* Validate structural denominators only; values are facts supplied by a
 * versioned description, not calibrated or chosen by this common utility. */
static inline bool DSL_hardware_performance_valid(const DSL_HARDWARE_PERFORMANCE_RECORD &p)
{
    return p.sms && p.warp_lanes && p.memory_bytes_per_cycle &&
        p.scalar_ops_per_cycle && p.local_issues_per_cycle &&
        p.copy_elements_per_cycle && p.latency_hiding_warps;
}
#endif
