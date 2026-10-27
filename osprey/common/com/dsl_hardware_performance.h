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

/* Validate structural denominators only; values are facts supplied by a
 * versioned description, not calibrated or chosen by this common utility. */
static inline bool DSL_hardware_performance_valid(const DSL_HARDWARE_PERFORMANCE_RECORD &p)
{
    return p.sms && p.warp_lanes && p.memory_bytes_per_cycle &&
        p.scalar_ops_per_cycle && p.local_issues_per_cycle &&
        p.copy_elements_per_cycle && p.latency_hiding_warps;
}
#endif
