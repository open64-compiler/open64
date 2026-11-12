/* Copyright (C) 2026 Open64 Project */
/* Common targ_info/access runtime performance/resource payloads and read-only
 * structural predicates. No WHIRL image layout, measurement, occupancy
 * estimation or cost selection; clients retain provenance/policy duties.
 * Design:
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

/* Optional policy-free scalar/movement timing envelope. Existing hardware
 * records keep their ABI and estimates; missing extension fields stay unknown.
 * Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md.
 * Rates: device-global bytes/cycle, per-SM shared bytes/cycle and scalar lanes/
 * cycle. Latencies are startup after byte service, except scalar latency is
 * instruction-start to result-ready (includes issue). All units reference cycles.
 * Each value has optimistic/nominal/pessimistic numeric bounds; rates reverse
 * direction when estimating durations. Provenance 0=unknown, 1=estimate,
 * 2=measured input, 3=synthetic experiment. No provenance is certification. */
enum DSL_SCALAR_EXECUTION_FIELD {
    DSL_SCALAR_GLOBAL_RATE, DSL_SCALAR_SHARED_RATE, DSL_SCALAR_COMPUTE_RATE,
    DSL_SCALAR_GLOBAL_LATENCY, DSL_SCALAR_CARRIER_LATENCY, DSL_SCALAR_SHARED_LATENCY,
    DSL_SCALAR_ADD_LATENCY, DSL_SCALAR_MUL_LATENCY, DSL_SCALAR_DIV_LATENCY,
    DSL_SCALAR_SQRT_LATENCY, DSL_SCALAR_CONTROL_CYCLES, DSL_SCALAR_BARRIER_CYCLES,
    DSL_SCALAR_GUARD_CYCLES, DSL_SCALAR_LAUNCH_CYCLES, DSL_SCALAR_WORKSPACE_CYCLES,
    DSL_SCALAR_PUBLICATION_CYCLES, DSL_SCALAR_SECTOR_BYTES,
    DSL_SCALAR_DIV_ISSUE_CYCLES, DSL_SCALAR_SQRT_ISSUE_CYCLES, DSL_SCALAR_FIELD_COUNT
};
typedef struct {
    uint32_t version, arch_id, description_revision, model_id;
    uint64_t values[DSL_SCALAR_FIELD_COUNT][3];
    uint32_t provenance[DSL_SCALAR_FIELD_COUNT];
} DSL_SCALAR_EXECUTION_PROFILE;

/* Read borrowed resource fields without mutation. Require the listed positive
 * limits/quanta and a positive shared budget no larger than capacity; return
 * false on rejection. This is not complete allocation, reserved-byte or
 * occupancy validation; consumers own those checks and policy. */
static inline bool DSL_hardware_resources_valid(const DSL_HARDWARE_RESOURCE_RECORD &r)
{
    return r.max_blocks && r.max_warps && r.max_threads && r.max_threads_per_cta &&
        r.registers_per_sm && r.max_registers_per_thread && r.register_quantum &&
        r.register_warp_quantum && r.shared_capacity && r.shared_budget &&
        r.shared_budget <= r.shared_capacity && r.shared_quantum;
}

/* Validate structural denominators only; values are facts supplied by a
 * versioned description, not calibrated or chosen by this target utility.
 * Read borrowed fields without mutation and return false for a zero required
 * divisor; zero latency fields remain permitted and are not inspected here. */
static inline bool DSL_hardware_performance_valid(const DSL_HARDWARE_PERFORMANCE_RECORD &p)
{
    return p.sms && p.warp_lanes && p.memory_bytes_per_cycle &&
        p.scalar_ops_per_cycle && p.local_issues_per_cycle &&
        p.copy_elements_per_cycle && p.latency_hiding_warps;
}
#endif
