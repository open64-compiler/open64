/* Copyright (C) 2026 Open64 Project */
/* Backend-owned H1c planner evidence. Copy driver facts before exploration;
 * detached snapshots have no native/file/context pointers. Physical service
 * bounds remain separate from legacy reference_cycles ranking. No new native
 * format or runtime authority. Design:
 * doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md. */
#ifndef dsl_gpu_plan_INCLUDED
#define dsl_gpu_plan_INCLUDED
#include "dsl_device_profile.h"
#include <cstring>
#include <stdio.h>
#include <limits>
#include <sstream>

/* POD for existing memset-initialized transient phase contracts, never mapped
 * WHIRL/runtime ABI. bound means imported resources; empty keys mean no device.
 * Explicit device-only requests retain an identity stamp without resource use. */
struct DSL_GPU_PLAN_STAMP {
    unsigned bound;
    char resource[65], execution[65], budget[65], context[65];
};

/* Capture independent dependency boundaries without borrowing context storage.
 * Device-only H1a retains identity-only planning, with its identity sealed for
 * freshness even when all resource facts are unknown. Only imports constrain
 * resources. Failure/missing imports cannot synthesize numeric defaults. */
inline DSL_GPU_PLAN_STAMP DSL_gpu_plan_stamp(const DSL_GPU_DEVICE_CONTEXT *context)
{
    DSL_GPU_PLAN_STAMP stamp;
    std::memset(&stamp, 0, sizeof(stamp));
    if (context) {
        stamp.bound = context->Imported() ? 1 : 0;
        std::strcpy(stamp.resource, context->Resource_sha256().c_str());
        std::strcpy(stamp.execution, context->Execution_sha256().c_str());
        std::strcpy(stamp.budget, context->Budget_sha256().c_str());
        std::strcpy(stamp.context, context->Context_sha256().c_str());
    }
    return stamp;
}

/* Compare exact fields, never struct padding; calibration invalidates complete
 * acceptance even when unchanged resource/budget legality may be reusable. */
inline bool DSL_gpu_plan_stamp_equal(const DSL_GPU_PLAN_STAMP &a,
    const DSL_GPU_PLAN_STAMP &b)
{
    return a.bound == b.bound &&
        (std::memcmp(a.resource, b.resource, 65) == 0 &&
         std::memcmp(a.execution, b.execution, 65) == 0 &&
         std::memcmp(a.budget, b.budget, 65) == 0 &&
         std::memcmp(a.context, b.context, 65) == 0);
}

/* Recheck current driver evidence before source mutation or guard export. No
 * recapture from a stale copied request can authorize a changed context. */
inline bool DSL_gpu_plan_stamp_current(const DSL_GPU_PLAN_STAMP &stamp,
    const DSL_GPU_DEVICE_CONTEXT *context)
{
    return DSL_gpu_plan_stamp_equal(stamp, DSL_gpu_plan_stamp(context));
}

class DSL_GPU_PLAN_SNAPSHOT {
    DSL_GPU_PLAN_STAMP _stamp;
    bool _budget_proved;
    std::string _device, _architecture, _partition, _model;
    DSL_GPU_RESOURCE_FACT _resources[DSL_GPU_RESOURCE_COUNT];
    DSL_GPU_EXECUTION_FACT _execution[DSL_GPU_EXECUTION_COUNT];
    DSL_GPU_RESOURCE_FACT _budget[DSL_GPU_BUDGET_COUNT];
public:
    /* Empty snapshots are architecture-only; no imported resource permission. */
    inline DSL_GPU_PLAN_SNAPSHOT() : _budget_proved(false) { std::memset(&_stamp, 0, sizeof(_stamp)); }
    /* Copy all quantities once under the driver lifetime. Published snapshots
     * are passed const through exploration; no global/native API is used. */
    static DSL_GPU_PLAN_SNAPSHOT Capture(const DSL_GPU_DEVICE_CONTEXT *context)
    {
        DSL_GPU_PLAN_SNAPSHOT result;
        result._stamp = DSL_gpu_plan_stamp(context);
        if (!context) return result;
        result._budget_proved = context->Budget_bounds_proved();
        result._device = context->Device(); result._architecture = context->Architecture();
        result._partition = context->Partition_id(); result._model = context->Model();
        for (unsigned i = 0; i < DSL_GPU_RESOURCE_COUNT; ++i)
            result._resources[i] = *context->Resource(DSL_GPU_RESOURCE_FIELD(i));
        for (unsigned i = 0; i < DSL_GPU_EXECUTION_COUNT; ++i)
            result._execution[i] = *context->Execution(DSL_GPU_EXECUTION_FIELD(i));
        for (unsigned i = 0; i < DSL_GPU_BUDGET_COUNT; ++i)
            result._budget[i] = *context->Budget(DSL_GPU_BUDGET_FIELD(i));
        return result;
    }
    /* Return imported admission state without fabricating a board identity. */
    inline bool Bound() const { return _stamp.bound != 0; }
    /* Borrow owned fixed dependency keys for a POD handoff. */
    inline const DSL_GPU_PLAN_STAMP &Stamp() const { return _stamp; }
    /* Revalidate complete selection evidence independently of candidate data. */
    inline bool Is_current(const DSL_GPU_DEVICE_CONTEXT *context) const
        { return DSL_gpu_plan_stamp_current(_stamp, context); }
    /* Preserve exact old cache spelling when no device is selected. Explicit
     * identity-only contexts also bind freshness; imports additionally bind
     * every file/resource/cost/budget change conservatively. */
    std::string Cache_key() const
    {
        if (!_stamp.context[0]) return "";
        if (!Bound()) return std::string("gpu-device-v1;") + _stamp.context;
        return std::string("gpu-plan-v1;") + _stamp.resource + ';' +
            _stamp.execution + ';' + _stamp.budget + ';' + _stamp.context;
    }
    /* Legality excludes execution evidence; resource/budget identities remain
     * explicit so a cost-only update need not discard a valid capacity proof. */
    std::string Legality_key() const
    {
        if (!_stamp.context[0]) return "";
        return std::string("gpu-legality-v1;") + _stamp.resource + ';' + _stamp.budget;
    }
    /* Borrow exact admitted ISA, with no architecture-only aliasing. */
    inline const std::string &Architecture() const { return _architecture; }
    /* Borrow the physical board key for truthful phase reporting. */
    inline const std::string &Device() const { return _device; }
    /* Borrow visible partition identity separately from board capacity. */
    inline const std::string &Partition() const { return _partition; }
    /* Borrow declared physical model evidence; not an old cycle model ID. */
    inline const std::string &Model() const { return _model; }
    /* Copy conservative capacity lower bounds. Warp topology and allocation
     * quanta require exact evidence: rounding by an uncertain lower quantum
     * is not a conservative reservation. Failure preserves output. */
    bool Resource_limit(int field, uint64_t *output) const
    {
        if (!output || field < 0 || field >= DSL_GPU_RESOURCE_COUNT || !_resources[field].known) return false;
        if ((field == DSL_GPU_REGISTER_QUANTUM || field == DSL_GPU_SHARED_QUANTUM ||
             field == DSL_GPU_WARP_LANES) && _resources[field].lower != _resources[field].upper) return false;
        *output = _resources[field].lower; return true;
    }
    /* A device candidate requires a proved usable budget, never all nominal HBM.
     * Program owners supply aggregate distinct allocation footprints. */
    inline bool Fits_storage(uint64_t live_bytes) const
    {
        return !Bound() || (_budget_proved && _budget[DSL_GPU_BUDGET_USABLE].known &&
            _resources[DSL_GPU_VISIBLE_HBM_BYTES].known &&
            live_bytes <= _budget[DSL_GPU_BUDGET_USABLE].lower &&
            live_bytes <= _resources[DSL_GPU_VISIBLE_HBM_BYTES].lower);
    }
    /* Reuse the existing backend scenario's per-warp RF rounding and groups
     * of four allocated warps. Imported quantum must be exact. This is the
     * declared allocator model, not proof of generated register allocation. */
    bool Register_reservation(uint64_t threads, uint64_t registers_per_thread,
        uint64_t *output) const
    {
        uint64_t lanes, quantum;
        if (!output || !threads || threads > 1024 || !registers_per_thread || registers_per_thread > 255 ||
            !Resource_limit(DSL_GPU_WARP_LANES,&lanes) || lanes != 32 ||
            !Resource_limit(DSL_GPU_REGISTER_QUANTUM,&quantum) || !quantum) return false;
        const uint64_t warps = (threads + lanes - 1) / lanes;
        const uint64_t allocated_warps = ((warps + 3) / 4) * 4;
        const uint64_t raw = registers_per_thread * lanes;
        const uint64_t padding = raw % quantum ? quantum - raw % quantum : 0;
        if (padding > std::numeric_limits<uint64_t>::max() - raw) return false;
        const uint64_t rounded = raw + padding;
        if (rounded > std::numeric_limits<uint64_t>::max() / allocated_warps) return false;
        *output = rounded * allocated_warps; return true;
    }
    /* Check one fixed CTA's declared upper reservations. This does not prove
     * actual register allocation/occupancy or opt-in SMEM runtime discharge.
     * Zero registers means unproved demand and refuses an imported candidate. */
    bool Fits_CTA(uint64_t threads, uint64_t registers_per_thread,
        uint64_t shared_bytes) const
    {
        if (!Bound()) return true;
        uint64_t lanes, max_threads, sm_threads, registers, demand;
        if (!threads || threads > 1024 || !registers_per_thread ||
            !Resource_limit(DSL_GPU_WARP_LANES, &lanes) || lanes != 32 ||
            !Resource_limit(DSL_GPU_MAX_THREADS_PER_CTA, &max_threads) || threads > max_threads ||
            !Resource_limit(DSL_GPU_MAX_THREADS_PER_SM, &sm_threads) || threads > sm_threads ||
            !Resource_limit(DSL_GPU_REGISTERS_PER_SM, &registers) ||
            !Register_reservation(threads,registers_per_thread,&demand) || demand > registers) return false;
        if (shared_bytes) {
            uint64_t cta_shared, sm_shared, shared_quantum;
            if (!Resource_limit(DSL_GPU_SHARED_BYTES_PER_CTA_DEFAULT, &cta_shared) ||
                !Resource_limit(DSL_GPU_SHARED_BYTES_PER_SM, &sm_shared) ||
                !Resource_limit(DSL_GPU_SHARED_QUANTUM, &shared_quantum) || !shared_quantum ||
                shared_bytes > cta_shared || shared_bytes > sm_shared) return false;
            const uint64_t padding = shared_bytes % shared_quantum ? shared_quantum - shared_bytes % shared_quantum : 0;
            if (padding > cta_shared - shared_bytes || padding > sm_shared - shared_bytes) return false;
        }
        return true;
    }
    /* Device-wide mixed-HBM service envelope in ns. This is one cost component,
     * not complete latency or a winner: no overlap/cache/bubble assumptions.
     * Unknown/nonfinite service leaves outputs unchanged; never convert to
     * reference_cycles. Physical rates cannot replace missing instruction data. */
    bool HBM_service_ns(uint64_t bytes, double *lower, double *upper) const
    {
        const DSL_GPU_EXECUTION_FACT &fact = _execution[DSL_GPU_HBM_MIXED_RATE];
        if (!Bound() || !lower || !upper || !fact.known || fact.lower <= 0 || fact.upper <= 0) return false;
        const double low = (double(bytes) / fact.upper) * 1e9;
        const double high = (double(bytes) / fact.lower) * 1e9;
        if (!(low >= 0) || !(high >= low) || high > std::numeric_limits<double>::max()) return false;
        *lower = low; *upper = high; return true;
    }
    /* Report explicit imported identities and a separate physical service
     * component; legacy cycle ranking remains a labelled scenario, not H20
     * calibration. No source/profile content is printed beyond bound tokens. */
    void Print(FILE *file, const char *family, uint64_t bytes) const
    {
        if (!file || !Bound()) return;
        double lower = 0, upper = 0;
        const bool known = HBM_service_ns(bytes, &lower, &upper);
        const DSL_GPU_FACT_PROVENANCE provenance = _execution[DSL_GPU_HBM_MIXED_RATE].provenance;
        const char *kind = !known ? "unknown" : provenance == DSL_GPU_FACT_MEASURED ? "declared_measured" :
            provenance == DSL_GPU_FACT_ESTIMATED ? "estimated" : "synthetic";
        fprintf(file, "DSL_GPU_PLAN 1 family=%s device=%s partition=%s binding=%s "
            "ranking=legacy-reference-cycles model=%s physical_component=HBM_service_ns physical_provenance=%s physical_known=%u "
            "bytes=%llu lower_ns=%.17g upper_ns=%.17g device_performance_qualified=0\n",
            family, _device.c_str(), _partition.c_str(), Cache_key().c_str(),
            _model.empty() ? "none" : _model.c_str(), kind, unsigned(known), (unsigned long long)bytes, lower, upper);
    }
};
#endif
