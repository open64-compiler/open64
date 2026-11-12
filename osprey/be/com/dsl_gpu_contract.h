/* Copyright (C) 2026 Open64 Project */
/* H1d owned host/serving requirements, separate from compiler search and live
 * device authority. Trusted runtime owners supply observations and retain the
 * reservation through execution/completion. No hardware probe, file access,
 * IR mutation, selection or CUDA dependency. Design:
 * doc/AI-COMPILER-H1D-DEVICE-HOST-CONTRACTS-v1.0.md. */
#ifndef dsl_gpu_contract_INCLUDED
#define dsl_gpu_contract_INCLUDED
#include "dsl_gpu_plan.h"

struct DSL_GPU_RUNTIME_OBSERVATION {
    std::string device,architecture,partition,toolchain;
    std::string resource_key,execution_key,budget_key;
    uint64_t context_generation,budget_generation,reservation_id,reserved_bytes;
    bool known,reservation_live;
    /* Unknown observations cannot authorize a device-specific plan. */
    inline DSL_GPU_RUNTIME_OBSERVATION() : context_generation(0),budget_generation(0),
        reservation_id(0),reserved_bytes(0),known(false),reservation_live(false) {}
    /* Exclude calibration from the lease: a rate update is not code unsafety.
     * Context/budget/reservation generations must remain stable per invocation. */
    std::string Lease_key() const
    {
        std::ostringstream key;key<<context_generation<<';'<<budget_generation<<';'<<reservation_id;
        return key.str();
    }
};

struct DSL_GPU_HOST_REQUIREMENTS {
    std::string device,architecture,partition,toolchain;
    std::string selection_key,resource_key,execution_key,budget_key;
    bool imported;
    /* Empty requirements preserve the existing architecture-only runtime path. */
    inline DSL_GPU_HOST_REQUIREMENTS() : imported(false) {}
    /* Copy driver evidence once; no borrowed context/path survives the handoff. */
    static DSL_GPU_HOST_REQUIREMENTS Capture(const DSL_GPU_DEVICE_CONTEXT *context)
    {
        DSL_GPU_HOST_REQUIREMENTS result;if(!context) return result;
        result.device=context->Device();result.architecture=context->Architecture();
        result.partition=context->Partition_id();result.toolchain=context->Toolchain();
        result.selection_key=DSL_GPU_PLAN_SNAPSHOT::Capture(context).Cache_key();
        result.resource_key=context->Resource_sha256();result.execution_key=context->Execution_sha256();
        result.budget_key=context->Budget_sha256();result.imported=context->Imported();return result;
    }
    /* Check conservative exact inventory/budget bindings and an owner-held live
     * reservation for the aggregate invocation backing. Profiles are evidence,
     * not hardware attestation; the observing owner authenticates those joins.
     * Changed calibration permits execution but forbids performance reuse.
     * Architecture-only paths neither use nor qualify device-specific evidence. */
    bool Check(const DSL_GPU_RUNTIME_OBSERVATION &actual,uint64_t live_bytes,
        bool &performance_reusable,std::string &reason) const
    {
        performance_reusable=false;reason.clear();
        if(selection_key.empty()) {
            if(imported || !device.empty() || !architecture.empty() || !partition.empty() ||
                !toolchain.empty() || !resource_key.empty() || !execution_key.empty() || !budget_key.empty()) {
                reason="gpu_malformed_requirements";return false;
            }
            return true;
        }
        if(!actual.known || !actual.context_generation) {reason="gpu_runtime_unknown";return false;}
        if(actual.device!=device || actual.architecture!=architecture || architecture!="sm_90") {
            reason="gpu_runtime_device_or_isa";return false;
        }
        if(!imported) return true;
        if(actual.partition!=partition || actual.toolchain!=toolchain || actual.resource_key!=resource_key) {
            reason="gpu_runtime_resource_binding";return false;
        }
        if(actual.budget_key!=budget_key || !actual.budget_generation || !actual.reservation_id ||
            !actual.reservation_live || actual.reserved_bytes<live_bytes) {
            reason="gpu_runtime_budget_reservation";return false;
        }
        performance_reusable=execution_key!="none" && !execution_key.empty() && actual.execution_key==execution_key;
        return true;
    }
};

/* Additive owned diagnostic sidecar. Existing Q3 fact/bundle layouts stay
 * unchanged; reading this result does not acquire a lease or hardware authority. */
struct DSL_GPU_SERVING_RESULT {
    std::string selection_key,lease_key;
    bool performance_reusable;
    /* No observation conveys no device-specific evidence reuse. */
    inline DSL_GPU_SERVING_RESULT() : performance_reusable(false) {}
};

/* Optional serving owner extension. observe is called under the registry lock;
 * it must not throw/reenter. It authenticates actual device/partition evidence,
 * context generation and a reservation held through completion. The callback
 * context outlives the registry. A failed check leaves the existing fallback to
 * the caller; it never silently runs a device plan on host pointers. */
struct DSL_GPU_SERVING_BINDING {
    DSL_GPU_HOST_REQUIREMENTS requirements;
    void *context;
    bool (*observe)(void *,DSL_GPU_RUNTIME_OBSERVATION &,std::string &);
    /* Disabled bindings preserve all existing runtime contracts. */
    inline DSL_GPU_SERVING_BINDING() : context(NULL),observe(NULL) {}
};
#endif
