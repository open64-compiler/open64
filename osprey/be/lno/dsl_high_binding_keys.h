/* Copyright (C) 2026 Open64 Project */
/* Read-only owned manifest identities shared by LNO and its selected bridge.
 * No native context, candidate evaluation or mutation. Contract:
 * doc/AI-COMPILER-SEARCH-M6-PRODUCTION-ROUTE-v1.0.md. */
#ifndef dsl_high_binding_keys_INCLUDED
#define dsl_high_binding_keys_INCLUDED
#include "dsl_high_observer.h"
#include <sstream>

/* Derive the same exact typed recipe/resource/model identities for the owning
 * observer and the later production consumer; H1c keys separate capacity
 * legality from complete cost/evidence. Never allocate WHIRL state. */
inline void LNO_DSL_fill_search_keys(const DSL_HIGH_SEARCH_INPUT &input, DSL_HIGH_SEARCH_REPORT &result) {
    result.manifest_key = VHO_tensor_recipe_key(input.algorithm);
    const VHO_TENSOR_GPU_PROFILE &gpu = input.gpu;
    const DSL_HARDWARE_RESOURCE_RECORD &resources = gpu.resources;
    std::ostringstream resource_key, model_key;
    resource_key << gpu.id.size() << ':' << gpu.id << '/' << gpu.architecture << '/' << gpu.sms << '/'
        << gpu.registers_per_thread << '/' << gpu.target_ctas_per_sm << '/' << gpu.scratch_budget;
    const uint64_t values[] = {resources.max_blocks,resources.max_warps,resources.max_threads,resources.max_threads_per_cta,
        resources.registers_per_sm,resources.max_registers_per_thread,resources.register_quantum,resources.register_warp_quantum,
        resources.shared_capacity,resources.shared_budget,resources.shared_quantum,resources.shared_reserved};
    for (unsigned i = 0; i < 12; ++i) resource_key << '/' << values[i];
    model_key << "mla-additive-waves-v1/" << input.model.id.size() << ':' << input.model.id << '/'
        << input.model.architecture << '/' << input.model.provenance << "/reference_cycles";
    for (unsigned i = 0; i < VHO_MLA_COST_FIELDS; ++i) model_key << '/' << input.model.values[i];
    if(input.movement_enabled) {
        model_key<<"/movement;"<<VHO_mla_movement_profile_key(input.movement_profile)
            <<';'<<VHO_mla_movement_catalog()<<';'<<VHO_mla_movement_capabilities();
    }
    if (!gpu.device.Cache_key().empty()) {
        resource_key << ";device;" << gpu.device.Legality_key() << ";high-private-reuse=" << gpu.high_private_reuse;
        model_key << ";device;" << gpu.device.Cache_key();
    }
    result.resource_key = resource_key.str(); result.model_key = model_key.str();
}

/* Seal the original complete owned input after selected analysis rebuild has
 * replaced its source/fact keys. The later bridge compares, never re-searches. */
inline std::string LNO_DSL_production_input_key(const DSL_HIGH_SEARCH_INPUT &input) {
    DSL_HIGH_SEARCH_REPORT report;LNO_DSL_fill_search_keys(input,report);
    const std::string fields[]={input.unit,input.function,input.source_key,input.facts_key,input.numeric,input.host_target,
        report.manifest_key,report.resource_key,report.model_key,
        input.production_controls?VHO_mla_search_control_key(input.controls):"legacy-controls"};
    std::ostringstream key;key<<(input.movement_enabled?"lno-production-input-v2;":"lno-production-input-v1;")<<input.allow_reassociate<<';'<<input.production_controls<<';';
    for(unsigned i=0;i<sizeof(fields)/sizeof(fields[0]);++i) key<<fields[i].size()<<':'<<fields[i]<<';';
    key<<input.buffers.size()<<';';for(unsigned i=0;i<input.buffers.size();++i) key<<input.buffers[i]<<';';
    return key.str();
}
#endif
