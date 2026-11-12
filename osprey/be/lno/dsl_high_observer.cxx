/* Copyright (C) 2026 Open64 Project */
/* Detached validation and estimation-only observer owned by LNO. No native
 * compiler context headers or allocation services are linked from this object.
 * Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. */
#include "dsl_high_observer.h"
#include <sstream>

/* Check full source/fact/context joins, complete ABI storage census, and typed
 * reusable validators. Missing facts are refused without invoking search. */
bool LNO_DSL_validate_search_input(const DSL_HIGH_SNAPSHOT &snapshot, const DSL_HIGH_BINDINGS &bindings,
    const DSL_HIGH_SEARCH_INPUT &input, std::string &reason) {
    if (input.unit != bindings.unit || input.numeric != bindings.numeric || input.host_target != bindings.target ||
        input.source_key != snapshot.Source_key() || input.facts_key != snapshot.Facts_key())
        return reason = "source_or_context_mismatch", false;
    if (!input.allow_reassociate) return reason = "manifest_numerical_permission_required", false;
    const VHO_TENSOR_PROGRAM &program = input.algorithm;
    if (program.source_file != input.unit || program.opt_level != 3 || program.threads != 128 ||
        program.fuse || program.cooperative || program.split_width || program.fuse_attention ||
        program.register_latent || program.auto_chunks || program.search_selected ||
        !VHO_tensor_validate_program(program, NULL)) return reason = "source_manifest_invalid", false;
    std::vector<unsigned> buffers;
    for (unsigned i = 0; i < program.values.size(); ++i)
        if (program.values[i].storage == i) buffers.push_back(i);
    if (buffers != input.buffers) return reason = "storage_manifest_mismatch", false;
    VHO_MLA_SEARCH_OPTIONS options;
    if (!VHO_tensor_gpu_profile_valid(input.gpu, NULL)) return reason = "gpu_resources_invalid", false;
    if (input.model.architecture != input.gpu.architecture || !VHO_mla_search_inputs_valid(input.model, options, NULL))
        return reason = "model_binding_invalid", false;
    if (input.production_controls && !VHO_mla_search_control_valid(input.controls,NULL))
        return reason = "search_controls_invalid", false;
    reason = "validated"; return true;
}

/* Reuse the existing estimator, candidate ordering, cache and scoped conclusion.
 * All fusion flags below describe detached implementation hypotheses only. */
bool LNO_DSL_observe_search(const DSL_HIGH_SEARCH_INPUT &input, DSL_HIGH_SEARCH_REPORT &output, std::string &reason) {
    output = DSL_HIGH_SEARCH_REPORT();
    VHO_TENSOR_PROGRAM program = input.algorithm;
    program.fuse = program.cooperative = program.fuse_attention = program.register_latent = true;
    /* The shared closed-DAG validator requires a capacity-safe seed for long
     * contexts. Evaluate overwrites this detached width for every proposal;
     * it is neither a selected width nor any source/native transformation. */
    program.split_width = 128;
    VHO_MLA_SEARCH_OPTIONS options;
    DSL_HIGH_SEARCH_REPORT result;
    if (input.production_controls) {
        if (!VHO_mla_explore_controlled(program,input.gpu,input.model,input.controls,&result.production,NULL))
            return reason = "controlled_search_failed", false;
        result.controlled = true; result.exploration = result.production.exploration;
        result.control_key = VHO_mla_search_control_key(input.controls);
    } else if (!VHO_mla_explore(program, input.gpu, input.model, options, &result.exploration, NULL))
        return reason = "no_estimated_candidate", false;
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
    result.resource_key = resource_key.str(); result.model_key = model_key.str();
    output = result; reason = "observed"; return true;
}
