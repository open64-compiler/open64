/* Copyright (C) 2026 Open64 Project */
/* Detached validation and estimation-only observer owned by LNO. No native
 * compiler context headers or allocation services are linked from this object.
 * Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. */
#include "dsl_high_observer.h"
#include "dsl_high_binding_keys.h"
#include <sstream>

/* Check full source/fact/context joins, complete ABI storage census, and typed
 * reusable validators. Missing facts are refused without invoking search. */
bool LNO_DSL_validate_search_input(const DSL_HIGH_SNAPSHOT &snapshot, const DSL_HIGH_BINDINGS &bindings,
    const DSL_HIGH_SEARCH_INPUT &input, std::string &reason) {
    if (input.unit != bindings.unit || input.numeric != bindings.numeric || input.host_target != bindings.target ||
        input.source_key != snapshot.Source_key() || input.facts_key != snapshot.Facts_key())
        return reason = "source_or_context_mismatch", false;
    if (input.gpu.device.Cache_key() != bindings.device.Cache_key())
        return reason = "device_snapshot_mismatch", false;
    if (input.gpu.device.Bound() && !input.gpu.high_private_reuse)
        return reason = "source_backing_accounting_required", false;
    if (input.algorithm.head_group!=1) return reason="unscheduled_source_head_group",false;
    if (input.hierarchy_enabled && (!input.movement_enabled || (!input.gpu.device.Bound() || (input.gpu.device.Device()!="h20_96gb" && input.gpu.device.Device()!="h20_141gb"))))
        return reason="h5_requires_H20_H2_H3",false;
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
    if(input.movement_enabled && (!input.production_controls ||
        !VHO_mla_movement_profile_valid(input.movement_profile,NULL) || input.movement_profile.architecture!=input.gpu.architecture))
        return reason="movement_profile_invalid",false;
    if (input.production_controls && !(input.movement_enabled?VHO_mla_movement_control_valid(input.controls,NULL):VHO_mla_search_control_valid(input.controls,NULL)))
        return reason = "search_controls_invalid", false;
    reason = "validated"; return true;
}

/* Reuse the existing estimator, candidate ordering, cache and scoped conclusion.
 * All fusion flags below describe detached implementation hypotheses only. */
bool LNO_DSL_observe_search(const DSL_HIGH_SEARCH_INPUT &input, DSL_HIGH_SEARCH_REPORT &output, std::string &reason,const DSL_HIGH_REUSE_FACTS *reuse,const DSL_MOVEMENT_FACTS *h3_facts,DSL_H3_SEARCH *h3_search) {
    output = DSL_HIGH_SEARCH_REPORT();
    VHO_TENSOR_PROGRAM program = input.algorithm;
    program.fuse = program.cooperative = program.fuse_attention = program.register_latent = true;
    /* The shared closed-DAG validator requires a capacity-safe seed for long
     * contexts. Evaluate overwrites this detached width for every proposal;
     * it is neither a selected width nor any source/native transformation. */
    program.split_width = 128;
    VHO_MLA_SEARCH_OPTIONS options;
    DSL_HIGH_SEARCH_REPORT result;
    if(input.hierarchy_enabled && (!h3_search || !h3_facts || !h3_facts->Available() || !input.movement_enabled))
        return reason="h5_requires_H20_H2_H3",false;
    if(input.movement_enabled) {
        if(!reuse || !reuse->Available()) return reason="movement_reuse_unproved",false;
        VHO_MLA_MOVEMENT_CONTEXT context;context.hierarchy=input.hierarchy_enabled;context.profile=input.movement_profile;context.proof_key=reuse->Binding_key();
        const std::vector<DSL_HIGH_REUSE_CACHE> &caches=reuse->Summary().caches;
        if(caches.size()!=2) return reason="movement_cache_census_invalid",false;
        for(size_t i=0;i<caches.size();++i) {
            if(caches[i].role==DSL_TENSOR_TRAIT_LATENT_KV_CACHE) context.latent_payload=caches[i].payload_bytes;
            if(caches[i].role==DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE) context.rotary_payload=caches[i].payload_bytes;
        }
        if(!(h3_search?(h3_facts && VHO_mla_explore_h3(program,input.gpu,input.model,input.controls,&context,*h3_facts,*h3_search,&result.production,NULL)):
            VHO_mla_explore_movement(program,input.gpu,input.model,input.controls,context,&result.production,NULL)))
            return reason="movement_search_failed",false;
        result.controlled=result.movement=true;result.exploration=result.production.exploration;
        result.control_key=VHO_mla_search_control_key(input.controls);result.movement_key=LNO_DSL_movement_binding(input,*reuse);
    } else if (input.production_controls) {
        if (!(h3_search?(h3_facts && VHO_mla_explore_h3(program,input.gpu,input.model,input.controls,NULL,*h3_facts,*h3_search,&result.production,NULL)):
            VHO_mla_explore_controlled(program,input.gpu,input.model,input.controls,&result.production,NULL)))
            return reason = "controlled_search_failed", false;
        result.controlled = true; result.exploration = result.production.exploration;
        result.control_key = VHO_mla_search_control_key(input.controls);
    } else if (!VHO_mla_explore(program, input.gpu, input.model, options, &result.exploration, NULL))
        return reason = "no_estimated_candidate", false;
    if(h3_search) {
        if(!input.production_controls) return reason="h3_requires_production_controls",false;
        result.h3_model_key=h3_search->Model().key;
    }
    LNO_DSL_search_report_keys(input,result);
    output = result; reason = "observed"; return true;
}

/* Derive exact source/resource/model identities through the one shared spelling.
 * No estimates, native allocation or mutation occurs; application compares all
 * fields against the sealed observer result before any selected construction. */
void LNO_DSL_search_report_keys(const DSL_HIGH_SEARCH_INPUT &input,DSL_HIGH_SEARCH_REPORT &result) {
    LNO_DSL_fill_search_keys(input,result);
}

/* No compact hash or profile ID substitutes for full model/proof bindings. */
std::string LNO_DSL_movement_binding(const DSL_HIGH_SEARCH_INPUT &i,const DSL_HIGH_REUSE_FACTS &r) {
    DSL_HIGH_SEARCH_REPORT keys;LNO_DSL_search_report_keys(i,keys);
    const std::string fields[]={keys.manifest_key,keys.resource_key,keys.model_key,VHO_mla_search_control_key(i.controls),r.Binding_key()};
    std::ostringstream out;out<<"mla-movement-binding-v1;";
    for(unsigned k=0;k<5;++k) out<<fields[k].size()<<':'<<fields[k]<<';';return out.str();
}
