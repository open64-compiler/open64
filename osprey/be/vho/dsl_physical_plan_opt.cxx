/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Builds the AIO-11 runtime-only CommonPhysicalPlanIR selector. It combines
 * the selected AIO-9 tile and AIO-10 movement plan with reviewed provider
 * capabilities, then compares complete plans through the AIO-2 cost service.
 * This selector-first slice never emits a runtime call or rewrites WHIRL. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md.
 */

#include <string.h>
#include <vector>

#include "dsl_physical_plan_opt.h"
#include "dsl_opcode.h"
#include "mtypes.h"
#include "pu_info.h"
#include "symtab.h"

struct DSL_PHYSICAL_PLAN_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TILE_ANALYSIS *tile;
    const DSL_TILE_PLAN_IR *tile_ir;
    const DSL_FETCH_PIPELINE_ANALYSIS *pipeline;
    const DSL_FETCH_PIPELINE_IR *pipeline_ir;
    VHO_DSL_PHYSICAL_PLAN_CONTROL control;
    std::vector<DSL_PHYSICAL_SITE_RECORD> sites;
    std::vector<DSL_PHYSICAL_IMPLEMENTATION_RECORD> implementations;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plan_contexts;
    DSL_PHYSICAL_PLAN_IR *ir;
    BOOL built;
};

static BOOL
DSL_Physical_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL physical plan error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Physical_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Physical_Active (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Physical_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu == &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

void
VHO_DSL_Physical_Plan_Control_Init
        (VHO_DSL_PHYSICAL_PLAN_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->select_plan = 1;
    control->optimization_level = 3;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->enabled_provider_mask =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT) |
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED);
    control->available_provider_mask = control->enabled_provider_mask;
    control->max_sites = 16;
    control->max_implementations_per_site = 16;
}

static BOOL
DSL_Physical_Control_Valid (const VHO_DSL_PHYSICAL_PLAN_CONTROL &control)
{
    UINT32 valid_mask =
        ((1U << DSL_PHYSICAL_PROVIDER_COUNT) - 1U) & ~1U;
    UINT32 direct =
        DSL_PHYSICAL_PROVIDER_MASK(DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT);
    return control.generate_candidates <= 1 &&
           control.select_plan <= 1 && control.apply_selected_plan == 0 &&
           (control.optimization_level == 0 ||
            control.optimization_level == 3) &&
           control.target_profile_id >= DSL_TARGET_PROFILE_CPU_BASELINE &&
           control.target_profile_id <= DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
           (control.enabled_provider_mask & ~valid_mask) == 0 &&
           (control.available_provider_mask & ~valid_mask) == 0 &&
           (control.enabled_provider_mask & direct) != 0 &&
           (control.available_provider_mask & direct) != 0 &&
           control.max_sites != 0 &&
           control.max_implementations_per_site != 0 &&
           control.reserved0 == 0 && control.reserved1 == 0 &&
           (!control.select_plan || control.generate_candidates);
}

static BOOL
DSL_Physical_Find_Capability
        (UINT32 provider, UINT32 profile,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor,
         TY_IDX result_ty, DSL_PROVIDER_CAPABILITY_RECORD *capability)
{
    UINT32 mtype = TY_mtype(TY_tensor_element_ty(result_ty));
    UINT32 rank = TY_tensor_rank(result_ty);
    for (UINT32 i = 0; i < DSL_provider_capability_count(); ++i) {
        DSL_PROVIDER_CAPABILITY_RECORD candidate;
        if (!DSL_provider_capability_get(i + 1, &candidate))
            return FALSE;
        if (candidate.provider == provider &&
            (candidate.target_profile_id == DSL_TARGET_PROFILE_UNKNOWN ||
             candidate.target_profile_id == profile) &&
            candidate.logical_operator == descriptor.logical_operator &&
            candidate.operator_version == descriptor.version &&
            candidate.element_mtype == mtype && candidate.rank == rank) {
            if (capability != NULL)
                *capability = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
DSL_Physical_Selected_Fetch
        (const DSL_FETCH_PIPELINE_IR *pipeline,
         const DSL_FETCH_SITE_RECORD &site,
         DSL_FETCH_PLAN_RECORD *selected)
{
    if (selected == NULL || site.selected_plan_id == 0)
        return FALSE;
    for (UINT32 i = 0; i < site.pipeline_plan_count; ++i) {
        DSL_FETCH_PLAN_RECORD candidate;
        if (!DSL_fetch_pipeline_ir_get_plan
                 (pipeline, site.first_pipeline_plan_id + i, &candidate))
            return FALSE;
        if (candidate.optimization_plan_id == site.selected_plan_id) {
            *selected = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static UINT64
DSL_Physical_Hash_Value (UINT64 hash, UINT64 value)
{
    for (UINT32 i = 0; i < 8; ++i) {
        hash ^= (value >> (i * 8)) & 0xff;
        hash *= 1099511628211ULL;
    }
    return hash;
}

static UINT64
DSL_Physical_Identity
        (const DSL_PHYSICAL_SITE_RECORD &site,
         const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation)
{
    UINT64 hash = 1469598103934665603ULL;
    hash = DSL_Physical_Hash_Value(hash, site.owner_pu_st);
    hash = DSL_Physical_Hash_Value(hash, site.semantic_node_id);
    hash = DSL_Physical_Hash_Value(hash, site.semantic_value_id);
    hash = DSL_Physical_Hash_Value(hash, implementation.provider);
    hash = DSL_Physical_Hash_Value(hash, implementation.capability_id);
    hash = DSL_Physical_Hash_Value(hash, implementation.target_profile_id);
    hash = DSL_Physical_Hash_Value(hash, implementation.tile_plan_id);
    hash = DSL_Physical_Hash_Value(hash, implementation.fetch_plan_id);
    hash = DSL_Physical_Hash_Value(hash, implementation.schedule_kind);
    return hash;
}

static UINT64
DSL_Physical_Div_Cost (UINT64 amount, UINT64 divisor)
{
    if (divisor == 0)
        return amount;
    UINT64 result = amount / divisor;
    return result == 0 ? 1 : result;
}

static void
DSL_Physical_Assign_Cost
        (DSL_PHYSICAL_IMPLEMENTATION_RECORD *implementation,
         const DSL_TILE_PLAN_RECORD &tile,
         const DSL_FETCH_PLAN_RECORD &fetch)
{
    switch (implementation->provider) {
    case DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 256);
        implementation->memory_cost =
            DSL_Physical_Div_Cost
                (tile.gmem_read_bytes + tile.gmem_write_bytes, 64);
        break;
    case DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 4096);
        implementation->memory_cost = fetch.unhidden_movement_cost;
        implementation->synchronization_cost = fetch.barrier_count;
        implementation->launch_cost = 12;
        break;
    case DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 8192);
        implementation->launch_cost = 24;
        break;
    case DSL_PHYSICAL_PROVIDER_TRITON:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 6144);
        implementation->launch_cost = 32;
        break;
    case DSL_PHYSICAL_PROVIDER_EXISTING_PTX:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 7168);
        implementation->launch_cost = 28;
        break;
    default:
        implementation->compute_cost =
            DSL_Physical_Div_Cost(tile.operation_count, 128);
        implementation->launch_cost = 64;
        break;
    }
    implementation->total_cost = implementation->compute_cost +
        implementation->memory_cost +
        implementation->synchronization_cost +
        implementation->launch_cost;
}

static void
DSL_Physical_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = DSL_OPT_COST_CONFIDENCE_LOW;
    term->evidence = evidence;
}

static BOOL
DSL_Physical_Add_Optimization_Plan
        (DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
         DSL_PHYSICAL_SITE_RECORD *site,
         DSL_PHYSICAL_IMPLEMENTATION_RECORD *implementation,
         const DSL_TILE_PLAN_RECORD &tile, FILE *diagnostic)
{
    DSL_OPT_PLAN_CONTEXT *context = analysis->plan_contexts.back();
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ?
        DSL_OPT_CANDIDATE_BASELINE : DSL_OPT_CANDIDATE_KERNEL;
    candidate.semantic_node_id = site->semantic_node_id;
    candidate.source_evolution_node_id = tile.result_evolution_node_id;
    candidate.result_evolution_node_id = tile.result_evolution_node_id;
    candidate.parent_candidate_id =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ? 0 :
        analysis->implementations[site->first_implementation_id - 1].candidate_id;
    candidate.legality = implementation->legality;
    candidate.rejection_reason = implementation->rejection_reason;
    candidate.ordering_key = implementation->id;
    candidate.flags =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ?
        DSL_OPT_CANDIDATE_FLAG_BASELINE : DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &implementation->candidate_id,
              diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = implementation->id;
    UINT32 evidence =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ?
        DSL_OPT_COST_EVIDENCE_BASELINE_POLICY :
        DSL_OPT_COST_EVIDENCE_TARGET_MODEL;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i) {
        UINT64 amount =
            i == DSL_OPT_COST_COMPUTE ? implementation->compute_cost :
            i == DSL_OPT_COST_MEMORY_UNHIDDEN ?
                implementation->memory_cost :
            i == DSL_OPT_COST_SYNC ?
                implementation->synchronization_cost :
            i == DSL_OPT_COST_LAUNCH ? implementation->launch_cost : 0;
        DSL_Physical_Cost_Term(&cost.terms[i], amount, evidence);
    }
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic))
        return FALSE;

    DSL_OPT_CANDIDATE_ID member = implementation->candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ? 0 :
        analysis->implementations[site->baseline_implementation_id - 1].
            optimization_plan_id;
    plan.legality = implementation->legality;
    plan.rejection_reason = implementation->rejection_reason;
    plan.ordering_key = implementation->id;
    plan.flags =
        (implementation->flags &
         DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0 ?
        DSL_OPT_PLAN_FLAG_BASELINE : DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    return DSL_opt_plan_add_plan
               (context, &plan, &implementation->optimization_plan_id,
                diagnostic);
}

static BOOL
DSL_Physical_Add_Implementation
        (DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
         DSL_PHYSICAL_SITE_RECORD *site, UINT32 provider,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor,
         TY_IDX result_ty, const DSL_TILE_PLAN_RECORD &tile,
         const DSL_FETCH_PLAN_RECORD &fetch, FILE *diagnostic)
{
    DSL_PHYSICAL_IMPLEMENTATION_RECORD implementation;
    DSL_PROVIDER_CAPABILITY_RECORD capability;
    memset(&implementation, 0, sizeof(implementation));
    memset(&capability, 0, sizeof(capability));
    implementation.id = analysis->implementations.size() + 1;
    implementation.site_id = site->id;
    implementation.provider = provider;
    implementation.target_profile_id = analysis->control.target_profile_id;
    implementation.legality = DSL_OPT_LEGALITY_PROVEN;
    implementation.rejection_reason = DSL_OPT_REJECT_NONE;
    implementation.flags =
        DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVISIONAL |
        DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING;
    if (!DSL_Physical_Find_Capability
             (provider, analysis->control.target_profile_id,
              descriptor, result_ty, &capability)) {
        implementation.implementation_kind =
            provider == DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ?
            DSL_PHYSICAL_IMPLEMENTATION_BASELINE_DIRECT :
            DSL_PHYSICAL_IMPLEMENTATION_LIBRARY;
        implementation.legality = DSL_OPT_LEGALITY_REJECTED;
        implementation.rejection_reason =
            DSL_OPT_REJECT_PROVIDER_MISMATCH;
    } else {
        implementation.capability_id = capability.id;
        implementation.implementation_kind =
            capability.implementation_kind;
        implementation.schedule_kind = capability.schedule_kind;
        if ((analysis->control.available_provider_mask &
             DSL_PHYSICAL_PROVIDER_MASK(provider)) == 0) {
            implementation.legality = DSL_OPT_LEGALITY_REJECTED;
            implementation.rejection_reason =
                DSL_OPT_REJECT_PROVIDER_UNAVAILABLE;
        } else {
            implementation.flags |=
                DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE;
        }
    }

    BOOL baseline = provider == DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT;
    if (baseline) {
        implementation.flags =
            DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE |
            DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE |
            DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING;
        implementation.tile_plan_id = 0;
        implementation.fetch_plan_id = 0;
    } else if (provider == DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED) {
        implementation.tile_plan_id = tile.id;
        implementation.fetch_plan_id = fetch.id;
        if (implementation.legality == DSL_OPT_LEGALITY_PROVEN &&
            (tile.legality != DSL_OPT_LEGALITY_PROVEN ||
             fetch.legality != DSL_OPT_LEGALITY_PROVEN)) {
            if (tile.legality == DSL_OPT_LEGALITY_UNKNOWN ||
                fetch.legality == DSL_OPT_LEGALITY_UNKNOWN) {
                implementation.legality = DSL_OPT_LEGALITY_UNKNOWN;
                implementation.rejection_reason =
                    DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
            } else {
                implementation.legality = DSL_OPT_LEGALITY_REJECTED;
                implementation.rejection_reason =
                    tile.legality == DSL_OPT_LEGALITY_REJECTED ?
                    tile.rejection_reason : fetch.rejection_reason;
            }
        }
    }
    DSL_Physical_Assign_Cost(&implementation, tile, fetch);
    implementation.identity = DSL_Physical_Identity(*site, implementation);
    implementation.fallback_implementation_id = baseline ? 0 :
        site->baseline_implementation_id;
    analysis->implementations.push_back(implementation);
    DSL_PHYSICAL_IMPLEMENTATION_RECORD &stored =
        analysis->implementations.back();
    if (baseline)
        site->baseline_implementation_id = stored.id;
    if (!DSL_Physical_Add_Optimization_Plan
             (analysis, site, &stored, tile, diagnostic))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_Physical_Select
        (DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
         DSL_PHYSICAL_SITE_RECORD *site, FILE *diagnostic)
{
    DSL_OPT_SELECTION_RESULT result;
    if (!DSL_opt_plan_select
             (analysis->plan_contexts.back(),
              analysis->control.target_profile_id, &result, diagnostic))
        return FALSE;
    for (UINT32 i = 0; i < site->implementation_count; ++i) {
        DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation =
            analysis->implementations
                [site->first_implementation_id - 1 + i];
        if (implementation.optimization_plan_id == result.selected_plan_id) {
            implementation.flags |=
                DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED;
            site->selected_implementation_id = implementation.id;
            site->selected_plan_id = result.selected_plan_id;
            return TRUE;
        }
    }
    return DSL_Physical_Report
               (diagnostic, "selected plan has no implementation", site->id);
}

DSL_PHYSICAL_PLAN_ANALYSIS *
VHO_DSL_Physical_Plan_Create
        (struct pu_info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TILE_ANALYSIS *tile,
         const DSL_FETCH_PIPELINE_ANALYSIS *pipeline,
         const VHO_DSL_PHYSICAL_PLAN_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tile == NULL || pipeline == NULL ||
        control == NULL || Current_PU_Info != pu ||
        !DSL_Physical_Control_Valid(*control) ||
        !VHO_DSL_Tile_Verify(tile, diagnostic) ||
        !VHO_DSL_Fetch_Pipeline_Verify(pipeline, diagnostic) ||
        !DSL_Physical_Owner_Valid(PU_Info_proc_sym(pu))) {
        DSL_Physical_Report(diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_PHYSICAL_PLAN_ANALYSIS *analysis =
        new DSL_PHYSICAL_PLAN_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->tile = tile;
    analysis->tile_ir = VHO_DSL_Tile_Get_IR(tile);
    analysis->pipeline = pipeline;
    analysis->pipeline_ir = VHO_DSL_Fetch_Pipeline_Get_IR(pipeline);
    analysis->control = *control;
    analysis->ir = NULL;
    analysis->built = FALSE;
    if (analysis->tile_ir == NULL || analysis->pipeline_ir == NULL) {
        delete analysis;
        DSL_Physical_Report(diagnostic, "missing input IR", 0);
        return NULL;
    }
    return analysis;
}

void
VHO_DSL_Physical_Plan_Destroy (DSL_PHYSICAL_PLAN_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i)
        DSL_opt_plan_destroy(analysis->plan_contexts[i]);
    DSL_physical_plan_ir_destroy(analysis->ir);
    delete analysis;
}

static BOOL
DSL_Physical_Materialize_IR
        (DSL_PHYSICAL_PLAN_ANALYSIS *analysis, FILE *diagnostic)
{
    DSL_PHYSICAL_PLAN_IR_CREATE_INFO info;
    memset(&info, 0, sizeof(info));
    info.owner_pu_st = analysis->owner_pu_st;
    info.sites = analysis->sites.empty() ? NULL : &analysis->sites[0];
    info.site_count = analysis->sites.size();
    info.implementations = analysis->implementations.empty() ? NULL :
                           &analysis->implementations[0];
    info.implementation_count = analysis->implementations.size();
    DSL_physical_plan_ir_destroy(analysis->ir);
    analysis->ir = DSL_physical_plan_ir_create(&info, diagnostic);
    return analysis->ir != NULL;
}

BOOL
VHO_DSL_Physical_Plan_Build
        (DSL_PHYSICAL_PLAN_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Physical_Active(analysis) || analysis->built)
        return DSL_Physical_Report(diagnostic, "inactive analysis", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return DSL_Physical_Materialize_IR(analysis, diagnostic) &&
               VHO_DSL_Physical_Plan_Verify(analysis, diagnostic);
    }
    for (UINT32 id = 1;
         id <= DSL_fetch_pipeline_ir_site_count(analysis->pipeline_ir); ++id) {
        DSL_FETCH_SITE_RECORD fetch_site;
        DSL_FETCH_PLAN_RECORD fetch;
        DSL_TILE_PLAN_RECORD tile;
        DSL_IR_NODE_RECORD node;
        DSL_IR_VALUE_RECORD value;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        if (!DSL_fetch_pipeline_ir_get_site
                 (analysis->pipeline_ir, id, &fetch_site) ||
            (analysis->control.focus_value_id != 0 &&
             fetch_site.semantic_value_id !=
                 analysis->control.focus_value_id))
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Physical_Report
                       (diagnostic, "site budget exhausted", id);
        if (!DSL_Physical_Selected_Fetch
                 (analysis->pipeline_ir, fetch_site, &fetch) ||
            !DSL_tile_plan_ir_get_plan
                 (analysis->tile_ir, fetch.tile_plan_id, &tile) ||
            !DSL_IR_Image_Get_Node(fetch_site.semantic_node_id, &node) ||
            !DSL_IR_Image_Get_Value(fetch_site.semantic_value_id, &value) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &descriptor) ||
            !TY_is_tensor_extension(value.ty))
            return DSL_Physical_Report
                       (diagnostic, "incomplete selected inputs", id);

        DSL_PHYSICAL_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_node_id = fetch_site.semantic_node_id;
        site.semantic_value_id = fetch_site.semantic_value_id;
        site.selected_tile_plan_id = tile.id;
        site.selected_fetch_plan_id = fetch.id;
        site.first_implementation_id = analysis->implementations.size() + 1;
        analysis->sites.push_back(site);
        DSL_PHYSICAL_SITE_RECORD &active_site = analysis->sites.back();

        DSL_OPT_PLAN_BUDGET budget;
        budget.max_candidates =
            analysis->control.max_implementations_per_site;
        budget.max_plans =
            analysis->control.max_implementations_per_site;
        DSL_OPT_PLAN_CONTEXT *context =
            DSL_opt_plan_create
                (analysis->pu, analysis->graph, &budget, diagnostic);
        if (context == NULL)
            return FALSE;
        analysis->plan_contexts.push_back(context);

        if (!DSL_Physical_Add_Implementation
                 (analysis, &active_site,
                  DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT,
                  descriptor, value.ty, tile, fetch, diagnostic))
            return FALSE;
        if (analysis->control.optimization_level == 3) {
            for (UINT32 provider =
                     DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED;
                 provider < DSL_PHYSICAL_PROVIDER_COUNT; ++provider) {
                if ((analysis->control.enabled_provider_mask &
                     DSL_PHYSICAL_PROVIDER_MASK(provider)) == 0)
                    continue;
                if (analysis->implementations.size() -
                        active_site.first_implementation_id + 1 >=
                    analysis->control.max_implementations_per_site)
                    return DSL_Physical_Report
                               (diagnostic,
                                "implementation budget exhausted",
                                active_site.id);
                if (!DSL_Physical_Add_Implementation
                         (analysis, &active_site, provider, descriptor,
                          value.ty, tile, fetch, diagnostic))
                    return FALSE;
            }
        }
        active_site.implementation_count =
            analysis->implementations.size() -
            active_site.first_implementation_id + 1;
        if (analysis->control.select_plan &&
            !DSL_Physical_Select(analysis, &active_site, diagnostic))
            return FALSE;
    }
    analysis->built = TRUE;
    return DSL_Physical_Materialize_IR(analysis, diagnostic) &&
           VHO_DSL_Physical_Plan_Verify(analysis, diagnostic);
}

BOOL
VHO_DSL_Physical_Plan_Verify
        (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Physical_Active(analysis) || !analysis->built ||
        analysis->ir == NULL ||
        !DSL_Physical_Control_Valid(analysis->control) ||
        !DSL_physical_plan_ir_verify(analysis->ir, diagnostic) ||
        DSL_physical_plan_ir_owner(analysis->ir) != analysis->owner_pu_st ||
        DSL_physical_plan_ir_site_count(analysis->ir) !=
            analysis->sites.size() ||
        DSL_physical_plan_ir_implementation_count(analysis->ir) !=
            analysis->implementations.size() ||
        analysis->sites.size() != analysis->plan_contexts.size())
        return DSL_Physical_Report(diagnostic, "invalid analysis", 0);
    UINT32 expected = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_PHYSICAL_SITE_RECORD &site = analysis->sites[i];
        DSL_PHYSICAL_SITE_RECORD ir_site;
        UINT32 selected_count = 0;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            !DSL_physical_plan_ir_get_site
                 (analysis->ir, site.id, &ir_site) ||
            memcmp(&site, &ir_site, sizeof(site)) != 0 ||
            site.first_implementation_id != expected ||
            site.implementation_count == 0 ||
            site.baseline_implementation_id != expected ||
            site.reserved != 0 ||
            !DSL_opt_plan_verify(analysis->plan_contexts[i], diagnostic))
            return DSL_Physical_Report
                       (diagnostic, "invalid physical site", site.id);
        for (UINT32 j = 0; j < site.implementation_count; ++j) {
            const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation =
                analysis->implementations[expected - 1];
            DSL_PHYSICAL_IMPLEMENTATION_RECORD ir_implementation;
            BOOL baseline = j == 0;
            if (implementation.id != expected ||
                !DSL_physical_plan_ir_get_implementation
                     (analysis->ir, implementation.id,
                      &ir_implementation) ||
                memcmp(&implementation, &ir_implementation,
                       sizeof(implementation)) != 0 ||
                implementation.site_id != site.id ||
                implementation.provider <= DSL_PHYSICAL_PROVIDER_UNKNOWN ||
                implementation.provider >= DSL_PHYSICAL_PROVIDER_COUNT ||
                implementation.target_profile_id !=
                    analysis->control.target_profile_id ||
                implementation.identity == 0 ||
                implementation.total_cost !=
                    implementation.compute_cost +
                    implementation.memory_cost +
                    implementation.synchronization_cost +
                    implementation.launch_cost ||
                implementation.candidate_id == 0 ||
                implementation.optimization_plan_id == 0 ||
                implementation.reserved != 0 ||
                baseline !=
                    ((implementation.flags &
                      DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE) != 0) ||
                (!baseline && implementation.fallback_implementation_id !=
                     site.baseline_implementation_id) ||
                (baseline &&
                 (implementation.fallback_implementation_id != 0 ||
                  implementation.provider !=
                      DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ||
                  implementation.tile_plan_id != 0 ||
                  implementation.fetch_plan_id != 0)))
                return DSL_Physical_Report
                           (diagnostic, "invalid implementation",
                            implementation.id);
            if (implementation.capability_id == 0) {
                if (implementation.rejection_reason !=
                    DSL_OPT_REJECT_PROVIDER_MISMATCH)
                    return DSL_Physical_Report
                               (diagnostic, "missing capability",
                                implementation.id);
            } else {
                DSL_PROVIDER_CAPABILITY_RECORD capability;
                if (!DSL_provider_capability_get
                         (implementation.capability_id, &capability) ||
                    capability.provider != implementation.provider ||
                    capability.implementation_kind !=
                        implementation.implementation_kind ||
                    capability.schedule_kind !=
                        implementation.schedule_kind)
                    return DSL_Physical_Report
                               (diagnostic, "capability mismatch",
                                implementation.id);
            }
            if ((implementation.flags &
                 DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) != 0) {
                ++selected_count;
                if (site.selected_implementation_id != implementation.id ||
                    site.selected_plan_id !=
                        implementation.optimization_plan_id ||
                    implementation.legality != DSL_OPT_LEGALITY_PROVEN)
                    return DSL_Physical_Report
                               (diagnostic, "invalid selected implementation",
                                implementation.id);
            }
            ++expected;
        }
        if (analysis->control.select_plan && selected_count != 1)
            return DSL_Physical_Report
                       (diagnostic, "missing selected implementation",
                        site.id);
    }
    if (expected != analysis->implementations.size() + 1)
        return DSL_Physical_Report
                   (diagnostic, "implementation table mismatch", 0);
    return TRUE;
}

void
VHO_DSL_Physical_Plan_Print
        (FILE *file, const DSL_PHYSICAL_PLAN_ANALYSIS *analysis)
{
    if (file == NULL || !DSL_Physical_Active(analysis))
        return;
    DSL_physical_plan_ir_print(file, analysis->ir);
    fprintf(file,
            "VHOPhysicalPlanDecision: owner=0x%x target=%s stage=G13 "
            "sites=%u implementations=%u opt_level=%u select=%s apply=no\n",
            analysis->owner_pu_st,
            DSL_target_profile_name(analysis->control.target_profile_id),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->implementations.size(),
            analysis->control.optimization_level,
            analysis->control.select_plan ? "yes" : "no");
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_PHYSICAL_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  physical-site id=%u node=%u value=%u tile=%u fetch=%u "
                "implementations=%u baseline=%u selected=%u plan=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.selected_tile_plan_id, site.selected_fetch_plan_id,
                site.implementation_count, site.baseline_implementation_id,
                site.selected_implementation_id, site.selected_plan_id);
        for (UINT32 j = 0; j < site.implementation_count; ++j) {
            const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation =
                analysis->implementations
                    [site.first_implementation_id - 1 + j];
            fprintf(file,
                    "    physical-implementation id=%u identity=0x%llx "
                    "kind=%s provider=%s capability=%u schedule=%s "
                    "tile=%u fetch=%u legality=%s reason=%s "
                    "cost=%llu compute=%llu memory=%llu sync=%llu "
                    "launch=%llu fallback=%u state=%s\n",
                    implementation.id,
                    (unsigned long long)implementation.identity,
                    DSL_physical_implementation_name
                        (implementation.implementation_kind),
                    DSL_physical_provider_name(implementation.provider),
                    implementation.capability_id,
                    DSL_physical_schedule_name
                        (implementation.schedule_kind),
                    implementation.tile_plan_id,
                    implementation.fetch_plan_id,
                    DSL_opt_legality_name(implementation.legality),
                    DSL_opt_rejection_reason_name
                        (implementation.rejection_reason),
                    (unsigned long long)implementation.total_cost,
                    (unsigned long long)implementation.compute_cost,
                    (unsigned long long)implementation.memory_cost,
                    (unsigned long long)
                        implementation.synchronization_cost,
                    (unsigned long long)implementation.launch_cost,
                    implementation.fallback_implementation_id,
                    (implementation.flags &
                     DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED) != 0 ?
                    "selected" : "not_selected");
        }
        DSL_opt_plan_print(file, analysis->plan_contexts[i]);
    }
}

const DSL_PHYSICAL_PLAN_IR *
VHO_DSL_Physical_Plan_Get_IR
        (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis)
{
    return analysis == NULL ? NULL : analysis->ir;
}

const DSL_OPT_PLAN_CONTEXT *
VHO_DSL_Physical_Plan_Get_Plan_Context
        (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis, DSL_PHYSICAL_SITE_ID id)
{
    return analysis == NULL || id == 0 ||
           id > analysis->plan_contexts.size() ?
           NULL : analysis->plan_contexts[id - 1];
}
