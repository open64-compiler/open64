/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Builds AIO-8 PU-local memory-residency alternatives from tensor locality and
 * target hierarchy facts. The service remains check-only and never allocates
 * target memory or rewrites WHIRL. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
 */

#include <string.h>
#include <vector>

#include "dsl_residency_candidate.h"
#include "pu_info.h"

struct DSL_RESIDENCY_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_RESIDENCY_CONTROL control;
    std::vector<DSL_RESIDENCY_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_RESIDENCY_SITE_RECORD> sites;
    std::vector<DSL_RESIDENCY_ALTERNATIVE_RECORD> alternatives;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    BOOL built;
};

static const char *DSL_residency_promotion_name_table[] = {
    "unknown", "none", "on_demand", "prefetch"
};

static const char *DSL_residency_demotion_name_table[] = {
    "unknown", "none", "last_use", "pressure"
};

static const char *DSL_residency_spill_name_table[] = {
    "unknown", "none", "lower_tier", "system"
};

static const char *DSL_residency_eviction_name_table[] = {
    "unknown", "none", "last_use", "pressure"
};

static BOOL
DSL_Residency_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL residency analysis error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Residency_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Residency_Active (const DSL_RESIDENCY_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Residency_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_residency_promotion_name (UINT32 policy)
{
    return policy < sizeof(DSL_residency_promotion_name_table) /
                        sizeof(DSL_residency_promotion_name_table[0]) ?
           DSL_residency_promotion_name_table[policy] : "unknown";
}

const char *
DSL_residency_demotion_name (UINT32 policy)
{
    return policy < sizeof(DSL_residency_demotion_name_table) /
                        sizeof(DSL_residency_demotion_name_table[0]) ?
           DSL_residency_demotion_name_table[policy] : "unknown";
}

const char *
DSL_residency_spill_name (UINT32 policy)
{
    return policy < sizeof(DSL_residency_spill_name_table) /
                        sizeof(DSL_residency_spill_name_table[0]) ?
           DSL_residency_spill_name_table[policy] : "unknown";
}

const char *
DSL_residency_eviction_name (UINT32 policy)
{
    return policy < sizeof(DSL_residency_eviction_name_table) /
                        sizeof(DSL_residency_eviction_name_table[0]) ?
           DSL_residency_eviction_name_table[policy] : "unknown";
}

void
DSL_residency_control_init (DSL_RESIDENCY_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->target_profile_id = DSL_TARGET_PROFILE_CPU_BASELINE;
    control->max_sites = 64;
    control->max_alternatives_per_site = 6;
    control->enable_system = 1;
    control->enable_pinned_host = 1;
    control->enable_hbm = 1;
    control->enable_l2 = 1;
    control->enable_shared = 1;
    control->enable_register = 1;
}

static BOOL
DSL_Residency_Control_Valid (const DSL_RESIDENCY_CONTROL &control)
{
    return control.generate_candidates <= 1 && control.select_plans <= 1 &&
           control.apply_transformation == 0 &&
           control.target_profile_id != DSL_TARGET_PROFILE_UNKNOWN &&
           control.max_sites != 0 &&
           control.max_alternatives_per_site != 0 &&
           control.enable_system <= 1 && control.enable_pinned_host <= 1 &&
           control.enable_hbm <= 1 && control.enable_l2 <= 1 &&
           control.enable_shared <= 1 && control.enable_register <= 1 &&
           control.reserved == 0 &&
           (!control.select_plans || control.generate_candidates) &&
           DSL_memory_hierarchy_validate
               (control.target_profile_id, NULL);
}

static BOOL
DSL_Residency_Tier_Enabled
        (const DSL_RESIDENCY_CONTROL &control, UINT32 kind)
{
    switch (kind) {
    case DSL_MEMORY_TIER_SYSTEM:
        return control.enable_system;
    case DSL_MEMORY_TIER_PINNED_HOST:
        return control.enable_pinned_host;
    case DSL_MEMORY_TIER_HBM:
        return control.enable_hbm;
    case DSL_MEMORY_TIER_L2:
        return control.enable_l2;
    case DSL_MEMORY_TIER_SHARED:
        return control.enable_shared;
    case DSL_MEMORY_TIER_REGISTER:
        return control.enable_register;
    default:
        return FALSE;
    }
}

static UINT64
DSL_Residency_Allocated_Bytes (UINT64 bytes, UINT64 granularity)
{
    if (bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64 || granularity == 0 ||
        bytes > DSL_TENSOR_LOCALITY_UNKNOWN_U64 - (granularity - 1))
        return DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    return ((bytes + granularity - 1) / granularity) * granularity;
}

static void
DSL_Residency_Set_Policies
        (UINT32 tier_kind, DSL_RESIDENCY_DESCRIPTOR_RECORD *descriptor)
{
    descriptor->promotion_policy = DSL_RESIDENCY_PROMOTION_ON_DEMAND;
    descriptor->demotion_policy = DSL_RESIDENCY_DEMOTION_PRESSURE;
    descriptor->spill_policy = DSL_RESIDENCY_SPILL_LOWER_TIER;
    descriptor->eviction_policy = DSL_RESIDENCY_EVICTION_PRESSURE;
    switch (tier_kind) {
    case DSL_MEMORY_TIER_SYSTEM:
        descriptor->promotion_policy = DSL_RESIDENCY_PROMOTION_NONE;
        descriptor->demotion_policy = DSL_RESIDENCY_DEMOTION_NONE;
        descriptor->spill_policy = DSL_RESIDENCY_SPILL_NONE;
        break;
    case DSL_MEMORY_TIER_PINNED_HOST:
        descriptor->promotion_policy = DSL_RESIDENCY_PROMOTION_PREFETCH;
        descriptor->demotion_policy = DSL_RESIDENCY_DEMOTION_LAST_USE;
        descriptor->spill_policy = DSL_RESIDENCY_SPILL_SYSTEM;
        descriptor->eviction_policy = DSL_RESIDENCY_EVICTION_LAST_USE;
        break;
    case DSL_MEMORY_TIER_SHARED:
        descriptor->promotion_policy = DSL_RESIDENCY_PROMOTION_PREFETCH;
        descriptor->demotion_policy = DSL_RESIDENCY_DEMOTION_LAST_USE;
        descriptor->eviction_policy = DSL_RESIDENCY_EVICTION_LAST_USE;
        break;
    case DSL_MEMORY_TIER_REGISTER:
        descriptor->demotion_policy = DSL_RESIDENCY_DEMOTION_LAST_USE;
        descriptor->eviction_policy = DSL_RESIDENCY_EVICTION_LAST_USE;
        break;
    default:
        break;
    }
}

static void
DSL_Residency_Classify
        (const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         const DSL_MEMORY_TIER_RECORD &tier,
         DSL_RESIDENCY_DESCRIPTOR_RECORD *descriptor,
         DSL_RESIDENCY_ALTERNATIVE_RECORD *alternative)
{
    /*
     * Capacity fit proves only that an allocation can fit in the modeled tier.
     * In particular, it does not promise cache retention, occupancy, or an
     * executable allocation; those belong to later planning stages.
     */
    if (locality.size_state == DSL_TENSOR_SIZE_STATIC)
        descriptor->required_bytes = DSL_Residency_Allocated_Bytes
                                         (locality.object_bytes,
                                          tier.allocation_granularity);
    else
        descriptor->required_bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    descriptor->flags |= DSL_RESIDENCY_FLAG_CAPACITY_CHECKED;

    if (locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_EFFECT;
    } else if (descriptor->required_bytes ==
                   DSL_TENSOR_LOCALITY_UNKNOWN_U64 ||
               tier.capacity_bytes == DSL_MEMORY_CAPACITY_UNKNOWN) {
        alternative->legality = DSL_OPT_LEGALITY_UNKNOWN;
        alternative->rejection_reason =
            DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else if (descriptor->required_bytes > tier.capacity_bytes) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_RESOURCE;
    } else if ((tier.kind == DSL_MEMORY_TIER_SHARED ||
                tier.kind == DSL_MEMORY_TIER_REGISTER) &&
               locality.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_OWNERSHIP;
    } else if ((tier.kind == DSL_MEMORY_TIER_SHARED ||
                tier.kind == DSL_MEMORY_TIER_REGISTER) &&
               locality.lifetime_state !=
                   DSL_TENSOR_LIFETIME_EXACT_BLOCK) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_DESCRIPTOR;
    } else {
        descriptor->flags |= DSL_RESIDENCY_FLAG_LIFETIME_CHECKED;
        alternative->legality = DSL_OPT_LEGALITY_PROVEN;
        alternative->rejection_reason = DSL_OPT_REJECT_NONE;
    }
}

static void
DSL_Residency_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 confidence,
         UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = confidence;
    term->evidence = evidence;
}

static BOOL
DSL_Residency_Build_Baseline_Plan
        (DSL_RESIDENCY_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_RESIDENCY_SITE_RECORD *site,
         DSL_OPT_PLAN_CONTEXT **context_out, FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;
    if (!DSL_tensor_evolution_find_semantic_root
             (analysis->graph, tensor.value_id, &root))
        return DSL_Residency_Report
                   (diagnostic, "missing semantic root", tensor.value_id);
    DSL_OPT_PLAN_BUDGET budget;
    budget.max_candidates = analysis->control.max_alternatives_per_site + 1;
    budget.max_plans = analysis->control.max_alternatives_per_site + 1;
    DSL_OPT_PLAN_CONTEXT *context = DSL_opt_plan_create
                                        (analysis->pu, analysis->graph,
                                         &budget, diagnostic);
    if (context == NULL)
        return FALSE;
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = tensor.producer_node_id;
    candidate.source_evolution_node_id = root.id;
    candidate.result_evolution_node_id = root.id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 1;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_BASELINE;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &site->baseline_candidate_id,
              diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }
    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = 1;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
        DSL_Residency_Cost_Term
            (&cost.terms[i], i == DSL_OPT_COST_MEMORY_UNHIDDEN ? 1000 : 0,
             DSL_OPT_COST_CONFIDENCE_MEDIUM,
             DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }
    DSL_OPT_CANDIDATE_ID member = site->baseline_candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 1;
    plan.flags = DSL_OPT_PLAN_FLAG_BASELINE |
                 DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_opt_plan_add_plan
             (context, &plan, &site->baseline_plan_id, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }
    *context_out = context;
    return TRUE;
}

static BOOL
DSL_Residency_Add_Alternative_Plan
        (DSL_RESIDENCY_ANALYSIS *analysis,
         DSL_OPT_PLAN_CONTEXT *context,
         const DSL_RESIDENCY_SITE_RECORD &site,
         const DSL_MEMORY_TIER_RECORD &tier,
         DSL_RESIDENCY_ALTERNATIVE_RECORD *alternative,
         UINT32 ordinal, FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Get_Value(site.semantic_value_id, &value))
        return FALSE;
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_RESIDENCY;
    candidate.semantic_node_id = value.producer_node_id;
    candidate.source_evolution_node_id = site.semantic_root_id;
    candidate.result_evolution_node_id =
        alternative->result_evolution_node_id;
    candidate.parent_candidate_id = site.baseline_candidate_id;
    candidate.legality = alternative->legality;
    candidate.rejection_reason = alternative->rejection_reason;
    candidate.ordering_key = ordinal + 1;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &alternative->candidate_id,
              diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = ordinal + 1;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
        DSL_Residency_Cost_Term
            (&cost.terms[i],
             i == DSL_OPT_COST_MEMORY_UNHIDDEN ?
                 (UINT64)tier.latency_class * 10ULL : 0,
             DSL_OPT_COST_CONFIDENCE_LOW,
             DSL_OPT_COST_EVIDENCE_TARGET_MODEL);
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic))
        return FALSE;
    DSL_OPT_CANDIDATE_ID member = alternative->candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id = site.baseline_plan_id;
    plan.legality = alternative->legality;
    plan.rejection_reason = alternative->rejection_reason;
    plan.ordering_key = ordinal + 1;
    plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    return DSL_opt_plan_add_plan
               (context, &plan, &alternative->plan_id, diagnostic);
}

static BOOL
DSL_Residency_Add_Alternative
        (DSL_RESIDENCY_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         DSL_RESIDENCY_SITE_RECORD *site,
         DSL_OPT_PLAN_CONTEXT *context,
         const DSL_MEMORY_TIER_RECORD &tier, FILE *diagnostic)
{
    DSL_RESIDENCY_DESCRIPTOR_RECORD descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.id = analysis->descriptors.size() + 1;
    descriptor.source_descriptor_ty = tensor.descriptor_ty;
    descriptor.target_profile_id = analysis->control.target_profile_id;
    descriptor.tier_id = tier.id;
    descriptor.tier_kind = tier.kind;
    descriptor.tier_scope = tier.scope;
    descriptor.capacity_bytes = tier.capacity_bytes;
    descriptor.allocation_granularity = tier.allocation_granularity;
    descriptor.minimum_alignment = tier.minimum_alignment;
    descriptor.flags = DSL_RESIDENCY_FLAG_PROVISIONAL |
                       DSL_RESIDENCY_FLAG_SEMANTICS_PRESERVING;
    DSL_Residency_Set_Policies(tier.kind, &descriptor);

    DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
    memset(&alternative, 0, sizeof(alternative));
    alternative.id = analysis->alternatives.size() + 1;
    alternative.site_id = site->id;
    alternative.descriptor_id = descriptor.id;
    DSL_Residency_Classify(locality, tier, &descriptor, &alternative);
    analysis->descriptors.push_back(descriptor);
    if (!DSL_tensor_evolution_add_local_physical
             (analysis->graph, site->semantic_root_id, descriptor.id,
              &alternative.result_evolution_node_id,
              &alternative.evolution_edge_id, diagnostic) ||
        !DSL_Residency_Add_Alternative_Plan
             (analysis, context, *site, tier, &alternative,
              site->alternative_count + 1, diagnostic)) {
        analysis->descriptors.pop_back();
        return FALSE;
    }
    analysis->alternatives.push_back(alternative);
    ++site->alternative_count;
    return TRUE;
}

DSL_RESIDENCY_ANALYSIS *
DSL_residency_create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_RESIDENCY_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Residency_Control_Valid(*control) ||
        !DSL_tensor_evolution_verify(graph, diagnostic) ||
        !DSL_tensor_analysis_verify(tensor_analysis, diagnostic) ||
        !DSL_tensor_locality_verify(locality, diagnostic)) {
        DSL_Residency_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_RESIDENCY_ANALYSIS *analysis = new DSL_RESIDENCY_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->tensor_analysis = tensor_analysis;
    analysis->locality = locality;
    analysis->control = *control;
    analysis->built = FALSE;
    return analysis;
}

void
DSL_residency_destroy (DSL_RESIDENCY_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_opt_plan_destroy(analysis->plans[i]);
    delete analysis;
}

BOOL
DSL_residency_build (DSL_RESIDENCY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Residency_Active(analysis) || analysis->built)
        return DSL_Residency_Report
                   (diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    DSL_MEMORY_HIERARCHY_PROFILE profile;
    if (!DSL_memory_hierarchy_get_profile
             (analysis->control.target_profile_id, &profile))
        return DSL_Residency_Report
                   (diagnostic, "missing target profile", 0);
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_tensor_analysis_fact_count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality;
        if (!DSL_tensor_analysis_get_fact
                 (analysis->tensor_analysis, id, &tensor) ||
            tensor.value_role != DSL_TENSOR_VALUE_ROLE_INTERMEDIATE ||
            tensor.rank < 1 || tensor.producer_node_id == 0 ||
            (analysis->control.focus_value_id != 0 &&
             tensor.value_id != analysis->control.focus_value_id))
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Residency_Report
                       (diagnostic, "residency site budget exhausted", id);
        if (!DSL_tensor_locality_find_fact
                 (analysis->locality, tensor.value_id, &locality))
            return DSL_Residency_Report
                       (diagnostic, "missing locality evidence", id);
        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        if (!DSL_tensor_evolution_find_semantic_root
                 (analysis->graph, tensor.value_id, &root))
            return DSL_Residency_Report
                       (diagnostic, "missing residency root", id);

        DSL_RESIDENCY_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_value_id = tensor.value_id;
        site.semantic_root_id = root.id;
        site.first_alternative_id = analysis->alternatives.size() + 1;
        DSL_OPT_PLAN_CONTEXT *context = NULL;
        if (!DSL_Residency_Build_Baseline_Plan
                 (analysis, tensor, &site, &context, diagnostic))
            return FALSE;
        for (UINT32 tier_id = 1; tier_id <= profile.tier_count; ++tier_id) {
            DSL_MEMORY_TIER_RECORD tier;
            if (!DSL_memory_hierarchy_get_tier
                     (profile.id, tier_id, &tier)) {
                DSL_opt_plan_destroy(context);
                return FALSE;
            }
            if (!DSL_Residency_Tier_Enabled(analysis->control, tier.kind))
                continue;
            if (site.alternative_count >=
                    analysis->control.max_alternatives_per_site)
                break;
            if (!DSL_Residency_Add_Alternative
                     (analysis, tensor, locality, &site, context, tier,
                      diagnostic)) {
                DSL_opt_plan_destroy(context);
                return FALSE;
            }
        }
        if (analysis->control.select_plans) {
            DSL_OPT_SELECTION_RESULT selection;
            if (!DSL_opt_plan_select
                     (context, analysis->control.target_profile_id,
                      &selection, diagnostic)) {
                DSL_opt_plan_destroy(context);
                return FALSE;
            }
            site.selected_plan_id = selection.selected_plan_id;
        }
        analysis->plans.push_back(context);
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_residency_verify(analysis, diagnostic);
}

BOOL
DSL_residency_verify
        (const DSL_RESIDENCY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Residency_Active(analysis) || !analysis->built ||
        !DSL_Residency_Control_Valid(analysis->control) ||
        analysis->sites.size() != analysis->plans.size() ||
        analysis->descriptors.size() != analysis->alternatives.size())
        return DSL_Residency_Report(diagnostic, "invalid analysis", 0);
    for (UINT32 i = 0; i < analysis->descriptors.size(); ++i) {
        const DSL_RESIDENCY_DESCRIPTOR_RECORD &descriptor =
            analysis->descriptors[i];
        const DSL_RESIDENCY_ALTERNATIVE_RECORD &alternative =
            analysis->alternatives[i];
        DSL_MEMORY_TIER_RECORD tier;
        DSL_TENSOR_EVOLUTION_NODE_RECORD node;
        DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
        if (descriptor.id != i + 1 || alternative.id != i + 1 ||
            alternative.descriptor_id != descriptor.id ||
            descriptor.target_profile_id !=
                analysis->control.target_profile_id ||
            !DSL_memory_hierarchy_get_tier
                 (descriptor.target_profile_id, descriptor.tier_id, &tier) ||
            descriptor.tier_kind != tier.kind ||
            descriptor.tier_scope != tier.scope ||
            descriptor.capacity_bytes != tier.capacity_bytes ||
            descriptor.allocation_granularity !=
                tier.allocation_granularity ||
            descriptor.minimum_alignment != tier.minimum_alignment ||
            TY_IDX_index(descriptor.source_descriptor_ty) == 0 ||
            !TY_is_tensor_extension(descriptor.source_descriptor_ty) ||
            !TY_tensor_is_canonical(descriptor.source_descriptor_ty) ||
            descriptor.promotion_policy == DSL_RESIDENCY_PROMOTION_UNKNOWN ||
            descriptor.promotion_policy > DSL_RESIDENCY_PROMOTION_PREFETCH ||
            descriptor.demotion_policy == DSL_RESIDENCY_DEMOTION_UNKNOWN ||
            descriptor.demotion_policy > DSL_RESIDENCY_DEMOTION_PRESSURE ||
            descriptor.spill_policy == DSL_RESIDENCY_SPILL_UNKNOWN ||
            descriptor.spill_policy > DSL_RESIDENCY_SPILL_SYSTEM ||
            descriptor.eviction_policy == DSL_RESIDENCY_EVICTION_UNKNOWN ||
            descriptor.eviction_policy > DSL_RESIDENCY_EVICTION_PRESSURE ||
            descriptor.reserved != 0 || alternative.reserved != 0 ||
            alternative.site_id == 0 ||
            alternative.site_id > analysis->sites.size() ||
            alternative.legality > DSL_OPT_LEGALITY_REJECTED ||
            (alternative.legality == DSL_OPT_LEGALITY_PROVEN &&
             alternative.rejection_reason != DSL_OPT_REJECT_NONE) ||
            (alternative.legality != DSL_OPT_LEGALITY_PROVEN &&
             alternative.rejection_reason == DSL_OPT_REJECT_NONE) ||
            !DSL_tensor_evolution_get_node
                 (analysis->graph, alternative.result_evolution_node_id,
                  &node) ||
            !DSL_tensor_evolution_get_edge
                 (analysis->graph, alternative.evolution_edge_id, &edge) ||
            node.kind != DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL ||
            node.representation_descriptor_id != descriptor.id ||
            node.descriptor_ty != descriptor.source_descriptor_ty ||
            edge.result_node_id != node.id ||
            edge.transformation_kind !=
                DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT)
            return DSL_Residency_Report
                       (diagnostic, "invalid residency alternative", i + 1);
    }
    UINT32 expected_alternative = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_RESIDENCY_SITE_RECORD &site = analysis->sites[i];
        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_alternative_id != expected_alternative ||
            site.alternative_count == 0 || site.reserved != 0 ||
            !DSL_tensor_evolution_find_semantic_root
                 (analysis->graph, site.semantic_value_id, &root) ||
            root.id != site.semantic_root_id ||
            !DSL_opt_plan_verify(analysis->plans[i], diagnostic))
            return DSL_Residency_Report
                       (diagnostic, "invalid residency site", site.id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            const DSL_RESIDENCY_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[site.first_alternative_id + j - 1];
            if (alternative.site_id != site.id)
                return DSL_Residency_Report
                           (diagnostic, "orphan residency alternative",
                            alternative.id);
        }
        expected_alternative += site.alternative_count;
    }
    return expected_alternative == analysis->alternatives.size() + 1 &&
           DSL_tensor_evolution_verify(analysis->graph, diagnostic);
}

static void
DSL_Residency_Print_U64 (FILE *file, UINT64 value)
{
    if (value == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
        fprintf(file, "<unknown>");
    else
        fprintf(file, "%llu", (unsigned long long)value);
}

void
DSL_residency_print
        (FILE *file, const DSL_RESIDENCY_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file,
            "CommonMemoryResidencyIR: owner=<%u,%u,%s> profile=%s "
            "sites=%u descriptors=%u alternatives=%u select=%s apply=no\n",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            DSL_Residency_Owner_Valid(analysis->owner_pu_st) ?
                ST_name(St_Table[analysis->owner_pu_st]) : "<invalid>",
            DSL_target_profile_name(analysis->control.target_profile_id),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->descriptors.size(),
            (UINT32)analysis->alternatives.size(),
            analysis->control.select_plans ? "yes" : "no");
    DSL_memory_hierarchy_print(file, analysis->control.target_profile_id);
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_RESIDENCY_SITE_RECORD &site = analysis->sites[i];
        fprintf(file, "  site[%u] value=%u root=%u alternatives=%u "
                      "baseline=%u selected=%u\n",
                site.id, site.semantic_value_id, site.semantic_root_id,
                site.alternative_count, site.baseline_plan_id,
                site.selected_plan_id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            const DSL_RESIDENCY_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[site.first_alternative_id + j - 1];
            const DSL_RESIDENCY_DESCRIPTOR_RECORD &descriptor =
                analysis->descriptors[alternative.descriptor_id - 1];
            fprintf(file, "    alternative[%u] tier=%s scope=%s required=",
                    alternative.id,
                    DSL_memory_tier_name(descriptor.tier_kind),
                    DSL_memory_scope_name(descriptor.tier_scope));
            DSL_Residency_Print_U64(file, descriptor.required_bytes);
            fprintf(file, " capacity=");
            DSL_Residency_Print_U64(file, descriptor.capacity_bytes);
            fprintf(file, " promote=%s demote=%s spill=%s evict=%s "
                          "legality=%s reason=%s node=%u edge=%u "
                          "candidate=%u plan=%u\n",
                    DSL_residency_promotion_name
                        (descriptor.promotion_policy),
                    DSL_residency_demotion_name
                        (descriptor.demotion_policy),
                    DSL_residency_spill_name(descriptor.spill_policy),
                    DSL_residency_eviction_name
                        (descriptor.eviction_policy),
                    DSL_opt_legality_name(alternative.legality),
                    DSL_opt_rejection_reason_name
                        (alternative.rejection_reason),
                    alternative.result_evolution_node_id,
                    alternative.evolution_edge_id,
                    alternative.candidate_id, alternative.plan_id);
        }
        DSL_opt_plan_print(file, analysis->plans[i]);
    }
}

UINT32
DSL_residency_descriptor_count (const DSL_RESIDENCY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->descriptors.size();
}

UINT32
DSL_residency_site_count (const DSL_RESIDENCY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_residency_alternative_count (const DSL_RESIDENCY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->alternatives.size();
}

BOOL
DSL_residency_get_descriptor
        (const DSL_RESIDENCY_ANALYSIS *analysis,
         DSL_RESIDENCY_DESCRIPTOR_ID id,
         DSL_RESIDENCY_DESCRIPTOR_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->descriptors.size())
        return FALSE;
    *record = analysis->descriptors[id - 1];
    return TRUE;
}

BOOL
DSL_residency_get_site
        (const DSL_RESIDENCY_ANALYSIS *analysis, DSL_RESIDENCY_SITE_ID id,
         DSL_RESIDENCY_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->sites.size())
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_residency_get_alternative
        (const DSL_RESIDENCY_ANALYSIS *analysis,
         DSL_RESIDENCY_ALTERNATIVE_ID id,
         DSL_RESIDENCY_ALTERNATIVE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->alternatives.size())
        return FALSE;
    *record = analysis->alternatives[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_residency_get_plan_context
        (const DSL_RESIDENCY_ANALYSIS *analysis, DSL_RESIDENCY_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
