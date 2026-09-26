/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Builds the AIO-12 runtime-only variant and guard contract. The first slice
 * wraps the selected AIO-11 cuBLASLt matmul implementation with two operand
 * alignment guards and retains the direct implementation as an unconditional
 * fallback. No executable or binary WHIRL is changed here. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md.
 */

#include <string.h>
#include <vector>

#include "dsl_runtime_variant.h"
#include "dsl_opcode.h"
#include "pu_info.h"
#include "symtab.h"

struct DSL_RUNTIME_VARIANT_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_PHYSICAL_PLAN_ANALYSIS *physical;
    DSL_RUNTIME_VARIANT_CONTROL control;
    std::vector<DSL_RUNTIME_VARIANT_SITE_RECORD> sites;
    std::vector<DSL_RUNTIME_VARIANT_RECORD> variants;
    std::vector<DSL_RUNTIME_GUARD_RECORD> guards;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plan_contexts;
    BOOL built;
};

static const char *DSL_runtime_guard_kind_name_table[] = {
    "unknown", "operand_alignment", "shape_dimension_equal",
    "shape_dimension_multiple"
};

static const char *DSL_runtime_guard_comparison_name_table[] = {
    "unknown", "at_least", "equal", "multiple_of"
};

static BOOL
DSL_Runtime_Variant_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL runtime variant error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Runtime_Variant_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Runtime_Variant_Active
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Runtime_Variant_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu == &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

static BOOL
DSL_Runtime_Is_Power_Of_Two (UINT64 value)
{
    return value != 0 && (value & (value - 1)) == 0;
}

const char *
DSL_runtime_guard_kind_name (UINT32 kind)
{
    return kind < sizeof(DSL_runtime_guard_kind_name_table) /
                      sizeof(DSL_runtime_guard_kind_name_table[0]) ?
           DSL_runtime_guard_kind_name_table[kind] : "unknown";
}

const char *
DSL_runtime_guard_comparison_name (UINT32 comparison)
{
    return comparison < sizeof(DSL_runtime_guard_comparison_name_table) /
                            sizeof(DSL_runtime_guard_comparison_name_table[0]) ?
           DSL_runtime_guard_comparison_name_table[comparison] : "unknown";
}

void
DSL_runtime_variant_control_init (DSL_RUNTIME_VARIANT_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_variants = 1;
    control->select_policy = 1;
    control->optimization_level = 3;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->required_operand_alignment = 16;
    control->guard_evaluation_cost = 2;
    control->max_sites = 16;
    control->max_variants_per_site = 8;
    control->max_guards_per_variant = 16;
}

static BOOL
DSL_Runtime_Control_Valid (const DSL_RUNTIME_VARIANT_CONTROL &control)
{
    return control.generate_variants <= 1 && control.select_policy <= 1 &&
           control.apply_selected_variant == 0 &&
           (control.optimization_level == 0 ||
            control.optimization_level == 3) &&
           control.target_profile_id >= DSL_TARGET_PROFILE_CPU_BASELINE &&
           control.target_profile_id <= DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
           DSL_Runtime_Is_Power_Of_Two
               (control.required_operand_alignment) &&
           control.required_operand_alignment >= 4 &&
           control.guard_evaluation_cost != 0 && control.max_sites != 0 &&
           control.max_variants_per_site >= 2 &&
           control.max_guards_per_variant >= 2 && control.reserved == 0 &&
           (!control.select_policy || control.generate_variants);
}

static UINT64
DSL_Runtime_Hash_Value (UINT64 hash, UINT64 value)
{
    for (UINT32 i = 0; i < 8; ++i) {
        hash ^= (value >> (i * 8)) & 0xff;
        hash *= 1099511628211ULL;
    }
    return hash;
}

static UINT64
DSL_Runtime_Variant_Identity
        (const DSL_RUNTIME_VARIANT_SITE_RECORD &site,
         const DSL_RUNTIME_VARIANT_RECORD &variant)
{
    UINT64 hash = 1469598103934665603ULL;
    hash = DSL_Runtime_Hash_Value(hash, site.owner_pu_st);
    hash = DSL_Runtime_Hash_Value(hash, site.semantic_node_id);
    hash = DSL_Runtime_Hash_Value(hash, site.semantic_value_id);
    hash = DSL_Runtime_Hash_Value
               (hash, variant.physical_implementation_id);
    hash = DSL_Runtime_Hash_Value(hash, variant.first_guard_id);
    hash = DSL_Runtime_Hash_Value(hash, variant.guard_count);
    hash = DSL_Runtime_Hash_Value(hash, variant.fallback_variant_id);
    return hash;
}

static void
DSL_Runtime_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = DSL_OPT_COST_CONFIDENCE_LOW;
    term->evidence = evidence;
}

static BOOL
DSL_Runtime_Add_Optimization_Plan
        (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_RECORD *variant,
         const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation,
         DSL_TENSOR_EVOLUTION_NODE_ID evolution_id, FILE *diagnostic)
{
    DSL_OPT_PLAN_CONTEXT *context = analysis->plan_contexts.back();
    BOOL baseline = (variant->flags &
                     DSL_RUNTIME_VARIANT_FLAG_BASELINE) != 0;
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = baseline ? DSL_OPT_CANDIDATE_BASELINE :
                     DSL_OPT_CANDIDATE_RUNTIME_VARIANT;
    candidate.semantic_node_id =
        analysis->sites.back().semantic_node_id;
    candidate.source_evolution_node_id = evolution_id;
    candidate.result_evolution_node_id = evolution_id;
    candidate.parent_candidate_id = baseline ? 0 :
        analysis->variants
            [analysis->sites.back().baseline_variant_id - 1].candidate_id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = variant->id;
    candidate.flags = baseline ? DSL_OPT_CANDIDATE_FLAG_BASELINE :
                      DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &variant->candidate_id, diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = variant->id;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i) {
        UINT64 amount =
            i == DSL_OPT_COST_COMPUTE ? implementation.compute_cost :
            i == DSL_OPT_COST_MEMORY_UNHIDDEN ? implementation.memory_cost :
            i == DSL_OPT_COST_SYNC ? implementation.synchronization_cost :
            i == DSL_OPT_COST_LAUNCH ? implementation.launch_cost :
            i == DSL_OPT_COST_RUNTIME_SELECTION ? variant->guard_cost : 0;
        DSL_Runtime_Cost_Term
            (&cost.terms[i], amount,
             baseline ? DSL_OPT_COST_EVIDENCE_BASELINE_POLICY :
                        DSL_OPT_COST_EVIDENCE_TARGET_MODEL);
    }
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic))
        return FALSE;

    DSL_OPT_CANDIDATE_ID member = variant->candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id = baseline ? 0 :
        analysis->variants
            [analysis->sites.back().baseline_variant_id - 1].
                optimization_plan_id;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = variant->id;
    plan.flags = baseline ? DSL_OPT_PLAN_FLAG_BASELINE :
                 DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    return DSL_opt_plan_add_plan
               (context, &plan, &variant->optimization_plan_id, diagnostic);
}

static BOOL
DSL_Runtime_Add_Guard
        (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_RECORD *variant, UINT32 operand_ordinal,
         FILE *diagnostic)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Get_Node
             (analysis->sites.back().semantic_node_id, &node) ||
        operand_ordinal >= node.operand_count ||
        !DSL_IR_Image_Get_Value_Reference
             (node.first_operand_reference_id + operand_ordinal,
              &reference) ||
        reference.owner_node_id != node.id ||
        reference.ordinal != operand_ordinal ||
        !DSL_IR_Image_Get_Value(reference.value_id, &value) ||
        !TY_is_tensor_extension(value.ty))
        return DSL_Runtime_Variant_Report
                   (diagnostic, "invalid guarded operand", operand_ordinal);
    if (variant->guard_count >=
        analysis->control.max_guards_per_variant)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "guard budget exhausted", variant->id);

    DSL_RUNTIME_GUARD_RECORD guard;
    memset(&guard, 0, sizeof(guard));
    guard.id = analysis->guards.size() + 1;
    guard.variant_id = variant->id;
    guard.kind = DSL_RUNTIME_GUARD_OPERAND_ALIGNMENT;
    guard.comparison = DSL_RUNTIME_GUARD_COMPARE_AT_LEAST;
    guard.operand_ordinal = operand_ordinal;
    guard.dimension_ordinal = DSL_RUNTIME_GUARD_NO_DIMENSION;
    guard.required_value = analysis->control.required_operand_alignment;
    guard.evaluation_cost = analysis->control.guard_evaluation_cost;
    guard.failure_action = DSL_RUNTIME_GUARD_FAILURE_FALLBACK;
    guard.flags = DSL_RUNTIME_GUARD_FLAG_REQUIRED;
    analysis->guards.push_back(guard);
    ++variant->guard_count;
    variant->guard_cost += guard.evaluation_cost;
    return TRUE;
}

static BOOL
DSL_Runtime_Add_Variant
        (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_RECORD *site,
         const DSL_PHYSICAL_IMPLEMENTATION_RECORD &implementation,
         DSL_TENSOR_EVOLUTION_NODE_ID evolution_id, BOOL baseline,
         FILE *diagnostic)
{
    if (site->variant_count >= analysis->control.max_variants_per_site)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "variant budget exhausted", site->id);
    DSL_RUNTIME_VARIANT_RECORD variant;
    memset(&variant, 0, sizeof(variant));
    variant.id = analysis->variants.size() + 1;
    variant.site_id = site->id;
    variant.physical_implementation_id = implementation.id;
    variant.physical_cost = implementation.total_cost;
    variant.flags = DSL_RUNTIME_VARIANT_FLAG_CERTIFIED;
    if (baseline) {
        variant.flags |= DSL_RUNTIME_VARIANT_FLAG_BASELINE;
    } else {
        variant.flags |= DSL_RUNTIME_VARIANT_FLAG_GUARDED;
        variant.first_guard_id = analysis->guards.size() + 1;
        variant.fallback_variant_id = site->baseline_variant_id;
        if (!DSL_Runtime_Add_Guard
                 (analysis, &variant, 0, diagnostic) ||
            !DSL_Runtime_Add_Guard
                 (analysis, &variant, 1, diagnostic))
            return FALSE;
    }
    if (~(UINT64)0 - variant.physical_cost < variant.guard_cost)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "variant cost overflow", variant.id);
    variant.total_cost = variant.physical_cost + variant.guard_cost;
    variant.identity = DSL_Runtime_Variant_Identity(*site, variant);
    analysis->variants.push_back(variant);
    DSL_RUNTIME_VARIANT_RECORD &stored = analysis->variants.back();
    if (baseline)
        site->baseline_variant_id = stored.id;
    ++site->variant_count;
    return DSL_Runtime_Add_Optimization_Plan
               (analysis, &stored, implementation, evolution_id, diagnostic);
}

static BOOL
DSL_Runtime_Select
        (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_RECORD *site, FILE *diagnostic)
{
    DSL_OPT_SELECTION_RESULT result;
    if (!DSL_opt_plan_select
             (analysis->plan_contexts.back(),
              analysis->control.target_profile_id, &result, diagnostic))
        return FALSE;
    for (UINT32 i = 0; i < site->variant_count; ++i) {
        DSL_RUNTIME_VARIANT_RECORD &variant =
            analysis->variants[site->first_variant_id - 1 + i];
        if (variant.optimization_plan_id == result.selected_plan_id) {
            variant.flags |= DSL_RUNTIME_VARIANT_FLAG_SELECTED;
            site->selected_variant_id = variant.id;
            return TRUE;
        }
    }
    return DSL_Runtime_Variant_Report
               (diagnostic, "selected plan has no variant", site->id);
}

DSL_RUNTIME_VARIANT_ANALYSIS *
DSL_runtime_variant_create
        (struct pu_info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_PHYSICAL_PLAN_ANALYSIS *physical,
         const DSL_RUNTIME_VARIANT_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || physical == NULL || control == NULL ||
        Current_PU_Info != pu || !DSL_Runtime_Control_Valid(*control) ||
        !DSL_Runtime_Variant_Owner_Valid(PU_Info_proc_sym(pu)) ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_physical_plan_verify(physical, diagnostic)) {
        DSL_Runtime_Variant_Report(diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_RUNTIME_VARIANT_ANALYSIS *analysis =
        new DSL_RUNTIME_VARIANT_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->physical = physical;
    analysis->control = *control;
    analysis->built = FALSE;
    return analysis;
}

void
DSL_runtime_variant_destroy (DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i)
        DSL_opt_plan_destroy(analysis->plan_contexts[i]);
    delete analysis;
}

BOOL
DSL_runtime_variant_build
        (DSL_RUNTIME_VARIANT_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Runtime_Variant_Active(analysis) || analysis->built)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "inactive analysis", 0);
    if (!analysis->control.generate_variants ||
        analysis->control.optimization_level == 0) {
        analysis->built = TRUE;
        return TRUE;
    }

    for (UINT32 id = 1;
         id <= DSL_physical_plan_site_count(analysis->physical); ++id) {
        DSL_PHYSICAL_SITE_RECORD physical_site;
        DSL_PHYSICAL_IMPLEMENTATION_RECORD baseline;
        DSL_PHYSICAL_IMPLEMENTATION_RECORD selected;
        DSL_PROVIDER_CAPABILITY_RECORD capability;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_TENSOR_EVOLUTION_NODE_RECORD evolution;
        if (!DSL_physical_plan_get_site
                 (analysis->physical, id, &physical_site) ||
            !DSL_physical_plan_get_implementation
                 (analysis->physical,
                  physical_site.baseline_implementation_id, &baseline) ||
            !DSL_physical_plan_get_implementation
                 (analysis->physical,
                  physical_site.selected_implementation_id, &selected))
            return DSL_Runtime_Variant_Report
                       (diagnostic, "incomplete physical site", id);
        if (analysis->control.focus_value_id != 0 &&
            physical_site.semantic_value_id !=
                analysis->control.focus_value_id)
            continue;
        if (selected.provider == DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT)
            continue;
        if (selected.provider != DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ||
            selected.implementation_kind !=
                DSL_PHYSICAL_IMPLEMENTATION_LIBRARY ||
            selected.target_profile_id !=
                analysis->control.target_profile_id ||
            !DSL_provider_capability_get
                 (selected.capability_id, &capability) ||
            capability.provider != selected.provider ||
            capability.target_profile_id != selected.target_profile_id ||
            (capability.flags & DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED) == 0 ||
            selected.legality != DSL_OPT_LEGALITY_PROVEN ||
            (selected.flags &
             (DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED |
              DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE |
              DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING)) !=
                (DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED |
                 DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE |
                 DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING) ||
            baseline.provider != DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ||
            baseline.legality != DSL_OPT_LEGALITY_PROVEN ||
            selected.fallback_implementation_id != baseline.id ||
            !DSL_IR_Image_Get_Node
                 (physical_site.semantic_node_id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                 (node.opcode_descriptor_id, &descriptor) ||
            descriptor.logical_operator != OPR_DSLMATMUL ||
            descriptor.version != 1 || node.operand_count != 2 ||
            !DSL_tensor_evolution_find_semantic_root
                 (analysis->graph, physical_site.semantic_value_id,
                  &evolution))
            return DSL_Runtime_Variant_Report
                       (diagnostic, "unsupported physical variant site", id);
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Runtime_Variant_Report
                       (diagnostic, "site budget exhausted", id);

        DSL_RUNTIME_VARIANT_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_node_id = physical_site.semantic_node_id;
        site.semantic_value_id = physical_site.semantic_value_id;
        site.physical_site_id = physical_site.id;
        site.first_variant_id = analysis->variants.size() + 1;
        site.selection_policy = DSL_RUNTIME_SELECTION_FIRST_MATCH;
        analysis->sites.push_back(site);
        DSL_RUNTIME_VARIANT_SITE_RECORD &active_site =
            analysis->sites.back();

        DSL_OPT_PLAN_BUDGET budget;
        budget.max_candidates = analysis->control.max_variants_per_site;
        budget.max_plans = analysis->control.max_variants_per_site;
        DSL_OPT_PLAN_CONTEXT *context = DSL_opt_plan_create
            (analysis->pu, analysis->graph, &budget, diagnostic);
        if (context == NULL)
            return FALSE;
        analysis->plan_contexts.push_back(context);
        if (!DSL_Runtime_Add_Variant
                 (analysis, &active_site, baseline, evolution.id,
                  TRUE, diagnostic) ||
            !DSL_Runtime_Add_Variant
                 (analysis, &active_site, selected, evolution.id,
                  FALSE, diagnostic) ||
            (analysis->control.select_policy &&
             !DSL_Runtime_Select(analysis, &active_site, diagnostic)))
            return FALSE;
    }
    analysis->built = TRUE;
    return DSL_runtime_variant_verify(analysis, diagnostic);
}

static BOOL
DSL_Runtime_Guard_Valid
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         const DSL_RUNTIME_VARIANT_RECORD &variant,
         const DSL_RUNTIME_GUARD_RECORD &guard)
{
    DSL_RUNTIME_VARIANT_SITE_RECORD site =
        analysis->sites[variant.site_id - 1];
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    DSL_IR_VALUE_RECORD value;
    return guard.variant_id == variant.id &&
           guard.kind == DSL_RUNTIME_GUARD_OPERAND_ALIGNMENT &&
           guard.comparison == DSL_RUNTIME_GUARD_COMPARE_AT_LEAST &&
           guard.dimension_ordinal == DSL_RUNTIME_GUARD_NO_DIMENSION &&
           DSL_Runtime_Is_Power_Of_Two(guard.required_value) &&
           guard.required_value ==
               analysis->control.required_operand_alignment &&
           guard.evaluation_cost ==
               analysis->control.guard_evaluation_cost &&
           guard.failure_action == DSL_RUNTIME_GUARD_FAILURE_FALLBACK &&
           guard.flags == DSL_RUNTIME_GUARD_FLAG_REQUIRED &&
           guard.reserved0 == 0 && guard.reserved1 == 0 &&
           DSL_IR_Image_Get_Node(site.semantic_node_id, &node) &&
           guard.operand_ordinal < node.operand_count &&
           DSL_IR_Image_Get_Value_Reference
               (node.first_operand_reference_id + guard.operand_ordinal,
                &reference) &&
           reference.owner_node_id == node.id &&
           reference.ordinal == guard.operand_ordinal &&
           DSL_IR_Image_Get_Value(reference.value_id, &value) &&
           TY_is_tensor_extension(value.ty);
}

BOOL
DSL_runtime_variant_verify
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Runtime_Variant_Active(analysis) || !analysis->built ||
        !DSL_Runtime_Control_Valid(analysis->control) ||
        !DSL_tensor_evolution_verify(analysis->graph, diagnostic) ||
        !DSL_physical_plan_verify(analysis->physical, diagnostic) ||
        analysis->sites.size() > analysis->control.max_sites)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "invalid analysis", 0);
    UINT32 expected_variant = 1;
    UINT32 expected_guard = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_RUNTIME_VARIANT_SITE_RECORD &site = analysis->sites[i];
        DSL_PHYSICAL_SITE_RECORD physical_site;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_variant_id != expected_variant ||
            site.variant_count != 2 ||
            site.variant_count > analysis->control.max_variants_per_site ||
            site.baseline_variant_id != site.first_variant_id ||
            (analysis->control.select_policy &&
             site.selected_variant_id == 0) ||
            (!analysis->control.select_policy &&
             site.selected_variant_id != 0) ||
            site.selection_policy != DSL_RUNTIME_SELECTION_FIRST_MATCH ||
            site.reserved != 0 ||
            !DSL_physical_plan_get_site
                 (analysis->physical, site.physical_site_id,
                  &physical_site) ||
            physical_site.owner_pu_st != site.owner_pu_st ||
            physical_site.semantic_node_id != site.semantic_node_id ||
            physical_site.semantic_value_id != site.semantic_value_id ||
            analysis->plan_contexts.size() <= i ||
            !DSL_opt_plan_verify(analysis->plan_contexts[i], diagnostic))
            return DSL_Runtime_Variant_Report
                       (diagnostic, "invalid variant site", site.id);
        for (UINT32 j = 0; j < site.variant_count; ++j) {
            if (expected_variant > analysis->variants.size())
                return DSL_Runtime_Variant_Report
                           (diagnostic, "missing variant", expected_variant);
            const DSL_RUNTIME_VARIANT_RECORD &variant =
                analysis->variants[expected_variant - 1];
            DSL_PHYSICAL_IMPLEMENTATION_RECORD implementation;
            DSL_OPT_PLAN_RECORD plan;
            DSL_OPT_COST_RECORD cost;
            BOOL baseline = j == 0;
            UINT32 expected_flags = baseline ?
                (DSL_RUNTIME_VARIANT_FLAG_BASELINE |
                 DSL_RUNTIME_VARIANT_FLAG_CERTIFIED) :
                (DSL_RUNTIME_VARIANT_FLAG_GUARDED |
                 DSL_RUNTIME_VARIANT_FLAG_CERTIFIED);
            if (variant.id == site.selected_variant_id)
                expected_flags |= DSL_RUNTIME_VARIANT_FLAG_SELECTED;
            if (variant.id != expected_variant ||
                variant.site_id != site.id ||
                !DSL_physical_plan_get_implementation
                     (analysis->physical,
                      variant.physical_implementation_id,
                      &implementation) ||
                implementation.site_id != site.physical_site_id ||
                implementation.legality != DSL_OPT_LEGALITY_PROVEN ||
                !DSL_opt_plan_get_plan
                     (analysis->plan_contexts[i],
                      variant.optimization_plan_id, &plan) ||
                !DSL_opt_plan_get_cost
                     (analysis->plan_contexts[i], plan.cost_id, &cost) ||
                cost.terms[DSL_OPT_COST_RUNTIME_SELECTION].amount !=
                    variant.guard_cost ||
                variant.physical_cost + variant.guard_cost !=
                    variant.total_cost || cost.total != variant.total_cost ||
                variant.identity !=
                    DSL_Runtime_Variant_Identity(site, variant) ||
                variant.reserved != 0 ||
                (baseline &&
                 (implementation.provider !=
                      DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT ||
                  variant.first_guard_id != 0 || variant.guard_count != 0 ||
                  variant.fallback_variant_id != 0 ||
                  variant.flags != expected_flags)) ||
                (!baseline &&
                 (implementation.id !=
                      physical_site.selected_implementation_id ||
                  implementation.provider !=
                      DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT ||
                  variant.first_guard_id != expected_guard ||
                  variant.guard_count != 2 ||
                  variant.fallback_variant_id !=
                      site.baseline_variant_id ||
                  variant.flags != expected_flags)))
                return DSL_Runtime_Variant_Report
                           (diagnostic, "invalid variant", variant.id);
            if ((variant.id == site.selected_variant_id) !=
                ((variant.flags &
                  DSL_RUNTIME_VARIANT_FLAG_SELECTED) != 0))
                return DSL_Runtime_Variant_Report
                           (diagnostic, "invalid selected variant", variant.id);
            for (UINT32 k = 0; k < variant.guard_count; ++k) {
                if (expected_guard > analysis->guards.size())
                    return DSL_Runtime_Variant_Report
                               (diagnostic, "missing guard", expected_guard);
                const DSL_RUNTIME_GUARD_RECORD &guard =
                    analysis->guards[expected_guard - 1];
                if (guard.id != expected_guard ||
                    !DSL_Runtime_Guard_Valid(analysis, variant, guard) ||
                    (k != 0 && guard.operand_ordinal <=
                                  analysis->guards[expected_guard - 2].
                                      operand_ordinal))
                    return DSL_Runtime_Variant_Report
                               (diagnostic, "invalid guard", guard.id);
                ++expected_guard;
            }
            ++expected_variant;
        }
    }
    if (expected_variant != analysis->variants.size() + 1 ||
        expected_guard != analysis->guards.size() + 1)
        return DSL_Runtime_Variant_Report
                   (diagnostic, "orphan runtime record", 0);
    return TRUE;
}

static BOOL
DSL_Runtime_Find_Observation
        (const DSL_RUNTIME_GUARD_OBSERVATION *observations,
         UINT32 observation_count, UINT32 operand_ordinal,
         const DSL_RUNTIME_GUARD_OBSERVATION **observation,
         FILE *diagnostic)
{
    *observation = NULL;
    for (UINT32 i = 0; i < observation_count; ++i) {
        if (observations[i].operand_ordinal != operand_ordinal)
            continue;
        if (*observation != NULL)
            return DSL_Runtime_Variant_Report
                       (diagnostic, "duplicate runtime observation",
                        operand_ordinal);
        *observation = &observations[i];
    }
    return TRUE;
}

BOOL
DSL_runtime_variant_evaluate
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_ID site_id,
         const DSL_RUNTIME_GUARD_OBSERVATION *observations,
         UINT32 observation_count,
         DSL_RUNTIME_VARIANT_EVALUATION_RESULT *result, FILE *diagnostic)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (!DSL_runtime_variant_verify(analysis, diagnostic) || result == NULL ||
        site_id == 0 || site_id > analysis->sites.size() ||
        (observation_count != 0 && observations == NULL))
        return FALSE;
    for (UINT32 i = 0; i < observation_count; ++i) {
        if (observations[i].reserved != 0 ||
            (observations[i].rank != 0 &&
             observations[i].dimensions == NULL) ||
            (observations[i].observed_alignment != 0 &&
             !DSL_Runtime_Is_Power_Of_Two
                 (observations[i].observed_alignment)))
            return DSL_Runtime_Variant_Report
                       (diagnostic, "invalid runtime observation", i + 1);
    }

    const DSL_RUNTIME_VARIANT_SITE_RECORD &site =
        analysis->sites[site_id - 1];
    DSL_IR_NODE_RECORD node;
    if (!DSL_IR_Image_Get_Node(site.semantic_node_id, &node))
        return DSL_Runtime_Variant_Report
                   (diagnostic, "runtime site node is missing", site.id);
    for (UINT32 i = 0; i < observation_count; ++i) {
        if (observations[i].operand_ordinal >= node.operand_count)
            return DSL_Runtime_Variant_Report
                       (diagnostic, "runtime observation operand is invalid",
                        observations[i].operand_ordinal);
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (observations[prior].operand_ordinal ==
                observations[i].operand_ordinal)
                return DSL_Runtime_Variant_Report
                           (diagnostic,
                            "duplicate runtime observation",
                            observations[i].operand_ordinal);
        }
    }
    DSL_RUNTIME_VARIANT_ID selected_id = site.selected_variant_id;
    UINT32 depth = 0;
    BOOL took_fallback = FALSE;
    result->site_id = site.id;
    while (selected_id != 0 && depth++ < site.variant_count) {
        const DSL_RUNTIME_VARIANT_RECORD &variant =
            analysis->variants[selected_id - 1];
        BOOL passed = TRUE;
        for (UINT32 i = 0; i < variant.guard_count; ++i) {
            const DSL_RUNTIME_GUARD_RECORD &guard =
                analysis->guards[variant.first_guard_id - 1 + i];
            const DSL_RUNTIME_GUARD_OBSERVATION *observation;
            if (!DSL_Runtime_Find_Observation
                     (observations, observation_count,
                      guard.operand_ordinal, &observation, diagnostic))
                return FALSE;
            ++result->evaluated_guard_count;
            result->evaluation_cost += guard.evaluation_cost;
            if (observation == NULL ||
                observation->observed_alignment < guard.required_value) {
                passed = FALSE;
                break;
            }
        }
        if (passed) {
            result->selected_variant_id = variant.id;
            result->selected_implementation_id =
                variant.physical_implementation_id;
            result->guard_passed = variant.guard_count != 0;
            result->fallback_taken = took_fallback;
            return TRUE;
        }
        if (variant.fallback_variant_id == 0)
            return DSL_Runtime_Variant_Report
                       (diagnostic, "guard has no fallback", variant.id);
        selected_id = variant.fallback_variant_id;
        took_fallback = TRUE;
    }
    return DSL_Runtime_Variant_Report
               (diagnostic, "runtime fallback cycle", site.id);
}

void
DSL_runtime_variant_print
        (FILE *file, const DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    if (file == NULL || !DSL_Runtime_Variant_Active(analysis))
        return;
    fprintf(file,
            "CommonRuntimeVariantIR: owner=0x%x target=%s stage=G15 "
            "sites=%u variants=%u guards=%u opt_level=%u policy=%s "
            "apply=no\n",
            analysis->owner_pu_st,
            DSL_target_profile_name(analysis->control.target_profile_id),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->variants.size(),
            (UINT32)analysis->guards.size(),
            analysis->control.optimization_level,
            analysis->control.select_policy ? "first_match" : "none");
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_RUNTIME_VARIANT_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  runtime-site id=%u node=%u value=%u physical=%u "
                "variants=%u baseline=%u selected=%u policy=first_match\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.physical_site_id, site.variant_count,
                site.baseline_variant_id, site.selected_variant_id);
        for (UINT32 j = 0; j < site.variant_count; ++j) {
            const DSL_RUNTIME_VARIANT_RECORD &variant =
                analysis->variants[site.first_variant_id - 1 + j];
            DSL_PHYSICAL_IMPLEMENTATION_RECORD implementation;
            (void)DSL_physical_plan_get_implementation
                      (analysis->physical,
                       variant.physical_implementation_id,
                       &implementation);
            fprintf(file,
                    "    runtime-variant id=%u identity=0x%llx "
                    "implementation=%u provider=%s guards=%u "
                    "physical_cost=%llu guard_cost=%llu total=%llu "
                    "fallback=%u candidate=%u plan=%u state=%s\n",
                    variant.id,
                    (unsigned long long)variant.identity,
                    variant.physical_implementation_id,
                    DSL_physical_provider_name(implementation.provider),
                    variant.guard_count,
                    (unsigned long long)variant.physical_cost,
                    (unsigned long long)variant.guard_cost,
                    (unsigned long long)variant.total_cost,
                    variant.fallback_variant_id, variant.candidate_id,
                    variant.optimization_plan_id,
                    (variant.flags & DSL_RUNTIME_VARIANT_FLAG_SELECTED) != 0 ?
                    "selected" :
                    (variant.flags & DSL_RUNTIME_VARIANT_FLAG_BASELINE) != 0 ?
                    "fallback" : "certified");
            for (UINT32 k = 0; k < variant.guard_count; ++k) {
                const DSL_RUNTIME_GUARD_RECORD &guard =
                    analysis->guards[variant.first_guard_id - 1 + k];
                fprintf(file,
                        "      runtime-guard id=%u kind=%s compare=%s "
                        "operand=kid%u required=%llu cost=%llu "
                        "failure=fallback\n",
                        guard.id,
                        DSL_runtime_guard_kind_name(guard.kind),
                        DSL_runtime_guard_comparison_name
                            (guard.comparison),
                        guard.operand_ordinal,
                        (unsigned long long)guard.required_value,
                        (unsigned long long)guard.evaluation_cost);
            }
        }
        DSL_opt_plan_print(file, analysis->plan_contexts[i]);
    }
}

void
DSL_runtime_variant_print_evaluation
        (FILE *file, const char *label,
         const DSL_RUNTIME_VARIANT_EVALUATION_RESULT *result)
{
    if (file == NULL || result == NULL)
        return;
    fprintf(file,
            "RuntimeVariantEvaluation: label=%s site=%u variant=%u "
            "implementation=%u guards=%u cost=%llu guard=%s fallback=%s\n",
            label == NULL ? "unknown" : label, result->site_id,
            result->selected_variant_id,
            result->selected_implementation_id,
            result->evaluated_guard_count,
            (unsigned long long)result->evaluation_cost,
            result->guard_passed ? "true" : "false",
            result->fallback_taken ? "yes" : "no");
}

UINT32
DSL_runtime_variant_site_count
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_runtime_variant_count
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->variants.size();
}

UINT32
DSL_runtime_guard_count (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->guards.size();
}

BOOL
DSL_runtime_variant_get_site
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_ID id,
         DSL_RUNTIME_VARIANT_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->sites.size())
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_runtime_variant_get_variant
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_ID id, DSL_RUNTIME_VARIANT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->variants.size())
        return FALSE;
    *record = analysis->variants[id - 1];
    return TRUE;
}

BOOL
DSL_runtime_variant_get_guard
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_GUARD_ID id, DSL_RUNTIME_GUARD_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->guards.size())
        return FALSE;
    *record = analysis->guards[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_runtime_variant_get_plan_context
        (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_ID site_id)
{
    return analysis == NULL || site_id == 0 ||
           site_id > analysis->plan_contexts.size() ? NULL :
           analysis->plan_contexts[site_id - 1];
}
