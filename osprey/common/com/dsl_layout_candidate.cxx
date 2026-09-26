/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Constructs AIO-6 PU-local logical-layout alternatives and compatibility
 * evidence while preserving canonical TensorDescriptorIR identity. It is a
 * runtime-only planning service. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md.
 */

#include <string.h>
#include <vector>

#include "dsl_layout_candidate.h"
#include "dsl_opcode.h"
#include "dsl_shape.h"
#include "pu_info.h"

struct DSL_LOGICAL_LAYOUT_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_LOGICAL_LAYOUT_CONTROL control;
    std::vector<DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_LOGICAL_LAYOUT_AXIS_RECORD> axes;
    std::vector<DSL_LOGICAL_LAYOUT_BLOCK_RECORD> blocks;
    std::vector<DSL_LOGICAL_LAYOUT_SITE_RECORD> sites;
    std::vector<DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD> alternatives;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    BOOL built;
};

static const char *DSL_logical_layout_kind_name[] = {
    "unknown", "permuted", "blocked", "packed_head", "domain"
};

static const char *DSL_layout_compatibility_name[] = {
    "unknown", "proven", "rejected"
};

static const char *DSL_layout_conversion_name[] = {
    "unknown", "known"
};

static BOOL
DSL_Logical_Layout_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL logical layout error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Logical_Layout_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Logical_Layout_Active (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Logical_Layout_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_Logical_Layout_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_logical_layout_kind_name) /
                      sizeof(DSL_logical_layout_kind_name[0]) ?
           DSL_logical_layout_kind_name[kind] : "unknown";
}

const char *
DSL_Layout_Compatibility_Name (UINT32 state)
{
    return state < sizeof(DSL_layout_compatibility_name) /
                       sizeof(DSL_layout_compatibility_name[0]) ?
           DSL_layout_compatibility_name[state] : "unknown";
}

const char *
DSL_Layout_Conversion_Name (UINT32 state)
{
    return state < sizeof(DSL_layout_conversion_name) /
                       sizeof(DSL_layout_conversion_name[0]) ?
           DSL_layout_conversion_name[state] : "unknown";
}

void
DSL_Logical_Layout_Control_Init (DSL_LOGICAL_LAYOUT_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->target_profile_id = 1;
    control->max_sites = 64;
    control->max_alternatives_per_site = 2;
    control->default_block_size = 8;
}

static BOOL
DSL_Logical_Layout_Control_Valid
        (const DSL_LOGICAL_LAYOUT_CONTROL &control)
{
    return control.generate_candidates <= 1 && control.select_plans <= 1 &&
           control.apply_transformation <= 1 &&
           control.apply_transformation == 0 &&
           (!control.select_plans || control.generate_candidates) &&
           control.target_profile_id != 0 && control.max_sites != 0 &&
           control.max_alternatives_per_site != 0 &&
           control.default_block_size > 1 && control.reserved == 0;
}

static BOOL
DSL_Logical_Layout_Axis_Equal
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor,
         const std::vector<UINT32> &order)
{
    if (descriptor.axis_count != order.size())
        return FALSE;
    for (UINT32 i = 0; i < descriptor.axis_count; ++i) {
        const DSL_LOGICAL_LAYOUT_AXIS_RECORD &axis =
            analysis->axes[descriptor.first_axis_id - 1 + i];
        if (axis.ordinal != i || axis.source_axis != order[i] ||
            axis.result_axis != i)
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Logical_Layout_Block_Equal
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor,
         const std::vector<UINT32> &block_axes,
         const std::vector<UINT32> &block_factors)
{
    if (descriptor.block_count != block_axes.size() ||
        block_axes.size() != block_factors.size())
        return FALSE;
    for (UINT32 i = 0; i < descriptor.block_count; ++i) {
        const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &block =
            analysis->blocks[descriptor.first_block_id - 1 + i];
        if (block.axis != block_axes[i] ||
            block.factor != block_factors[i])
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Logical_Layout_Intern_Descriptor
        (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis, TY_IDX source_ty,
         UINT32 kind, const std::vector<UINT32> &order,
         const std::vector<UINT32> &block_axes,
         const std::vector<UINT32> &block_factors,
         DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID *descriptor_id,
         FILE *diagnostic)
{
    UINT32 rank = order.size();
    if (descriptor_id != NULL)
        *descriptor_id = DSL_LOGICAL_LAYOUT_INVALID_ID;
    if (!DSL_Logical_Layout_Active(analysis) || descriptor_id == NULL ||
        TY_IDX_index(source_ty) == 0 || !TY_is_tensor_extension(source_ty) ||
        !TY_tensor_is_canonical(source_ty) || rank == 0 ||
        rank != (UINT32)TY_tensor_rank(source_ty) ||
        kind < DSL_LOGICAL_LAYOUT_PERMUTED ||
        kind > DSL_LOGICAL_LAYOUT_DOMAIN ||
        block_axes.size() != block_factors.size())
        return DSL_Logical_Layout_Report
                   (diagnostic, "invalid descriptor input", kind);
    std::vector<BOOL> seen(rank, FALSE);
    for (UINT32 i = 0; i < rank; ++i) {
        if (order[i] >= rank || seen[order[i]])
            return DSL_Logical_Layout_Report
                       (diagnostic, "axis order is not a permutation", i);
        seen[order[i]] = TRUE;
    }
    std::vector<BOOL> blocked(rank, FALSE);
    for (UINT32 i = 0; i < block_axes.size(); ++i) {
        if (block_axes[i] >= rank || blocked[block_axes[i]] ||
            block_factors[i] <= 1)
            return DSL_Logical_Layout_Report
                       (diagnostic, "invalid block specification", i);
        blocked[block_axes[i]] = TRUE;
    }
    if ((kind == DSL_LOGICAL_LAYOUT_BLOCKED) != !block_axes.empty())
        return DSL_Logical_Layout_Report
                   (diagnostic, "layout kind and blocks disagree", kind);

    /*
     * Logical alternatives are immutable and uniqued by semantic source type,
     * permutation, and blocking. They do not create a replacement TY_IDX until
     * a later transformation is selected and applied.
     */
    for (UINT32 i = 0; i < analysis->descriptors.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &candidate =
            analysis->descriptors[i];
        if (candidate.source_descriptor_ty == source_ty &&
            candidate.kind == kind && candidate.rank == rank &&
            DSL_Logical_Layout_Axis_Equal
                (analysis, candidate, order) &&
            DSL_Logical_Layout_Block_Equal
                (analysis, candidate, block_axes, block_factors)) {
            *descriptor_id = candidate.id;
            return TRUE;
        }
    }

    DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.id = analysis->descriptors.size() + 1;
    descriptor.source_descriptor_ty = source_ty;
    descriptor.kind = kind;
    descriptor.rank = rank;
    descriptor.first_axis_id = analysis->axes.size() + 1;
    descriptor.axis_count = rank;
    descriptor.first_block_id = block_axes.empty() ? 0 :
                                analysis->blocks.size() + 1;
    descriptor.block_count = block_axes.size();
    descriptor.minimum_alignment = TY_align(source_ty);
    descriptor.flags = DSL_LOGICAL_LAYOUT_FLAG_SEMANTICS_PRESERVING |
                       DSL_LOGICAL_LAYOUT_FLAG_LOGICAL_ONLY;
    for (UINT32 i = 0; i < rank; ++i) {
        DSL_LOGICAL_LAYOUT_AXIS_RECORD axis;
        memset(&axis, 0, sizeof(axis));
        axis.id = analysis->axes.size() + 1;
        axis.descriptor_id = descriptor.id;
        axis.ordinal = i;
        axis.source_axis = order[i];
        axis.result_axis = i;
        analysis->axes.push_back(axis);
    }
    for (UINT32 i = 0; i < block_axes.size(); ++i) {
        DSL_LOGICAL_LAYOUT_BLOCK_RECORD block;
        memset(&block, 0, sizeof(block));
        block.id = analysis->blocks.size() + 1;
        block.descriptor_id = descriptor.id;
        block.axis = block_axes[i];
        block.factor = block_factors[i];
        analysis->blocks.push_back(block);
    }
    analysis->descriptors.push_back(descriptor);
    *descriptor_id = descriptor.id;
    return TRUE;
}

static BOOL
DSL_Logical_Layout_Node_Info
        (DSL_IR_NODE_ID node_id, DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    return node != NULL && descriptor != NULL &&
           DSL_IR_Image_Get_Node(node_id, node) &&
           (node->flags & DSL_IR_NODE_FLAG_RETIRED) == 0 &&
           DSL_IR_Image_Get_Opcode_Descriptor
               (node->opcode_descriptor_id, descriptor);
}

static UINT32
DSL_Logical_Layout_Compatibility
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor, UINT32 *rejection_reason)
{
    DSL_TENSOR_LOCALITY_FACT_RECORD locality;
    BOOL all_layout_consumers = tensor.use_count != 0;
    *rejection_reason = DSL_OPT_REJECT_NONE;
    if (!DSL_Tensor_Locality_Find_Fact
             (analysis->locality, tensor.value_id, &locality)) {
        *rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        return DSL_LAYOUT_COMPATIBILITY_UNKNOWN;
    }
    if (locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT) {
        *rejection_reason = DSL_OPT_REJECT_EFFECT;
        return DSL_LAYOUT_COMPATIBILITY_REJECTED;
    }
    if (locality.lifetime_state != DSL_TENSOR_LIFETIME_EXACT_BLOCK) {
        *rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        return DSL_LAYOUT_COMPATIBILITY_UNKNOWN;
    }
    for (UINT32 i = 0; i < tensor.use_count; ++i) {
        DSL_TENSOR_USE_FACT_RECORD use;
        DSL_IR_NODE_RECORD consumer;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        if (!DSL_Tensor_Analysis_Get_Use
                 (analysis->tensor_analysis, tensor.first_use_id + i, &use) ||
            !DSL_Logical_Layout_Node_Info
                 (use.consumer_node_id, &consumer, &descriptor)) {
            *rejection_reason = DSL_OPT_REJECT_INVALID_REFERENCE;
            return DSL_LAYOUT_COMPATIBILITY_REJECTED;
        }
        if (descriptor.effect_model != DSL_EFFECT_MODEL_PURE) {
            *rejection_reason = DSL_OPT_REJECT_EFFECT;
            return DSL_LAYOUT_COMPATIBILITY_REJECTED;
        }
        if (use.shape_rule != DSL_SHAPE_RULE_LAYOUT &&
            use.shape_rule != DSL_SHAPE_RULE_IDENTITY)
            all_layout_consumers = FALSE;
    }
    if (!all_layout_consumers) {
        *rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        return DSL_LAYOUT_COMPATIBILITY_UNKNOWN;
    }
    return DSL_LAYOUT_COMPATIBILITY_PROVEN;
}

static void
DSL_Logical_Layout_Classify
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD *alternative)
{
    DSL_TENSOR_LOCALITY_FACT_RECORD locality;
    alternative->conversion_bytes = DSL_LOGICAL_LAYOUT_UNKNOWN_U64;
    alternative->compatibility_state = DSL_Logical_Layout_Compatibility
                                            (analysis, tensor,
                                             &alternative->rejection_reason);
    if (DSL_Tensor_Locality_Find_Fact
            (analysis->locality, tensor.value_id, &locality) &&
        locality.size_state == DSL_TENSOR_SIZE_STATIC &&
        locality.object_bytes <= DSL_LOGICAL_LAYOUT_UNKNOWN_U64 / 2) {
        alternative->conversion_state = DSL_LAYOUT_CONVERSION_KNOWN;
        alternative->conversion_bytes = locality.object_bytes * 2;
    } else {
        alternative->conversion_state = DSL_LAYOUT_CONVERSION_UNKNOWN;
    }
    if (alternative->compatibility_state ==
            DSL_LAYOUT_COMPATIBILITY_REJECTED) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
    } else if (alternative->compatibility_state ==
                   DSL_LAYOUT_COMPATIBILITY_UNKNOWN ||
               alternative->conversion_state ==
                   DSL_LAYOUT_CONVERSION_UNKNOWN) {
        alternative->legality = DSL_OPT_LEGALITY_UNKNOWN;
        alternative->rejection_reason =
            DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else {
        alternative->legality = DSL_OPT_LEGALITY_PROVEN;
        alternative->rejection_reason = DSL_OPT_REJECT_NONE;
    }
}

static void
DSL_Logical_Layout_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 confidence,
         UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = confidence;
    term->evidence = evidence;
}

static void
DSL_Logical_Layout_Unknown_Cost (DSL_OPT_COST_TERM *term)
{
    memset(term, 0, sizeof(*term));
}

static BOOL
DSL_Logical_Layout_Build_Plans
        (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_SITE_RECORD *site,
         const DSL_TENSOR_FACT_RECORD &tensor, FILE *diagnostic)
{
    DSL_OPT_PLAN_BUDGET budget;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_SELECTION_RESULT selection;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_CANDIDATE_ID member_id;
    budget.max_candidates = site->alternative_count + 1;
    budget.max_plans = site->alternative_count + 1;
    DSL_OPT_PLAN_CONTEXT *context = DSL_Opt_Plan_Create
                                        (analysis->pu, analysis->graph,
                                         &budget, diagnostic);
    if (context == NULL)
        return FALSE;

    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = tensor.producer_node_id;
    candidate.source_evolution_node_id = site->semantic_root_id;
    candidate.result_evolution_node_id = site->semantic_root_id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 1;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_BASELINE;
    if (!DSL_Opt_Plan_Add_Candidate
             (context, &candidate, &site->baseline_candidate_id,
              diagnostic)) {
        DSL_Opt_Plan_Destroy(context);
        return FALSE;
    }
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = 1;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
        DSL_Logical_Layout_Cost_Term
            (&cost.terms[i], i == DSL_OPT_COST_COMPUTE ? 100 : 0,
             DSL_OPT_COST_CONFIDENCE_MEDIUM,
             DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    if (!DSL_Opt_Plan_Add_Cost(context, &cost, &cost_id, diagnostic)) {
        DSL_Opt_Plan_Destroy(context);
        return FALSE;
    }
    member_id = site->baseline_candidate_id;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member_id;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 1;
    plan.flags = DSL_OPT_PLAN_FLAG_BASELINE |
                 DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_Opt_Plan_Add_Plan
             (context, &plan, &site->baseline_plan_id, diagnostic)) {
        DSL_Opt_Plan_Destroy(context);
        return FALSE;
    }

    for (UINT32 i = 0; i < site->alternative_count; ++i) {
        DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD &alternative =
            analysis->alternatives[site->first_alternative_id - 1 + i];
        memset(&candidate, 0, sizeof(candidate));
        candidate.kind = DSL_OPT_CANDIDATE_LAYOUT;
        candidate.semantic_node_id = tensor.producer_node_id;
        candidate.source_evolution_node_id = site->semantic_root_id;
        candidate.result_evolution_node_id =
            alternative.result_evolution_node_id;
        candidate.parent_candidate_id = site->baseline_candidate_id;
        candidate.legality = alternative.legality;
        candidate.rejection_reason = alternative.rejection_reason;
        candidate.ordering_key = i + 2;
        candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
        if (!DSL_Opt_Plan_Add_Candidate
                 (context, &candidate, &alternative.candidate_id,
                  diagnostic)) {
            DSL_Opt_Plan_Destroy(context);
            return FALSE;
        }
        memset(&cost, 0, sizeof(cost));
        cost.target_profile_id = analysis->control.target_profile_id;
        cost.ordering_key = i + 2;
        for (UINT32 term = 0; term < DSL_OPT_COST_TERM_COUNT; ++term)
            DSL_Logical_Layout_Cost_Term
                (&cost.terms[term], term == DSL_OPT_COST_COMPUTE ? 100 : 0,
                 DSL_OPT_COST_CONFIDENCE_MEDIUM,
                 DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
        DSL_Logical_Layout_Unknown_Cost
            (&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN]);
        if (!DSL_Opt_Plan_Add_Cost(context, &cost, &cost_id, diagnostic)) {
            DSL_Opt_Plan_Destroy(context);
            return FALSE;
        }
        member_id = alternative.candidate_id;
        memset(&plan, 0, sizeof(plan));
        plan.candidate_ids = &member_id;
        plan.candidate_count = 1;
        plan.cost_id = cost_id;
        plan.fallback_plan_id = site->baseline_plan_id;
        plan.legality = alternative.legality;
        plan.rejection_reason = alternative.rejection_reason;
        plan.ordering_key = i + 2;
        plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
        if (!DSL_Opt_Plan_Add_Plan
                 (context, &plan, &alternative.plan_id, diagnostic)) {
            DSL_Opt_Plan_Destroy(context);
            return FALSE;
        }
    }
    if (!DSL_Opt_Plan_Verify(context, diagnostic)) {
        DSL_Opt_Plan_Destroy(context);
        return FALSE;
    }
    if (analysis->control.select_plans) {
        if (!DSL_Opt_Plan_Select
                 (context, analysis->control.target_profile_id,
                  &selection, diagnostic)) {
            DSL_Opt_Plan_Destroy(context);
            return FALSE;
        }
        site->selected_plan_id = selection.selected_plan_id;
    }
    analysis->plans.push_back(context);
    return TRUE;
}

static BOOL
DSL_Logical_Layout_Add_Alternative
        (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_SITE_RECORD *site,
         const DSL_TENSOR_FACT_RECORD &tensor, UINT32 kind,
         const std::vector<UINT32> &order,
         const std::vector<UINT32> &block_axes,
         const std::vector<UINT32> &block_factors, FILE *diagnostic)
{
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID edge_id;
    if (!DSL_Logical_Layout_Intern_Descriptor
             (analysis, tensor.descriptor_ty, kind, order, block_axes,
              block_factors, &descriptor_id, diagnostic) ||
        !DSL_Tensor_Evolution_Add_Logical_Layout
             (analysis->graph, tensor.semantic_root_id, descriptor_id,
              &result_node_id, &edge_id, diagnostic))
        return FALSE;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD alternative;
    memset(&alternative, 0, sizeof(alternative));
    alternative.id = analysis->alternatives.size() + 1;
    alternative.site_id = site->id;
    alternative.descriptor_id = descriptor_id;
    alternative.result_evolution_node_id = result_node_id;
    alternative.evolution_edge_id = edge_id;
    DSL_Logical_Layout_Classify(analysis, tensor, &alternative);
    analysis->alternatives.push_back(alternative);
    ++site->alternative_count;
    return TRUE;
}

DSL_LOGICAL_LAYOUT_ANALYSIS *
DSL_Logical_Layout_Create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_LOGICAL_LAYOUT_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_Tensor_Evolution_Owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Logical_Layout_Control_Valid(*control) ||
        !DSL_Tensor_Evolution_Verify(graph, diagnostic) ||
        !DSL_Tensor_Analysis_Verify(tensor_analysis, diagnostic) ||
        !DSL_Tensor_Locality_Verify(locality, diagnostic)) {
        DSL_Logical_Layout_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_LOGICAL_LAYOUT_ANALYSIS *analysis =
        new DSL_LOGICAL_LAYOUT_ANALYSIS;
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
DSL_Logical_Layout_Destroy (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_Opt_Plan_Destroy(analysis->plans[i]);
    delete analysis;
}

BOOL
DSL_Logical_Layout_Build
        (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Logical_Layout_Active(analysis) || analysis->built)
        return DSL_Logical_Layout_Report
                   (diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_Tensor_Analysis_Fact_Count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD tensor;
        if (!DSL_Tensor_Analysis_Get_Fact
                 (analysis->tensor_analysis, id, &tensor))
            return DSL_Logical_Layout_Report
                       (diagnostic, "missing tensor fact", id);
        if (tensor.value_role != DSL_TENSOR_VALUE_ROLE_INTERMEDIATE ||
            tensor.rank < 2 || tensor.producer_node_id == 0 ||
            (analysis->control.focus_value_id != 0 &&
             tensor.value_id != analysis->control.focus_value_id))
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Logical_Layout_Report
                       (diagnostic, "layout site budget exhausted", id);

        DSL_LOGICAL_LAYOUT_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_value_id = tensor.value_id;
        site.semantic_root_id = tensor.semantic_root_id;
        site.first_alternative_id = analysis->alternatives.size() + 1;

        std::vector<UINT32> order(tensor.rank);
        std::vector<UINT32> no_blocks;
        for (UINT32 axis = 0; axis < (UINT32)tensor.rank; ++axis)
            order[axis] = axis;
        UINT32 swap = order[tensor.rank - 1];
        order[tensor.rank - 1] = order[tensor.rank - 2];
        order[tensor.rank - 2] = swap;
        if (!DSL_Logical_Layout_Add_Alternative
                 (analysis, &site, tensor,
                  DSL_LOGICAL_LAYOUT_PERMUTED, order, no_blocks,
                  no_blocks, diagnostic))
            return FALSE;

        if (site.alternative_count <
                analysis->control.max_alternatives_per_site &&
            tensor.dimension_state == DSL_TENSOR_DIMENSION_STATIC) {
            const char *shape = TY_tensor_attribute
                                    (tensor.descriptor_ty,
                                     TY_TENSOR_SCHEMA_SHAPE);
            std::vector<UINT64> dimensions(tensor.rank);
            UINT32 parsed_rank = 0;
            if (shape != NULL &&
                DSL_Shape_Parse_Static_Dimensions
                    (shape, &dimensions[0], dimensions.size(),
                     &parsed_rank) &&
                parsed_rank == (UINT32)tensor.rank &&
                dimensions[tensor.rank - 1] %
                    analysis->control.default_block_size == 0 &&
                dimensions[tensor.rank - 2] %
                    analysis->control.default_block_size == 0) {
                std::vector<UINT32> identity(tensor.rank);
                std::vector<UINT32> block_axes(2);
                std::vector<UINT32> block_factors(2);
                for (UINT32 axis = 0; axis < (UINT32)tensor.rank; ++axis)
                    identity[axis] = axis;
                block_axes[0] = tensor.rank - 2;
                block_axes[1] = tensor.rank - 1;
                block_factors[0] = analysis->control.default_block_size;
                block_factors[1] = analysis->control.default_block_size;
                if (!DSL_Logical_Layout_Add_Alternative
                         (analysis, &site, tensor,
                          DSL_LOGICAL_LAYOUT_BLOCKED, identity,
                          block_axes, block_factors, diagnostic))
                    return FALSE;
            }
        }
        if (site.alternative_count == 0)
            continue;
        if (!DSL_Logical_Layout_Build_Plans
                 (analysis, &site, tensor, diagnostic))
            return FALSE;
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_Logical_Layout_Verify(analysis, diagnostic);
}

static BOOL
DSL_Logical_Layout_Verify_Descriptor
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor,
         FILE *diagnostic)
{
    if (descriptor.id == 0 ||
        descriptor.id > analysis->descriptors.size() ||
        TY_IDX_index(descriptor.source_descriptor_ty) == 0 ||
        !TY_is_tensor_extension(descriptor.source_descriptor_ty) ||
        !TY_tensor_is_canonical(descriptor.source_descriptor_ty) ||
        descriptor.kind < DSL_LOGICAL_LAYOUT_PERMUTED ||
        descriptor.kind > DSL_LOGICAL_LAYOUT_DOMAIN ||
        descriptor.rank == 0 ||
        descriptor.rank !=
            (UINT32)TY_tensor_rank(descriptor.source_descriptor_ty) ||
        descriptor.first_axis_id == 0 ||
        descriptor.axis_count != descriptor.rank ||
        descriptor.first_axis_id + descriptor.axis_count - 1 >
            analysis->axes.size() ||
        (descriptor.block_count == 0) !=
            (descriptor.first_block_id == 0) ||
        (descriptor.block_count != 0 &&
         descriptor.first_block_id + descriptor.block_count - 1 >
             analysis->blocks.size()) ||
        (descriptor.kind == DSL_LOGICAL_LAYOUT_BLOCKED) !=
            (descriptor.block_count != 0) ||
        descriptor.minimum_alignment == 0 ||
        descriptor.flags !=
            (DSL_LOGICAL_LAYOUT_FLAG_SEMANTICS_PRESERVING |
             DSL_LOGICAL_LAYOUT_FLAG_LOGICAL_ONLY) ||
        descriptor.reserved != 0)
        return DSL_Logical_Layout_Report
                   (diagnostic, "invalid layout descriptor", descriptor.id);
    std::vector<BOOL> seen(descriptor.rank, FALSE);
    for (UINT32 i = 0; i < descriptor.axis_count; ++i) {
        const DSL_LOGICAL_LAYOUT_AXIS_RECORD &axis =
            analysis->axes[descriptor.first_axis_id - 1 + i];
        if (axis.descriptor_id != descriptor.id || axis.ordinal != i ||
            axis.source_axis >= descriptor.rank ||
            axis.result_axis != i || seen[axis.source_axis] ||
            axis.reserved != 0)
            return DSL_Logical_Layout_Report
                       (diagnostic, "invalid layout axis", axis.id);
        seen[axis.source_axis] = TRUE;
    }
    std::vector<BOOL> blocked(descriptor.rank, FALSE);
    for (UINT32 i = 0; i < descriptor.block_count; ++i) {
        const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &block =
            analysis->blocks[descriptor.first_block_id - 1 + i];
        if (block.descriptor_id != descriptor.id ||
            block.axis >= descriptor.rank || blocked[block.axis] ||
            block.factor <= 1 || block.reserved0 != 0 ||
            block.reserved1 != 0)
            return DSL_Logical_Layout_Report
                       (diagnostic, "invalid layout block", block.id);
        blocked[block.axis] = TRUE;
    }
    return TRUE;
}

static BOOL
DSL_Logical_Layout_Descriptors_Equal
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &left,
         const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &right)
{
    if (left.source_descriptor_ty != right.source_descriptor_ty ||
        left.kind != right.kind || left.rank != right.rank ||
        left.axis_count != right.axis_count ||
        left.block_count != right.block_count)
        return FALSE;
    for (UINT32 i = 0; i < left.axis_count; ++i) {
        const DSL_LOGICAL_LAYOUT_AXIS_RECORD &left_axis =
            analysis->axes[left.first_axis_id - 1 + i];
        const DSL_LOGICAL_LAYOUT_AXIS_RECORD &right_axis =
            analysis->axes[right.first_axis_id - 1 + i];
        if (left_axis.source_axis != right_axis.source_axis ||
            left_axis.result_axis != right_axis.result_axis)
            return FALSE;
    }
    for (UINT32 i = 0; i < left.block_count; ++i) {
        const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &left_block =
            analysis->blocks[left.first_block_id - 1 + i];
        const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &right_block =
            analysis->blocks[right.first_block_id - 1 + i];
        if (left_block.axis != right_block.axis ||
            left_block.factor != right_block.factor)
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_Logical_Layout_Verify
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Logical_Layout_Active(analysis) || !analysis->built ||
        !DSL_Logical_Layout_Control_Valid(analysis->control) ||
        !DSL_Tensor_Evolution_Verify(analysis->graph, diagnostic) ||
        !DSL_Tensor_Analysis_Verify
             (analysis->tensor_analysis, diagnostic) ||
        !DSL_Tensor_Locality_Verify(analysis->locality, diagnostic) ||
        analysis->sites.size() != analysis->plans.size() ||
        analysis->sites.size() > analysis->control.max_sites)
        return DSL_Logical_Layout_Report(diagnostic, "invalid analysis", 0);
    UINT32 expected_axis = 1;
    UINT32 expected_block = 1;
    for (UINT32 i = 0; i < analysis->descriptors.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor =
            analysis->descriptors[i];
        if (descriptor.id != i + 1 ||
            descriptor.first_axis_id != expected_axis ||
            (descriptor.block_count != 0 &&
             descriptor.first_block_id != expected_block) ||
            !DSL_Logical_Layout_Verify_Descriptor
                 (analysis, descriptor, diagnostic))
            return FALSE;
        for (UINT32 j = 0; j < i; ++j) {
            if (DSL_Logical_Layout_Descriptors_Equal
                    (analysis, descriptor, analysis->descriptors[j]))
                return DSL_Logical_Layout_Report
                           (diagnostic, "duplicate layout descriptor",
                            descriptor.id);
        }
        expected_axis += descriptor.axis_count;
        expected_block += descriptor.block_count;
    }
    if (expected_axis != analysis->axes.size() + 1 ||
        expected_block != analysis->blocks.size() + 1)
        return DSL_Logical_Layout_Report
                   (diagnostic, "orphan layout detail", 0);

    UINT32 expected_alternative = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_SITE_RECORD &site = analysis->sites[i];
        DSL_TENSOR_FACT_RECORD tensor;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_alternative_id != expected_alternative ||
            site.alternative_count == 0 ||
            site.alternative_count >
                analysis->control.max_alternatives_per_site ||
            site.baseline_candidate_id == 0 || site.baseline_plan_id == 0 ||
            !DSL_Tensor_Analysis_Find_Fact
                 (analysis->tensor_analysis, site.semantic_value_id,
                  &tensor) ||
            tensor.semantic_root_id != site.semantic_root_id ||
            (analysis->control.select_plans &&
             site.selected_plan_id != site.baseline_plan_id) ||
            (!analysis->control.select_plans &&
             site.selected_plan_id != 0) ||
            !DSL_Opt_Plan_Verify(analysis->plans[i], diagnostic))
            return DSL_Logical_Layout_Report
                       (diagnostic, "invalid layout site", site.id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            const DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[expected_alternative - 1];
            DSL_TENSOR_EVOLUTION_NODE_RECORD node;
            DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
            if (alternative.id != expected_alternative ||
                alternative.site_id != site.id ||
                alternative.descriptor_id == 0 ||
                alternative.descriptor_id > analysis->descriptors.size() ||
                !DSL_Tensor_Evolution_Get_Node
                     (analysis->graph,
                      alternative.result_evolution_node_id, &node) ||
                !DSL_Tensor_Evolution_Get_Edge
                     (analysis->graph, alternative.evolution_edge_id,
                      &edge) ||
                node.kind !=
                    DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT ||
                node.semantic_root_id != site.semantic_root_id ||
                node.representation_descriptor_id !=
                    alternative.descriptor_id ||
                analysis->descriptors[alternative.descriptor_id - 1].
                    source_descriptor_ty != tensor.descriptor_ty ||
                edge.source_node_id != site.semantic_root_id ||
                edge.result_node_id != node.id ||
                edge.transformation_kind !=
                    DSL_TENSOR_EVOLUTION_TRANSFORM_LOGICAL_LAYOUT ||
                edge.flags !=
                    DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING ||
                alternative.compatibility_state >
                    DSL_LAYOUT_COMPATIBILITY_REJECTED ||
                alternative.conversion_state >
                    DSL_LAYOUT_CONVERSION_KNOWN ||
                alternative.candidate_id == 0 || alternative.plan_id == 0 ||
                alternative.reserved != 0 ||
                (alternative.conversion_state ==
                     DSL_LAYOUT_CONVERSION_KNOWN &&
                 alternative.conversion_bytes ==
                     DSL_LOGICAL_LAYOUT_UNKNOWN_U64) ||
                (alternative.conversion_state ==
                     DSL_LAYOUT_CONVERSION_UNKNOWN &&
                 alternative.conversion_bytes !=
                     DSL_LOGICAL_LAYOUT_UNKNOWN_U64))
                return DSL_Logical_Layout_Report
                           (diagnostic, "invalid layout alternative",
                            alternative.id);
            if ((alternative.legality == DSL_OPT_LEGALITY_PROVEN &&
                 (alternative.compatibility_state !=
                      DSL_LAYOUT_COMPATIBILITY_PROVEN ||
                  alternative.conversion_state !=
                      DSL_LAYOUT_CONVERSION_KNOWN ||
                  alternative.rejection_reason != DSL_OPT_REJECT_NONE)) ||
                (alternative.legality == DSL_OPT_LEGALITY_UNKNOWN &&
                 alternative.rejection_reason !=
                     DSL_OPT_REJECT_INCOMPLETE_ANALYSIS) ||
                (alternative.legality == DSL_OPT_LEGALITY_REJECTED &&
                 alternative.rejection_reason == DSL_OPT_REJECT_NONE))
                return DSL_Logical_Layout_Report
                           (diagnostic, "inconsistent layout legality",
                            alternative.id);
            ++expected_alternative;
        }
    }
    if (expected_alternative != analysis->alternatives.size() + 1)
        return DSL_Logical_Layout_Report
                   (diagnostic, "orphan layout alternative", 0);
    return TRUE;
}

static void
DSL_Logical_Layout_Print_U64 (FILE *file, UINT64 value)
{
    if (value == DSL_LOGICAL_LAYOUT_UNKNOWN_U64)
        fprintf(file, "<unknown>");
    else
        fprintf(file, "%llu", (unsigned long long)value);
}

void
DSL_Logical_Layout_Print
        (FILE *file, const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file,
            "DSLLogicalLayouts: owner=<%u,%u> descriptors=%u sites=%u "
            "alternatives=%u generate=%s select=%s apply=%s target=%u\n",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            (UINT32)analysis->descriptors.size(),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->alternatives.size(),
            analysis->control.generate_candidates ? "yes" : "no",
            analysis->control.select_plans ? "yes" : "no",
            analysis->control.apply_transformation ? "yes" : "no",
            analysis->control.target_profile_id);
    for (UINT32 i = 0; i < analysis->descriptors.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD &descriptor =
            analysis->descriptors[i];
        fprintf(file, "  descriptor %u kind=%s source_ty=%u rank=%u axes=[",
                descriptor.id,
                DSL_Logical_Layout_Kind_Name(descriptor.kind),
                TY_IDX_index(descriptor.source_descriptor_ty),
                descriptor.rank);
        for (UINT32 j = 0; j < descriptor.axis_count; ++j) {
            const DSL_LOGICAL_LAYOUT_AXIS_RECORD &axis =
                analysis->axes[descriptor.first_axis_id - 1 + j];
            fprintf(file, "%s%u", j == 0 ? "" : ",", axis.source_axis);
        }
        fprintf(file, "] blocks=[");
        for (UINT32 j = 0; j < descriptor.block_count; ++j) {
            const DSL_LOGICAL_LAYOUT_BLOCK_RECORD &block =
                analysis->blocks[descriptor.first_block_id - 1 + j];
            fprintf(file, "%s%u:%u", j == 0 ? "" : ",", block.axis,
                    block.factor);
        }
        fprintf(file, "] alignment=%u flags=0x%x\n",
                descriptor.minimum_alignment, descriptor.flags);
    }
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_LOGICAL_LAYOUT_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  site %u value=%u root=%u alternatives=%u "
                "baseline=%u/%u selected=%u\n",
                site.id, site.semantic_value_id, site.semantic_root_id,
                site.alternative_count, site.baseline_candidate_id,
                site.baseline_plan_id, site.selected_plan_id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            const DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[site.first_alternative_id - 1 + j];
            fprintf(file,
                    "    alternative %u descriptor=%u evolution=%u/%u "
                    "compatibility=%s conversion=%s bytes=",
                    alternative.id, alternative.descriptor_id,
                    alternative.result_evolution_node_id,
                    alternative.evolution_edge_id,
                    DSL_Layout_Compatibility_Name
                        (alternative.compatibility_state),
                    DSL_Layout_Conversion_Name
                        (alternative.conversion_state));
            DSL_Logical_Layout_Print_U64
                (file, alternative.conversion_bytes);
            fprintf(file, " legality=%s reason=%s candidate=%u plan=%u\n",
                    DSL_Opt_Legality_Name(alternative.legality),
                    DSL_Opt_Rejection_Reason_Name
                        (alternative.rejection_reason),
                    alternative.candidate_id, alternative.plan_id);
        }
        DSL_Opt_Plan_Print(file, analysis->plans[i]);
    }
}

UINT32
DSL_Logical_Layout_Descriptor_Count
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->descriptors.size();
}

UINT32
DSL_Logical_Layout_Axis_Count
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->axes.size();
}

UINT32
DSL_Logical_Layout_Block_Count
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->blocks.size();
}

UINT32
DSL_Logical_Layout_Site_Count
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_Logical_Layout_Alternative_Count
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->alternatives.size();
}

BOOL
DSL_Logical_Layout_Get_Descriptor
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID id,
         DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->descriptors.size())
        return FALSE;
    *record = analysis->descriptors[id - 1];
    return TRUE;
}

BOOL
DSL_Logical_Layout_Get_Axis
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_AXIS_ID id,
         DSL_LOGICAL_LAYOUT_AXIS_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->axes.size())
        return FALSE;
    *record = analysis->axes[id - 1];
    return TRUE;
}

BOOL
DSL_Logical_Layout_Get_Block
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_BLOCK_ID id,
         DSL_LOGICAL_LAYOUT_BLOCK_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->blocks.size())
        return FALSE;
    *record = analysis->blocks[id - 1];
    return TRUE;
}

BOOL
DSL_Logical_Layout_Get_Site
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_SITE_ID id,
         DSL_LOGICAL_LAYOUT_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->sites.size())
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_Logical_Layout_Find_Site
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_IR_VALUE_ID semantic_value_id,
         DSL_LOGICAL_LAYOUT_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || semantic_value_id == 0)
        return FALSE;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        if (analysis->sites[i].semantic_value_id == semantic_value_id) {
            *record = analysis->sites[i];
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_Logical_Layout_Get_Alternative
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID id,
         DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->alternatives.size())
        return FALSE;
    *record = analysis->alternatives[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_Logical_Layout_Get_Plan_Context
        (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
         DSL_LOGICAL_LAYOUT_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
