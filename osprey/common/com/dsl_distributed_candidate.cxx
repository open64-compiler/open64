/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Builds AIO-7 PU-local placement, sharding, ownership, and communication
 * candidates. The analysis is runtime-only and does not rewrite WHIRL or alter
 * mapped-image contracts. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md.
 */

#include <string.h>
#include <vector>

#include "dsl_distributed_candidate.h"
#include "dsl_opcode.h"
#include "dsl_shape.h"
#include "pu_info.h"

struct DSL_DISTRIBUTED_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    DSL_DISTRIBUTED_CONTROL control;
    std::vector<DSL_DISTRIBUTED_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_DISTRIBUTED_ALIAS_RECORD> aliases;
    std::vector<DSL_DISTRIBUTED_RANGE_RECORD> ranges;
    std::vector<DSL_DISTRIBUTED_SITE_RECORD> sites;
    std::vector<DSL_DISTRIBUTED_ALTERNATIVE_RECORD> alternatives;
    std::vector<DSL_COMMUNICATION_EPOCH_RECORD> epochs;
    std::vector<DSL_COMMUNICATION_INTENT_RECORD> intents;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    BOOL built;
};

static const char *DSL_placement_kind_name[] = {
    "unknown", "replicated", "partitioned", "remote_single"
};

static const char *DSL_sharding_kind_name[] = {
    "unknown", "replicated", "axis", "partial_reduction", "migrated"
};

static const char *DSL_distributed_ownership_name[] = {
    "unknown", "disjoint", "replicated", "reduced", "migrated"
};

static const char *DSL_distributed_range_state_name[] = {
    "unknown", "exact"
};

static const char *DSL_distributed_disjoint_state_name[] = {
    "unknown", "proven", "overlap"
};

static const char *DSL_communication_kind_name[] = {
    "unknown", "all_gather", "scatter", "all_reduce", "reduce_scatter",
    "all_to_all", "peer_copy"
};

static BOOL
DSL_Distributed_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL distributed analysis error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Distributed_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Distributed_Active (const DSL_DISTRIBUTED_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Distributed_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_Placement_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_placement_kind_name) /
                      sizeof(DSL_placement_kind_name[0]) ?
           DSL_placement_kind_name[kind] : "unknown";
}

const char *
DSL_Sharding_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_sharding_kind_name) /
                      sizeof(DSL_sharding_kind_name[0]) ?
           DSL_sharding_kind_name[kind] : "unknown";
}

const char *
DSL_Distributed_Ownership_Name (UINT32 kind)
{
    return kind < sizeof(DSL_distributed_ownership_name) /
                      sizeof(DSL_distributed_ownership_name[0]) ?
           DSL_distributed_ownership_name[kind] : "unknown";
}

const char *
DSL_Distributed_Range_State_Name (UINT32 state)
{
    return state < sizeof(DSL_distributed_range_state_name) /
                       sizeof(DSL_distributed_range_state_name[0]) ?
           DSL_distributed_range_state_name[state] : "unknown";
}

const char *
DSL_Distributed_Disjoint_State_Name (UINT32 state)
{
    return state < sizeof(DSL_distributed_disjoint_state_name) /
                       sizeof(DSL_distributed_disjoint_state_name[0]) ?
           DSL_distributed_disjoint_state_name[state] : "unknown";
}

const char *
DSL_Communication_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_communication_kind_name) /
                      sizeof(DSL_communication_kind_name[0]) ?
           DSL_communication_kind_name[kind] : "unknown";
}

void
DSL_Distributed_Control_Init (DSL_DISTRIBUTED_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->derive_communication = 1;
    control->target_profile_id = 1;
    control->device_count = 2;
    control->max_sites = 64;
    control->max_alternatives_per_site = 4;
    control->enable_replication = 1;
    control->enable_axis_sharding = 1;
    control->enable_partial_reduction = 1;
    control->enable_migration = 1;
}

static BOOL
DSL_Distributed_Control_Valid (const DSL_DISTRIBUTED_CONTROL &control)
{
    return control.reserved == 0 && control.generate_candidates <= 1 &&
           control.derive_communication <= 1 && control.select_plans <= 1 &&
           control.apply_transformation <= 1 &&
           control.enable_replication <= 1 &&
           control.enable_axis_sharding <= 1 &&
           control.enable_partial_reduction <= 1 &&
           control.enable_migration <= 1 && control.target_profile_id != 0 &&
           control.device_count >= 2 && control.max_sites != 0 &&
           control.max_alternatives_per_site != 0 &&
           (!control.select_plans || control.generate_candidates) &&
           control.apply_transformation == 0;
}

static BOOL
DSL_Distributed_Node_Info
        (DSL_IR_NODE_ID node_id, DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    return node != NULL && descriptor != NULL &&
           DSL_IR_Image_Get_Node(node_id, node) &&
           (node->flags & DSL_IR_NODE_FLAG_RETIRED) == 0 &&
           DSL_IR_Image_Get_Opcode_Descriptor
               (node->opcode_descriptor_id, descriptor);
}

static BOOL
DSL_Distributed_Node_Has_State_Effect (DSL_IR_NODE_ID node_id)
{
    for (DSL_STATE_EFFECT_ID id = 1;
         id <= DSL_Effect_Image_State_Effect_Count(); ++id) {
        DSL_STATE_EFFECT_RECORD effect;
        if (DSL_Effect_Image_Get_State_Effect(id, &effect) &&
            effect.owner_node_id == node_id)
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Distributed_Static_Dimension
        (const DSL_TENSOR_FACT_RECORD &tensor, UINT32 axis, UINT64 *dimension)
{
    if (dimension != NULL)
        *dimension = DSL_DISTRIBUTED_UNKNOWN_U64;
    if (dimension == NULL || tensor.rank <= 0 ||
        axis >= (UINT32)tensor.rank ||
        tensor.dimension_state != DSL_TENSOR_DIMENSION_STATIC)
        return FALSE;
    const char *shape = TY_tensor_attribute
                            (tensor.descriptor_ty, TY_TENSOR_SCHEMA_SHAPE);
    std::vector<UINT64> dimensions(tensor.rank);
    UINT32 parsed_rank = 0;
    if (shape == NULL ||
        !DSL_Shape_Parse_Static_Dimensions
             (shape, &dimensions[0], dimensions.size(), &parsed_rank) ||
        parsed_rank != (UINT32)tensor.rank)
        return FALSE;
    *dimension = dimensions[axis];
    return TRUE;
}

static BOOL
DSL_Distributed_Add_Range
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         DSL_DISTRIBUTED_ALIAS_ID alias_id, UINT32 device_ordinal,
         UINT32 axis, UINT64 lower, UINT64 upper, UINT32 state)
{
    DSL_DISTRIBUTED_RANGE_RECORD range;
    memset(&range, 0, sizeof(range));
    range.id = analysis->ranges.size() + 1;
    range.alias_id = alias_id;
    range.device_ordinal = device_ordinal;
    range.axis = axis;
    range.lower = lower;
    range.upper = upper;
    range.state = state;
    analysis->ranges.push_back(range);
    return TRUE;
}

static BOOL
DSL_Distributed_Add_Descriptor
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_COMMUNICATION_EPOCH_ID epoch_id, UINT32 placement_kind,
         UINT32 sharding_kind, UINT32 shard_axis,
         DSL_DISTRIBUTED_DESCRIPTOR_ID *descriptor_id,
         FILE *diagnostic)
{
    UINT64 dimension = DSL_DISTRIBUTED_UNKNOWN_U64;
    BOOL dimension_known = DSL_Distributed_Static_Dimension
                               (tensor, shard_axis == DSL_DISTRIBUTED_NO_AXIS ?
                                    0 : shard_axis, &dimension);
    DSL_DISTRIBUTED_DESCRIPTOR_RECORD descriptor;
    DSL_DISTRIBUTED_ALIAS_RECORD alias;
    memset(&descriptor, 0, sizeof(descriptor));
    memset(&alias, 0, sizeof(alias));
    descriptor.id = analysis->descriptors.size() + 1;
    descriptor.source_descriptor_ty = tensor.descriptor_ty;
    descriptor.placement_kind = placement_kind;
    descriptor.sharding_kind = sharding_kind;
    descriptor.device_count = analysis->control.device_count;
    descriptor.shard_axis = shard_axis;
    descriptor.alias_id = analysis->aliases.size() + 1;
    descriptor.flags = DSL_DISTRIBUTED_FLAG_PROVISIONAL |
                       DSL_DISTRIBUTED_FLAG_SEMANTICS_PRESERVING;
    alias.id = descriptor.alias_id;
    alias.descriptor_id = descriptor.id;
    alias.first_range_id = analysis->ranges.size() + 1;
    alias.visibility_epoch = epoch_id;

    /*
     * Ownership and exact per-device ranges are primary. Communication is
     * derived later from this descriptor, never accepted as an unrelated
     * frontend annotation.
     */
    UINT32 range_count = analysis->control.device_count;
    if (sharding_kind == DSL_SHARDING_MIGRATED)
        range_count = 1;
    alias.range_count = range_count;
    if (sharding_kind == DSL_SHARDING_AXIS && dimension_known &&
        dimension % analysis->control.device_count == 0) {
        descriptor.ownership = DSL_DISTRIBUTED_OWNERSHIP_DISJOINT;
        alias.ownership = descriptor.ownership;
        alias.disjoint_state = DSL_DISTRIBUTED_DISJOINT_PROVEN;
        UINT64 chunk = dimension / analysis->control.device_count;
        for (UINT32 i = 0; i < range_count; ++i)
            DSL_Distributed_Add_Range
                (analysis, alias.id, i, shard_axis, i * chunk,
                 (i + 1) * chunk, DSL_DISTRIBUTED_RANGE_EXACT);
    } else if (sharding_kind == DSL_SHARDING_AXIS) {
        descriptor.ownership = DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN;
        alias.ownership = descriptor.ownership;
        alias.disjoint_state = DSL_DISTRIBUTED_DISJOINT_UNKNOWN;
        for (UINT32 i = 0; i < range_count; ++i)
            DSL_Distributed_Add_Range
                (analysis, alias.id, i, shard_axis,
                 DSL_DISTRIBUTED_UNKNOWN_U64,
                 DSL_DISTRIBUTED_UNKNOWN_U64,
                 DSL_DISTRIBUTED_RANGE_UNKNOWN);
    } else if (sharding_kind == DSL_SHARDING_REPLICATED) {
        descriptor.ownership = DSL_DISTRIBUTED_OWNERSHIP_REPLICATED;
        alias.ownership = descriptor.ownership;
        alias.disjoint_state = dimension_known ?
            DSL_DISTRIBUTED_DISJOINT_OVERLAP :
            DSL_DISTRIBUTED_DISJOINT_UNKNOWN;
        for (UINT32 i = 0; i < range_count; ++i)
            DSL_Distributed_Add_Range
                (analysis, alias.id, i, shard_axis, dimension_known ? 0 :
                 DSL_DISTRIBUTED_UNKNOWN_U64,
                 dimension_known ? dimension : DSL_DISTRIBUTED_UNKNOWN_U64,
                 dimension_known ? DSL_DISTRIBUTED_RANGE_EXACT :
                                   DSL_DISTRIBUTED_RANGE_UNKNOWN);
    } else if (sharding_kind == DSL_SHARDING_PARTIAL_REDUCTION) {
        descriptor.ownership = dimension_known ?
            DSL_DISTRIBUTED_OWNERSHIP_REDUCED :
            DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN;
        alias.ownership = descriptor.ownership;
        alias.disjoint_state = dimension_known ?
            DSL_DISTRIBUTED_DISJOINT_OVERLAP :
            DSL_DISTRIBUTED_DISJOINT_UNKNOWN;
        for (UINT32 i = 0; i < range_count; ++i)
            DSL_Distributed_Add_Range
                (analysis, alias.id, i, shard_axis, dimension_known ? 0 :
                 DSL_DISTRIBUTED_UNKNOWN_U64,
                 dimension_known ? dimension : DSL_DISTRIBUTED_UNKNOWN_U64,
                 dimension_known ? DSL_DISTRIBUTED_RANGE_EXACT :
                                   DSL_DISTRIBUTED_RANGE_UNKNOWN);
    } else if (sharding_kind == DSL_SHARDING_MIGRATED) {
        descriptor.ownership = dimension_known ?
            DSL_DISTRIBUTED_OWNERSHIP_MIGRATED :
            DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN;
        alias.ownership = descriptor.ownership;
        alias.disjoint_state = dimension_known ?
            DSL_DISTRIBUTED_DISJOINT_PROVEN :
            DSL_DISTRIBUTED_DISJOINT_UNKNOWN;
        DSL_Distributed_Add_Range
            (analysis, alias.id, 1, shard_axis, dimension_known ? 0 :
             DSL_DISTRIBUTED_UNKNOWN_U64,
             dimension_known ? dimension : DSL_DISTRIBUTED_UNKNOWN_U64,
             dimension_known ? DSL_DISTRIBUTED_RANGE_EXACT :
                               DSL_DISTRIBUTED_RANGE_UNKNOWN);
    } else {
        return DSL_Distributed_Report
                   (diagnostic, "unsupported distributed descriptor",
                    descriptor.id);
    }
    analysis->descriptors.push_back(descriptor);
    analysis->aliases.push_back(alias);
    *descriptor_id = descriptor.id;
    return TRUE;
}

static UINT32
DSL_Distributed_Communication_Kind (UINT32 sharding_kind)
{
    switch (sharding_kind) {
    case DSL_SHARDING_REPLICATED:
        return DSL_COMMUNICATION_ALL_GATHER;
    case DSL_SHARDING_AXIS:
        return DSL_COMMUNICATION_SCATTER;
    case DSL_SHARDING_PARTIAL_REDUCTION:
        return DSL_COMMUNICATION_ALL_REDUCE;
    case DSL_SHARDING_MIGRATED:
        return DSL_COMMUNICATION_PEER_COPY;
    default:
        return DSL_COMMUNICATION_UNKNOWN;
    }
}

static BOOL
DSL_Distributed_Descriptor_Kinds_Valid
        (const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor)
{
    switch (descriptor.sharding_kind) {
    case DSL_SHARDING_REPLICATED:
        return descriptor.placement_kind == DSL_PLACEMENT_REPLICATED &&
               descriptor.ownership ==
                   DSL_DISTRIBUTED_OWNERSHIP_REPLICATED;
    case DSL_SHARDING_AXIS:
        return descriptor.placement_kind == DSL_PLACEMENT_PARTITIONED &&
               (descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_DISJOINT ||
                descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN);
    case DSL_SHARDING_PARTIAL_REDUCTION:
        return descriptor.placement_kind == DSL_PLACEMENT_PARTITIONED &&
               (descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_REDUCED ||
                descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN);
    case DSL_SHARDING_MIGRATED:
        return descriptor.placement_kind == DSL_PLACEMENT_REMOTE_SINGLE &&
               (descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_MIGRATED ||
                descriptor.ownership ==
                    DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN);
    default:
        return FALSE;
    }
}

static UINT64
DSL_Distributed_Communication_Bytes
        (const DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         UINT32 sharding_kind)
{
    if (locality.size_state != DSL_TENSOR_SIZE_STATIC ||
        locality.object_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
        return DSL_DISTRIBUTED_UNKNOWN_U64;
    if (sharding_kind == DSL_SHARDING_REPLICATED) {
        if (locality.object_bytes > DSL_DISTRIBUTED_UNKNOWN_U64 /
                                      (analysis->control.device_count - 1))
            return DSL_DISTRIBUTED_UNKNOWN_U64;
        return locality.object_bytes * (analysis->control.device_count - 1);
    }
    if (sharding_kind == DSL_SHARDING_PARTIAL_REDUCTION) {
        if (locality.object_bytes > DSL_DISTRIBUTED_UNKNOWN_U64 /
                                      analysis->control.device_count)
            return DSL_DISTRIBUTED_UNKNOWN_U64;
        return locality.object_bytes * analysis->control.device_count;
    }
    return locality.object_bytes;
}

static void
DSL_Distributed_Classify
        (const DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &producer,
         const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor,
         DSL_DISTRIBUTED_ALTERNATIVE_RECORD *alternative)
{
    const DSL_DISTRIBUTED_ALIAS_RECORD &alias =
        analysis->aliases[descriptor.alias_id - 1];
    alternative->communication_bytes = DSL_Distributed_Communication_Bytes
                                           (analysis, locality,
                                            descriptor.sharding_kind);
    if (producer.effect_model != DSL_EFFECT_MODEL_PURE ||
        DSL_Distributed_Node_Has_State_Effect(tensor.producer_node_id) ||
        locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_EFFECT;
    } else if ((descriptor.sharding_kind == DSL_SHARDING_AXIS ||
                descriptor.sharding_kind == DSL_SHARDING_MIGRATED) &&
               locality.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_OWNERSHIP;
    } else if (descriptor.sharding_kind ==
                   DSL_SHARDING_PARTIAL_REDUCTION &&
               producer.shape_rule != DSL_SHAPE_RULE_CONTRACTION) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_DESCRIPTOR;
    } else if (descriptor.sharding_kind == DSL_SHARDING_AXIS &&
               tensor.dimension_state == DSL_TENSOR_DIMENSION_STATIC &&
               descriptor.ownership !=
                   DSL_DISTRIBUTED_OWNERSHIP_DISJOINT) {
        alternative->legality = DSL_OPT_LEGALITY_REJECTED;
        alternative->rejection_reason = DSL_OPT_REJECT_DESCRIPTOR;
    } else if (locality.lifetime_state != DSL_TENSOR_LIFETIME_EXACT_BLOCK ||
               alias.disjoint_state == DSL_DISTRIBUTED_DISJOINT_UNKNOWN ||
               alternative->communication_bytes ==
                   DSL_DISTRIBUTED_UNKNOWN_U64 ||
               !analysis->control.derive_communication) {
        alternative->legality = DSL_OPT_LEGALITY_UNKNOWN;
        alternative->rejection_reason =
            DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else {
        alternative->legality = DSL_OPT_LEGALITY_PROVEN;
        alternative->rejection_reason = DSL_OPT_REJECT_NONE;
    }
}

static void
DSL_Distributed_Cost_Term
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
DSL_Distributed_Unknown_Cost (DSL_OPT_COST_TERM *term)
{
    memset(term, 0, sizeof(*term));
}

static BOOL
DSL_Distributed_Build_Baseline_Plan
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_DISTRIBUTED_SITE_RECORD *site,
         DSL_OPT_PLAN_CONTEXT **context_out, FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;
    DSL_OPT_PLAN_BUDGET budget;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_CANDIDATE_ID member;
    if (!DSL_Tensor_Evolution_Find_Semantic_Root
             (analysis->graph, tensor.value_id, &root))
        return DSL_Distributed_Report
                   (diagnostic, "missing semantic root", tensor.value_id);
    budget.max_candidates = analysis->control.max_alternatives_per_site + 1;
    budget.max_plans = analysis->control.max_alternatives_per_site + 1;
    DSL_OPT_PLAN_CONTEXT *context = DSL_Opt_Plan_Create
                                        (analysis->pu, analysis->graph,
                                         &budget, diagnostic);
    if (context == NULL)
        return FALSE;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = tensor.producer_node_id;
    candidate.source_evolution_node_id = root.id;
    candidate.result_evolution_node_id = root.id;
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
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMPUTE], 1000,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    for (UINT32 i = DSL_OPT_COST_MEMORY_UNHIDDEN;
         i < DSL_OPT_COST_TERM_COUNT; ++i)
        DSL_Distributed_Cost_Term
            (&cost.terms[i], 0, DSL_OPT_COST_CONFIDENCE_MEDIUM,
             DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    if (!DSL_Opt_Plan_Add_Cost(context, &cost, &cost_id, diagnostic)) {
        DSL_Opt_Plan_Destroy(context);
        return FALSE;
    }
    member = site->baseline_candidate_id;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
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
    *context_out = context;
    return TRUE;
}

static BOOL
DSL_Distributed_Add_Alternative_Plan
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         DSL_OPT_PLAN_CONTEXT *context,
         const DSL_DISTRIBUTED_SITE_RECORD &site,
         const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor,
         DSL_DISTRIBUTED_ALTERNATIVE_RECORD *alternative,
         UINT32 ordinal, FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_CANDIDATE_ID member;
    if (!DSL_Tensor_Evolution_Find_Semantic_Root
             (analysis->graph, site.semantic_value_id, &root))
        return FALSE;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = descriptor.sharding_kind == DSL_SHARDING_AXIS ||
                     descriptor.sharding_kind ==
                         DSL_SHARDING_PARTIAL_REDUCTION ?
                     DSL_OPT_CANDIDATE_SHARDING :
                     DSL_OPT_CANDIDATE_PLACEMENT;
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Get_Value(site.semantic_value_id, &value))
        return FALSE;
    candidate.semantic_node_id = value.producer_node_id;
    candidate.source_evolution_node_id = root.id;
    candidate.result_evolution_node_id =
        alternative->result_evolution_node_id;
    candidate.parent_candidate_id = site.baseline_candidate_id;
    candidate.legality = alternative->legality;
    candidate.rejection_reason = alternative->rejection_reason;
    candidate.ordering_key = ordinal + 1;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_Opt_Plan_Add_Candidate
             (context, &candidate, &alternative->candidate_id,
              diagnostic))
        return FALSE;

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = ordinal + 1;
    UINT64 compute = descriptor.sharding_kind == DSL_SHARDING_AXIS ||
                     descriptor.sharding_kind ==
                         DSL_SHARDING_PARTIAL_REDUCTION ?
                     1000 / analysis->control.device_count : 1000;
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMPUTE], compute,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN], 0,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (alternative->legality == DSL_OPT_LEGALITY_PROVEN)
        DSL_Distributed_Cost_Term
            (&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN],
             alternative->communication_bytes,
             DSL_OPT_COST_CONFIDENCE_LOW,
             DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    else
        DSL_Distributed_Unknown_Cost
            (&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN]);
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_SYNC],
         descriptor.sharding_kind == DSL_SHARDING_PARTIAL_REDUCTION ? 20 : 10,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_LAUNCH], 0,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Distributed_Cost_Term
        (&cost.terms[DSL_OPT_COST_RUNTIME_SELECTION], 0,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (!DSL_Opt_Plan_Add_Cost(context, &cost, &cost_id, diagnostic))
        return FALSE;

    member = alternative->candidate_id;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id = site.baseline_plan_id;
    plan.legality = alternative->legality;
    plan.rejection_reason = alternative->rejection_reason;
    plan.ordering_key = ordinal + 1;
    plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    return DSL_Opt_Plan_Add_Plan
               (context, &plan, &alternative->plan_id, diagnostic);
}

static BOOL
DSL_Distributed_Add_Alternative
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &producer,
         DSL_DISTRIBUTED_SITE_RECORD *site,
         DSL_OPT_PLAN_CONTEXT *context, UINT32 placement_kind,
         UINT32 sharding_kind, UINT32 shard_axis, FILE *diagnostic)
{
    DSL_DISTRIBUTED_DESCRIPTOR_ID descriptor_id;
    if (!DSL_Distributed_Add_Descriptor
             (analysis, tensor, site->epoch_id, placement_kind,
              sharding_kind, shard_axis, &descriptor_id, diagnostic))
        return FALSE;
    const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor =
        analysis->descriptors[descriptor_id - 1];
    DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
    memset(&alternative, 0, sizeof(alternative));
    alternative.id = analysis->alternatives.size() + 1;
    alternative.site_id = site->id;
    alternative.descriptor_id = descriptor_id;
    DSL_Distributed_Classify
        (analysis, tensor, locality, producer, descriptor, &alternative);
    UINT32 transform = sharding_kind == DSL_SHARDING_REPLICATED ||
                       sharding_kind == DSL_SHARDING_MIGRATED ?
                       DSL_TENSOR_EVOLUTION_TRANSFORM_PLACE :
                       DSL_TENSOR_EVOLUTION_TRANSFORM_SHARD;
    if (!DSL_Tensor_Evolution_Add_Distributed
             (analysis->graph, site->semantic_root_id, descriptor_id,
              transform, &alternative.result_evolution_node_id,
              &alternative.evolution_edge_id, diagnostic))
        return FALSE;

    if (analysis->control.derive_communication) {
        DSL_COMMUNICATION_INTENT_RECORD intent;
        memset(&intent, 0, sizeof(intent));
        intent.id = analysis->intents.size() + 1;
        intent.alternative_id = alternative.id;
        intent.epoch_id = site->epoch_id;
        intent.kind = DSL_Distributed_Communication_Kind(sharding_kind);
        intent.source_count = sharding_kind == DSL_SHARDING_MIGRATED ? 1 :
                              analysis->control.device_count;
        intent.destination_count = sharding_kind == DSL_SHARDING_MIGRATED ?
                                   1 : analysis->control.device_count;
        intent.bytes = alternative.communication_bytes;
        intent.legality = alternative.legality;
        intent.rejection_reason = alternative.rejection_reason;
        alternative.communication_intent_id = intent.id;
        analysis->intents.push_back(intent);
    }
    if (!DSL_Distributed_Add_Alternative_Plan
             (analysis, context, *site, descriptor, &alternative,
              site->alternative_count + 1, diagnostic))
        return FALSE;
    analysis->alternatives.push_back(alternative);
    ++site->alternative_count;
    return TRUE;
}

DSL_DISTRIBUTED_ANALYSIS *
DSL_Distributed_Create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_DISTRIBUTED_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_Tensor_Evolution_Owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Distributed_Control_Valid(*control) ||
        !DSL_Tensor_Evolution_Verify(graph, diagnostic) ||
        !DSL_Tensor_Analysis_Verify(tensor_analysis, diagnostic) ||
        !DSL_Tensor_Locality_Verify(locality, diagnostic)) {
        DSL_Distributed_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_DISTRIBUTED_ANALYSIS *analysis = new DSL_DISTRIBUTED_ANALYSIS;
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
DSL_Distributed_Destroy (DSL_DISTRIBUTED_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_Opt_Plan_Destroy(analysis->plans[i]);
    delete analysis;
}

static BOOL
DSL_Distributed_Add_Epoch
        (DSL_DISTRIBUTED_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         DSL_COMMUNICATION_EPOCH_ID *epoch_id, FILE *diagnostic)
{
    DSL_COMMUNICATION_EPOCH_RECORD epoch;
    memset(&epoch, 0, sizeof(epoch));
    epoch.id = analysis->epochs.size() + 1;
    epoch.site_id = analysis->sites.size() + 1;
    epoch.semantic_value_id = tensor.value_id;
    epoch.producer_node_id = tensor.producer_node_id;
    epoch.last_consumer_node_id = locality.last_consumer_node_id;
    epoch.consumer_count = tensor.use_count;
    if (tensor.use_count != 0) {
        DSL_TENSOR_USE_FACT_RECORD use;
        if (!DSL_Tensor_Analysis_Get_Use
                 (analysis->tensor_analysis, tensor.first_use_id, &use))
            return DSL_Distributed_Report
                       (diagnostic, "missing first communication use",
                        tensor.value_id);
        epoch.first_consumer_node_id = use.consumer_node_id;
    }
    analysis->epochs.push_back(epoch);
    *epoch_id = epoch.id;
    return TRUE;
}

BOOL
DSL_Distributed_Build
        (DSL_DISTRIBUTED_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Distributed_Active(analysis) || analysis->built)
        return DSL_Distributed_Report
                   (diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_Tensor_Analysis_Fact_Count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality;
        DSL_IR_NODE_RECORD producer_node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD producer;
        if (!DSL_Tensor_Analysis_Get_Fact
                 (analysis->tensor_analysis, id, &tensor) ||
            tensor.value_role != DSL_TENSOR_VALUE_ROLE_INTERMEDIATE ||
            tensor.rank < 1 || tensor.producer_node_id == 0 ||
            (analysis->control.focus_value_id != 0 &&
             tensor.value_id != analysis->control.focus_value_id))
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Distributed_Report
                       (diagnostic, "distributed site budget exhausted", id);
        if (!DSL_Tensor_Locality_Find_Fact
                 (analysis->locality, tensor.value_id, &locality) ||
            !DSL_Distributed_Node_Info
                 (tensor.producer_node_id, &producer_node, &producer))
            return DSL_Distributed_Report
                       (diagnostic, "missing distributed site evidence", id);

        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        if (!DSL_Tensor_Evolution_Find_Semantic_Root
                 (analysis->graph, tensor.value_id, &root))
            return DSL_Distributed_Report
                       (diagnostic, "missing distributed root", id);
        DSL_DISTRIBUTED_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_value_id = tensor.value_id;
        site.semantic_root_id = root.id;
        site.first_alternative_id = analysis->alternatives.size() + 1;
        if (!DSL_Distributed_Add_Epoch
                 (analysis, tensor, locality, &site.epoch_id, diagnostic))
            return FALSE;
        DSL_OPT_PLAN_CONTEXT *context = NULL;
        if (!DSL_Distributed_Build_Baseline_Plan
                 (analysis, tensor, &site, &context, diagnostic))
            return FALSE;

        if (analysis->control.enable_replication &&
            site.alternative_count <
                analysis->control.max_alternatives_per_site &&
            !DSL_Distributed_Add_Alternative
                 (analysis, tensor, locality, producer, &site, context,
                  DSL_PLACEMENT_REPLICATED, DSL_SHARDING_REPLICATED,
                  0, diagnostic))
            return FALSE;
        if (analysis->control.enable_axis_sharding &&
            site.alternative_count <
                analysis->control.max_alternatives_per_site &&
            !DSL_Distributed_Add_Alternative
                 (analysis, tensor, locality, producer, &site, context,
                  DSL_PLACEMENT_PARTITIONED, DSL_SHARDING_AXIS,
                  0, diagnostic))
            return FALSE;
        if (analysis->control.enable_partial_reduction &&
            site.alternative_count <
                analysis->control.max_alternatives_per_site &&
            !DSL_Distributed_Add_Alternative
                 (analysis, tensor, locality, producer, &site, context,
                  DSL_PLACEMENT_PARTITIONED,
                  DSL_SHARDING_PARTIAL_REDUCTION, 0, diagnostic))
            return FALSE;
        if (analysis->control.enable_migration &&
            site.alternative_count <
                analysis->control.max_alternatives_per_site &&
            !DSL_Distributed_Add_Alternative
                 (analysis, tensor, locality, producer, &site, context,
                  DSL_PLACEMENT_REMOTE_SINGLE, DSL_SHARDING_MIGRATED,
                  0, diagnostic))
            return FALSE;

        if (analysis->control.select_plans) {
            DSL_OPT_SELECTION_RESULT selection;
            if (!DSL_Opt_Plan_Select
                     (context, analysis->control.target_profile_id,
                      &selection, diagnostic))
                return FALSE;
            site.selected_plan_id = selection.selected_plan_id;
        }
        analysis->plans.push_back(context);
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_Distributed_Verify(analysis, diagnostic);
}

BOOL
DSL_Distributed_Verify
        (const DSL_DISTRIBUTED_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Distributed_Active(analysis) || !analysis->built ||
        !DSL_Distributed_Control_Valid(analysis->control) ||
        analysis->sites.size() != analysis->plans.size() ||
        analysis->sites.size() != analysis->epochs.size() ||
        analysis->descriptors.size() != analysis->aliases.size() ||
        analysis->alternatives.size() != analysis->descriptors.size() ||
        (analysis->control.derive_communication &&
         analysis->intents.size() != analysis->alternatives.size()) ||
        (!analysis->control.derive_communication &&
         !analysis->intents.empty()))
        return DSL_Distributed_Report(diagnostic, "invalid analysis", 0);
    UINT32 expected_alternative = 1;
    UINT32 expected_range = 1;
    for (UINT32 i = 0; i < analysis->descriptors.size(); ++i) {
        const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor =
            analysis->descriptors[i];
        const DSL_DISTRIBUTED_ALIAS_RECORD &alias = analysis->aliases[i];
        if (descriptor.id != i + 1 || descriptor.alias_id != alias.id ||
            alias.id != i + 1 || alias.descriptor_id != descriptor.id ||
            TY_IDX_index(descriptor.source_descriptor_ty) == 0 ||
            !TY_is_tensor_extension(descriptor.source_descriptor_ty) ||
            !TY_tensor_is_canonical(descriptor.source_descriptor_ty) ||
            descriptor.placement_kind < DSL_PLACEMENT_REPLICATED ||
            descriptor.placement_kind > DSL_PLACEMENT_REMOTE_SINGLE ||
            descriptor.sharding_kind < DSL_SHARDING_REPLICATED ||
            descriptor.sharding_kind > DSL_SHARDING_MIGRATED ||
            descriptor.ownership > DSL_DISTRIBUTED_OWNERSHIP_MIGRATED ||
            !DSL_Distributed_Descriptor_Kinds_Valid(descriptor) ||
            TY_tensor_rank(descriptor.source_descriptor_ty) <= 0 ||
            descriptor.shard_axis >=
                (UINT32)TY_tensor_rank(descriptor.source_descriptor_ty) ||
            descriptor.device_count != analysis->control.device_count ||
            descriptor.flags !=
                (DSL_DISTRIBUTED_FLAG_PROVISIONAL |
                 DSL_DISTRIBUTED_FLAG_SEMANTICS_PRESERVING) ||
            descriptor.reserved != 0 ||
            alias.first_range_id != expected_range ||
            alias.range_count == 0 ||
            alias.ownership != descriptor.ownership ||
            alias.disjoint_state > DSL_DISTRIBUTED_DISJOINT_OVERLAP ||
            alias.visibility_epoch == 0 ||
            alias.visibility_epoch > analysis->epochs.size() ||
            alias.range_count !=
                (descriptor.sharding_kind == DSL_SHARDING_MIGRATED ?
                     1 : descriptor.device_count) ||
            alias.reserved != 0)
            return DSL_Distributed_Report
                       (diagnostic, "invalid distributed descriptor", i + 1);
        for (UINT32 j = 0; j < alias.range_count; ++j) {
            if (expected_range > analysis->ranges.size())
                return DSL_Distributed_Report
                           (diagnostic, "missing ownership range", alias.id);
            const DSL_DISTRIBUTED_RANGE_RECORD &range =
                analysis->ranges[expected_range - 1];
            if (range.id != expected_range || range.alias_id != alias.id ||
                range.axis != descriptor.shard_axis ||
                range.device_ordinal >= descriptor.device_count ||
                range.state > DSL_DISTRIBUTED_RANGE_EXACT ||
                range.reserved != 0 ||
                (range.state == DSL_DISTRIBUTED_RANGE_EXACT &&
                 (range.lower == DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.upper == DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.lower >= range.upper)) ||
                (range.state == DSL_DISTRIBUTED_RANGE_UNKNOWN &&
                 (range.lower != DSL_DISTRIBUTED_UNKNOWN_U64 ||
                  range.upper != DSL_DISTRIBUTED_UNKNOWN_U64)))
                return DSL_Distributed_Report
                           (diagnostic, "invalid ownership range", range.id);
            ++expected_range;
        }
    }
    if (expected_range != analysis->ranges.size() + 1)
        return DSL_Distributed_Report(diagnostic, "orphan ownership range", 0);

    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_DISTRIBUTED_SITE_RECORD &site = analysis->sites[i];
        const DSL_COMMUNICATION_EPOCH_RECORD &epoch = analysis->epochs[i];
        DSL_IR_VALUE_RECORD semantic_value;
        DSL_TENSOR_EVOLUTION_NODE_RECORD semantic_root;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_alternative_id != expected_alternative ||
            site.alternative_count == 0 ||
            site.alternative_count >
                analysis->control.max_alternatives_per_site ||
            site.epoch_id != epoch.id || epoch.id != i + 1 ||
            epoch.site_id != site.id ||
            epoch.semantic_value_id != site.semantic_value_id ||
            !DSL_IR_Image_Get_Value
                 (site.semantic_value_id, &semantic_value) ||
            semantic_value.producer_node_id != epoch.producer_node_id ||
            !DSL_Tensor_Evolution_Get_Node
                 (analysis->graph, site.semantic_root_id, &semantic_root) ||
            semantic_root.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC ||
            semantic_root.semantic_value_id != site.semantic_value_id ||
            site.baseline_candidate_id == 0 || site.baseline_plan_id == 0 ||
            (analysis->control.select_plans && site.selected_plan_id == 0) ||
            (!analysis->control.select_plans && site.selected_plan_id != 0) ||
            site.reserved != 0 || epoch.reserved != 0 ||
            !DSL_Opt_Plan_Verify(analysis->plans[i], diagnostic))
            return DSL_Distributed_Report
                       (diagnostic, "invalid distributed site", site.id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            if (expected_alternative > analysis->alternatives.size())
                return DSL_Distributed_Report
                           (diagnostic, "missing distributed alternative",
                            site.id);
            const DSL_DISTRIBUTED_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[expected_alternative - 1];
            if (alternative.descriptor_id == 0 ||
                alternative.descriptor_id > analysis->descriptors.size())
                return DSL_Distributed_Report
                           (diagnostic, "invalid alternative descriptor",
                            alternative.id);
            const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor =
                analysis->descriptors[alternative.descriptor_id - 1];
            DSL_TENSOR_EVOLUTION_NODE_RECORD result;
            if (alternative.id != expected_alternative ||
                alternative.site_id != site.id ||
                alternative.descriptor_id != alternative.id ||
                alternative.legality > DSL_OPT_LEGALITY_REJECTED ||
                alternative.candidate_id == 0 || alternative.plan_id == 0 ||
                alternative.reserved != 0 ||
                !DSL_Tensor_Evolution_Get_Node
                     (analysis->graph,
                      alternative.result_evolution_node_id, &result) ||
                result.kind != DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED ||
                result.semantic_root_id != site.semantic_root_id ||
                result.semantic_value_id != site.semantic_value_id ||
                result.representation_descriptor_id != descriptor.id ||
                descriptor.alias_id == 0 ||
                descriptor.alias_id > analysis->aliases.size() ||
                analysis->aliases[descriptor.alias_id - 1].visibility_epoch !=
                    site.epoch_id ||
                (analysis->control.derive_communication &&
                 alternative.communication_intent_id != alternative.id) ||
                (!analysis->control.derive_communication &&
                 alternative.communication_intent_id != 0))
                return DSL_Distributed_Report
                           (diagnostic, "invalid distributed alternative",
                            alternative.id);
            if (analysis->control.derive_communication) {
                if (alternative.communication_intent_id == 0 ||
                    alternative.communication_intent_id >
                        analysis->intents.size())
                    return DSL_Distributed_Report
                               (diagnostic, "missing communication intent",
                                alternative.id);
                const DSL_COMMUNICATION_INTENT_RECORD &intent =
                    analysis->intents[alternative.communication_intent_id - 1];
                if (intent.id != alternative.communication_intent_id ||
                    intent.alternative_id != alternative.id ||
                    intent.epoch_id != site.epoch_id ||
                    intent.kind != DSL_Distributed_Communication_Kind
                                       (descriptor.sharding_kind) ||
                    intent.bytes != alternative.communication_bytes ||
                    intent.legality != alternative.legality ||
                    intent.rejection_reason !=
                        alternative.rejection_reason ||
                    intent.source_count == 0 ||
                    intent.destination_count == 0 ||
                    intent.reserved0 != 0 || intent.reserved1 != 0)
                    return DSL_Distributed_Report
                               (diagnostic, "invalid communication intent",
                                intent.id);
            }
            ++expected_alternative;
        }
    }
    if (expected_alternative != analysis->alternatives.size() + 1)
        return DSL_Distributed_Report
                   (diagnostic, "orphan distributed alternative", 0);
    return DSL_Tensor_Evolution_Verify(analysis->graph, diagnostic);
}

static void
DSL_Distributed_Print_U64 (FILE *file, UINT64 value)
{
    if (value == DSL_DISTRIBUTED_UNKNOWN_U64)
        fprintf(file, "<unknown>");
    else
        fprintf(file, "%llu", (unsigned long long)value);
}

void
DSL_Distributed_Print
        (FILE *file, const DSL_DISTRIBUTED_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file,
            "DSLDistributedCandidates: owner=<%u,%u> sites=%u "
            "descriptors=%u aliases=%u ranges=%u epochs=%u intents=%u "
            "devices=%u generate=%s derive_communication=%s select=%s "
            "apply=%s target=%u\n",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->descriptors.size(),
            (UINT32)analysis->aliases.size(),
            (UINT32)analysis->ranges.size(),
            (UINT32)analysis->epochs.size(),
            (UINT32)analysis->intents.size(),
            analysis->control.device_count,
            analysis->control.generate_candidates ? "yes" : "no",
            analysis->control.derive_communication ? "yes" : "no",
            analysis->control.select_plans ? "yes" : "no",
            analysis->control.apply_transformation ? "yes" : "no",
            analysis->control.target_profile_id);
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_DISTRIBUTED_SITE_RECORD &site = analysis->sites[i];
        const DSL_COMMUNICATION_EPOCH_RECORD &epoch =
            analysis->epochs[site.epoch_id - 1];
        fprintf(file,
                "  site %u value=%u root=%u alternatives=%u epoch=%u "
                "producer=%u consumers=%u first=%u last=%u baseline=%u/%u "
                "selected=%u\n",
                site.id, site.semantic_value_id, site.semantic_root_id,
                site.alternative_count, site.epoch_id,
                epoch.producer_node_id, epoch.consumer_count,
                epoch.first_consumer_node_id, epoch.last_consumer_node_id,
                site.baseline_candidate_id, site.baseline_plan_id,
                site.selected_plan_id);
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            const DSL_DISTRIBUTED_ALTERNATIVE_RECORD &alternative =
                analysis->alternatives[site.first_alternative_id - 1 + j];
            const DSL_DISTRIBUTED_DESCRIPTOR_RECORD &descriptor =
                analysis->descriptors[alternative.descriptor_id - 1];
            const DSL_DISTRIBUTED_ALIAS_RECORD &alias =
                analysis->aliases[descriptor.alias_id - 1];
            fprintf(file,
                    "    alternative %u placement=%s sharding=%s "
                    "ownership=%s axis=",
                    alternative.id,
                    DSL_Placement_Kind_Name(descriptor.placement_kind),
                    DSL_Sharding_Kind_Name(descriptor.sharding_kind),
                    DSL_Distributed_Ownership_Name(descriptor.ownership));
            if (descriptor.shard_axis == DSL_DISTRIBUTED_NO_AXIS)
                fprintf(file, "<none>");
            else
                fprintf(file, "%u", descriptor.shard_axis);
            fprintf(file,
                    " disjoint=%s legality=%s reason=%s communication_bytes=",
                    DSL_Distributed_Disjoint_State_Name(alias.disjoint_state),
                    DSL_Opt_Legality_Name(alternative.legality),
                    DSL_Opt_Rejection_Reason_Name
                        (alternative.rejection_reason));
            DSL_Distributed_Print_U64
                (file, alternative.communication_bytes);
            fprintf(file, " candidate=%u plan=%u evolution=%u/%u\n",
                    alternative.candidate_id, alternative.plan_id,
                    alternative.result_evolution_node_id,
                    alternative.evolution_edge_id);
            for (UINT32 range = 0; range < alias.range_count; ++range) {
                const DSL_DISTRIBUTED_RANGE_RECORD &record =
                    analysis->ranges[alias.first_range_id - 1 + range];
                fprintf(file,
                        "      range %u device=%u axis=%u state=%s [",
                        record.id, record.device_ordinal, record.axis,
                        DSL_Distributed_Range_State_Name(record.state));
                DSL_Distributed_Print_U64(file, record.lower);
                fprintf(file, ",");
                DSL_Distributed_Print_U64(file, record.upper);
                fprintf(file, ")\n");
            }
            if (alternative.communication_intent_id != 0) {
                const DSL_COMMUNICATION_INTENT_RECORD &intent =
                    analysis->intents
                        [alternative.communication_intent_id - 1];
                fprintf(file,
                        "      communication %u kind=%s epoch=%u sources=%u "
                        "destinations=%u bytes=",
                        intent.id, DSL_Communication_Kind_Name(intent.kind),
                        intent.epoch_id, intent.source_count,
                        intent.destination_count);
                DSL_Distributed_Print_U64(file, intent.bytes);
                fprintf(file, " legality=%s reason=%s\n",
                        DSL_Opt_Legality_Name(intent.legality),
                        DSL_Opt_Rejection_Reason_Name
                            (intent.rejection_reason));
            }
        }
        DSL_Opt_Plan_Print(file, analysis->plans[i]);
    }
}

#define DSL_DISTRIBUTED_COUNT_GETTERS(kind, field)                         \
UINT32 DSL_Distributed_##kind##_Count                                     \
        (const DSL_DISTRIBUTED_ANALYSIS *analysis)                        \
{                                                                         \
    return analysis == NULL ? 0 : analysis->field.size();                 \
}

DSL_DISTRIBUTED_COUNT_GETTERS(Descriptor, descriptors)
DSL_DISTRIBUTED_COUNT_GETTERS(Alias, aliases)
DSL_DISTRIBUTED_COUNT_GETTERS(Range, ranges)
DSL_DISTRIBUTED_COUNT_GETTERS(Site, sites)
DSL_DISTRIBUTED_COUNT_GETTERS(Alternative, alternatives)

UINT32
DSL_Communication_Epoch_Count (const DSL_DISTRIBUTED_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->epochs.size();
}

UINT32
DSL_Communication_Intent_Count (const DSL_DISTRIBUTED_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->intents.size();
}

#define DSL_DISTRIBUTED_GETTER(name, id_type, record_type, field)          \
BOOL DSL_Distributed_Get_##name                                           \
        (const DSL_DISTRIBUTED_ANALYSIS *analysis, id_type id,            \
         record_type *record)                                             \
{                                                                         \
    if (analysis == NULL || record == NULL || id == 0 ||                  \
        id > analysis->field.size())                                      \
        return FALSE;                                                     \
    *record = analysis->field[id - 1];                                    \
    return TRUE;                                                          \
}

DSL_DISTRIBUTED_GETTER(Descriptor, DSL_DISTRIBUTED_DESCRIPTOR_ID,
                       DSL_DISTRIBUTED_DESCRIPTOR_RECORD, descriptors)
DSL_DISTRIBUTED_GETTER(Alias, DSL_DISTRIBUTED_ALIAS_ID,
                       DSL_DISTRIBUTED_ALIAS_RECORD, aliases)
DSL_DISTRIBUTED_GETTER(Range, DSL_DISTRIBUTED_RANGE_ID,
                       DSL_DISTRIBUTED_RANGE_RECORD, ranges)
DSL_DISTRIBUTED_GETTER(Site, DSL_DISTRIBUTED_SITE_ID,
                       DSL_DISTRIBUTED_SITE_RECORD, sites)
DSL_DISTRIBUTED_GETTER(Alternative, DSL_DISTRIBUTED_ALTERNATIVE_ID,
                       DSL_DISTRIBUTED_ALTERNATIVE_RECORD, alternatives)

BOOL
DSL_Communication_Get_Epoch
        (const DSL_DISTRIBUTED_ANALYSIS *analysis,
         DSL_COMMUNICATION_EPOCH_ID id,
         DSL_COMMUNICATION_EPOCH_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->epochs.size())
        return FALSE;
    *record = analysis->epochs[id - 1];
    return TRUE;
}

BOOL
DSL_Communication_Get_Intent
        (const DSL_DISTRIBUTED_ANALYSIS *analysis,
         DSL_COMMUNICATION_INTENT_ID id,
         DSL_COMMUNICATION_INTENT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->intents.size())
        return FALSE;
    *record = analysis->intents[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_Distributed_Get_Plan_Context
        (const DSL_DISTRIBUTED_ANALYSIS *analysis,
         DSL_DISTRIBUTED_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
