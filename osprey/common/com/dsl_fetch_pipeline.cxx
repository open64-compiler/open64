/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Builds the AIO-10 runtime-only CommonFetchPlanIR and CommonPipelineIR.
 * AIO-9 supplies tile and buffering hints; this phase selects no target
 * instruction and does not mutate executable or binary WHIRL. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
 */

#include <algorithm>
#include <string.h>
#include <vector>

#include "dsl_fetch_pipeline.h"
#include "pu_info.h"

struct DSL_FETCH_PIPELINE_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    const DSL_DISTRIBUTED_ANALYSIS *distributed;
    const DSL_TILE_ANALYSIS *tile;
    DSL_FETCH_PIPELINE_CONTROL control;
    std::vector<DSL_FETCH_SITE_RECORD> sites;
    std::vector<DSL_FETCH_PLAN_RECORD> plans;
    std::vector<DSL_FETCH_RECORD> fetches;
    std::vector<DSL_PIPELINE_STAGE_RECORD> stages;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plan_contexts;
    BOOL built;
};

static const char *DSL_fetch_issue_name[] = {
    "unknown", "consumer", "previous_k_tile", "prologue"
};

static const char *DSL_fetch_barrier_name_table[] = {
    "none", "cta", "arrival"
};

static const char *DSL_fetch_wait_name[] = {
    "none", "before_consumer", "pipeline_stage"
};

static BOOL
DSL_Fetch_Pipeline_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL fetch pipeline error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Fetch_Pipeline_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Fetch_Pipeline_Active (const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Fetch_Pipeline_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_fetch_issue_point_name (UINT32 point)
{
    return point < sizeof(DSL_fetch_issue_name) /
                       sizeof(DSL_fetch_issue_name[0]) ?
           DSL_fetch_issue_name[point] : "unknown";
}

const char *
DSL_fetch_barrier_name (UINT32 barrier)
{
    return barrier < sizeof(DSL_fetch_barrier_name_table) /
                         sizeof(DSL_fetch_barrier_name_table[0]) ?
           DSL_fetch_barrier_name_table[barrier] : "unknown";
}

const char *
DSL_fetch_wait_point_name (UINT32 point)
{
    return point < sizeof(DSL_fetch_wait_name) /
                       sizeof(DSL_fetch_wait_name[0]) ?
           DSL_fetch_wait_name[point] : "unknown";
}

void
DSL_fetch_pipeline_control_init (DSL_FETCH_PIPELINE_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->select_plans = 1;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->max_sites = 16;
    control->max_plans_per_site = 8;
    control->maximum_buffer_stages = 3;
    control->prefetch_distance_hint = 1;
    control->enable_vector = 1;
    control->enable_async_copy = 1;
    control->enable_multidimensional_async = 1;
}

static BOOL
DSL_Fetch_Pipeline_Control_Valid
        (const DSL_FETCH_PIPELINE_CONTROL &control)
{
    return control.generate_candidates <= 1 &&
           control.select_plans <= 1 &&
           control.apply_transformation == 0 &&
           control.target_profile_id >= DSL_TARGET_PROFILE_CPU_BASELINE &&
           control.target_profile_id <= DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
           control.max_sites != 0 && control.max_plans_per_site >= 1 &&
           control.maximum_buffer_stages != 0 &&
           control.enable_vector <= 1 && control.enable_async_copy <= 1 &&
           control.enable_multidimensional_async <= 1 &&
           control.reserved == 0 &&
           (!control.select_plans || control.generate_candidates);
}

static UINT64
DSL_Fetch_Ceil_Div (UINT64 value, UINT64 divisor)
{
    return divisor == 0 ? 0 : value / divisor + (value % divisor != 0);
}

static BOOL
DSL_Fetch_Add_U64 (UINT64 lhs, UINT64 rhs, UINT64 *result)
{
    if (result == NULL || lhs > ~(UINT64)0 - rhs)
        return FALSE;
    *result = lhs + rhs;
    return TRUE;
}

static BOOL
DSL_Fetch_Mul_U64 (UINT64 lhs, UINT64 rhs, UINT64 *result)
{
    if (result == NULL || (rhs != 0 && lhs > ~(UINT64)0 / rhs))
        return FALSE;
    *result = lhs * rhs;
    return TRUE;
}

static BOOL
DSL_Fetch_Node_Operand
        (DSL_IR_NODE_ID node_id, UINT32 ordinal,
         DSL_IR_VALUE_RECORD *value)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    return value != NULL && DSL_IR_Image_Get_Node(node_id, &node) &&
           ordinal < node.operand_count &&
           DSL_IR_Image_Get_Value_Reference
               (node.first_operand_reference_id + ordinal, &reference) &&
           reference.owner_node_id == node.id &&
           reference.ordinal == ordinal &&
           DSL_IR_Image_Get_Value(reference.value_id, value);
}

static BOOL
DSL_Fetch_Selected_Tile
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         const DSL_TILE_SITE_RECORD &site, DSL_TILE_PLAN_RECORD *tile)
{
    if (tile == NULL)
        return FALSE;
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        DSL_TILE_PLAN_RECORD candidate;
        if (!DSL_tile_get_plan
                 (analysis->tile, site.first_tile_plan_id + i,
                  &candidate))
            return FALSE;
        if (candidate.optimization_plan_id == site.selected_plan_id) {
            *tile = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
DSL_Fetch_Distributed_Safe
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_IR_VALUE_ID value_id)
{
    if (analysis->distributed == NULL)
        return TRUE;
    for (UINT32 id = 1;
         id <= DSL_distributed_site_count(analysis->distributed); ++id) {
        DSL_DISTRIBUTED_SITE_RECORD site;
        if (!DSL_distributed_get_site
                 (analysis->distributed, id, &site))
            return FALSE;
        if (site.semantic_value_id != value_id)
            continue;
        for (UINT32 i = 0; i < site.alternative_count; ++i) {
            DSL_DISTRIBUTED_ALTERNATIVE_RECORD alternative;
            if (!DSL_distributed_get_alternative
                     (analysis->distributed,
                      site.first_alternative_id + i, &alternative))
                return FALSE;
            if (alternative.plan_id == site.selected_plan_id)
                return alternative.communication_intent_id == 0;
        }
        return site.selected_plan_id == 0;
    }
    return TRUE;
}

static BOOL
DSL_Fetch_Operand_Safe
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_IR_VALUE_ID value_id, UINT32 *reason)
{
    DSL_TENSOR_LOCALITY_FACT_RECORD locality;
    if (reason == NULL)
        return FALSE;
    *reason = DSL_OPT_REJECT_NONE;
    if (!DSL_tensor_locality_find_fact
             (analysis->locality, value_id, &locality)) {
        *reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        return FALSE;
    }
    if (locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT) {
        *reason = DSL_OPT_REJECT_EFFECT;
        return FALSE;
    }
    if (locality.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE) {
        *reason = DSL_OPT_REJECT_OWNERSHIP;
        return FALSE;
    }
    if (locality.lifetime_state != DSL_TENSOR_LIFETIME_EXACT_BLOCK) {
        *reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
        return FALSE;
    }
    if (!DSL_Fetch_Distributed_Safe(analysis, value_id)) {
        *reason = DSL_OPT_REJECT_OWNERSHIP;
        return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Fetch_Plan_Operands
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_IR_NODE_ID semantic_node_id,
         const DSL_TILE_PLAN_RECORD &tile,
         DSL_IR_VALUE_RECORD operands[2], UINT64 bytes[2],
         DSL_TENSOR_EVOLUTION_NODE_RECORD roots[2])
{
    if (!DSL_Fetch_Node_Operand
             (semantic_node_id, 0, &operands[0]) ||
        !DSL_Fetch_Node_Operand
             (semantic_node_id, 1, &operands[1]))
        return FALSE;
    UINT64 element_bytes = TY_size(TY_tensor_element_ty(operands[0].ty));
    if (element_bytes == 0 ||
        TY_tensor_element_ty(operands[0].ty) !=
            TY_tensor_element_ty(operands[1].ty) ||
        !DSL_tensor_evolution_find_semantic_root
             (analysis->graph, operands[0].id, &roots[0]) ||
        !DSL_tensor_evolution_find_semantic_root
             (analysis->graph, operands[1].id, &roots[1]))
        return FALSE;
    if (tile.cta_m == 0 || tile.cta_n == 0 || tile.cta_k == 0) {
        DSL_TENSOR_LOCALITY_FACT_RECORD locality0;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality1;
        if (!DSL_tensor_locality_find_fact
                 (analysis->locality, operands[0].id, &locality0) ||
            !DSL_tensor_locality_find_fact
                 (analysis->locality, operands[1].id, &locality1) ||
            locality0.object_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64 ||
            locality1.object_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            return FALSE;
        bytes[0] = locality0.object_bytes;
        bytes[1] = locality1.object_bytes;
        return TRUE;
    }
    return DSL_Fetch_Mul_U64(tile.cta_m, tile.cta_k, &bytes[0]) &&
           DSL_Fetch_Mul_U64(bytes[0], element_bytes, &bytes[0]) &&
           DSL_Fetch_Mul_U64(tile.cta_k, tile.cta_n, &bytes[1]) &&
           DSL_Fetch_Mul_U64(bytes[1], element_bytes, &bytes[1]);
}

static void
DSL_Fetch_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = DSL_OPT_COST_CONFIDENCE_LOW;
    term->evidence = evidence;
}

static BOOL
DSL_Fetch_Add_Optimization_Plan
        (DSL_FETCH_PIPELINE_ANALYSIS *analysis, DSL_FETCH_SITE_RECORD *site,
         DSL_FETCH_PLAN_RECORD *pipeline, FILE *diagnostic)
{
    DSL_OPT_PLAN_CONTEXT *context = analysis->plan_contexts.back();
    DSL_TILE_PLAN_RECORD tile;
    if (!DSL_tile_get_plan
             (analysis->tile, pipeline->tile_plan_id, &tile))
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "missing pipeline tile", pipeline->id);
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind =
        (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
        DSL_OPT_CANDIDATE_BASELINE : DSL_OPT_CANDIDATE_PIPELINE;
    candidate.semantic_node_id = site->semantic_node_id;
    candidate.source_evolution_node_id = tile.result_evolution_node_id;
    candidate.result_evolution_node_id = tile.result_evolution_node_id;
    candidate.parent_candidate_id =
        (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
        0 : analysis->plans[site->first_pipeline_plan_id - 1].candidate_id;
    candidate.legality = pipeline->legality;
    candidate.rejection_reason = pipeline->rejection_reason;
    candidate.ordering_key = pipeline->id;
    candidate.flags =
        (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
        DSL_OPT_CANDIDATE_FLAG_BASELINE :
        DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &pipeline->candidate_id, diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = pipeline->id;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i) {
        DSL_Fetch_Cost_Term
            (&cost.terms[i],
             i == DSL_OPT_COST_MEMORY_UNHIDDEN ?
                 pipeline->unhidden_movement_cost :
             i == DSL_OPT_COST_SYNC ? pipeline->barrier_count : 0,
             (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
                 DSL_OPT_COST_EVIDENCE_BASELINE_POLICY :
                 DSL_OPT_COST_EVIDENCE_TARGET_MODEL);
    }
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic))
        return FALSE;

    DSL_OPT_CANDIDATE_ID member = pipeline->candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id =
        (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
        0 : site->baseline_plan_id;
    plan.legality = pipeline->legality;
    plan.rejection_reason = pipeline->rejection_reason;
    plan.ordering_key = pipeline->id;
    plan.flags =
        (pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0 ?
        DSL_OPT_PLAN_FLAG_BASELINE : DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_opt_plan_add_plan
             (context, &plan, &pipeline->optimization_plan_id, diagnostic))
        return FALSE;
    if ((pipeline->flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0)
        site->baseline_plan_id = pipeline->optimization_plan_id;
    return TRUE;
}

static BOOL
DSL_Fetch_Add_Stages
        (DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_FETCH_PLAN_RECORD *pipeline)
{
    pipeline->first_stage_id = analysis->stages.size() + 1;
    for (UINT32 i = 0; i < pipeline->stage_count; ++i) {
        DSL_PIPELINE_STAGE_RECORD stage;
        memset(&stage, 0, sizeof(stage));
        stage.id = analysis->stages.size() + 1;
        stage.pipeline_plan_id = pipeline->id;
        stage.ordinal = i;
        stage.buffer_slot = i;
        stage.prefetch_distance = pipeline->prefetch_distance;
        stage.issue_point = pipeline->issue_point;
        stage.arrival_barrier = pipeline->arrival_barrier;
        stage.wait_point = pipeline->wait_point;
        analysis->stages.push_back(stage);
    }
    return TRUE;
}

static BOOL
DSL_Fetch_Add_Plan
        (DSL_FETCH_PIPELINE_ANALYSIS *analysis, DSL_FETCH_SITE_RECORD *site,
         const DSL_TILE_PLAN_RECORD &tile, UINT32 engine,
         BOOL baseline, FILE *diagnostic)
{
    DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD capability;
    DSL_IR_VALUE_RECORD operands[2];
    DSL_TENSOR_EVOLUTION_NODE_RECORD roots[2];
    UINT64 bytes[2];
    DSL_FETCH_PLAN_RECORD pipeline;
    memset(&pipeline, 0, sizeof(pipeline));
    if (!DSL_memory_hierarchy_find_movement
             (analysis->control.target_profile_id, engine, &capability))
        return baseline ?
               DSL_Fetch_Pipeline_Report
                   (diagnostic, "missing demand capability", site->id) :
               TRUE;
    if (!DSL_Fetch_Plan_Operands
             (analysis, site->semantic_node_id, tile,
              operands, bytes, roots))
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "invalid tile operands", site->id);

    pipeline.id = analysis->plans.size() + 1;
    pipeline.site_id = site->id;
    pipeline.tile_plan_id = tile.id;
    pipeline.target_profile_id = analysis->control.target_profile_id;
    pipeline.engine = engine;
    pipeline.first_fetch_id = analysis->fetches.size() + 1;
    pipeline.fetch_count = 2;
    pipeline.edge_policy = tile.edge_policy;
    pipeline.legality = DSL_OPT_LEGALITY_PROVEN;
    pipeline.rejection_reason = DSL_OPT_REJECT_NONE;
    pipeline.flags = DSL_FETCH_PLAN_FLAG_SEMANTICS_PRESERVING |
                     (baseline ? DSL_FETCH_PLAN_FLAG_BASELINE :
                      DSL_FETCH_PLAN_FLAG_PROVISIONAL);

    UINT32 desired_stages = 1;
    if (engine == DSL_MEMORY_MOVEMENT_ASYNC_COPY)
        desired_stages = 2;
    else if (engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC)
        desired_stages = 3;
    pipeline.stage_count = std::min
        (desired_stages,
         std::min(capability.maximum_stages,
                  analysis->control.maximum_buffer_stages));
    if (!baseline && tile.maximum_buffer_stages != 0)
        pipeline.stage_count = std::min
            (pipeline.stage_count, tile.maximum_buffer_stages);
    pipeline.prefetch_distance =
        (capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0 ?
        analysis->control.prefetch_distance_hint : 0;
    pipeline.issue_point =
        engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ?
        DSL_FETCH_ISSUE_PROLOGUE :
        engine == DSL_MEMORY_MOVEMENT_ASYNC_COPY ?
        DSL_FETCH_ISSUE_PREVIOUS_K_TILE : DSL_FETCH_ISSUE_CONSUMER;
    pipeline.arrival_barrier =
        engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ?
        DSL_FETCH_BARRIER_ARRIVAL :
        (capability.flags & DSL_MEMORY_MOVEMENT_FLAG_REQUIRES_BARRIER) != 0 ?
        DSL_FETCH_BARRIER_CTA : DSL_FETCH_BARRIER_NONE;
    pipeline.wait_point =
        (capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0 ?
        DSL_FETCH_WAIT_PIPELINE_STAGE :
        pipeline.arrival_barrier != DSL_FETCH_BARRIER_NONE ?
        DSL_FETCH_WAIT_BEFORE_CONSUMER : DSL_FETCH_WAIT_NONE;
    pipeline.barrier_count =
        pipeline.arrival_barrier == DSL_FETCH_BARRIER_NONE ? 0 : 1;
    if ((capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0)
        pipeline.flags |= DSL_FETCH_PLAN_FLAG_ASYNC;

    UINT32 safety_reason = DSL_OPT_REJECT_NONE;
    for (UINT32 ordinal = 0; ordinal < 2; ++ordinal) {
        if (!baseline &&
            !DSL_Fetch_Operand_Safe
                 (analysis, operands[ordinal].id, &safety_reason) &&
            pipeline.legality != DSL_OPT_LEGALITY_REJECTED) {
            pipeline.legality =
                safety_reason == DSL_OPT_REJECT_INCOMPLETE_ANALYSIS ?
                DSL_OPT_LEGALITY_UNKNOWN : DSL_OPT_LEGALITY_REJECTED;
            pipeline.rejection_reason = safety_reason;
        }
    }
    if (!baseline && tile.legality != DSL_OPT_LEGALITY_PROVEN) {
        pipeline.legality = tile.legality;
        pipeline.rejection_reason = tile.rejection_reason;
    }
    if (!baseline && pipeline.stage_count < desired_stages) {
        pipeline.legality = DSL_OPT_LEGALITY_REJECTED;
        pipeline.rejection_reason = DSL_OPT_REJECT_RESOURCE;
    }
    if ((capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0 &&
        pipeline.prefetch_distance >= pipeline.stage_count) {
        pipeline.legality = DSL_OPT_LEGALITY_REJECTED;
        pipeline.rejection_reason = DSL_OPT_REJECT_RESOURCE;
    }
    if (engine == DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC &&
        ((tile.flags & DSL_TILE_PLAN_FLAG_TMA_CANDIDATE) == 0 ||
         tile.edge_policy != DSL_TILE_EDGE_EXACT)) {
        pipeline.legality = DSL_OPT_LEGALITY_REJECTED;
        pipeline.rejection_reason = DSL_OPT_REJECT_TARGET_MISMATCH;
    }

    UINT64 stage_bytes;
    if (!DSL_Fetch_Add_U64(bytes[0], bytes[1], &stage_bytes) ||
        (!baseline &&
         !DSL_Fetch_Mul_U64
              (stage_bytes, pipeline.stage_count,
               &pipeline.buffering_bytes)))
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "buffering cost overflow", pipeline.id);
    pipeline.buffering_capacity_bytes = tile.shared_capacity_bytes;
    if (!baseline &&
        pipeline.buffering_capacity_bytes != DSL_MEMORY_CAPACITY_UNKNOWN &&
        pipeline.buffering_bytes > pipeline.buffering_capacity_bytes) {
        pipeline.legality = DSL_OPT_LEGALITY_REJECTED;
        pipeline.rejection_reason = DSL_OPT_REJECT_RESOURCE;
    }

    for (UINT32 ordinal = 0; ordinal < 2; ++ordinal) {
        DSL_FETCH_RECORD fetch;
        memset(&fetch, 0, sizeof(fetch));
        fetch.id = analysis->fetches.size() + 1;
        fetch.pipeline_plan_id = pipeline.id;
        fetch.operand_ordinal = ordinal;
        fetch.value_id = operands[ordinal].id;
        fetch.descriptor_ty = operands[ordinal].ty;
        fetch.source_tier_kind = capability.source_tier_kind;
        fetch.destination_tier_kind = capability.destination_tier_kind;
        fetch.engine = engine;
        fetch.transaction_bytes = capability.transaction_bytes;
        fetch.minimum_alignment = capability.minimum_alignment;
        fetch.bytes_per_stage = bytes[ordinal];
        fetch.source_evolution_node_id = roots[ordinal].id;
        fetch.result_evolution_node_id = roots[ordinal].id;
        fetch.legality = pipeline.legality;
        fetch.rejection_reason = pipeline.rejection_reason;
        if (!baseline) {
            fetch.flags |= DSL_FETCH_RECORD_FLAG_STAGED;
            if ((capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0)
                fetch.flags |= DSL_FETCH_RECORD_FLAG_ASYNC;
            if ((capability.flags &
                 DSL_MEMORY_MOVEMENT_FLAG_REQUIRES_BARRIER) != 0)
                fetch.flags |= DSL_FETCH_RECORD_FLAG_REQUIRES_BARRIER;
        }
        UINT64 transactions = DSL_Fetch_Ceil_Div
                                  (fetch.bytes_per_stage,
                                   fetch.transaction_bytes);
        if (!DSL_Fetch_Mul_U64
                 (transactions, capability.latency_class,
                  &fetch.raw_movement_cost))
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "movement cost overflow", fetch.id);
        if ((capability.flags & DSL_MEMORY_MOVEMENT_FLAG_ASYNC) != 0 &&
            pipeline.stage_count > 1)
            fetch.hidden_movement_cost =
                fetch.raw_movement_cost * (pipeline.stage_count - 1) /
                pipeline.stage_count;
        fetch.unhidden_movement_cost =
            fetch.raw_movement_cost - fetch.hidden_movement_cost;
        if (!baseline && pipeline.legality == DSL_OPT_LEGALITY_PROVEN &&
            !DSL_tensor_evolution_add_staged_buffer
                 (analysis->graph, roots[ordinal].id, fetch.id,
                  &fetch.result_evolution_node_id,
                  &fetch.evolution_edge_id, diagnostic))
            return FALSE;
        if (!DSL_Fetch_Add_U64
                 (pipeline.raw_movement_cost, fetch.raw_movement_cost,
                  &pipeline.raw_movement_cost) ||
            !DSL_Fetch_Add_U64
                 (pipeline.hidden_movement_cost,
                  fetch.hidden_movement_cost,
                  &pipeline.hidden_movement_cost) ||
            !DSL_Fetch_Add_U64
                 (pipeline.unhidden_movement_cost,
                  fetch.unhidden_movement_cost,
                  &pipeline.unhidden_movement_cost))
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "pipeline cost overflow", pipeline.id);
        analysis->fetches.push_back(fetch);
    }
    if (!DSL_Fetch_Add_Stages(analysis, &pipeline))
        return FALSE;
    analysis->plans.push_back(pipeline);
    if (!DSL_Fetch_Add_Optimization_Plan
             (analysis, site, &analysis->plans.back(), diagnostic))
        return FALSE;
    ++site->pipeline_plan_count;
    return TRUE;
}

DSL_FETCH_PIPELINE_ANALYSIS *
DSL_fetch_pipeline_create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_DISTRIBUTED_ANALYSIS *distributed,
         const DSL_TILE_ANALYSIS *tile,
         const DSL_FETCH_PIPELINE_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || locality == NULL || tile == NULL ||
        control == NULL || Current_PU_Info != pu ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Fetch_Pipeline_Control_Valid(*control) ||
        !DSL_memory_hierarchy_validate
             (control->target_profile_id, diagnostic) ||
        !DSL_tensor_evolution_verify(graph, diagnostic) ||
        !DSL_tensor_locality_verify(locality, diagnostic) ||
        !DSL_tile_verify(tile, diagnostic) ||
        (distributed != NULL &&
         !DSL_distributed_verify(distributed, diagnostic))) {
        DSL_Fetch_Pipeline_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_FETCH_PIPELINE_ANALYSIS *analysis =
        new DSL_FETCH_PIPELINE_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->locality = locality;
    analysis->distributed = distributed;
    analysis->tile = tile;
    analysis->control = *control;
    analysis->built = FALSE;
    return analysis;
}

void
DSL_fetch_pipeline_destroy (DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i)
        DSL_opt_plan_destroy(analysis->plan_contexts[i]);
    delete analysis;
}

BOOL
DSL_fetch_pipeline_build
        (DSL_FETCH_PIPELINE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Fetch_Pipeline_Active(analysis) || analysis->built)
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    for (DSL_TILE_SITE_ID tile_site_id = 1;
         tile_site_id <= DSL_tile_site_count(analysis->tile);
         ++tile_site_id) {
        DSL_TILE_SITE_RECORD tile_site;
        DSL_TILE_PLAN_RECORD tile;
        if (!DSL_tile_get_site(analysis->tile, tile_site_id, &tile_site) ||
            (analysis->control.focus_value_id != 0 &&
             tile_site.semantic_value_id !=
                 analysis->control.focus_value_id))
            continue;
        if (tile_site.selected_plan_id == 0 ||
            !DSL_Fetch_Selected_Tile(analysis, tile_site, &tile))
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "missing selected tile", tile_site_id);
        if (tile.target_profile_id != analysis->control.target_profile_id)
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "tile target mismatch", tile.id);
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "site budget exhausted", tile_site_id);

        DSL_FETCH_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_node_id = tile_site.semantic_node_id;
        site.semantic_value_id = tile_site.semantic_value_id;
        site.tile_site_id = tile_site.id;
        site.selected_tile_plan_id = tile.id;
        site.first_pipeline_plan_id = analysis->plans.size() + 1;
        analysis->sites.push_back(site);

        DSL_OPT_PLAN_BUDGET budget;
        budget.max_candidates = analysis->control.max_plans_per_site;
        budget.max_plans = analysis->control.max_plans_per_site;
        DSL_OPT_PLAN_CONTEXT *context = DSL_opt_plan_create
            (analysis->pu, analysis->graph, &budget, diagnostic);
        if (context == NULL)
            return FALSE;
        analysis->plan_contexts.push_back(context);
        DSL_FETCH_SITE_RECORD &active_site = analysis->sites.back();
        if (!DSL_Fetch_Add_Plan
                 (analysis, &active_site, tile,
                  DSL_MEMORY_MOVEMENT_DEMAND, TRUE, diagnostic))
            return FALSE;
        if ((tile.flags & DSL_TILE_PLAN_FLAG_BASELINE) == 0) {
            if (analysis->control.enable_vector &&
                !DSL_Fetch_Add_Plan
                     (analysis, &active_site, tile,
                      DSL_MEMORY_MOVEMENT_VECTOR, FALSE, diagnostic))
                return FALSE;
            if (analysis->control.enable_async_copy &&
                !DSL_Fetch_Add_Plan
                     (analysis, &active_site, tile,
                      DSL_MEMORY_MOVEMENT_ASYNC_COPY, FALSE, diagnostic))
                return FALSE;
            if (analysis->control.enable_multidimensional_async &&
                !DSL_Fetch_Add_Plan
                     (analysis, &active_site, tile,
                      DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC,
                      FALSE, diagnostic))
                return FALSE;
        }
        if (analysis->control.select_plans) {
            DSL_OPT_SELECTION_RESULT selection;
            if (!DSL_opt_plan_select
                     (context, analysis->control.target_profile_id,
                      &selection, diagnostic))
                return FALSE;
            active_site.selected_plan_id = selection.selected_plan_id;
        }
    }
    analysis->built = TRUE;
    return DSL_fetch_pipeline_verify(analysis, diagnostic);
}

BOOL
DSL_fetch_pipeline_verify
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Fetch_Pipeline_Active(analysis))
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "program unit is not active", 0);
    if (!analysis->built)
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "analysis is not built", 0);
    if (analysis->plan_contexts.size() != analysis->sites.size())
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "site plan count mismatch", 0);
    UINT32 expected_plan = 1;
    UINT32 expected_fetch = 1;
    UINT32 expected_stage = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_FETCH_SITE_RECORD &site = analysis->sites[i];
        DSL_IR_NODE_RECORD node;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_pipeline_plan_id != expected_plan ||
            site.pipeline_plan_count == 0 || site.baseline_plan_id == 0 ||
            site.selected_tile_plan_id == 0 || site.reserved != 0 ||
            !DSL_IR_Image_Get_Node(site.semantic_node_id, &node) ||
            node.result_value_id != site.semantic_value_id ||
            !DSL_opt_plan_verify
                 (analysis->plan_contexts[i], diagnostic))
            return DSL_Fetch_Pipeline_Report
                       (diagnostic, "invalid fetch site", site.id);
        for (UINT32 j = 0; j < site.pipeline_plan_count; ++j) {
            const DSL_FETCH_PLAN_RECORD &pipeline =
                analysis->plans[expected_plan - 1];
            BOOL baseline = j == 0;
            UINT64 raw = 0;
            UINT64 hidden = 0;
            UINT64 unhidden = 0;
            if (pipeline.id != expected_plan ||
                pipeline.site_id != site.id ||
                pipeline.tile_plan_id != site.selected_tile_plan_id ||
                pipeline.target_profile_id !=
                    analysis->control.target_profile_id ||
                pipeline.first_fetch_id != expected_fetch ||
                pipeline.fetch_count != 2 ||
                pipeline.first_stage_id != expected_stage ||
                pipeline.stage_count == 0 ||
                pipeline.engine < DSL_MEMORY_MOVEMENT_DEMAND ||
                pipeline.engine >
                    DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ||
                pipeline.issue_point < DSL_FETCH_ISSUE_CONSUMER ||
                pipeline.issue_point > DSL_FETCH_ISSUE_PROLOGUE ||
                pipeline.arrival_barrier > DSL_FETCH_BARRIER_ARRIVAL ||
                pipeline.wait_point > DSL_FETCH_WAIT_PIPELINE_STAGE ||
                pipeline.candidate_id == 0 ||
                pipeline.optimization_plan_id == 0 ||
                pipeline.reserved != 0 ||
                baseline !=
                    ((pipeline.flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0) ||
                (baseline && pipeline.engine !=
                     DSL_MEMORY_MOVEMENT_DEMAND))
                return DSL_Fetch_Pipeline_Report
                           (diagnostic, "invalid pipeline plan",
                            pipeline.id);
            for (UINT32 f = 0; f < pipeline.fetch_count; ++f) {
                const DSL_FETCH_RECORD &fetch =
                    analysis->fetches[expected_fetch - 1];
                DSL_IR_VALUE_RECORD operand;
                DSL_TENSOR_EVOLUTION_NODE_RECORD result;
                if (fetch.id != expected_fetch ||
                    fetch.pipeline_plan_id != pipeline.id ||
                    fetch.operand_ordinal != f ||
                    !DSL_Fetch_Node_Operand
                         (site.semantic_node_id, f, &operand) ||
                    fetch.value_id != operand.id ||
                    fetch.descriptor_ty != operand.ty ||
                    fetch.engine != pipeline.engine ||
                    fetch.transaction_bytes == 0 ||
                    fetch.minimum_alignment == 0 ||
                    fetch.bytes_per_stage == 0 ||
                    fetch.raw_movement_cost !=
                        fetch.hidden_movement_cost +
                        fetch.unhidden_movement_cost ||
                    fetch.legality != pipeline.legality ||
                    fetch.rejection_reason !=
                        pipeline.rejection_reason ||
                    fetch.reserved != 0 ||
                    (baseline &&
                     (fetch.result_evolution_node_id !=
                          fetch.source_evolution_node_id ||
                      fetch.evolution_edge_id != 0 || fetch.flags != 0)) ||
                    (!baseline && pipeline.legality ==
                         DSL_OPT_LEGALITY_PROVEN &&
                     (!DSL_tensor_evolution_get_node
                          (analysis->graph,
                           fetch.result_evolution_node_id, &result) ||
                      result.kind !=
                          DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER ||
                      result.semantic_value_id != fetch.value_id ||
                      fetch.evolution_edge_id == 0)))
                    return DSL_Fetch_Pipeline_Report
                               (diagnostic, "invalid fetch record",
                                fetch.id);
                raw += fetch.raw_movement_cost;
                hidden += fetch.hidden_movement_cost;
                unhidden += fetch.unhidden_movement_cost;
                ++expected_fetch;
            }
            if (pipeline.raw_movement_cost != raw ||
                pipeline.hidden_movement_cost != hidden ||
                pipeline.unhidden_movement_cost != unhidden)
                return DSL_Fetch_Pipeline_Report
                           (diagnostic, "pipeline cost mismatch",
                            pipeline.id);
            for (UINT32 s = 0; s < pipeline.stage_count; ++s) {
                const DSL_PIPELINE_STAGE_RECORD &stage =
                    analysis->stages[expected_stage - 1];
                if (stage.id != expected_stage ||
                    stage.pipeline_plan_id != pipeline.id ||
                    stage.ordinal != s || stage.buffer_slot != s ||
                    stage.prefetch_distance !=
                        pipeline.prefetch_distance ||
                    stage.issue_point != pipeline.issue_point ||
                    stage.arrival_barrier !=
                        pipeline.arrival_barrier ||
                    stage.wait_point != pipeline.wait_point ||
                    stage.reserved != 0)
                    return DSL_Fetch_Pipeline_Report
                               (diagnostic, "invalid pipeline stage",
                                stage.id);
                ++expected_stage;
            }
            ++expected_plan;
        }
    }
    if (expected_plan != analysis->plans.size() + 1 ||
        expected_fetch != analysis->fetches.size() + 1 ||
        expected_stage != analysis->stages.size() + 1 ||
        !DSL_tensor_evolution_verify(analysis->graph, diagnostic))
        return DSL_Fetch_Pipeline_Report
                   (diagnostic, "pipeline table mismatch", 0);
    return TRUE;
}

void
DSL_fetch_pipeline_print
        (FILE *file, const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    if (file == NULL || !DSL_Fetch_Pipeline_Active(analysis))
        return;
    fprintf(file,
            "CommonFetchPlanIR/CommonPipelineIR: owner=0x%x target=%s "
            "stage=G12 sites=%u plans=%u fetches=%u stages=%u "
            "select=%s apply=no\n",
            analysis->owner_pu_st,
            DSL_target_profile_name(analysis->control.target_profile_id),
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->plans.size(),
            (UINT32)analysis->fetches.size(),
            (UINT32)analysis->stages.size(),
            analysis->control.select_plans ? "yes" : "no");
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_FETCH_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  fetch-site id=%u node=%u value=%u tile_site=%u "
                "tile_plan=%u plans=%u selected=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.tile_site_id, site.selected_tile_plan_id,
                site.pipeline_plan_count, site.selected_plan_id);
        for (UINT32 j = 0; j < site.pipeline_plan_count; ++j) {
            const DSL_FETCH_PLAN_RECORD &pipeline =
                analysis->plans[site.first_pipeline_plan_id - 1 + j];
            fprintf(file,
                    "    pipeline-plan id=%u engine=%s buffers=%u "
                    "distance=%u issue=%s arrival=%s wait=%s edge=%s "
                    "buffering=%llu/%llu barriers=%u legality=%s "
                    "reason=%s candidate=%u plan=%u\n",
                    pipeline.id,
                    DSL_memory_movement_name(pipeline.engine),
                    pipeline.stage_count, pipeline.prefetch_distance,
                    DSL_fetch_issue_point_name(pipeline.issue_point),
                    DSL_fetch_barrier_name(pipeline.arrival_barrier),
                    DSL_fetch_wait_point_name(pipeline.wait_point),
                    DSL_tile_edge_policy_name(pipeline.edge_policy),
                    (unsigned long long)pipeline.buffering_bytes,
                    (unsigned long long)pipeline.buffering_capacity_bytes,
                    pipeline.barrier_count,
                    DSL_opt_legality_name(pipeline.legality),
                    DSL_opt_rejection_reason_name
                        (pipeline.rejection_reason),
                    pipeline.candidate_id,
                    pipeline.optimization_plan_id);
            fprintf(file,
                    "      movement raw=%llu hidden=%llu unhidden=%llu\n",
                    (unsigned long long)pipeline.raw_movement_cost,
                    (unsigned long long)pipeline.hidden_movement_cost,
                    (unsigned long long)pipeline.unhidden_movement_cost);
            for (UINT32 f = 0; f < pipeline.fetch_count; ++f) {
                const DSL_FETCH_RECORD &fetch =
                    analysis->fetches[pipeline.first_fetch_id - 1 + f];
                fprintf(file,
                        "      fetch id=%u operand=%u value=%u ty=%u "
                        "source=%s destination=%s bytes=%llu "
                        "transaction=%u alignment=%u raw=%llu hidden=%llu "
                        "unhidden=%llu evolution=%u flags=0x%x\n",
                        fetch.id, fetch.operand_ordinal, fetch.value_id,
                        TY_IDX_index(fetch.descriptor_ty),
                        DSL_memory_tier_name(fetch.source_tier_kind),
                        DSL_memory_tier_name(fetch.destination_tier_kind),
                        (unsigned long long)fetch.bytes_per_stage,
                        fetch.transaction_bytes, fetch.minimum_alignment,
                        (unsigned long long)fetch.raw_movement_cost,
                        (unsigned long long)fetch.hidden_movement_cost,
                        (unsigned long long)fetch.unhidden_movement_cost,
                        fetch.result_evolution_node_id, fetch.flags);
            }
            for (UINT32 s = 0; s < pipeline.stage_count; ++s) {
                const DSL_PIPELINE_STAGE_RECORD &stage =
                    analysis->stages[pipeline.first_stage_id - 1 + s];
                fprintf(file,
                        "      pipeline-stage id=%u ordinal=%u slot=%u "
                        "distance=%u issue=%s arrival=%s wait=%s\n",
                        stage.id, stage.ordinal, stage.buffer_slot,
                        stage.prefetch_distance,
                        DSL_fetch_issue_point_name(stage.issue_point),
                        DSL_fetch_barrier_name(stage.arrival_barrier),
                        DSL_fetch_wait_point_name(stage.wait_point));
            }
        }
        DSL_opt_plan_print(file, analysis->plan_contexts[i]);
    }
    DSL_memory_hierarchy_print
        (file, analysis->control.target_profile_id);
    DSL_tensor_evolution_print(file, analysis->graph);
}

UINT32
DSL_fetch_pipeline_site_count
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_fetch_pipeline_plan_count
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->plans.size();
}

UINT32
DSL_fetch_pipeline_fetch_count
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->fetches.size();
}

UINT32
DSL_fetch_pipeline_stage_count
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->stages.size();
}

BOOL
DSL_fetch_pipeline_get_site
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_FETCH_SITE_ID id, DSL_FETCH_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->sites.size())
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_get_plan
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_FETCH_PLAN_ID id, DSL_FETCH_PLAN_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->plans.size())
        return FALSE;
    *record = analysis->plans[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_get_fetch
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_FETCH_RECORD_ID id, DSL_FETCH_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->fetches.size())
        return FALSE;
    *record = analysis->fetches[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_get_stage
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
         DSL_PIPELINE_STAGE_ID id, DSL_PIPELINE_STAGE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->stages.size())
        return FALSE;
    *record = analysis->stages[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_fetch_pipeline_get_plan_context
        (const DSL_FETCH_PIPELINE_ANALYSIS *analysis, DSL_FETCH_SITE_ID id)
{
    return analysis == NULL || id == 0 ||
           id > analysis->plan_contexts.size() ?
           NULL : analysis->plan_contexts[id - 1];
}
