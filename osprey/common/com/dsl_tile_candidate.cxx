/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-9 check-only hierarchical tile-plan construction.
 *
 * This file deliberately builds runtime-only candidates: it reads logical
 * DSL/TensorDescriptorIR facts, reuses AIO-2 planning and AIO-8 target
 * resources, and adds provisional TensorEvolutionGraph overlays. It does not
 * rewrite executable or binary WHIRL. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO9-HIERARCHICAL-TILING.md.
 */

#include <algorithm>
#include <string.h>
#include <vector>

#include "dsl_tile_candidate.h"
#include "dsl_opcode.h"
#include "dsl_shape.h"
#include "pu_info.h"
#include "strtab.h"

typedef struct {
    UINT32 family;
    UINT32 cta_m;
    UINT32 cta_n;
    UINT32 cta_k;
    UINT32 warp_m;
    UINT32 warp_n;
    UINT32 thread_m;
    UINT32 thread_n;
    UINT32 instruction_m;
    UINT32 instruction_n;
    UINT32 instruction_k;
    UINT32 vector_width;
} DSL_TILE_FAMILY_SEED;

struct dsl_tile_analysis {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    const DSL_RESIDENCY_ANALYSIS *residency;
    DSL_TILE_CONTROL control;
    std::vector<DSL_TILE_SITE_RECORD> sites;
    std::vector<DSL_TILE_PLAN_RECORD> tile_plans;
    std::vector<DSL_TILE_STAGE_RECORD> stages;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    BOOL built;
};

static const char *DSL_tile_phase_name[] = {
    "P7.0", "P7.1", "P7.2", "P7.3", "P7.4", "P7.5",
    "P7.6", "P7.7", "P7.8", "P7.9", "P7.10", "P7.11"
};

static const char *DSL_tile_level_name[] = {
    "unknown", "problem", "output", "coalescing", "reduction",
    "shared", "resource", "thread", "register_reuse", "vector",
    "family", "warp", "instruction"
};

static const char *DSL_tile_family_name[] = {
    "unknown", "baseline", "cuda_64", "cuda_128", "blackwell_wide"
};

static const char *DSL_tile_edge_name[] = {
    "unknown", "exact", "predicated"
};

static const char *DSL_tile_instruction_name[] = {
    "unknown", "scalar_fma", "vector_fma"
};

static const DSL_TILE_FAMILY_SEED DSL_cuda_64 = {
    DSL_TILE_FAMILY_CUDA_64,
    64, 64, 16, 32, 32, 8, 4, 1, 4, 1, 4
};

static const DSL_TILE_FAMILY_SEED DSL_cuda_128 = {
    DSL_TILE_FAMILY_CUDA_128,
    128, 128, 32, 64, 32, 8, 8, 1, 4, 1, 4
};

static const DSL_TILE_FAMILY_SEED DSL_blackwell_wide = {
    DSL_TILE_FAMILY_BLACKWELL_WIDE,
    128, 256, 32, 64, 32, 8, 8, 1, 4, 1, 4
};

static BOOL
DSL_Tile_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL tile analysis error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Tile_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Tile_Active (const DSL_TILE_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Tile_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_Tile_Phase_Name (UINT32 phase)
{
    return phase < DSL_TILE_PHASE_COUNT ?
           DSL_tile_phase_name[phase] : "unknown";
}

const char *
DSL_Tile_Level_Name (UINT32 level)
{
    return level < sizeof(DSL_tile_level_name) /
                       sizeof(DSL_tile_level_name[0]) ?
           DSL_tile_level_name[level] : "unknown";
}

const char *
DSL_Tile_Family_Name (UINT32 family)
{
    return family < sizeof(DSL_tile_family_name) /
                        sizeof(DSL_tile_family_name[0]) ?
           DSL_tile_family_name[family] : "unknown";
}

const char *
DSL_Tile_Edge_Policy_Name (UINT32 policy)
{
    return policy < sizeof(DSL_tile_edge_name) /
                        sizeof(DSL_tile_edge_name[0]) ?
           DSL_tile_edge_name[policy] : "unknown";
}

const char *
DSL_Tile_Instruction_Name (UINT32 family)
{
    return family < sizeof(DSL_tile_instruction_name) /
                        sizeof(DSL_tile_instruction_name[0]) ?
           DSL_tile_instruction_name[family] : "unknown";
}

void
DSL_Tile_Control_Init (DSL_TILE_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->select_plans = 1;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->maximum_phase = DSL_TILE_PHASE_P7_11_INSTRUCTION;
    control->max_sites = 16;
    control->max_plans_per_site = 8;
    control->enable_cuda_64 = 1;
    control->enable_cuda_128 = 1;
    control->enable_blackwell_wide = 1;
}

static BOOL
DSL_Tile_Control_Valid (const DSL_TILE_CONTROL &control)
{
    return control.generate_candidates <= 1 &&
           control.select_plans <= 1 &&
           control.apply_transformation == 0 &&
           control.target_profile_id >= DSL_TARGET_PROFILE_CPU_BASELINE &&
           control.target_profile_id <= DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
           control.maximum_phase < DSL_TILE_PHASE_COUNT &&
           control.max_sites != 0 && control.max_plans_per_site >= 2 &&
           control.enable_cuda_64 <= 1 && control.enable_cuda_128 <= 1 &&
           control.enable_blackwell_wide <= 1 && control.reserved == 0 &&
           (!control.select_plans || control.generate_candidates);
}

static BOOL
DSL_Tile_Mul_U64 (UINT64 lhs, UINT64 rhs, UINT64 *result)
{
    if (result == NULL || (rhs != 0 && lhs > DSL_TILE_UNKNOWN_U64 / rhs))
        return FALSE;
    *result = lhs * rhs;
    return TRUE;
}

static BOOL
DSL_Tile_Add_U64 (UINT64 lhs, UINT64 rhs, UINT64 *result)
{
    if (result == NULL || lhs > DSL_TILE_UNKNOWN_U64 - rhs)
        return FALSE;
    *result = lhs + rhs;
    return TRUE;
}

static UINT64
DSL_Tile_Ceil_Div (UINT64 value, UINT64 divisor)
{
    return divisor == 0 ? 0 : value / divisor + (value % divisor != 0);
}

static BOOL
DSL_Tile_Attribute_Equals
        (const DSL_IR_NODE_RECORD &node, const char *name,
         const char *expected)
{
    for (UINT32 i = 0; i < node.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (!DSL_IR_Image_Get_Attribute
                 (node.first_attribute_id + i, &attribute))
            return FALSE;
        if (strcmp(Index_To_Str(attribute.name), name) == 0)
            return attribute.value_kind !=
                       DSL_IR_ATTRIBUTE_VALUE_UNKNOWN &&
                   strcmp(Index_To_Str(attribute.value), expected) == 0;
    }
    return FALSE;
}

static BOOL
DSL_Tile_Operand
        (const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
         DSL_IR_VALUE_RECORD *value)
{
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    return value != NULL && ordinal < node.operand_count &&
           DSL_IR_Image_Get_Value_Reference
               (node.first_operand_reference_id + ordinal, &reference) &&
           reference.owner_node_id == node.id &&
           reference.ordinal == ordinal &&
           DSL_IR_Image_Get_Value(reference.value_id, value);
}

static BOOL
DSL_Tile_Node_Has_State_Effect (DSL_IR_NODE_ID node_id)
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
DSL_Tile_Problem
        (const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_IR_NODE_RECORD *node, UINT64 *m, UINT64 *n, UINT64 *k,
         UINT64 *element_bytes)
{
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_IR_VALUE_RECORD kid0;
    DSL_IR_VALUE_RECORD kid1;
    UINT64 kid0_shape[2];
    UINT64 kid1_shape[2];
    UINT64 result_shape[2];
    UINT32 rank0 = 0;
    UINT32 rank1 = 0;
    UINT32 result_rank = 0;
    if (node == NULL || m == NULL || n == NULL || k == NULL ||
        element_bytes == NULL || tensor.producer_node_id == 0 ||
        tensor.rank != 2 ||
        !DSL_IR_Image_Get_Node(tensor.producer_node_id, node) ||
        (node->flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node->opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != OPR_DSLMATMUL ||
        descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
        descriptor.shape_rule != DSL_SHAPE_RULE_CONTRACTION ||
        node->operand_count != 2 ||
        DSL_Tile_Node_Has_State_Effect(node->id) ||
        !DSL_Tile_Attribute_Equals
             (*node, "attr.transpose_kid0", "false") ||
        !DSL_Tile_Attribute_Equals
             (*node, "attr.transpose_kid1", "false") ||
        !DSL_Tile_Operand(*node, 0, &kid0) ||
        !DSL_Tile_Operand(*node, 1, &kid1) ||
        !DSL_Shape_Parse_Static_Dimensions
             (TY_tensor_attribute(kid0.ty, TY_TENSOR_SCHEMA_SHAPE),
              kid0_shape, 2, &rank0) ||
        !DSL_Shape_Parse_Static_Dimensions
             (TY_tensor_attribute(kid1.ty, TY_TENSOR_SCHEMA_SHAPE),
              kid1_shape, 2, &rank1) ||
        !DSL_Shape_Parse_Static_Dimensions
             (TY_tensor_attribute(tensor.descriptor_ty,
                                  TY_TENSOR_SCHEMA_SHAPE),
              result_shape, 2, &result_rank) ||
        rank0 != 2 || rank1 != 2 || result_rank != 2 ||
        kid0_shape[1] != kid1_shape[0] ||
        result_shape[0] != kid0_shape[0] ||
        result_shape[1] != kid1_shape[1] ||
        TY_tensor_element_ty(kid0.ty) !=
            TY_tensor_element_ty(tensor.descriptor_ty) ||
        TY_tensor_element_ty(kid1.ty) !=
            TY_tensor_element_ty(tensor.descriptor_ty))
        return FALSE;
    *m = result_shape[0];
    *n = result_shape[1];
    *k = kid0_shape[1];
    *element_bytes = TY_size(TY_tensor_element_ty(tensor.descriptor_ty));
    return *m != 0 && *n != 0 && *k != 0 && *element_bytes != 0;
}

static BOOL
DSL_Tile_Source_Evolution
        (const DSL_TILE_ANALYSIS *analysis,
         const DSL_TENSOR_FACT_RECORD &tensor,
         DSL_TENSOR_EVOLUTION_NODE_RECORD *source)
{
    if (!DSL_Tensor_Evolution_Find_Semantic_Root
             (analysis->graph, tensor.value_id, source))
        return FALSE;
    if (analysis->residency == NULL)
        return TRUE;
    for (UINT32 site_id = 1;
         site_id <= DSL_Residency_Site_Count(analysis->residency);
         ++site_id) {
        DSL_RESIDENCY_SITE_RECORD site;
        if (!DSL_Residency_Get_Site
                 (analysis->residency, site_id, &site))
            return FALSE;
        if (site.semantic_value_id != tensor.value_id)
            continue;
        for (UINT32 i = 0; i < site.alternative_count; ++i) {
            DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
            if (!DSL_Residency_Get_Alternative
                     (analysis->residency,
                      site.first_alternative_id + i, &alternative))
                return FALSE;
            if (alternative.plan_id == site.selected_plan_id)
                return DSL_Tensor_Evolution_Get_Node
                           (analysis->graph,
                            alternative.result_evolution_node_id, source);
        }
        return TRUE;
    }
    return TRUE;
}

static void
DSL_Tile_Cost_Term
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
DSL_Tile_Unknown_Cost (DSL_OPT_COST_TERM *term)
{
    memset(term, 0, sizeof(*term));
}

static BOOL
DSL_Tile_Add_Stage
        (DSL_TILE_ANALYSIS *analysis, DSL_TILE_PLAN_RECORD *plan,
         UINT32 phase, UINT32 level, UINT64 m, UINT64 n, UINT64 k)
{
    DSL_TILE_STAGE_RECORD stage;
    memset(&stage, 0, sizeof(stage));
    stage.id = analysis->stages.size() + 1;
    stage.tile_plan_id = plan->id;
    stage.ordinal = plan->stage_count;
    stage.phase = phase;
    stage.level_kind = level;
    stage.extent_m = m;
    stage.extent_n = n;
    stage.extent_k = k;
    analysis->stages.push_back(stage);
    ++plan->stage_count;
    return TRUE;
}

static BOOL
DSL_Tile_Add_Stages
        (DSL_TILE_ANALYSIS *analysis, DSL_TILE_PLAN_RECORD *plan)
{
    /* Keep each P7 refinement visible as its matching G-stage evidence. */
    plan->first_stage_id = analysis->stages.size() + 1;
    if (!DSL_Tile_Add_Stage
             (analysis, plan, DSL_TILE_PHASE_P7_0_BASELINE,
              DSL_TILE_LEVEL_PROBLEM, plan->problem_m, plan->problem_n,
              plan->problem_k))
        return FALSE;
    if ((plan->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0)
        return TRUE;
    const UINT32 last = analysis->control.maximum_phase;
    if (last >= DSL_TILE_PHASE_P7_1_OUTPUT)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_1_OUTPUT,
             DSL_TILE_LEVEL_OUTPUT, plan->cta_m, plan->cta_n, 1);
    if (last >= DSL_TILE_PHASE_P7_2_COALESCING)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_2_COALESCING,
             DSL_TILE_LEVEL_COALESCING, 1, plan->vector_width,
             plan->transaction_bytes);
    if (last >= DSL_TILE_PHASE_P7_3_REDUCTION)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_3_REDUCTION,
             DSL_TILE_LEVEL_REDUCTION, 1, 1, plan->cta_k);
    if (last >= DSL_TILE_PHASE_P7_4_SHARED)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_4_SHARED,
             DSL_TILE_LEVEL_SHARED, plan->cta_m, plan->cta_n,
             plan->cta_k);
    if (last >= DSL_TILE_PHASE_P7_5_RESOURCE)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_5_RESOURCE,
             DSL_TILE_LEVEL_RESOURCE, plan->threads_per_cta,
             plan->warps_per_cta, plan->maximum_buffer_stages);
    if (last >= DSL_TILE_PHASE_P7_6_THREAD)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_6_THREAD,
             DSL_TILE_LEVEL_THREAD, plan->thread_m, plan->thread_n, 1);
    if (last >= DSL_TILE_PHASE_P7_7_REGISTER_REUSE)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_7_REGISTER_REUSE,
             DSL_TILE_LEVEL_REGISTER_REUSE, plan->thread_m,
             plan->thread_n, plan->cta_k);
    if (last >= DSL_TILE_PHASE_P7_8_VECTOR)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_8_VECTOR,
             DSL_TILE_LEVEL_VECTOR, 1, plan->vector_width, 1);
    if (last >= DSL_TILE_PHASE_P7_9_FAMILY)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_9_FAMILY,
             DSL_TILE_LEVEL_FAMILY, plan->cta_m, plan->cta_n,
             plan->cta_k);
    if (last >= DSL_TILE_PHASE_P7_10_WARP)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_10_WARP,
             DSL_TILE_LEVEL_WARP, plan->warp_m, plan->warp_n,
             plan->cta_k);
    if (last >= DSL_TILE_PHASE_P7_11_INSTRUCTION)
        DSL_Tile_Add_Stage
            (analysis, plan, DSL_TILE_PHASE_P7_11_INSTRUCTION,
             DSL_TILE_LEVEL_INSTRUCTION, plan->instruction_m,
             plan->instruction_n, plan->instruction_k);
    return TRUE;
}

static BOOL
DSL_Tile_Problem_Cost
        (UINT64 m, UINT64 n, UINT64 k, UINT64 element_bytes,
         UINT64 *operations, UINT64 *read_bytes, UINT64 *write_bytes)
{
    UINT64 mn;
    UINT64 mnk;
    UINT64 reads;
    if (!DSL_Tile_Mul_U64(m, n, &mn) ||
        !DSL_Tile_Mul_U64(mn, k, &mnk) ||
        !DSL_Tile_Mul_U64(mnk, 2, operations) ||
        !DSL_Tile_Mul_U64(mnk, 2, &reads) ||
        !DSL_Tile_Mul_U64(reads, element_bytes, read_bytes) ||
        !DSL_Tile_Mul_U64(mn, element_bytes, write_bytes))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_Tile_Tiled_Traffic
        (DSL_TILE_PLAN_RECORD *plan, UINT64 element_bytes)
{
    UINT64 a_elements;
    UINT64 b_elements;
    UINT64 per_k_tile;
    UINT64 tile_count;
    UINT64 read_elements;
    UINT64 mn;
    if (!DSL_Tile_Mul_U64(plan->cta_m, plan->cta_k, &a_elements) ||
        !DSL_Tile_Mul_U64(plan->cta_k, plan->cta_n, &b_elements) ||
        !DSL_Tile_Add_U64(a_elements, b_elements, &per_k_tile) ||
        !DSL_Tile_Mul_U64
             (DSL_Tile_Ceil_Div(plan->problem_m, plan->cta_m),
              DSL_Tile_Ceil_Div(plan->problem_n, plan->cta_n),
              &tile_count) ||
        !DSL_Tile_Mul_U64
             (tile_count,
              DSL_Tile_Ceil_Div(plan->problem_k, plan->cta_k),
              &tile_count) ||
        !DSL_Tile_Mul_U64(tile_count, per_k_tile, &read_elements) ||
        !DSL_Tile_Mul_U64(read_elements, element_bytes,
                          &plan->gmem_read_bytes) ||
        !DSL_Tile_Mul_U64(plan->problem_m, plan->problem_n, &mn) ||
        !DSL_Tile_Mul_U64(mn, element_bytes,
                          &plan->gmem_write_bytes))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_Tile_Add_Optimization_Plan
        (DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_RECORD *site,
         DSL_TILE_PLAN_RECORD *tile, BOOL complete_cost,
         FILE *diagnostic)
{
    /* Adapt the typed tile record to the shared AIO-2 candidate/plan service. */
    DSL_OPT_PLAN_CONTEXT *context = analysis->plans.back();
    DSL_OPT_CANDIDATE_INPUT candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
                     DSL_OPT_CANDIDATE_BASELINE : DSL_OPT_CANDIDATE_TILE;
    candidate.semantic_node_id = site->semantic_node_id;
    candidate.source_evolution_node_id = tile->source_evolution_node_id;
    candidate.result_evolution_node_id = tile->result_evolution_node_id;
    candidate.parent_candidate_id =
        (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
        0 : analysis->tile_plans[site->first_tile_plan_id - 1].candidate_id;
    candidate.legality = tile->legality;
    candidate.rejection_reason = tile->rejection_reason;
    candidate.ordering_key = tile->id;
    candidate.flags = (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
                      DSL_OPT_CANDIDATE_FLAG_BASELINE :
                      DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_Opt_Plan_Add_Candidate
             (context, &candidate, &tile->candidate_id, diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = tile->id;
    if (complete_cost) {
        UINT64 compute = std::max((UINT64)1,
                                  tile->operation_count / 1024);
        UINT64 memory = std::max
            ((UINT64)1,
             (tile->gmem_read_bytes + tile->gmem_write_bytes) / 1024);
        for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
            DSL_Tile_Cost_Term
                (&cost.terms[i],
                 i == DSL_OPT_COST_COMPUTE ? compute :
                 i == DSL_OPT_COST_MEMORY_UNHIDDEN ? memory :
                 i == DSL_OPT_COST_SYNC ? tile->barrier_count : 0,
                 DSL_OPT_COST_CONFIDENCE_LOW,
                 (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
                     DSL_OPT_COST_EVIDENCE_BASELINE_POLICY :
                     DSL_OPT_COST_EVIDENCE_TARGET_MODEL);
    } else {
        for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
            DSL_Tile_Unknown_Cost(&cost.terms[i]);
    }
    DSL_OPT_COST_ID cost_id;
    if (!DSL_Opt_Plan_Add_Cost(context, &cost, &cost_id, diagnostic))
        return FALSE;

    DSL_OPT_CANDIDATE_ID member = tile->candidate_id;
    DSL_OPT_PLAN_INPUT plan;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member;
    plan.candidate_count = 1;
    plan.cost_id = cost_id;
    plan.fallback_plan_id =
        (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
        0 : site->baseline_plan_id;
    plan.legality = tile->legality;
    plan.rejection_reason = tile->rejection_reason;
    plan.ordering_key = tile->id;
    plan.flags = (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
                 DSL_OPT_PLAN_FLAG_BASELINE :
                 DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_Opt_Plan_Add_Plan
             (context, &plan, &tile->optimization_plan_id, diagnostic))
        return FALSE;
    if ((tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0)
        site->baseline_plan_id = tile->optimization_plan_id;
    return TRUE;
}

static BOOL
DSL_Tile_Add_Baseline
        (DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_RECORD *site,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_EVOLUTION_NODE_RECORD &source,
         UINT64 m, UINT64 n, UINT64 k, UINT64 element_bytes,
         FILE *diagnostic)
{
    DSL_TILE_PLAN_RECORD tile;
    memset(&tile, 0, sizeof(tile));
    tile.id = analysis->tile_plans.size() + 1;
    tile.site_id = site->id;
    tile.target_profile_id = analysis->control.target_profile_id;
    tile.family = DSL_TILE_FAMILY_BASELINE;
    tile.source_descriptor_ty = tensor.descriptor_ty;
    tile.source_evolution_node_id = source.id;
    tile.result_evolution_node_id = source.id;
    tile.problem_m = m;
    tile.problem_n = n;
    tile.problem_k = k;
    tile.vector_width = 1;
    tile.transaction_bytes = element_bytes;
    tile.edge_policy = DSL_TILE_EDGE_EXACT;
    tile.instruction_family = DSL_TILE_INSTRUCTION_SCALAR_FMA;
    tile.legality = DSL_OPT_LEGALITY_PROVEN;
    tile.rejection_reason = DSL_OPT_REJECT_NONE;
    tile.flags = DSL_TILE_PLAN_FLAG_BASELINE |
                 DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING;
    if (!DSL_Tile_Problem_Cost
             (m, n, k, element_bytes, &tile.operation_count,
              &tile.gmem_read_bytes, &tile.gmem_write_bytes) ||
        !DSL_Tile_Add_Stages(analysis, &tile))
        return DSL_Tile_Report(diagnostic, "baseline cost overflow", tile.id);
    analysis->tile_plans.push_back(tile);
    if (!DSL_Tile_Add_Optimization_Plan
             (analysis, site, &analysis->tile_plans.back(), TRUE,
              diagnostic))
        return FALSE;
    ++site->tile_plan_count;
    return TRUE;
}

static void
DSL_Tile_Classify
        (const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         DSL_TILE_PLAN_RECORD *tile)
{
    /* Resource feasibility is meaningful only after locality and ownership. */
    if (locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT) {
        tile->legality = DSL_OPT_LEGALITY_REJECTED;
        tile->rejection_reason = DSL_OPT_REJECT_EFFECT;
    } else if (locality.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE) {
        tile->legality = DSL_OPT_LEGALITY_REJECTED;
        tile->rejection_reason = DSL_OPT_REJECT_OWNERSHIP;
    } else if (locality.lifetime_state !=
                   DSL_TENSOR_LIFETIME_EXACT_BLOCK) {
        tile->legality = DSL_OPT_LEGALITY_UNKNOWN;
        tile->rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else if (tile->threads_per_cta == 0 ||
               tile->threads_per_cta > 1024 ||
               tile->shared_bytes_per_stage >
                   tile->shared_capacity_bytes ||
               tile->registers_per_thread >
                   tile->register_capacity_per_thread ||
               tile->maximum_buffer_stages == 0) {
        tile->legality = DSL_OPT_LEGALITY_REJECTED;
        tile->rejection_reason = DSL_OPT_REJECT_RESOURCE;
    } else {
        tile->legality = DSL_OPT_LEGALITY_PROVEN;
        tile->rejection_reason = DSL_OPT_REJECT_NONE;
    }
}

static BOOL
DSL_Tile_Add_Family
        (DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_RECORD *site,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const DSL_TENSOR_LOCALITY_FACT_RECORD &locality,
         const DSL_TENSOR_EVOLUTION_NODE_RECORD &source,
         const DSL_TILE_FAMILY_SEED &seed,
         UINT64 m, UINT64 n, UINT64 k, UINT64 element_bytes,
         FILE *diagnostic)
{
    DSL_MEMORY_TIER_RECORD shared;
    DSL_MEMORY_TIER_RECORD registers;
    DSL_TILE_PLAN_RECORD tile;
    UINT64 shared_a;
    UINT64 shared_b;
    UINT64 shared_elements;
    UINT64 register_elements;
    memset(&tile, 0, sizeof(tile));
    tile.id = analysis->tile_plans.size() + 1;
    tile.site_id = site->id;
    tile.target_profile_id = analysis->control.target_profile_id;
    tile.family = seed.family;
    tile.source_descriptor_ty = tensor.descriptor_ty;
    tile.source_evolution_node_id = source.id;
    tile.problem_m = m;
    tile.problem_n = n;
    tile.problem_k = k;
    tile.cta_m = seed.cta_m;
    tile.cta_n = seed.cta_n;
    tile.cta_k = seed.cta_k;
    tile.warp_m = seed.warp_m;
    tile.warp_n = seed.warp_n;
    tile.thread_m = seed.thread_m;
    tile.thread_n = seed.thread_n;
    tile.instruction_m = seed.instruction_m;
    tile.instruction_n = seed.instruction_n;
    tile.instruction_k = seed.instruction_k;
    tile.vector_width = seed.vector_width;
    tile.transaction_bytes = 128;
    tile.edge_policy =
        m % seed.cta_m == 0 && n % seed.cta_n == 0 &&
        k % seed.cta_k == 0 ?
        DSL_TILE_EDGE_EXACT : DSL_TILE_EDGE_PREDICATED;
    tile.instruction_family = DSL_TILE_INSTRUCTION_VECTOR_FMA;
    tile.flags = DSL_TILE_PLAN_FLAG_PROVISIONAL |
                 DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING |
                 DSL_TILE_PLAN_FLAG_PREFETCH_HINT;
    if (tile.edge_policy == DSL_TILE_EDGE_EXACT)
        tile.flags |= DSL_TILE_PLAN_FLAG_TMA_CANDIDATE;
    if (!DSL_Tile_Problem_Cost
             (m, n, k, element_bytes, &tile.operation_count,
              &tile.gmem_read_bytes, &tile.gmem_write_bytes) ||
        !DSL_Tile_Tiled_Traffic(&tile, element_bytes) ||
        seed.thread_m == 0 || seed.thread_n == 0 ||
        seed.cta_m % seed.thread_m != 0 ||
        seed.cta_n % seed.thread_n != 0 ||
        !DSL_Tile_Mul_U64(seed.cta_m, seed.cta_k, &shared_a) ||
        !DSL_Tile_Mul_U64(seed.cta_k, seed.cta_n, &shared_b) ||
        !DSL_Tile_Add_U64(shared_a, shared_b, &shared_elements) ||
        !DSL_Tile_Mul_U64
             (shared_elements, element_bytes,
              &tile.shared_bytes_per_stage) ||
        !DSL_Tile_Mul_U64
             (seed.thread_m, seed.thread_n, &register_elements) ||
        !DSL_Tile_Add_U64
             (register_elements, seed.thread_m + seed.thread_n,
              &register_elements) ||
        register_elements > UINT32_MAX / element_bytes)
        return DSL_Tile_Report(diagnostic, "tile resource overflow", tile.id);
    tile.registers_per_thread = register_elements * element_bytes;
    tile.threads_per_cta =
        (seed.cta_m / seed.thread_m) *
        (seed.cta_n / seed.thread_n);
    tile.warps_per_cta =
        DSL_Tile_Ceil_Div(tile.threads_per_cta, 32);
    tile.barrier_count = 2;
    if (!DSL_Memory_Hierarchy_Find_Tier
             (analysis->control.target_profile_id,
              DSL_MEMORY_TIER_SHARED, &shared) ||
        !DSL_Memory_Hierarchy_Find_Tier
             (analysis->control.target_profile_id,
              DSL_MEMORY_TIER_REGISTER, &registers))
        return DSL_Tile_Report(diagnostic, "missing target resources", tile.id);
    tile.shared_capacity_bytes = shared.capacity_bytes;
    tile.register_capacity_per_thread =
        registers.capacity_bytes > ~(UINT32)0 ?
        ~(UINT32)0 : (UINT32)registers.capacity_bytes;
    if (tile.shared_bytes_per_stage != 0 &&
        shared.capacity_bytes != DSL_MEMORY_CAPACITY_UNKNOWN)
        tile.maximum_buffer_stages = std::min
            ((UINT64)3,
             shared.capacity_bytes / tile.shared_bytes_per_stage);

    if (analysis->control.maximum_phase <
            DSL_TILE_PHASE_P7_5_RESOURCE) {
        tile.legality = DSL_OPT_LEGALITY_UNKNOWN;
        tile.rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else {
        DSL_Tile_Classify(locality, &tile);
    }
    if (!DSL_Tensor_Evolution_Add_Tile
             (analysis->graph, source.id, tile.id,
              &tile.result_evolution_node_id, &tile.evolution_edge_id,
              diagnostic) ||
        !DSL_Tile_Add_Stages(analysis, &tile))
        return FALSE;
    analysis->tile_plans.push_back(tile);
    BOOL complete_cost =
        analysis->control.maximum_phase >= DSL_TILE_PHASE_P7_5_RESOURCE;
    if (!DSL_Tile_Add_Optimization_Plan
             (analysis, site, &analysis->tile_plans.back(), complete_cost,
              diagnostic))
        return FALSE;
    ++site->tile_plan_count;
    return TRUE;
}

DSL_TILE_ANALYSIS *
DSL_Tile_Create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_RESIDENCY_ANALYSIS *residency,
         const DSL_TILE_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_Tensor_Evolution_Owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Tile_Control_Valid(*control) ||
        !DSL_Tensor_Evolution_Verify(graph, diagnostic) ||
        !DSL_Tensor_Analysis_Verify(tensor_analysis, diagnostic) ||
        !DSL_Tensor_Locality_Verify(locality, diagnostic) ||
        (residency != NULL &&
         !DSL_Residency_Verify(residency, diagnostic))) {
        DSL_Tile_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_TILE_ANALYSIS *analysis = new DSL_TILE_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->tensor_analysis = tensor_analysis;
    analysis->locality = locality;
    analysis->residency = residency;
    analysis->control = *control;
    analysis->built = FALSE;
    return analysis;
}

void
DSL_Tile_Destroy (DSL_TILE_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_Opt_Plan_Destroy(analysis->plans[i]);
    delete analysis;
}

BOOL
DSL_Tile_Build (DSL_TILE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tile_Active(analysis) || analysis->built)
        return DSL_Tile_Report(diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_Tensor_Analysis_Fact_Count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality;
        DSL_TENSOR_EVOLUTION_NODE_RECORD source;
        DSL_IR_NODE_RECORD node;
        UINT64 m;
        UINT64 n;
        UINT64 k;
        UINT64 element_bytes;
        if (!DSL_Tensor_Analysis_Get_Fact
                 (analysis->tensor_analysis, id, &tensor) ||
            (analysis->control.focus_value_id != 0 &&
             tensor.value_id != analysis->control.focus_value_id))
            continue;
        if (!DSL_Tile_Problem
                 (tensor, &node, &m, &n, &k, &element_bytes))
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Tile_Report
                       (diagnostic, "tile site budget exhausted", id);
        if (!DSL_Tensor_Locality_Find_Fact
                 (analysis->locality, tensor.value_id, &locality) ||
            !DSL_Tile_Source_Evolution(analysis, tensor, &source))
            return DSL_Tile_Report
                       (diagnostic, "missing tile input evidence", id);

        DSL_TILE_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.semantic_node_id = node.id;
        site.semantic_value_id = tensor.value_id;
        site.semantic_root_id = source.semantic_root_id;
        site.source_evolution_node_id = source.id;
        site.first_tile_plan_id = analysis->tile_plans.size() + 1;

        DSL_OPT_PLAN_BUDGET budget;
        budget.max_candidates = analysis->control.max_plans_per_site;
        budget.max_plans = analysis->control.max_plans_per_site;
        DSL_OPT_PLAN_CONTEXT *context = DSL_Opt_Plan_Create
            (analysis->pu, analysis->graph, &budget, diagnostic);
        if (context == NULL)
            return FALSE;
        analysis->plans.push_back(context);
        if (!DSL_Tile_Add_Baseline
                 (analysis, &site, tensor, source, m, n, k,
                  element_bytes, diagnostic))
            return FALSE;
        /* Target adapters bound the candidate family; they do not lower it. */
        BOOL cuda_profile =
            analysis->control.target_profile_id ==
                DSL_TARGET_PROFILE_NVIDIA_HOPPER ||
            analysis->control.target_profile_id ==
                DSL_TARGET_PROFILE_NVIDIA_BLACKWELL;
        if (analysis->control.maximum_phase >=
                DSL_TILE_PHASE_P7_1_OUTPUT && cuda_profile) {
            if (analysis->control.enable_cuda_64 &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_cuda_64, m, n, k, element_bytes, diagnostic))
                return FALSE;
            if (analysis->control.enable_cuda_128 &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_cuda_128, m, n, k, element_bytes, diagnostic))
                return FALSE;
            if (analysis->control.enable_blackwell_wide &&
                analysis->control.target_profile_id ==
                    DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_blackwell_wide, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
        }
        if (analysis->control.select_plans) {
            DSL_OPT_SELECTION_RESULT selection;
            if (!DSL_Opt_Plan_Select
                     (context, analysis->control.target_profile_id,
                      &selection, diagnostic))
                return FALSE;
            site.selected_plan_id = selection.selected_plan_id;
        }
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_Tile_Verify(analysis, diagnostic);
}

BOOL
DSL_Tile_Verify
        (const DSL_TILE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tile_Active(analysis))
        return DSL_Tile_Report(diagnostic, "program unit is not active", 0);
    if (!analysis->built)
        return DSL_Tile_Report(diagnostic, "analysis is not built", 0);
    if (analysis->plans.size() != analysis->sites.size())
        return DSL_Tile_Report(diagnostic, "site plan count mismatch", 0);
    UINT32 expected_plan = 1;
    UINT32 expected_stage = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_TILE_SITE_RECORD &site = analysis->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.first_tile_plan_id != expected_plan ||
            site.tile_plan_count == 0 || site.baseline_plan_id == 0 ||
            site.reserved != 0 ||
            !DSL_Opt_Plan_Verify(analysis->plans[i], diagnostic))
            return DSL_Tile_Report(diagnostic, "invalid tile site", site.id);
        for (UINT32 j = 0; j < site.tile_plan_count; ++j) {
            const DSL_TILE_PLAN_RECORD &tile =
                analysis->tile_plans[expected_plan - 1];
            BOOL baseline = j == 0;
            if (tile.id != expected_plan || tile.site_id != site.id ||
                tile.target_profile_id != analysis->control.target_profile_id ||
                tile.source_descriptor_ty == TY_IDX_ZERO ||
                tile.first_stage_id != expected_stage ||
                tile.stage_count == 0 || tile.problem_m == 0 ||
                tile.problem_n == 0 || tile.problem_k == 0 ||
                tile.candidate_id == 0 ||
                tile.optimization_plan_id == 0 || tile.reserved != 0 ||
                baseline !=
                    ((tile.flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0) ||
                (baseline &&
                 (tile.family != DSL_TILE_FAMILY_BASELINE ||
                  tile.stage_count != 1 ||
                  tile.result_evolution_node_id !=
                      tile.source_evolution_node_id)) ||
                (!baseline &&
                 (tile.family < DSL_TILE_FAMILY_CUDA_64 ||
                  tile.family > DSL_TILE_FAMILY_BLACKWELL_WIDE ||
                  tile.result_evolution_node_id == 0 ||
                  tile.evolution_edge_id == 0)))
                return DSL_Tile_Report
                           (diagnostic, "invalid tile plan", tile.id);
            for (UINT32 s = 0; s < tile.stage_count; ++s) {
                const DSL_TILE_STAGE_RECORD &stage =
                    analysis->stages[expected_stage - 1];
                if (stage.id != expected_stage ||
                    stage.tile_plan_id != tile.id || stage.ordinal != s ||
                    stage.phase != s ||
                    stage.phase > analysis->control.maximum_phase ||
                    stage.level_kind == DSL_TILE_LEVEL_UNKNOWN ||
                    stage.reserved != 0)
                    return DSL_Tile_Report
                               (diagnostic, "invalid tile stage", stage.id);
                ++expected_stage;
            }
            ++expected_plan;
        }
    }
    if (expected_plan != analysis->tile_plans.size() + 1 ||
        expected_stage != analysis->stages.size() + 1 ||
        !DSL_Tensor_Evolution_Verify(analysis->graph, diagnostic))
        return DSL_Tile_Report(diagnostic, "tile table mismatch", 0);
    return TRUE;
}

static void
DSL_Tile_Print_U64 (FILE *file, UINT64 value)
{
    if (value == DSL_TILE_UNKNOWN_U64)
        fprintf(file, "pending");
    else
        fprintf(file, "%llu", (unsigned long long)value);
}

void
DSL_Tile_Print (FILE *file, const DSL_TILE_ANALYSIS *analysis)
{
    if (file == NULL || !DSL_Tile_Active(analysis))
        return;
    fprintf(file,
            "CommonTilePlanIR: owner=0x%x target=%s stage=G%u "
            "sites=%u plans=%u stages=%u select=%s apply=no\n",
            analysis->owner_pu_st,
            DSL_Target_Profile_Name(analysis->control.target_profile_id),
            analysis->control.maximum_phase,
            (UINT32)analysis->sites.size(),
            (UINT32)analysis->tile_plans.size(),
            (UINT32)analysis->stages.size(),
            analysis->control.select_plans ? "yes" : "no");
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_TILE_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  tile-site id=%u node=%u value=%u source_evolution=%u "
                "plans=%u selected=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.source_evolution_node_id, site.tile_plan_count,
                site.selected_plan_id);
        for (UINT32 j = 0; j < site.tile_plan_count; ++j) {
            const DSL_TILE_PLAN_RECORD &tile =
                analysis->tile_plans[site.first_tile_plan_id - 1 + j];
            fprintf(file,
                    "    tile-plan id=%u family=%s problem=[",
                    tile.id, DSL_Tile_Family_Name(tile.family));
            DSL_Tile_Print_U64(file, tile.problem_m);
            fprintf(file, ",");
            DSL_Tile_Print_U64(file, tile.problem_n);
            fprintf(file, ",");
            DSL_Tile_Print_U64(file, tile.problem_k);
            fprintf(file,
                    "] cta=[%u,%u,%u] warp=[%u,%u] thread=[%u,%u] "
                    "instruction=[%u,%u,%u] vector=%u edge=%s "
                    "instruction_family=%s legality=%s reason=%s "
                    "candidate=%u plan=%u evolution=%u stages=%u\n",
                    tile.cta_m, tile.cta_n, tile.cta_k,
                    tile.warp_m, tile.warp_n,
                    tile.thread_m, tile.thread_n,
                    tile.instruction_m, tile.instruction_n,
                    tile.instruction_k, tile.vector_width,
                    DSL_Tile_Edge_Policy_Name(tile.edge_policy),
                    DSL_Tile_Instruction_Name(tile.instruction_family),
                    DSL_Opt_Legality_Name(tile.legality),
                    DSL_Opt_Rejection_Reason_Name(tile.rejection_reason),
                    tile.candidate_id, tile.optimization_plan_id,
                    tile.result_evolution_node_id, tile.stage_count);
            fprintf(file,
                    "      access read=%llu write=%llu ops=%llu "
                    "transaction=%u\n",
                    (unsigned long long)tile.gmem_read_bytes,
                    (unsigned long long)tile.gmem_write_bytes,
                    (unsigned long long)tile.operation_count,
                    tile.transaction_bytes);
            fprintf(file,
                    "      resource shared=%llu/%llu buffers=%u "
                    "register_bytes=%u/%u threads=%u warps=%u barriers=%u\n",
                    (unsigned long long)tile.shared_bytes_per_stage,
                    (unsigned long long)tile.shared_capacity_bytes,
                    tile.maximum_buffer_stages,
                    tile.registers_per_thread,
                    tile.register_capacity_per_thread,
                    tile.threads_per_cta, tile.warps_per_cta,
                    tile.barrier_count);
            for (UINT32 s = 0; s < tile.stage_count; ++s) {
                const DSL_TILE_STAGE_RECORD &stage =
                    analysis->stages[tile.first_stage_id - 1 + s];
                fprintf(file,
                        "      tile-stage id=%u stage=G%u phase=%s "
                        "level=%s extent=[%llu,%llu,%llu]\n",
                        stage.id, stage.phase,
                        DSL_Tile_Phase_Name(stage.phase),
                        DSL_Tile_Level_Name(stage.level_kind),
                        (unsigned long long)stage.extent_m,
                        (unsigned long long)stage.extent_n,
                        (unsigned long long)stage.extent_k);
            }
        }
        DSL_Opt_Plan_Print(file, analysis->plans[i]);
    }
    DSL_Tensor_Evolution_Print(file, analysis->graph);
}

UINT32
DSL_Tile_Site_Count (const DSL_TILE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_Tile_Plan_Count (const DSL_TILE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->tile_plans.size();
}

UINT32
DSL_Tile_Stage_Count (const DSL_TILE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->stages.size();
}

BOOL
DSL_Tile_Get_Site
        (const DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_ID id,
         DSL_TILE_SITE_RECORD *record)
{
    if (analysis == NULL || id == 0 || id > analysis->sites.size() ||
        record == NULL)
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_Tile_Get_Plan
        (const DSL_TILE_ANALYSIS *analysis, DSL_TILE_PLAN_ID id,
         DSL_TILE_PLAN_RECORD *record)
{
    if (analysis == NULL || id == 0 || id > analysis->tile_plans.size() ||
        record == NULL)
        return FALSE;
    *record = analysis->tile_plans[id - 1];
    return TRUE;
}

BOOL
DSL_Tile_Get_Stage
        (const DSL_TILE_ANALYSIS *analysis, DSL_TILE_STAGE_ID id,
         DSL_TILE_STAGE_RECORD *record)
{
    if (analysis == NULL || id == 0 || id > analysis->stages.size() ||
        record == NULL)
        return FALSE;
    *record = analysis->stages[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_Tile_Get_Plan_Context
        (const DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
