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

#include "dsl_tile_candidate_opt.h"
#include "config_dsl.h"
#include "dsl_opt_plan_opt.h"
#include "dsl_tensor_analysis_opt.h"
#include "dsl_opcode.h"
#include "dsl_shape.h"
#include "pu_info.h"
#include "strtab.h"
#include "tracing.h"


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

struct DSL_TILE_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    const DSL_RESIDENCY_ANALYSIS *residency;
    const DSL_RESIDENCY_PLAN_IR *residency_ir;
    VHO_DSL_TILE_CONTROL control;
    std::vector<DSL_TILE_SITE_RECORD> sites;
    std::vector<DSL_TILE_PLAN_RECORD> tile_plans;
    std::vector<DSL_TILE_STAGE_RECORD> stages;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    DSL_TILE_PLAN_IR *ir;
    BOOL built;
};

typedef struct {
    UINT64 bytes;
    UINT64 compute_ns;
    UINT64 memory_ns;
    UINT64 bound_ns;
} DSL_TILE_ROOFLINE;

/* Published H100 SXM FP32/HBM peaks, not a calibrated Hopper-wide ceiling. */
static BOOL
DSL_Tile_Roofline_Target (UINT32 target, UINT64 *fp32_gflops,
                          UINT64 *hbm_gbps)
{
    if (target != DSL_TARGET_PROFILE_NVIDIA_HOPPER)
        return FALSE;
    *fp32_gflops = 67000;
    *hbm_gbps = 3350;
    return TRUE;
}

static BOOL
DSL_Tile_Roofline_Project (const DSL_TILE_PLAN_RECORD &tile,
                            UINT64 fp32_gflops, UINT64 hbm_gbps,
                            DSL_TILE_ROOFLINE *projection)
{
    if (projection == NULL || fp32_gflops == 0 || hbm_gbps == 0 ||
        tile.gmem_read_bytes > ~(UINT64)0 - tile.gmem_write_bytes)
        return FALSE;
    projection->bytes = tile.gmem_read_bytes + tile.gmem_write_bytes;
    projection->compute_ns = tile.operation_count / fp32_gflops +
        (tile.operation_count % fp32_gflops != 0);
    projection->memory_ns = projection->bytes / hbm_gbps +
        (projection->bytes % hbm_gbps != 0);
    projection->bound_ns = std::max(projection->compute_ns,
                                    projection->memory_ns);
    return projection->bytes != 0 && projection->bound_ns != 0;
}

static BOOL
DSL_Tile_Select_With_Roofline (DSL_TILE_ANALYSIS *analysis,
                                const DSL_TILE_SITE_RECORD &site,
                                DSL_OPT_SELECTION_RESULT *result,
                                const char **reason, FILE *diagnostic)
{
    UINT64 fp32_gflops, hbm_gbps;
    DSL_OPT_PLAN_CONTEXT *context = analysis->plans[site.id - 1];
    if (!DSL_Tile_Roofline_Target(analysis->control.target_profile_id,
                                  &fp32_gflops, &hbm_gbps)) {
        *reason = "lowest_complete_relative_cost";
        return VHO_DSL_Opt_Plan_Select
            (context, analysis->control.target_profile_id, result, diagnostic);
    }
    DSL_OPT_SELECTION_RESULT selection;
    memset(&selection, 0, sizeof(selection));
    selection.target_profile_id = analysis->control.target_profile_id;
    UINT64 best_cost = ~(UINT64)0;
    DSL_TILE_ROOFLINE best_roof;
    memset(&best_roof, 0, sizeof(best_roof));
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        const DSL_TILE_PLAN_RECORD &tile =
            analysis->tile_plans[site.first_tile_plan_id - 1 + i];
        DSL_OPT_PLAN_RECORD plan;
        DSL_OPT_COST_RECORD cost;
        DSL_TILE_ROOFLINE roof;
        if (!DSL_opt_plan_get_plan(context, tile.optimization_plan_id, &plan) ||
            !DSL_opt_plan_get_cost(context, plan.cost_id, &cost) ||
            !DSL_Tile_Roofline_Project(tile, fp32_gflops, hbm_gbps, &roof))
            return FALSE;
        if (plan.legality != DSL_OPT_LEGALITY_PROVEN) {
            ++selection.rejected_plan_count;
            continue;
        }
        ++selection.legal_plan_count;
        if (cost.target_profile_id != selection.target_profile_id) {
            ++selection.target_mismatch_count;
            continue;
        }
        if (!cost.complete) {
            ++selection.incomplete_cost_count;
            continue;
        }
        ++selection.complete_cost_count;
        if (cost.total < best_cost) {
            best_cost = cost.total;
            best_roof = roof;
            selection.selected_plan_id = plan.id;
        }
    }
    if (selection.selected_plan_id == 0)
        return FALSE;
    *reason = "lowest_complete_relative_cost";
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        const DSL_TILE_PLAN_RECORD &tile =
            analysis->tile_plans[site.first_tile_plan_id - 1 + i];
        DSL_OPT_PLAN_RECORD plan;
        DSL_OPT_COST_RECORD cost;
        DSL_TILE_ROOFLINE roof;
        if (!DSL_opt_plan_get_plan(context, tile.optimization_plan_id, &plan) ||
            !DSL_opt_plan_get_cost(context, plan.cost_id, &cost) ||
            !DSL_Tile_Roofline_Project(tile, fp32_gflops, hbm_gbps, &roof))
            return FALSE;
        /* Roofline only resolves close relative-cost choices. */
        if (plan.legality != DSL_OPT_LEGALITY_PROVEN || !cost.complete ||
            cost.target_profile_id != selection.target_profile_id ||
            cost.total < best_cost ||
            cost.total - best_cost > best_cost / 33 ||
            roof.bound_ns >= best_roof.bound_ns ||
            roof.bound_ns > best_roof.bound_ns - best_roof.bound_ns / 20)
            continue;
        best_roof = roof;
        selection.selected_plan_id = plan.id;
        *reason = "roofline_near_cost";
    }
    if (!DSL_opt_plan_record_selection(context, &selection, diagnostic))
        return FALSE;
    *result = selection;
    return TRUE;
}

static UINT64 DSL_Tile_Ceil_Div (UINT64 value, UINT64 divisor);

static BOOL
DSL_Tile_Trace_Selection (const DSL_TILE_ANALYSIS *analysis,
                          const DSL_TILE_SITE_RECORD &site,
                          const DSL_OPT_SELECTION_RESULT &selection,
                          const char *selection_reason)
{
    const DSL_OPT_PLAN_CONTEXT *context = analysis->plans[site.id - 1];
    FILE *file = TFile;
    DSL_IR_VALUE_RECORD result_value;
    UINT32 source_line = 0;
    if (DSL_IR_Image_Get_Value(site.semantic_value_id, &result_value) &&
        result_value.st != ST_IDX_ZERO)
        source_line = ST_Line(St_Table[result_value.st]);
    UINT64 fp32_gflops = 0, hbm_gbps = 0;
    BOOL roofline_available = DSL_Tile_Roofline_Target
        (selection.target_profile_id, &fp32_gflops, &hbm_gbps);
    UINT32 trace_version = 7;
    fprintf(file, "AI_OPT %u COST_MODEL 5 valid_predicated_elements "
                  "dynamic_cta_k_barriers bank_conflict_work "
                  "source_warp_sectors smem_all_operand_issues smem_address_work\n",
            trace_version);
    fprintf(file, "AI_OPT %u ROOF_TARGET %u %s %llu %llu "
                  "traffic=global_requests_as_cold_hbm_proxy "
                  "ceiling=published_peak\n",
            trace_version, selection.target_profile_id,
            roofline_available ? "h100_sxm_reference" : "unavailable",
            (unsigned long long)fp32_gflops,
            (unsigned long long)hbm_gbps);
    for (UINT32 i = 0; i < site.tile_plan_count; ++i) {
        const DSL_TILE_PLAN_RECORD &tile =
            analysis->tile_plans[site.first_tile_plan_id - 1 + i];
        DSL_OPT_PLAN_RECORD plan;
        DSL_OPT_COST_RECORD cost;
        if (!DSL_opt_plan_get_plan(context, tile.optimization_plan_id,
                                   &plan) ||
            !DSL_opt_plan_get_cost(context, plan.cost_id, &cost))
            return FALSE;
        DSL_TILE_ROOFLINE roof;
        memset(&roof, 0, sizeof(roof));
        if (roofline_available && !DSL_Tile_Roofline_Project
                                  (tile, fp32_gflops, hbm_gbps, &roof))
            return FALSE;
        fprintf(file,
                "AI_OPT %u TILE_CAND %u %u %u %u %s %s %s %u %s %s "
                "%llu %llu %llu %llu %llu %llu %llu %llu %u %u "
                "%u %u %u %u %llu %llu %llu %llu %llu\n",
                trace_version, site.id, tile.id, tile.candidate_id,
                tile.optimization_plan_id, DSL_tile_family_name(tile.family),
                DSL_opt_legality_name(plan.legality),
                DSL_opt_rejection_reason_name(plan.rejection_reason),
                cost.complete,
                DSL_opt_cost_unit_name(cost.terms[0].unit),
                DSL_opt_cost_confidence_name(cost.minimum_confidence),
                (unsigned long long)cost.terms[DSL_OPT_COST_COMPUTE].amount,
                (unsigned long long)cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN].amount,
                (unsigned long long)cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN].amount,
                (unsigned long long)cost.terms[DSL_OPT_COST_SYNC].amount,
                (unsigned long long)cost.terms[DSL_OPT_COST_LAUNCH].amount,
                (unsigned long long)cost.terms[DSL_OPT_COST_RUNTIME_SELECTION].amount,
                (unsigned long long)cost.total,
                (unsigned long long)tile.shared_bytes_per_stage,
                tile.registers_per_thread, tile.threads_per_cta,
                tile.cta_m, tile.cta_n, tile.cta_k, source_line,
                (unsigned long long)tile.operation_count,
                (unsigned long long)roof.bytes,
                (unsigned long long)roof.compute_ns,
                (unsigned long long)roof.memory_ns,
                (unsigned long long)roof.bound_ns);
        UINT64 grid_m = tile.cta_m ?
            DSL_Tile_Ceil_Div(tile.problem_m, tile.cta_m) : 0;
        UINT64 grid_n = tile.cta_n ?
            DSL_Tile_Ceil_Div(tile.problem_n, tile.cta_n) : 0;
        UINT64 k_iterations = tile.cta_k ?
            DSL_Tile_Ceil_Div(tile.problem_k, tile.cta_k) : 0;
        fprintf(file, "AI_OPT %u TILE_GRID %u %u %u %llu %llu %llu "
                      "%llu %llu %llu %llu\n",
                trace_version, site.id, tile.id, tile.optimization_plan_id,
                (unsigned long long)tile.problem_m,
                (unsigned long long)tile.problem_n,
                (unsigned long long)tile.problem_k,
                (unsigned long long)grid_m,
                (unsigned long long)grid_n,
                (unsigned long long)(grid_m * grid_n),
                (unsigned long long)k_iterations);
        fprintf(file, "AI_OPT %u TILE_LAYOUT %u %u %u linear %s %u %llu %u %u\n",
                trace_version, site.id, tile.id, tile.optimization_plan_id,
                tile.a_shared_layout ==
                    DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                    "transposed_padded" : "row_major",
                tile.a_shared_stride,
                (unsigned long long)tile.smem_bank_conflict_work,
                tile.thread_n == 0 ? 0 : tile.cta_n / tile.thread_n,
                tile.warps_per_cta);
        fprintf(file, "AI_OPT %u TILE_GMEM %u %u %u %u %llu %llu %llu %llu %llu\n",
                trace_version, site.id, tile.id, tile.optimization_plan_id,
                tile.family == DSL_TILE_FAMILY_BASELINE ? 0 : 32,
                (unsigned long long)tile.gmem_a_sector_bytes,
                (unsigned long long)tile.gmem_b_sector_bytes,
                (unsigned long long)tile.gmem_c_sector_bytes,
                (unsigned long long)tile.gmem_aligned_sector_bytes,
                (unsigned long long)tile.smem_a_load_issue_work);
        fprintf(file, "AI_OPT %u TILE_SMEM %u %u %u %u %u %s "
                      "%llu %llu %llu %llu %llu %llu %llu %llu\n",
                trace_version, site.id, tile.id, tile.optimization_plan_id,
                tile.smem_bank_count, tile.smem_bank_word_bytes,
                tile.b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
                    "xor_columns" : "row_major",
                (unsigned long long)tile.smem_a_allocation_bytes,
                (unsigned long long)tile.smem_b_allocation_bytes,
                (unsigned long long)tile.smem_a_copy_conflicts,
                (unsigned long long)tile.smem_b_copy_conflicts,
                (unsigned long long)tile.smem_a_read_conflicts,
                (unsigned long long)tile.smem_b_read_conflicts,
                (unsigned long long)tile.smem_b_load_issue_work,
                (unsigned long long)tile.smem_address_work);
    }
    fprintf(file, "AI_OPT %u TILE_SELECT %u %u %u %u %u %u %u "
                  "selection_reason=%s\n",
            trace_version, site.id, site.baseline_plan_id, selection.selected_plan_id,
            selection.target_profile_id, selection.legal_plan_count,
            selection.complete_cost_count, selection.rejected_plan_count,
            selection_reason);
    fflush(file);
    return !ferror(file);
}

static const DSL_TILE_FAMILY_SEED DSL_cuda_64 = {
    DSL_TILE_FAMILY_CUDA_64,
    64, 64, 16, 32, 32, 8, 4, 1, 4, 1, 4
};

static const DSL_TILE_FAMILY_SEED DSL_cuda_128 = {
    DSL_TILE_FAMILY_CUDA_128,
    128, 128, 32, 64, 32, 8, 8, 1, 4, 1, 4
};

static const DSL_TILE_FAMILY_SEED DSL_cuda_128_coalesced = {
    DSL_TILE_FAMILY_CUDA_128_COALESCED,
    128, 128, 32, 64, 64, 16, 2, 1, 2, 1, 2
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

void
VHO_DSL_Tile_Control_Init (VHO_DSL_TILE_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->select_plans = 1;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->maximum_phase = DSL_TILE_PHASE_P7_11_INSTRUCTION;
    control->max_sites = 16;
    control->max_plans_per_site = 16;
    control->enable_cuda_64 = 1;
    control->enable_cuda_128 = 1;
    control->enable_blackwell_wide = 1;
}

static BOOL
DSL_Tile_Control_Valid (const VHO_DSL_TILE_CONTROL &control)
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
            TY_tensor_element_ty(tensor.descriptor_ty) ||
        TY_mtype(TY_tensor_element_ty(tensor.descriptor_ty)) != MTYPE_F4)
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
    if (!DSL_tensor_evolution_find_semantic_root
             (analysis->graph, tensor.value_id, source))
        return FALSE;
    if (analysis->residency_ir == NULL)
        return TRUE;
    for (UINT32 site_id = 1;
         site_id <= DSL_residency_plan_ir_site_count(analysis->residency_ir);
         ++site_id) {
        DSL_RESIDENCY_SITE_RECORD site;
        if (!DSL_residency_plan_ir_get_site
                 (analysis->residency_ir, site_id, &site))
            return FALSE;
        if (site.semantic_value_id != tensor.value_id)
            continue;
        for (UINT32 i = 0; i < site.alternative_count; ++i) {
            DSL_RESIDENCY_ALTERNATIVE_RECORD alternative;
            if (!DSL_residency_plan_ir_get_alternative
                     (analysis->residency_ir,
                      site.first_alternative_id + i, &alternative))
                return FALSE;
            if (alternative.plan_id == site.selected_plan_id)
                return DSL_tensor_evolution_get_node
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
    UINT64 read_elements;
    UINT64 mn;
    if (!DSL_Tile_Mul_U64(plan->problem_m, plan->problem_k, &a_elements) ||
        !DSL_Tile_Mul_U64
             (a_elements, DSL_Tile_Ceil_Div(plan->problem_n, plan->cta_n),
              &a_elements) ||
        !DSL_Tile_Mul_U64(plan->problem_k, plan->problem_n, &b_elements) ||
        !DSL_Tile_Mul_U64
             (b_elements,
              DSL_Tile_Ceil_Div(plan->problem_m, plan->cta_m),
              &b_elements) ||
        !DSL_Tile_Add_U64(a_elements, b_elements, &read_elements) ||
        !DSL_Tile_Mul_U64(read_elements, element_bytes,
                          &plan->gmem_read_bytes) ||
        !DSL_Tile_Mul_U64(plan->problem_m, plan->problem_n, &mn) ||
        !DSL_Tile_Mul_U64(mn, element_bytes,
                          &plan->gmem_write_bytes))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_Tile_Dynamic_Barriers
        (const DSL_TILE_PLAN_RECORD *plan, UINT64 *barriers)
{
    if (plan->barrier_count == 0) {
        *barriers = 0;
        return TRUE;
    }
    UINT64 ctas;
    return plan->cta_m != 0 && plan->cta_n != 0 && plan->cta_k != 0 &&
        DSL_Tile_Mul_U64
            (DSL_Tile_Ceil_Div(plan->problem_m, plan->cta_m),
             DSL_Tile_Ceil_Div(plan->problem_n, plan->cta_n),
             &ctas) &&
        DSL_Tile_Mul_U64
            (ctas, DSL_Tile_Ceil_Div(plan->problem_k, plan->cta_k),
             &ctas) &&
        DSL_Tile_Mul_U64(ctas, plan->barrier_count, barriers);
}

/* Count distinct 32-byte sectors for each source-level warp memory issue.
 * The CTA tile origins are multiples of eight F32 elements for these seeds,
 * so their absolute positions do not change the sector residue. */
static UINT64
DSL_Tile_Local_Sectors (const DSL_TILE_PLAN_RECORD *tile, UINT32 operand,
                        UINT32 valid_m, UINT32 valid_n, UINT32 valid_k,
                        UINT32 base_words)
{
    UINT64 requests = 0;
    const UINT32 threads = tile->threads_per_cta;
    const UINT32 columns = tile->cta_n / tile->thread_n;
    const UINT32 elements = operand == 0 ? tile->cta_m * tile->cta_k :
                            tile->cta_k * tile->cta_n;
    for (UINT32 warp = 0; warp < threads / 32; ++warp) {
        const UINT32 repetitions = operand == 2 ?
            tile->thread_m * tile->thread_n :
            DSL_Tile_Ceil_Div(elements, threads);
        for (UINT32 issue = 0; issue < repetitions; ++issue) {
            UINT64 sectors[32];
            UINT32 count = 0;
            for (UINT32 lane = 0; lane < 32; ++lane) {
                UINT32 tid = warp * 32 + lane;
                UINT64 word;
                if (operand == 2) {
                    UINT32 i = issue / tile->thread_n;
                    UINT32 j = issue % tile->thread_n;
                    UINT32 row = (tid / columns) * tile->thread_m + i;
                    UINT32 col = (tid % columns) * tile->thread_n + j;
                    if (row >= valid_m || col >= valid_n)
                        continue;
                    word = (UINT64)row * tile->problem_n + col;
                } else {
                    UINT32 offset = tid + issue * threads;
                    if (offset >= elements)
                        continue;
                    UINT32 row = offset / (operand == 0 ? tile->cta_k :
                                           tile->cta_n);
                    UINT32 col = offset % (operand == 0 ? tile->cta_k :
                                           tile->cta_n);
                    if ((operand == 0 && (row >= valid_m || col >= valid_k)) ||
                        (operand == 1 && (row >= valid_k || col >= valid_n)))
                        continue;
                    word = operand == 0 ?
                        (UINT64)row * tile->problem_k + col :
                        (UINT64)row * tile->problem_n + col;
                }
                UINT64 sector = (word + base_words) / 8;
                UINT32 index = 0;
                while (index < count && sectors[index] != sector)
                    ++index;
                if (index == count)
                    sectors[count++] = sector;
            }
            requests += count;
        }
    }
    return requests;
}

static BOOL
DSL_Tile_Sector_Bytes (DSL_TILE_PLAN_RECORD *tile)
{
    if (tile->cta_m % 8 != 0 || tile->cta_n % 8 != 0 ||
        tile->cta_k % 8 != 0 || tile->threads_per_cta % 32 != 0 ||
        tile->threads_per_cta == 0 || tile->thread_n == 0)
        return FALSE;
    const UINT64 m_full = tile->problem_m / tile->cta_m;
    const UINT64 n_full = tile->problem_n / tile->cta_n;
    const UINT64 k_full = tile->problem_k / tile->cta_k;
    const UINT32 m_tail = tile->problem_m % tile->cta_m;
    const UINT32 n_tail = tile->problem_n % tile->cta_n;
    const UINT32 k_tail = tile->problem_k % tile->cta_k;
    UINT64 aligned = 0;
    for (UINT32 operand = 0; operand < 3; ++operand) {
        UINT64 worst = 0;
        for (UINT32 alignment = 0; alignment < 8; ++alignment) {
            UINT64 requests = 0;
            for (UINT32 mi = 0; mi < 2; ++mi) {
                UINT64 mc = mi == 0 ? m_full : (m_tail != 0);
                UINT32 vm = mi == 0 ? tile->cta_m : m_tail;
                if (mc == 0) continue;
                for (UINT32 ni = 0; ni < 2; ++ni) {
                    UINT64 nc = ni == 0 ? n_full : (n_tail != 0);
                    UINT32 vn = ni == 0 ? tile->cta_n : n_tail;
                    if (nc == 0) continue;
                    for (UINT32 ki = 0; ki < (operand == 2 ? 1U : 2U); ++ki) {
                        UINT64 kc = operand == 2 ? 1 :
                            (ki == 0 ? k_full : (k_tail != 0));
                        UINT32 vk = ki == 0 ? tile->cta_k : k_tail;
                        UINT64 multiplier, local, contribution;
                        if (kc == 0) continue;
                        if (!DSL_Tile_Mul_U64(mc, nc, &multiplier) ||
                            !DSL_Tile_Mul_U64(multiplier, kc, &multiplier))
                            return FALSE;
                        local = DSL_Tile_Local_Sectors(tile, operand, vm,
                                                       vn, vk, alignment);
                        if (!DSL_Tile_Mul_U64(multiplier, local,
                                              &contribution) ||
                            !DSL_Tile_Add_U64(requests, contribution,
                                              &requests))
                            return FALSE;
                    }
                }
            }
            if (alignment == 0 &&
                (!DSL_Tile_Mul_U64(requests, 32, &requests) ||
                 !DSL_Tile_Add_U64(aligned, requests, &aligned)))
                return FALSE;
            if (alignment != 0 &&
                !DSL_Tile_Mul_U64(requests, 32, &requests))
                return FALSE;
            worst = std::max(worst, requests);
        }
        if (operand == 0) tile->gmem_a_sector_bytes = worst;
        else if (operand == 1) tile->gmem_b_sector_bytes = worst;
        else tile->gmem_c_sector_bytes = worst;
    }
    tile->gmem_aligned_sector_bytes = aligned;
    return TRUE;
}

static UINT32
DSL_Tile_Bank_Excess (const UINT32 *words, UINT32 count)
{
    UINT32 banks[32] = {0};
    UINT32 rounds = 0;
    for (UINT32 i = 0; i < count; ++i) {
        BOOL duplicate = FALSE;
        for (UINT32 j = 0; j < i; ++j)
            if (words[j] == words[i]) duplicate = TRUE;
        if (!duplicate)
            rounds = std::max(rounds, ++banks[words[i] % 32]);
    }
    return rounds == 0 ? 0 : rounds - 1;
}

static BOOL
DSL_Tile_SMEM_Work (DSL_TILE_PLAN_RECORD *plan)
{
    DSL_SHARED_BANK_MODEL_RECORD model;
    if (!DSL_memory_hierarchy_shared_bank_model(plan->target_profile_id, &model) ||
        model.bank_count != 32 || model.word_bytes != 4 || model.warp_lanes != 32)
        return FALSE;
    plan->smem_bank_count = model.bank_count;
    plan->smem_bank_word_bytes = model.word_bytes;
    plan->smem_address_work = 0;
    UINT64 conflicts[4] = {0, 0, 0, 0};
    UINT64 b_copy_issues = 0;
    const UINT32 columns = plan->cta_n / plan->thread_n;
    for (UINT32 warp = 0; warp < plan->warps_per_cta; ++warp) {
        for (UINT32 operand = 0; operand < 2; ++operand) {
            UINT32 elements = plan->cta_k *
                (operand == 0 ? plan->cta_m : plan->cta_n);
            UINT32 width = operand == 0 ? plan->cta_k : plan->cta_n;
            for (UINT32 issue = 0;
                 issue < DSL_Tile_Ceil_Div(elements, plan->threads_per_cta); ++issue) {
                UINT32 words[32], count = 0;
                for (UINT32 lane = 0; lane < 32; ++lane) {
                    UINT32 offset = warp * 32 + lane + issue * plan->threads_per_cta;
                    if (offset >= elements) continue;
                    UINT32 row = offset / width, col = offset % width;
                    words[count++] = operand == 0 ?
                        (plan->a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                         col * plan->a_shared_stride + row : offset) :
                        row * width + (plan->b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
                                       col ^ (col / 32) : col);
                }
                conflicts[operand] += DSL_Tile_Bank_Excess(words, count);
                if (operand == 1 && count != 0) ++b_copy_issues;
            }
        }
        // q changes all bank indices by the same offset; count one q then scale.
        for (UINT32 i = 0; i < plan->thread_m; ++i) {
            UINT32 words[32];
            for (UINT32 lane = 0; lane < 32; ++lane) {
                UINT32 row = ((warp * 32 + lane) / columns) * plan->thread_m + i;
                words[lane] = plan->a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED ?
                              row : row * plan->cta_k;
            }
            conflicts[2] += DSL_Tile_Bank_Excess(words, 32);
        }
        for (UINT32 j = 0; j < plan->thread_n; ++j) {
            UINT32 words[32];
            for (UINT32 lane = 0; lane < 32; ++lane) {
                UINT32 col = ((warp * 32 + lane) % columns) * plan->thread_n + j;
                words[lane] = plan->b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS ?
                              col ^ (col / 32) : col;
            }
            conflicts[3] += DSL_Tile_Bank_Excess(words, 32) * plan->thread_m;
        }
    }
    UINT64 grid, iterations, compute_steps;
    if (!DSL_Tile_Mul_U64(DSL_Tile_Ceil_Div(plan->problem_m, plan->cta_m),
                          DSL_Tile_Ceil_Div(plan->problem_n, plan->cta_n), &grid) ||
        !DSL_Tile_Mul_U64(grid, DSL_Tile_Ceil_Div(plan->problem_k, plan->cta_k),
                          &iterations) ||
        !DSL_Tile_Mul_U64(iterations, plan->cta_k, &compute_steps) ||
        !DSL_Tile_Mul_U64(conflicts[0], iterations, &plan->smem_a_copy_conflicts) ||
        !DSL_Tile_Mul_U64(conflicts[1], iterations, &plan->smem_b_copy_conflicts) ||
        !DSL_Tile_Mul_U64(conflicts[2], compute_steps, &plan->smem_a_read_conflicts) ||
        !DSL_Tile_Mul_U64(conflicts[3], compute_steps, &plan->smem_b_read_conflicts) ||
        !DSL_Tile_Mul_U64(compute_steps, plan->warps_per_cta,
                          &plan->smem_a_load_issue_work) ||
        !DSL_Tile_Mul_U64(plan->smem_a_load_issue_work, plan->thread_m,
                          &plan->smem_a_load_issue_work) ||
        !DSL_Tile_Mul_U64(plan->smem_a_load_issue_work, plan->thread_n,
                          &plan->smem_b_load_issue_work) ||
        !DSL_Tile_Add_U64(plan->smem_a_copy_conflicts, plan->smem_b_copy_conflicts,
                          &plan->smem_bank_conflict_work) ||
        !DSL_Tile_Add_U64(plan->smem_bank_conflict_work, plan->smem_a_read_conflicts,
                          &plan->smem_bank_conflict_work) ||
        !DSL_Tile_Add_U64(plan->smem_bank_conflict_work, plan->smem_b_read_conflicts,
                          &plan->smem_bank_conflict_work))
        return FALSE;
    if (plan->b_shared_layout == DSL_TILE_B_LAYOUT_XOR_COLUMNS &&
        (!DSL_Tile_Mul_U64(b_copy_issues, iterations, &plan->smem_address_work) ||
         !DSL_Tile_Add_U64(plan->smem_address_work, plan->smem_b_load_issue_work,
                           &plan->smem_address_work) ||
         !DSL_Tile_Mul_U64(plan->smem_address_work, 2, &plan->smem_address_work)))
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
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &tile->candidate_id, diagnostic))
        return FALSE;

    DSL_OPT_COST_INPUT cost;
    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = tile->id;
    if (complete_cost) {
        UINT64 traffic, sync, memory_work;
        if (!DSL_Tile_Add_U64(tile->gmem_a_sector_bytes,
                              tile->gmem_b_sector_bytes, &traffic) ||
            !DSL_Tile_Add_U64(traffic, tile->gmem_c_sector_bytes, &traffic) ||
            !DSL_Tile_Dynamic_Barriers(tile, &sync))
            return DSL_Tile_Report(diagnostic, "tile cost overflow", tile->id);
        UINT64 compute = std::max((UINT64)1,
                                  tile->operation_count / 1024);
        if (!DSL_Tile_Add_U64(compute, tile->smem_address_work / 1024, &compute))
            return DSL_Tile_Report(diagnostic, "tile address cost overflow", tile->id);
        if (!DSL_Tile_Add_U64(std::max((UINT64)1, traffic / 1024),
                              tile->smem_bank_conflict_work / 32,
                              &memory_work) ||
            !DSL_Tile_Add_U64(memory_work,
                              tile->smem_a_load_issue_work / 32,
                              &memory_work) ||
            !DSL_Tile_Add_U64(memory_work,
                              tile->smem_b_load_issue_work / 32,
                              &memory_work))
            return DSL_Tile_Report(diagnostic, "tile memory cost overflow",
                                   tile->id);
        UINT64 memory = memory_work;
        for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
            DSL_Tile_Cost_Term
                (&cost.terms[i],
                 i == DSL_OPT_COST_COMPUTE ? compute :
                 i == DSL_OPT_COST_MEMORY_UNHIDDEN ? memory :
                 i == DSL_OPT_COST_SYNC ? sync : 0,
                 DSL_OPT_COST_CONFIDENCE_LOW,
                 (tile->flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0 ?
                     DSL_OPT_COST_EVIDENCE_BASELINE_POLICY :
                     DSL_OPT_COST_EVIDENCE_TARGET_MODEL);
    } else {
        for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i)
            DSL_Tile_Unknown_Cost(&cost.terms[i]);
    }
    DSL_OPT_COST_ID cost_id;
    if (!DSL_opt_plan_add_cost(context, &cost, &cost_id, diagnostic))
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
    if (!DSL_opt_plan_add_plan
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
    tile.gmem_a_sector_bytes = tile.gmem_read_bytes;
    tile.gmem_c_sector_bytes = tile.gmem_write_bytes;
    if (!DSL_Tile_Add_U64(tile.gmem_read_bytes, tile.gmem_write_bytes,
                          &tile.gmem_aligned_sector_bytes))
        return DSL_Tile_Report(diagnostic, "baseline traffic overflow", tile.id);
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
         const DSL_TILE_FAMILY_SEED &seed, BOOL transpose_a,
         UINT64 m, UINT64 n, UINT64 k, UINT64 element_bytes,
         FILE *diagnostic, BOOL swizzle_b = FALSE)
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
    tile.a_shared_layout = transpose_a ?
        DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED : DSL_TILE_A_LAYOUT_ROW_MAJOR;
    tile.a_shared_stride = transpose_a ? seed.cta_m + 1 : seed.cta_k;
    tile.b_shared_layout = swizzle_b ? DSL_TILE_B_LAYOUT_XOR_COLUMNS :
                                     DSL_TILE_B_LAYOUT_ROW_MAJOR;
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
    if (!transpose_a && !swizzle_b && tile.edge_policy == DSL_TILE_EDGE_EXACT)
        tile.flags |= DSL_TILE_PLAN_FLAG_TMA_CANDIDATE;
    if (!DSL_Tile_Problem_Cost
             (m, n, k, element_bytes, &tile.operation_count,
              &tile.gmem_read_bytes, &tile.gmem_write_bytes) ||
        !DSL_Tile_Tiled_Traffic(&tile, element_bytes) ||
        seed.thread_m == 0 || seed.thread_n == 0 ||
        seed.cta_m % seed.thread_m != 0 ||
        seed.cta_n % seed.thread_n != 0 ||
        !DSL_Tile_Mul_U64
             (transpose_a ? seed.cta_m + 1 : seed.cta_m,
              seed.cta_k, &shared_a) ||
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
    tile.smem_a_allocation_bytes = shared_a * element_bytes;
    tile.smem_b_allocation_bytes = shared_b * element_bytes;
    if (!DSL_Tile_Sector_Bytes(&tile))
        return DSL_Tile_Report(diagnostic, "GMEM sector cost overflow", tile.id);
    if (!DSL_Tile_SMEM_Work(&tile))
        return DSL_Tile_Report(diagnostic, "bank cost overflow", tile.id);
    tile.barrier_count = 2;
    if (!DSL_memory_hierarchy_find_tier
             (analysis->control.target_profile_id,
              DSL_MEMORY_TIER_SHARED, &shared) ||
        !DSL_memory_hierarchy_find_tier
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
    if (!DSL_tensor_evolution_add_tile
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
VHO_DSL_Tile_Create
        (PU_Info *pu, DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_RESIDENCY_ANALYSIS *residency,
         const VHO_DSL_TILE_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Tile_Control_Valid(*control) ||
        !DSL_tensor_evolution_verify(graph, diagnostic) ||
        !VHO_DSL_Tensor_Analysis_Verify(tensor_analysis, diagnostic) ||
        !DSL_tensor_locality_verify(locality, diagnostic) ||
        (residency != NULL &&
         !VHO_DSL_Residency_Verify(residency, diagnostic))) {
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
    analysis->residency_ir = residency == NULL ? NULL :
                             VHO_DSL_Residency_Get_IR(residency);
    analysis->control = *control;
    analysis->ir = NULL;
    analysis->built = FALSE;
    return analysis;
}

void
VHO_DSL_Tile_Destroy (DSL_TILE_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_opt_plan_destroy(analysis->plans[i]);
    DSL_tile_plan_ir_destroy(analysis->ir);
    delete analysis;
}

static BOOL
DSL_Tile_Materialize_IR (DSL_TILE_ANALYSIS *analysis, FILE *diagnostic)
{
    DSL_TILE_PLAN_IR_CREATE_INFO info;
    memset(&info, 0, sizeof(info));
    info.owner_pu_st = analysis->owner_pu_st;
    info.sites = analysis->sites.empty() ? NULL : &analysis->sites[0];
    info.site_count = analysis->sites.size();
    info.plans = analysis->tile_plans.empty() ? NULL :
                 &analysis->tile_plans[0];
    info.plan_count = analysis->tile_plans.size();
    info.stages = analysis->stages.empty() ? NULL : &analysis->stages[0];
    info.stage_count = analysis->stages.size();
    DSL_tile_plan_ir_destroy(analysis->ir);
    analysis->ir = DSL_tile_plan_ir_create(&info, diagnostic);
    return analysis->ir != NULL;
}

BOOL
VHO_DSL_Tile_Build (DSL_TILE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tile_Active(analysis) || analysis->built)
        return DSL_Tile_Report(diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return DSL_Tile_Materialize_IR(analysis, diagnostic) &&
               VHO_DSL_Tile_Verify(analysis, diagnostic);
    }
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_tensor_analysis_fact_count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality;
        DSL_TENSOR_EVOLUTION_NODE_RECORD source;
        DSL_IR_NODE_RECORD node;
        UINT64 m;
        UINT64 n;
        UINT64 k;
        UINT64 element_bytes;
        if (!DSL_tensor_analysis_get_fact
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
        if (!DSL_tensor_locality_find_fact
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
        DSL_OPT_PLAN_CONTEXT *context = DSL_opt_plan_create
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
                      DSL_cuda_64, FALSE, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
            if (analysis->control.enable_cuda_128 &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_cuda_128, FALSE, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
            if (analysis->control.enable_cuda_128 &&
                !VHO_DSL_CUDA_TMA_Prefetch &&
                site.tile_plan_count < analysis->control.max_plans_per_site &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_cuda_128_coalesced, FALSE, m, n, k,
                      element_bytes, diagnostic))
                return FALSE;
            if (analysis->control.enable_blackwell_wide &&
                analysis->control.target_profile_id ==
                    DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_blackwell_wide, FALSE, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
            if (analysis->control.enable_cuda_128 &&
                !VHO_DSL_CUDA_TMA_Prefetch &&
                site.tile_plan_count < analysis->control.max_plans_per_site &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_cuda_128, TRUE, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
            if (analysis->control.enable_blackwell_wide &&
                analysis->control.target_profile_id ==
                    DSL_TARGET_PROFILE_NVIDIA_BLACKWELL &&
                !VHO_DSL_CUDA_TMA_Prefetch &&
                site.tile_plan_count < analysis->control.max_plans_per_site &&
                !DSL_Tile_Add_Family
                     (analysis, &site, tensor, locality, source,
                      DSL_blackwell_wide, TRUE, m, n, k, element_bytes,
                      diagnostic))
                return FALSE;
        }
        if (cuda_profile && VHO_DSL_CUDA_SMEM_Layout_Search &&
            !VHO_DSL_CUDA_TMA_Prefetch) {
            const UINT32 original_count = site.tile_plan_count;
            for (UINT32 p = 1; p < original_count &&
                 site.tile_plan_count < analysis->control.max_plans_per_site; ++p) {
                const DSL_TILE_PLAN_RECORD original =
                    analysis->tile_plans[site.first_tile_plan_id - 1 + p];
                const DSL_TILE_FAMILY_SEED &seed =
                    original.family == DSL_TILE_FAMILY_CUDA_64 ? DSL_cuda_64 :
                    original.family == DSL_TILE_FAMILY_CUDA_128 ? DSL_cuda_128 :
                    original.family == DSL_TILE_FAMILY_CUDA_128_COALESCED ?
                        DSL_cuda_128_coalesced : DSL_blackwell_wide;
                if (!DSL_Tile_Add_Family
                         (analysis, &site, tensor, locality, source, seed,
                          original.a_shared_layout == DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED,
                          m, n, k, element_bytes, diagnostic, TRUE))
                    return FALSE;
            }
        }
        if (analysis->control.select_plans) {
            DSL_OPT_SELECTION_RESULT selection;
            const char *selection_reason;
            if (!DSL_Tile_Select_With_Roofline
                     (analysis, site, &selection, &selection_reason,
                      diagnostic))
                return FALSE;
            site.selected_plan_id = selection.selected_plan_id;
            if (VHO_DSL_AI_Opt_Trace &&
                Get_Trace(TP_WOPT2, AI_OPT_TRACE_FLAG) &&
                !DSL_Tile_Trace_Selection
                     (analysis, site, selection, selection_reason))
                return DSL_Tile_Report
                           (diagnostic, "failed to write selection trace",
                            site.id);
        }
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_Tile_Materialize_IR(analysis, diagnostic) &&
           VHO_DSL_Tile_Verify(analysis, diagnostic);
}

BOOL
VHO_DSL_Tile_Verify
        (const DSL_TILE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tile_Active(analysis))
        return DSL_Tile_Report(diagnostic, "program unit is not active", 0);
    if (!analysis->built)
        return DSL_Tile_Report(diagnostic, "analysis is not built", 0);
    if (analysis->ir == NULL ||
        !DSL_tile_plan_ir_verify(analysis->ir, diagnostic) ||
        DSL_tile_plan_ir_owner(analysis->ir) != analysis->owner_pu_st ||
        DSL_tile_plan_ir_site_count(analysis->ir) !=
            analysis->sites.size() ||
        DSL_tile_plan_ir_plan_count(analysis->ir) !=
            analysis->tile_plans.size() ||
        DSL_tile_plan_ir_stage_count(analysis->ir) !=
            analysis->stages.size() ||
        analysis->plans.size() != analysis->sites.size())
        return DSL_Tile_Report(diagnostic, "site plan count mismatch", 0);
    UINT32 expected_plan = 1;
    UINT32 expected_stage = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_TILE_SITE_RECORD &site = analysis->sites[i];
        DSL_TILE_SITE_RECORD ir_site;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            !DSL_tile_plan_ir_get_site(analysis->ir, site.id, &ir_site) ||
            memcmp(&site, &ir_site, sizeof(site)) != 0 ||
            site.first_tile_plan_id != expected_plan ||
            site.tile_plan_count == 0 || site.baseline_plan_id == 0 ||
            site.reserved != 0 ||
            !DSL_opt_plan_verify(analysis->plans[i], diagnostic))
            return DSL_Tile_Report(diagnostic, "invalid tile site", site.id);
        for (UINT32 j = 0; j < site.tile_plan_count; ++j) {
            const DSL_TILE_PLAN_RECORD &tile =
                analysis->tile_plans[expected_plan - 1];
            DSL_TILE_PLAN_RECORD ir_tile;
            BOOL baseline = j == 0;
            if (tile.id != expected_plan || tile.site_id != site.id ||
                !DSL_tile_plan_ir_get_plan
                     (analysis->ir, tile.id, &ir_tile) ||
                memcmp(&tile, &ir_tile, sizeof(tile)) != 0 ||
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
                  tile.family > DSL_TILE_FAMILY_CUDA_128_COALESCED ||
                  tile.result_evolution_node_id == 0 ||
                  tile.evolution_edge_id == 0)))
                return DSL_Tile_Report
                           (diagnostic, "invalid tile plan", tile.id);
            if (!baseline) {
                DSL_TILE_PLAN_RECORD expected = tile;
                if (!DSL_Tile_SMEM_Work(&expected) ||
                    expected.smem_bank_conflict_work != tile.smem_bank_conflict_work ||
                    expected.smem_a_copy_conflicts != tile.smem_a_copy_conflicts ||
                    expected.smem_b_copy_conflicts != tile.smem_b_copy_conflicts ||
                    expected.smem_a_read_conflicts != tile.smem_a_read_conflicts ||
                    expected.smem_b_read_conflicts != tile.smem_b_read_conflicts ||
                    expected.smem_a_load_issue_work != tile.smem_a_load_issue_work ||
                    expected.smem_b_load_issue_work != tile.smem_b_load_issue_work ||
                    expected.smem_address_work != tile.smem_address_work)
                    return DSL_Tile_Report(diagnostic, "invalid shared bank work", tile.id);
            }
            for (UINT32 s = 0; s < tile.stage_count; ++s) {
                const DSL_TILE_STAGE_RECORD &stage =
                    analysis->stages[expected_stage - 1];
                DSL_TILE_STAGE_RECORD ir_stage;
                if (stage.id != expected_stage ||
                    !DSL_tile_plan_ir_get_stage
                         (analysis->ir, stage.id, &ir_stage) ||
                    memcmp(&stage, &ir_stage, sizeof(stage)) != 0 ||
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
        !DSL_tensor_evolution_verify(analysis->graph, diagnostic))
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
VHO_DSL_Tile_Print (FILE *file, const DSL_TILE_ANALYSIS *analysis)
{
    if (file == NULL || !DSL_Tile_Active(analysis))
        return;
    DSL_tile_plan_ir_print(file, analysis->ir);
    fprintf(file,
            "VHOTilePlanDecision: owner=0x%x target=%s stage=G%u "
            "sites=%u plans=%u stages=%u select=%s apply=no\n",
            analysis->owner_pu_st,
            DSL_target_profile_name(analysis->control.target_profile_id),
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
                    tile.id, DSL_tile_family_name(tile.family));
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
                    DSL_tile_edge_policy_name(tile.edge_policy),
                    DSL_tile_instruction_name(tile.instruction_family),
                    DSL_opt_legality_name(tile.legality),
                    DSL_opt_rejection_reason_name(tile.rejection_reason),
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
                        DSL_tile_phase_name(stage.phase),
                        DSL_tile_level_name(stage.level_kind),
                        (unsigned long long)stage.extent_m,
                        (unsigned long long)stage.extent_n,
                        (unsigned long long)stage.extent_k);
            }
        }
        DSL_opt_plan_print(file, analysis->plans[i]);
    }
    DSL_tensor_evolution_print(file, analysis->graph);
}

const DSL_TILE_PLAN_IR *
VHO_DSL_Tile_Get_IR (const DSL_TILE_ANALYSIS *analysis)
{
    return analysis == NULL ? NULL : analysis->ir;
}

const DSL_OPT_PLAN_CONTEXT *
VHO_DSL_Tile_Get_Plan_Context
        (const DSL_TILE_ANALYSIS *analysis, DSL_TILE_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
