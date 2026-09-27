/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Policy-free AIO-9 CommonTilePlanIR storage and structural services.
 * Candidate discovery, target legality, costing, and selection belong to VHO.
 */

#include <string.h>
#include <vector>

#include "dsl_tile_candidate.h"

struct DSL_TILE_PLAN_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_TILE_SITE_RECORD> sites;
    std::vector<DSL_TILE_PLAN_RECORD> plans;
    std::vector<DSL_TILE_STAGE_RECORD> stages;
};

static const char *DSL_tile_phase_name_table[] = {
    "P7.0", "P7.1", "P7.2", "P7.3", "P7.4", "P7.5",
    "P7.6", "P7.7", "P7.8", "P7.9", "P7.10", "P7.11"
};

static const char *DSL_tile_level_name_table[] = {
    "unknown", "problem", "output", "coalescing", "reduction",
    "shared", "resource", "thread", "register_reuse", "vector",
    "family", "warp", "instruction"
};

static const char *DSL_tile_family_name_table[] = {
    "unknown", "baseline", "cuda_64", "cuda_128", "blackwell_wide"
};

static const char *DSL_tile_edge_name_table[] = {
    "unknown", "exact", "predicated"
};

static const char *DSL_tile_instruction_name_table[] = {
    "unknown", "scalar_fma", "vector_fma"
};

static BOOL
DSL_Tile_IR_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL tile IR error: %s id=%u\n", message, id);
    return FALSE;
}

const char *
DSL_tile_phase_name (UINT32 phase)
{
    return phase < DSL_TILE_PHASE_COUNT ?
           DSL_tile_phase_name_table[phase] : "unknown";
}

const char *
DSL_tile_level_name (UINT32 level)
{
    return level < sizeof(DSL_tile_level_name_table) /
                       sizeof(DSL_tile_level_name_table[0]) ?
           DSL_tile_level_name_table[level] : "unknown";
}

const char *
DSL_tile_family_name (UINT32 family)
{
    return family < sizeof(DSL_tile_family_name_table) /
                        sizeof(DSL_tile_family_name_table[0]) ?
           DSL_tile_family_name_table[family] : "unknown";
}

const char *
DSL_tile_edge_policy_name (UINT32 policy)
{
    return policy < sizeof(DSL_tile_edge_name_table) /
                        sizeof(DSL_tile_edge_name_table[0]) ?
           DSL_tile_edge_name_table[policy] : "unknown";
}

const char *
DSL_tile_instruction_name (UINT32 instruction)
{
    return instruction < sizeof(DSL_tile_instruction_name_table) /
                             sizeof(DSL_tile_instruction_name_table[0]) ?
           DSL_tile_instruction_name_table[instruction] : "unknown";
}

BOOL
DSL_tile_plan_ir_verify (const DSL_TILE_PLAN_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Tile_IR_Report(diagnostic, "invalid owner", 0);

    UINT32 expected_plan = 1;
    UINT32 expected_stage = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_TILE_SITE_RECORD &site = ir->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_node_id == 0 || site.semantic_value_id == 0 ||
            site.semantic_root_id == 0 ||
            site.source_evolution_node_id == 0 ||
            site.first_tile_plan_id != expected_plan ||
            site.tile_plan_count == 0 || site.baseline_plan_id == 0 ||
            site.reserved != 0)
            return DSL_Tile_IR_Report(diagnostic, "invalid site", site.id);
        BOOL selected_found = site.selected_plan_id == 0;
        for (UINT32 j = 0; j < site.tile_plan_count; ++j) {
            if (expected_plan > ir->plans.size())
                return DSL_Tile_IR_Report
                           (diagnostic, "plan range overflow", site.id);
            const DSL_TILE_PLAN_RECORD &plan = ir->plans[expected_plan - 1];
            BOOL baseline = j == 0;
            UINT32 known_flags = DSL_TILE_PLAN_FLAG_BASELINE |
                                 DSL_TILE_PLAN_FLAG_PROVISIONAL |
                                 DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING |
                                 DSL_TILE_PLAN_FLAG_PREFETCH_HINT |
                                 DSL_TILE_PLAN_FLAG_TMA_CANDIDATE;
            if (plan.id != expected_plan || plan.site_id != site.id ||
                plan.target_profile_id == 0 ||
                plan.family <= DSL_TILE_FAMILY_UNKNOWN ||
                plan.family > DSL_TILE_FAMILY_BLACKWELL_WIDE ||
                plan.source_descriptor_ty == TY_IDX_ZERO ||
                plan.first_stage_id != expected_stage ||
                plan.stage_count == 0 || plan.source_evolution_node_id == 0 ||
                plan.problem_m == 0 || plan.problem_n == 0 ||
                plan.problem_k == 0 || plan.edge_policy <= DSL_TILE_EDGE_UNKNOWN ||
                plan.edge_policy > DSL_TILE_EDGE_PREDICATED ||
                plan.instruction_family <= DSL_TILE_INSTRUCTION_UNKNOWN ||
                plan.instruction_family > DSL_TILE_INSTRUCTION_VECTOR_FMA ||
                plan.legality > DSL_OPT_LEGALITY_REJECTED ||
                plan.rejection_reason > DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                plan.candidate_id == 0 || plan.optimization_plan_id == 0 ||
                (plan.flags & ~known_flags) != 0 || plan.reserved != 0 ||
                baseline !=
                    ((plan.flags & DSL_TILE_PLAN_FLAG_BASELINE) != 0) ||
                (baseline &&
                 (plan.family != DSL_TILE_FAMILY_BASELINE ||
                  plan.stage_count != 1 ||
                  plan.result_evolution_node_id !=
                      plan.source_evolution_node_id ||
                  plan.evolution_edge_id != 0)) ||
                (!baseline &&
                 (plan.family < DSL_TILE_FAMILY_CUDA_64 ||
                  plan.result_evolution_node_id == 0 ||
                  plan.evolution_edge_id == 0)))
                return DSL_Tile_IR_Report
                           (diagnostic, "invalid plan", plan.id);
            if (site.selected_plan_id == plan.optimization_plan_id)
                selected_found = TRUE;
            for (UINT32 s = 0; s < plan.stage_count; ++s) {
                if (expected_stage > ir->stages.size())
                    return DSL_Tile_IR_Report
                               (diagnostic, "stage range overflow", plan.id);
                const DSL_TILE_STAGE_RECORD &stage =
                    ir->stages[expected_stage - 1];
                if (stage.id != expected_stage ||
                    stage.tile_plan_id != plan.id || stage.ordinal != s ||
                    stage.phase >= DSL_TILE_PHASE_COUNT ||
                    stage.level_kind <= DSL_TILE_LEVEL_UNKNOWN ||
                    stage.level_kind > DSL_TILE_LEVEL_INSTRUCTION ||
                    stage.flags != 0 || stage.reserved != 0)
                    return DSL_Tile_IR_Report
                               (diagnostic, "invalid stage", stage.id);
                ++expected_stage;
            }
            ++expected_plan;
        }
        if (!selected_found)
            return DSL_Tile_IR_Report
                       (diagnostic, "selected plan is absent", site.id);
    }
    if (expected_plan != ir->plans.size() + 1 ||
        expected_stage != ir->stages.size() + 1)
        return DSL_Tile_IR_Report(diagnostic, "table range mismatch", 0);
    return TRUE;
}

DSL_TILE_PLAN_IR *
DSL_tile_plan_ir_create
        (const DSL_TILE_PLAN_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->plan_count != 0 && info->plans == NULL) ||
        (info->stage_count != 0 && info->stages == NULL)) {
        DSL_Tile_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_TILE_PLAN_IR *ir = new DSL_TILE_PLAN_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->plan_count != 0)
        ir->plans.assign(info->plans, info->plans + info->plan_count);
    if (info->stage_count != 0)
        ir->stages.assign(info->stages, info->stages + info->stage_count);
    if (!DSL_tile_plan_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void
DSL_tile_plan_ir_destroy (DSL_TILE_PLAN_IR *ir)
{
    delete ir;
}

void
DSL_tile_plan_ir_print (FILE *file, const DSL_TILE_PLAN_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file, "CommonTilePlanIR: owner=0x%x sites=%u plans=%u stages=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->plans.size(), (UINT32)ir->stages.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_TILE_SITE_RECORD &site = ir->sites[i];
        fprintf(file,
                "  tile-site id=%u node=%u value=%u source_evolution=%u "
                "plans=%u selected=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.source_evolution_node_id, site.tile_plan_count,
                site.selected_plan_id);
        for (UINT32 j = 0; j < site.tile_plan_count; ++j) {
            const DSL_TILE_PLAN_RECORD &plan =
                ir->plans[site.first_tile_plan_id - 1 + j];
            fprintf(file,
                    "    tile-plan id=%u family=%s problem=%llux%llux%llu "
                    "cta=%ux%ux%u warp=%ux%u thread=%ux%u "
                    "instruction=%ux%ux%u vector=%u edge=%s legality=%s "
                    "reason=%s candidate=%u plan=%u\n",
                    plan.id, DSL_tile_family_name(plan.family),
                    (unsigned long long)plan.problem_m,
                    (unsigned long long)plan.problem_n,
                    (unsigned long long)plan.problem_k,
                    plan.cta_m, plan.cta_n, plan.cta_k,
                    plan.warp_m, plan.warp_n, plan.thread_m, plan.thread_n,
                    plan.instruction_m, plan.instruction_n,
                    plan.instruction_k, plan.vector_width,
                    DSL_tile_edge_policy_name(plan.edge_policy),
                    DSL_opt_legality_name(plan.legality),
                    DSL_opt_rejection_reason_name(plan.rejection_reason),
                    plan.candidate_id, plan.optimization_plan_id);
            for (UINT32 s = 0; s < plan.stage_count; ++s) {
                const DSL_TILE_STAGE_RECORD &stage =
                    ir->stages[plan.first_stage_id - 1 + s];
                fprintf(file,
                        "      tile-stage id=%u phase=%s level=%s "
                        "extent=%llux%llux%llu\n",
                        stage.id, DSL_tile_phase_name(stage.phase),
                        DSL_tile_level_name(stage.level_kind),
                        (unsigned long long)stage.extent_m,
                        (unsigned long long)stage.extent_n,
                        (unsigned long long)stage.extent_k);
            }
        }
    }
}

ST_IDX
DSL_tile_plan_ir_owner (const DSL_TILE_PLAN_IR *ir)
{
    return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st;
}

UINT32
DSL_tile_plan_ir_site_count (const DSL_TILE_PLAN_IR *ir)
{
    return ir == NULL ? 0 : ir->sites.size();
}

UINT32
DSL_tile_plan_ir_plan_count (const DSL_TILE_PLAN_IR *ir)
{
    return ir == NULL ? 0 : ir->plans.size();
}

UINT32
DSL_tile_plan_ir_stage_count (const DSL_TILE_PLAN_IR *ir)
{
    return ir == NULL ? 0 : ir->stages.size();
}

BOOL
DSL_tile_plan_ir_get_site
        (const DSL_TILE_PLAN_IR *ir, DSL_TILE_SITE_ID id,
         DSL_TILE_SITE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->sites.size())
        return FALSE;
    *record = ir->sites[id - 1];
    return TRUE;
}

BOOL
DSL_tile_plan_ir_get_plan
        (const DSL_TILE_PLAN_IR *ir, DSL_TILE_PLAN_ID id,
         DSL_TILE_PLAN_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->plans.size())
        return FALSE;
    *record = ir->plans[id - 1];
    return TRUE;
}

BOOL
DSL_tile_plan_ir_get_stage
        (const DSL_TILE_PLAN_IR *ir, DSL_TILE_STAGE_ID id,
         DSL_TILE_STAGE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->stages.size())
        return FALSE;
    *record = ir->stages[id - 1];
    return TRUE;
}
