/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Policy-free AIO-10 CommonFetchPlanIR/CommonPipelineIR storage and
 * structural services. Movement selection and target reasoning belong to VHO.
 */

#include <vector>

#include "dsl_fetch_pipeline.h"

struct DSL_FETCH_PIPELINE_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_FETCH_SITE_RECORD> sites;
    std::vector<DSL_FETCH_PLAN_RECORD> plans;
    std::vector<DSL_FETCH_RECORD> fetches;
    std::vector<DSL_PIPELINE_STAGE_RECORD> stages;
};

static const char *DSL_fetch_issue_name_table[] = {
    "unknown", "consumer", "previous_k_tile", "prologue"
};

static const char *DSL_fetch_barrier_name_table[] = {
    "none", "cta", "arrival"
};

static const char *DSL_fetch_wait_name_table[] = {
    "none", "before_consumer", "pipeline_stage"
};

static BOOL
DSL_Fetch_IR_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL fetch pipeline IR error: %s id=%u\n",
                message, id);
    return FALSE;
}

const char *
DSL_fetch_issue_point_name (UINT32 point)
{
    return point < sizeof(DSL_fetch_issue_name_table) /
                       sizeof(DSL_fetch_issue_name_table[0]) ?
           DSL_fetch_issue_name_table[point] : "unknown";
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
    return point < sizeof(DSL_fetch_wait_name_table) /
                       sizeof(DSL_fetch_wait_name_table[0]) ?
           DSL_fetch_wait_name_table[point] : "unknown";
}

BOOL
DSL_fetch_pipeline_ir_verify
        (const DSL_FETCH_PIPELINE_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Fetch_IR_Report(diagnostic, "invalid owner", 0);

    UINT32 expected_plan = 1;
    UINT32 expected_fetch = 1;
    UINT32 expected_stage = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_FETCH_SITE_RECORD &site = ir->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_node_id == 0 || site.semantic_value_id == 0 ||
            site.tile_site_id == 0 || site.selected_tile_plan_id == 0 ||
            site.first_pipeline_plan_id != expected_plan ||
            site.pipeline_plan_count == 0 || site.baseline_plan_id == 0 ||
            site.reserved != 0)
            return DSL_Fetch_IR_Report(diagnostic, "invalid site", site.id);
        BOOL selected_found = site.selected_plan_id == 0;
        for (UINT32 j = 0; j < site.pipeline_plan_count; ++j) {
            if (expected_plan > ir->plans.size())
                return DSL_Fetch_IR_Report
                           (diagnostic, "plan range overflow", site.id);
            const DSL_FETCH_PLAN_RECORD &plan = ir->plans[expected_plan - 1];
            BOOL baseline = j == 0;
            UINT32 known_flags = DSL_FETCH_PLAN_FLAG_BASELINE |
                                 DSL_FETCH_PLAN_FLAG_PROVISIONAL |
                                 DSL_FETCH_PLAN_FLAG_ASYNC |
                                 DSL_FETCH_PLAN_FLAG_SEMANTICS_PRESERVING;
            if (plan.id != expected_plan || plan.site_id != site.id ||
                plan.tile_plan_id != site.selected_tile_plan_id ||
                plan.target_profile_id == 0 ||
                plan.engine < DSL_MEMORY_MOVEMENT_DEMAND ||
                plan.engine > DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC ||
                plan.first_fetch_id != expected_fetch || plan.fetch_count == 0 ||
                plan.first_stage_id != expected_stage || plan.stage_count == 0 ||
                plan.issue_point < DSL_FETCH_ISSUE_CONSUMER ||
                plan.issue_point > DSL_FETCH_ISSUE_PROLOGUE ||
                plan.arrival_barrier > DSL_FETCH_BARRIER_ARRIVAL ||
                plan.wait_point > DSL_FETCH_WAIT_PIPELINE_STAGE ||
                plan.edge_policy <= DSL_TILE_EDGE_UNKNOWN ||
                plan.edge_policy > DSL_TILE_EDGE_PREDICATED ||
                plan.raw_movement_cost !=
                    plan.hidden_movement_cost + plan.unhidden_movement_cost ||
                plan.legality > DSL_OPT_LEGALITY_REJECTED ||
                plan.rejection_reason > DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                plan.candidate_id == 0 || plan.optimization_plan_id == 0 ||
                (plan.flags & ~known_flags) != 0 || plan.reserved != 0 ||
                baseline !=
                    ((plan.flags & DSL_FETCH_PLAN_FLAG_BASELINE) != 0) ||
                (baseline && plan.engine != DSL_MEMORY_MOVEMENT_DEMAND))
                return DSL_Fetch_IR_Report
                           (diagnostic, "invalid plan", plan.id);
            if (site.selected_plan_id == plan.optimization_plan_id)
                selected_found = TRUE;
            UINT64 raw = 0;
            UINT64 hidden = 0;
            UINT64 unhidden = 0;
            for (UINT32 f = 0; f < plan.fetch_count; ++f) {
                if (expected_fetch > ir->fetches.size())
                    return DSL_Fetch_IR_Report
                               (diagnostic, "fetch range overflow", plan.id);
                const DSL_FETCH_RECORD &fetch =
                    ir->fetches[expected_fetch - 1];
                UINT32 fetch_flags = DSL_FETCH_RECORD_FLAG_STAGED |
                                     DSL_FETCH_RECORD_FLAG_ASYNC |
                                     DSL_FETCH_RECORD_FLAG_REQUIRES_BARRIER;
                if (fetch.id != expected_fetch ||
                    fetch.pipeline_plan_id != plan.id ||
                    fetch.operand_ordinal != f || fetch.value_id == 0 ||
                    fetch.descriptor_ty == TY_IDX_ZERO ||
                    fetch.source_tier_kind == DSL_MEMORY_TIER_UNKNOWN ||
                    fetch.destination_tier_kind == DSL_MEMORY_TIER_UNKNOWN ||
                    fetch.engine != plan.engine ||
                    fetch.transaction_bytes == 0 ||
                    fetch.minimum_alignment == 0 ||
                    fetch.bytes_per_stage == 0 ||
                    fetch.raw_movement_cost !=
                        fetch.hidden_movement_cost +
                        fetch.unhidden_movement_cost ||
                    fetch.legality != plan.legality ||
                    fetch.rejection_reason != plan.rejection_reason ||
                    (fetch.flags & ~fetch_flags) != 0 ||
                    fetch.reserved != 0 ||
                    (baseline &&
                     (fetch.result_evolution_node_id !=
                          fetch.source_evolution_node_id ||
                      fetch.evolution_edge_id != 0 || fetch.flags != 0)))
                    return DSL_Fetch_IR_Report
                               (diagnostic, "invalid fetch", fetch.id);
                raw += fetch.raw_movement_cost;
                hidden += fetch.hidden_movement_cost;
                unhidden += fetch.unhidden_movement_cost;
                ++expected_fetch;
            }
            if (plan.raw_movement_cost != raw ||
                plan.hidden_movement_cost != hidden ||
                plan.unhidden_movement_cost != unhidden)
                return DSL_Fetch_IR_Report
                           (diagnostic, "plan cost mismatch", plan.id);
            for (UINT32 s = 0; s < plan.stage_count; ++s) {
                if (expected_stage > ir->stages.size())
                    return DSL_Fetch_IR_Report
                               (diagnostic, "stage range overflow", plan.id);
                const DSL_PIPELINE_STAGE_RECORD &stage =
                    ir->stages[expected_stage - 1];
                if (stage.id != expected_stage ||
                    stage.pipeline_plan_id != plan.id ||
                    stage.ordinal != s || stage.buffer_slot != s ||
                    stage.prefetch_distance != plan.prefetch_distance ||
                    stage.issue_point != plan.issue_point ||
                    stage.arrival_barrier != plan.arrival_barrier ||
                    stage.wait_point != plan.wait_point ||
                    stage.reserved != 0)
                    return DSL_Fetch_IR_Report
                               (diagnostic, "invalid stage", stage.id);
                ++expected_stage;
            }
            ++expected_plan;
        }
        if (!selected_found)
            return DSL_Fetch_IR_Report
                       (diagnostic, "selected plan is absent", site.id);
    }
    if (expected_plan != ir->plans.size() + 1 ||
        expected_fetch != ir->fetches.size() + 1 ||
        expected_stage != ir->stages.size() + 1)
        return DSL_Fetch_IR_Report(diagnostic, "table range mismatch", 0);
    return TRUE;
}

DSL_FETCH_PIPELINE_IR *
DSL_fetch_pipeline_ir_create
        (const DSL_FETCH_PIPELINE_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->plan_count != 0 && info->plans == NULL) ||
        (info->fetch_count != 0 && info->fetches == NULL) ||
        (info->stage_count != 0 && info->stages == NULL)) {
        DSL_Fetch_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_FETCH_PIPELINE_IR *ir = new DSL_FETCH_PIPELINE_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->plan_count != 0)
        ir->plans.assign(info->plans, info->plans + info->plan_count);
    if (info->fetch_count != 0)
        ir->fetches.assign(info->fetches, info->fetches + info->fetch_count);
    if (info->stage_count != 0)
        ir->stages.assign(info->stages, info->stages + info->stage_count);
    if (!DSL_fetch_pipeline_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void
DSL_fetch_pipeline_ir_destroy (DSL_FETCH_PIPELINE_IR *ir)
{
    delete ir;
}

void
DSL_fetch_pipeline_ir_print (FILE *file, const DSL_FETCH_PIPELINE_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "CommonFetchPlanIR/CommonPipelineIR: owner=0x%x sites=%u "
            "plans=%u fetches=%u stages=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->plans.size(), (UINT32)ir->fetches.size(),
            (UINT32)ir->stages.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_FETCH_SITE_RECORD &site = ir->sites[i];
        fprintf(file,
                "  fetch-site id=%u node=%u value=%u tile_site=%u "
                "tile_plan=%u plans=%u selected=%u\n",
                site.id, site.semantic_node_id, site.semantic_value_id,
                site.tile_site_id, site.selected_tile_plan_id,
                site.pipeline_plan_count, site.selected_plan_id);
        for (UINT32 j = 0; j < site.pipeline_plan_count; ++j) {
            const DSL_FETCH_PLAN_RECORD &plan =
                ir->plans[site.first_pipeline_plan_id - 1 + j];
            fprintf(file,
                    "    pipeline-plan id=%u engine=%s buffers=%u "
                    "distance=%u issue=%s arrival=%s wait=%s edge=%s "
                    "buffering=%llu/%llu barriers=%u legality=%s "
                    "reason=%s candidate=%u plan=%u\n",
                    plan.id, DSL_memory_movement_name(plan.engine),
                    plan.stage_count, plan.prefetch_distance,
                    DSL_fetch_issue_point_name(plan.issue_point),
                    DSL_fetch_barrier_name(plan.arrival_barrier),
                    DSL_fetch_wait_point_name(plan.wait_point),
                    DSL_tile_edge_policy_name(plan.edge_policy),
                    (unsigned long long)plan.buffering_bytes,
                    (unsigned long long)plan.buffering_capacity_bytes,
                    plan.barrier_count, DSL_opt_legality_name(plan.legality),
                    DSL_opt_rejection_reason_name(plan.rejection_reason),
                    plan.candidate_id, plan.optimization_plan_id);
            for (UINT32 f = 0; f < plan.fetch_count; ++f) {
                const DSL_FETCH_RECORD &fetch =
                    ir->fetches[plan.first_fetch_id - 1 + f];
                fprintf(file,
                        "      fetch id=%u operand=%u value=%u ty=%u "
                        "source=%s destination=%s bytes=%llu transaction=%u "
                        "alignment=%u raw=%llu hidden=%llu unhidden=%llu "
                        "evolution=%u flags=0x%x\n",
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
            for (UINT32 s = 0; s < plan.stage_count; ++s) {
                const DSL_PIPELINE_STAGE_RECORD &stage =
                    ir->stages[plan.first_stage_id - 1 + s];
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
    }
}

ST_IDX
DSL_fetch_pipeline_ir_owner (const DSL_FETCH_PIPELINE_IR *ir)
{
    return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st;
}

UINT32
DSL_fetch_pipeline_ir_site_count (const DSL_FETCH_PIPELINE_IR *ir)
{
    return ir == NULL ? 0 : ir->sites.size();
}

UINT32
DSL_fetch_pipeline_ir_plan_count (const DSL_FETCH_PIPELINE_IR *ir)
{
    return ir == NULL ? 0 : ir->plans.size();
}

UINT32
DSL_fetch_pipeline_ir_fetch_count (const DSL_FETCH_PIPELINE_IR *ir)
{
    return ir == NULL ? 0 : ir->fetches.size();
}

UINT32
DSL_fetch_pipeline_ir_stage_count (const DSL_FETCH_PIPELINE_IR *ir)
{
    return ir == NULL ? 0 : ir->stages.size();
}

BOOL
DSL_fetch_pipeline_ir_get_site
        (const DSL_FETCH_PIPELINE_IR *ir, DSL_FETCH_SITE_ID id,
         DSL_FETCH_SITE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->sites.size())
        return FALSE;
    *record = ir->sites[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_ir_get_plan
        (const DSL_FETCH_PIPELINE_IR *ir, DSL_FETCH_PLAN_ID id,
         DSL_FETCH_PLAN_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->plans.size())
        return FALSE;
    *record = ir->plans[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_ir_get_fetch
        (const DSL_FETCH_PIPELINE_IR *ir, DSL_FETCH_RECORD_ID id,
         DSL_FETCH_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->fetches.size())
        return FALSE;
    *record = ir->fetches[id - 1];
    return TRUE;
}

BOOL
DSL_fetch_pipeline_ir_get_stage
        (const DSL_FETCH_PIPELINE_IR *ir, DSL_PIPELINE_STAGE_ID id,
         DSL_PIPELINE_STAGE_RECORD *record)
{
    if (ir == NULL || record == NULL || id == 0 || id > ir->stages.size())
        return FALSE;
    *record = ir->stages[id - 1];
    return TRUE;
}
