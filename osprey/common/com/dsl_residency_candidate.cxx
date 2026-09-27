/*
 * Copyright (C) 2026 Open64 Project
 */

/* Policy-free AIO-8 CommonResidencyPlanIR storage and structural services. */

#include <vector>

#include "dsl_residency_candidate.h"

struct DSL_RESIDENCY_PLAN_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_RESIDENCY_DESCRIPTOR_RECORD> descriptors;
    std::vector<DSL_RESIDENCY_SITE_RECORD> sites;
    std::vector<DSL_RESIDENCY_ALTERNATIVE_RECORD> alternatives;
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
DSL_Residency_IR_Report (FILE *file, const char *message, UINT32 id)
{
    if (file != NULL)
        fprintf(file, "DSL residency IR error: %s id=%u\n", message, id);
    return FALSE;
}

#define DSL_RESIDENCY_NAME(function, table)                              \
const char *function (UINT32 value)                                      \
{                                                                        \
    return value < sizeof(table) / sizeof(table[0]) ? table[value] :      \
           "unknown";                                                    \
}
DSL_RESIDENCY_NAME(DSL_residency_promotion_name,
                   DSL_residency_promotion_name_table)
DSL_RESIDENCY_NAME(DSL_residency_demotion_name,
                   DSL_residency_demotion_name_table)
DSL_RESIDENCY_NAME(DSL_residency_spill_name,
                   DSL_residency_spill_name_table)
DSL_RESIDENCY_NAME(DSL_residency_eviction_name,
                   DSL_residency_eviction_name_table)
#undef DSL_RESIDENCY_NAME

BOOL
DSL_residency_plan_ir_verify
        (const DSL_RESIDENCY_PLAN_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Residency_IR_Report(diagnostic, "invalid owner", 0);
    for (UINT32 i = 0; i < ir->descriptors.size(); ++i) {
        const DSL_RESIDENCY_DESCRIPTOR_RECORD &record = ir->descriptors[i];
        if (record.id != i + 1 || record.source_descriptor_ty == TY_IDX_ZERO ||
            record.target_profile_id == 0 || record.tier_id == 0 ||
            record.tier_kind == 0 || record.minimum_alignment == 0 ||
            record.promotion_policy <= DSL_RESIDENCY_PROMOTION_UNKNOWN ||
            record.promotion_policy > DSL_RESIDENCY_PROMOTION_PREFETCH ||
            record.demotion_policy <= DSL_RESIDENCY_DEMOTION_UNKNOWN ||
            record.demotion_policy > DSL_RESIDENCY_DEMOTION_PRESSURE ||
            record.spill_policy <= DSL_RESIDENCY_SPILL_UNKNOWN ||
            record.spill_policy > DSL_RESIDENCY_SPILL_SYSTEM ||
            record.eviction_policy <= DSL_RESIDENCY_EVICTION_UNKNOWN ||
            record.eviction_policy > DSL_RESIDENCY_EVICTION_PRESSURE ||
            (record.flags & ~(DSL_RESIDENCY_FLAG_PROVISIONAL |
                              DSL_RESIDENCY_FLAG_SEMANTICS_PRESERVING |
                              DSL_RESIDENCY_FLAG_CAPACITY_CHECKED |
                              DSL_RESIDENCY_FLAG_LIFETIME_CHECKED)) != 0 ||
            record.reserved != 0)
            return DSL_Residency_IR_Report
                       (diagnostic, "invalid descriptor", record.id);
    }
    UINT32 alternative_id = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_RESIDENCY_SITE_RECORD &site = ir->sites[i];
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.semantic_value_id == 0 || site.semantic_root_id == 0 ||
            site.first_alternative_id != alternative_id ||
            site.alternative_count == 0 || site.baseline_candidate_id == 0 ||
            site.baseline_plan_id == 0 || site.reserved != 0)
            return DSL_Residency_IR_Report
                       (diagnostic, "invalid site", site.id);
        BOOL selected_found = site.selected_plan_id == 0 ||
                              site.selected_plan_id == site.baseline_plan_id;
        for (UINT32 j = 0; j < site.alternative_count;
             ++j, ++alternative_id) {
            if (alternative_id > ir->alternatives.size())
                return DSL_Residency_IR_Report
                           (diagnostic, "alternative range overflow", site.id);
            const DSL_RESIDENCY_ALTERNATIVE_RECORD &alternative =
                ir->alternatives[alternative_id - 1];
            if (alternative.id != alternative_id ||
                alternative.site_id != site.id ||
                alternative.descriptor_id == 0 ||
                alternative.descriptor_id > ir->descriptors.size() ||
                alternative.result_evolution_node_id == 0 ||
                alternative.evolution_edge_id == 0 ||
                alternative.legality > DSL_OPT_LEGALITY_REJECTED ||
                alternative.rejection_reason >
                    DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
                alternative.candidate_id == 0 || alternative.plan_id == 0 ||
                alternative.reserved != 0)
                return DSL_Residency_IR_Report
                           (diagnostic, "invalid alternative", alternative.id);
            if (alternative.plan_id == site.selected_plan_id)
                selected_found = TRUE;
        }
        if (!selected_found)
            return DSL_Residency_IR_Report
                       (diagnostic, "selected plan is absent", site.id);
    }
    return alternative_id == ir->alternatives.size() + 1;
}

DSL_RESIDENCY_PLAN_IR *
DSL_residency_plan_ir_create
        (const DSL_RESIDENCY_PLAN_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->descriptor_count != 0 && info->descriptors == NULL) ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->alternative_count != 0 && info->alternatives == NULL)) {
        DSL_Residency_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_RESIDENCY_PLAN_IR *ir = new DSL_RESIDENCY_PLAN_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->descriptor_count != 0)
        ir->descriptors.assign
            (info->descriptors, info->descriptors + info->descriptor_count);
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->alternative_count != 0)
        ir->alternatives.assign
            (info->alternatives, info->alternatives + info->alternative_count);
    if (!DSL_residency_plan_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void DSL_residency_plan_ir_destroy (DSL_RESIDENCY_PLAN_IR *ir) { delete ir; }

void
DSL_residency_plan_ir_print (FILE *file, const DSL_RESIDENCY_PLAN_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file,
            "CommonResidencyPlanIR: owner=0x%x sites=%u descriptors=%u "
            "alternatives=%u\n", ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->descriptors.size(), (UINT32)ir->alternatives.size());
}

ST_IDX DSL_residency_plan_ir_owner(const DSL_RESIDENCY_PLAN_IR *ir)
{ return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st; }
UINT32 DSL_residency_plan_ir_descriptor_count(const DSL_RESIDENCY_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->descriptors.size(); }
UINT32 DSL_residency_plan_ir_site_count(const DSL_RESIDENCY_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->sites.size(); }
UINT32 DSL_residency_plan_ir_alternative_count(const DSL_RESIDENCY_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->alternatives.size(); }

#define DSL_RESIDENCY_GET(name, type, record_type, field)                \
BOOL DSL_residency_plan_ir_get_##name                                    \
        (const DSL_RESIDENCY_PLAN_IR *ir, type id, record_type *record)   \
{                                                                        \
    if (ir == NULL || record == NULL || id == 0 || id > ir->field.size()) \
        return FALSE;                                                     \
    *record = ir->field[id - 1];                                         \
    return TRUE;                                                         \
}
DSL_RESIDENCY_GET(descriptor, DSL_RESIDENCY_DESCRIPTOR_ID,
                  DSL_RESIDENCY_DESCRIPTOR_RECORD, descriptors)
DSL_RESIDENCY_GET(site, DSL_RESIDENCY_SITE_ID,
                  DSL_RESIDENCY_SITE_RECORD, sites)
DSL_RESIDENCY_GET(alternative, DSL_RESIDENCY_ALTERNATIVE_ID,
                  DSL_RESIDENCY_ALTERNATIVE_RECORD, alternatives)
#undef DSL_RESIDENCY_GET
