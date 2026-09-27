/*
 * Copyright (C) 2026 Open64 Project
 */

/* Policy-free AIO-5 CommonFusionPlanIR storage and structural services. */

#include <vector>

#include "dsl_fusion_candidate.h"

struct DSL_FUSION_PLAN_IR {
    ST_IDX owner_pu_st;
    std::vector<DSL_FUSION_SITE_RECORD> sites;
    std::vector<DSL_FUSION_MEMBER_RECORD> members;
    std::vector<DSL_FUSION_BOUNDARY_RECORD> boundaries;
};

static const char *DSL_fusion_pattern_name_table[] = {
    "unknown", "matmul_bias_activation", "residual_activation",
    "generic_cluster"
};
static const char *DSL_fusion_member_role_name_table[] = {
    "unknown", "matmul", "bias_add", "residual_add", "activation",
    "generic_contraction", "generic_pointwise"
};
static const char *DSL_fusion_boundary_kind_name_table[] = {
    "unknown", "input", "output", "alternative_cut"
};
static const char *DSL_fusion_fact_state_name_table[] = {
    "unknown", "proven", "rejected"
};

static BOOL
DSL_Fusion_IR_Report (FILE *file, const char *message, UINT32 id)
{
    if (file != NULL)
        fprintf(file, "DSL fusion IR error: %s id=%u\n", message, id);
    return FALSE;
}

const char *
DSL_fusion_pattern_name (UINT32 value)
{
    return value < sizeof(DSL_fusion_pattern_name_table) /
                       sizeof(DSL_fusion_pattern_name_table[0]) ?
           DSL_fusion_pattern_name_table[value] : "unknown";
}

const char *
DSL_fusion_member_role_name (UINT32 value)
{
    return value < sizeof(DSL_fusion_member_role_name_table) /
                       sizeof(DSL_fusion_member_role_name_table[0]) ?
           DSL_fusion_member_role_name_table[value] : "unknown";
}

const char *
DSL_fusion_boundary_kind_name (UINT32 value)
{
    return value < sizeof(DSL_fusion_boundary_kind_name_table) /
                       sizeof(DSL_fusion_boundary_kind_name_table[0]) ?
           DSL_fusion_boundary_kind_name_table[value] : "unknown";
}

const char *
DSL_fusion_fact_state_name (UINT32 value)
{
    return value < sizeof(DSL_fusion_fact_state_name_table) /
                       sizeof(DSL_fusion_fact_state_name_table[0]) ?
           DSL_fusion_fact_state_name_table[value] : "unknown";
}

BOOL
DSL_fusion_plan_ir_verify (const DSL_FUSION_PLAN_IR *ir, FILE *diagnostic)
{
    if (ir == NULL || ir->owner_pu_st == ST_IDX_ZERO)
        return DSL_Fusion_IR_Report(diagnostic, "invalid owner", 0);
    UINT32 member = 1;
    UINT32 boundary = 1;
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_FUSION_SITE_RECORD &site = ir->sites[i];
        UINT32 facts[] = { site.semantic_state, site.descriptor_state,
                           site.effect_state, site.resource_state,
                           site.layout_state };
        if (site.id != i + 1 || site.owner_pu_st != ir->owner_pu_st ||
            site.pattern <= DSL_FUSION_PATTERN_UNKNOWN ||
            site.pattern > DSL_FUSION_PATTERN_GENERIC_CLUSTER ||
            site.root_node_id == 0 || site.result_value_id == 0 ||
            site.first_member_id != member || site.member_count < 2 ||
            site.first_boundary_id != boundary || site.boundary_count == 0 ||
            site.legality > DSL_OPT_LEGALITY_REJECTED ||
            site.rejection_reason > DSL_OPT_REJECT_PROVIDER_UNAVAILABLE ||
            site.baseline_candidate_id == 0 ||
            site.fusion_candidate_id == 0 || site.baseline_plan_id == 0 ||
            site.fusion_plan_id == 0 || site.reserved != 0)
            return DSL_Fusion_IR_Report(diagnostic, "invalid site", site.id);
        for (UINT32 f = 0; f < sizeof(facts) / sizeof(facts[0]); ++f)
            if (facts[f] > DSL_FUSION_FACT_REJECTED)
                return DSL_Fusion_IR_Report
                           (diagnostic, "invalid fact state", site.id);
        if (site.selected_plan_id != 0 &&
            site.selected_plan_id != site.baseline_plan_id &&
            site.selected_plan_id != site.fusion_plan_id)
            return DSL_Fusion_IR_Report
                       (diagnostic, "invalid selected plan", site.id);
        for (UINT32 j = 0; j < site.member_count; ++j, ++member) {
            if (member > ir->members.size())
                return DSL_Fusion_IR_Report
                           (diagnostic, "member range overflow", site.id);
            const DSL_FUSION_MEMBER_RECORD &record = ir->members[member - 1];
            if (record.id != member || record.site_id != site.id ||
                record.node_id == 0 || record.result_value_id == 0 ||
                record.role <= DSL_FUSION_MEMBER_UNKNOWN ||
                record.role > DSL_FUSION_MEMBER_GENERIC_POINTWISE ||
                record.ordinal != j || record.reserved0 != 0 ||
                record.reserved1 != 0)
                return DSL_Fusion_IR_Report
                           (diagnostic, "invalid member", record.id);
        }
        for (UINT32 j = 0; j < site.boundary_count; ++j, ++boundary) {
            if (boundary > ir->boundaries.size())
                return DSL_Fusion_IR_Report
                           (diagnostic, "boundary range overflow", site.id);
            const DSL_FUSION_BOUNDARY_RECORD &record =
                ir->boundaries[boundary - 1];
            if (record.id != boundary || record.site_id != site.id ||
                record.value_id == 0 ||
                record.kind <= DSL_FUSION_BOUNDARY_UNKNOWN ||
                record.kind > DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT ||
                record.reserved != 0)
                return DSL_Fusion_IR_Report
                           (diagnostic, "invalid boundary", record.id);
        }
    }
    if (member != ir->members.size() + 1 ||
        boundary != ir->boundaries.size() + 1)
        return DSL_Fusion_IR_Report(diagnostic, "table range mismatch", 0);
    return TRUE;
}

DSL_FUSION_PLAN_IR *
DSL_fusion_plan_ir_create
        (const DSL_FUSION_PLAN_IR_CREATE_INFO *info, FILE *diagnostic)
{
    if (info == NULL || info->owner_pu_st == ST_IDX_ZERO ||
        (info->site_count != 0 && info->sites == NULL) ||
        (info->member_count != 0 && info->members == NULL) ||
        (info->boundary_count != 0 && info->boundaries == NULL)) {
        DSL_Fusion_IR_Report(diagnostic, "invalid create info", 0);
        return NULL;
    }
    DSL_FUSION_PLAN_IR *ir = new DSL_FUSION_PLAN_IR;
    ir->owner_pu_st = info->owner_pu_st;
    if (info->site_count != 0)
        ir->sites.assign(info->sites, info->sites + info->site_count);
    if (info->member_count != 0)
        ir->members.assign(info->members, info->members + info->member_count);
    if (info->boundary_count != 0)
        ir->boundaries.assign
            (info->boundaries, info->boundaries + info->boundary_count);
    if (!DSL_fusion_plan_ir_verify(ir, diagnostic)) {
        delete ir;
        return NULL;
    }
    return ir;
}

void DSL_fusion_plan_ir_destroy (DSL_FUSION_PLAN_IR *ir) { delete ir; }

void
DSL_fusion_plan_ir_print (FILE *file, const DSL_FUSION_PLAN_IR *ir)
{
    if (file == NULL || ir == NULL)
        return;
    fprintf(file, "CommonFusionPlanIR: owner=0x%x sites=%u members=%u boundaries=%u\n",
            ir->owner_pu_st, (UINT32)ir->sites.size(),
            (UINT32)ir->members.size(), (UINT32)ir->boundaries.size());
    for (UINT32 i = 0; i < ir->sites.size(); ++i) {
        const DSL_FUSION_SITE_RECORD &site = ir->sites[i];
        fprintf(file,
                "  fusion-site id=%u pattern=%s root=%u result=%u members=%u "
                "boundaries=%u legality=%s reason=%s selected=%u\n",
                site.id, DSL_fusion_pattern_name(site.pattern),
                site.root_node_id, site.result_value_id, site.member_count,
                site.boundary_count, DSL_opt_legality_name(site.legality),
                DSL_opt_rejection_reason_name(site.rejection_reason),
                site.selected_plan_id);
    }
}

ST_IDX DSL_fusion_plan_ir_owner(const DSL_FUSION_PLAN_IR *ir)
{ return ir == NULL ? ST_IDX_ZERO : ir->owner_pu_st; }
UINT32 DSL_fusion_plan_ir_site_count(const DSL_FUSION_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->sites.size(); }
UINT32 DSL_fusion_plan_ir_member_count(const DSL_FUSION_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->members.size(); }
UINT32 DSL_fusion_plan_ir_boundary_count(const DSL_FUSION_PLAN_IR *ir)
{ return ir == NULL ? 0 : ir->boundaries.size(); }

#define DSL_FUSION_GETTER(name, plural, type, field)                      \
BOOL DSL_fusion_plan_ir_get_##name                                       \
        (const DSL_FUSION_PLAN_IR *ir, type id,                           \
         DSL_FUSION_##field##_RECORD *record)                             \
{                                                                        \
    if (ir == NULL || record == NULL || id == 0 || id > ir->plural.size()) \
        return FALSE;                                                     \
    *record = ir->plural[id - 1];                                         \
    return TRUE;                                                         \
}

DSL_FUSION_GETTER(site, sites, DSL_FUSION_SITE_ID, SITE)
DSL_FUSION_GETTER(member, members, DSL_FUSION_MEMBER_ID, MEMBER)
DSL_FUSION_GETTER(boundary, boundaries, DSL_FUSION_BOUNDARY_ID, BOUNDARY)

#undef DSL_FUSION_GETTER
