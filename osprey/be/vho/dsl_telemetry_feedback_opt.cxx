/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Implements the AIO-13 PU-local feedback consumer. It builds a separate
 * measured-cost plan view and never mutates source legality, source costs,
 * source selection, RuntimeVariantIR, or WHIRL.
 */

#include <string.h>
#include <vector>

#include "dsl_telemetry_feedback_opt.h"
#include "dsl_opt_plan_opt.h"
#include "pu_info.h"
#include "symtab.h"

struct DSL_TELEMETRY_FEEDBACK_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_RUNTIME_VARIANT_ANALYSIS *runtime;
    const DSL_RUNTIME_VARIANT_IR *runtime_ir;
    const DSL_TELEMETRY_PROFILE_IR *profile;
    VHO_DSL_TELEMETRY_FEEDBACK_CONTROL control;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plan_contexts;
    std::vector<VHO_DSL_TELEMETRY_VARIANT_RESULT> variant_results;
    std::vector<VHO_DSL_TELEMETRY_SITE_RESULT> site_results;
    UINT32 status;
    BOOL built;
};

static BOOL
DSL_Telemetry_Feedback_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL telemetry feedback error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Telemetry_Feedback_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Telemetry_Feedback_Active
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Telemetry_Feedback_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu == &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

void
VHO_DSL_Telemetry_Feedback_Control_Init
        (VHO_DSL_TELEMETRY_FEEDBACK_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->consume_profile = 1;
    control->target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    control->expected_profile_generation = 1;
    control->require_complete_profile = 1;
    control->max_records = 1024;
}

static BOOL
DSL_Telemetry_Feedback_Control_Valid
        (const VHO_DSL_TELEMETRY_FEEDBACK_CONTROL &control)
{
    return control.consume_profile <= 1 &&
           control.target_profile_id != 0 &&
           control.expected_profile_generation != 0 &&
           control.require_complete_profile <= 1 &&
           control.max_records != 0 && control.reserved0 == 0 &&
           control.reserved1 == 0 && control.reserved2 == 0;
}

DSL_TELEMETRY_FEEDBACK_ANALYSIS *
VHO_DSL_Telemetry_Feedback_Create
        (struct pu_info *pu, const DSL_RUNTIME_VARIANT_ANALYSIS *runtime,
         const DSL_TELEMETRY_PROFILE_IR *profile,
         const VHO_DSL_TELEMETRY_FEEDBACK_CONTROL *control,
         FILE *diagnostic)
{
    if (pu == NULL || runtime == NULL || control == NULL ||
        Current_PU_Info != pu ||
        !DSL_Telemetry_Feedback_Control_Valid(*control) ||
        !DSL_Telemetry_Feedback_Owner_Valid(PU_Info_proc_sym(pu)) ||
        !VHO_DSL_Runtime_Variant_Verify(runtime, diagnostic) ||
        VHO_DSL_Runtime_Variant_Get_IR(runtime) == NULL ||
        VHO_DSL_Runtime_Variant_Get_Evolution_Graph(runtime) == NULL) {
        DSL_Telemetry_Feedback_Report
            (diagnostic, "invalid create request", 0);
        return NULL;
    }
    DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis =
        new DSL_TELEMETRY_FEEDBACK_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->runtime = runtime;
    analysis->runtime_ir = VHO_DSL_Runtime_Variant_Get_IR(runtime);
    analysis->profile = profile;
    analysis->control = *control;
    analysis->status = VHO_DSL_TELEMETRY_STATUS_UNKNOWN;
    analysis->built = FALSE;
    return analysis;
}

void
VHO_DSL_Telemetry_Feedback_Destroy
        (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i)
        DSL_opt_plan_destroy(analysis->plan_contexts[i]);
    delete analysis;
}

static BOOL
DSL_Telemetry_Find_Record
        (const DSL_TELEMETRY_PROFILE_IR *profile,
         DSL_RUNTIME_VARIANT_SITE_ID site_id,
         DSL_RUNTIME_VARIANT_ID variant_id,
         DSL_TELEMETRY_RECORD *record)
{
    UINT32 count = DSL_telemetry_profile_record_count(profile);
    for (UINT32 id = 1; id <= count; ++id) {
        DSL_TELEMETRY_RECORD candidate;
        if (!DSL_telemetry_profile_get_record(profile, id, &candidate))
            return FALSE;
        if (candidate.site_id == site_id &&
            candidate.variant_id == variant_id) {
            *record = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static void
DSL_Telemetry_Measured_Cost
        (DSL_OPT_COST_INPUT *cost, UINT32 target_profile_id,
         UINT64 ordering_key, const DSL_TELEMETRY_RECORD &record)
{
    UINT64 average = record.latency_ns_total / record.sample_count;
    if (record.latency_ns_total % record.sample_count != 0)
        ++average;
    memset(cost, 0, sizeof(*cost));
    cost->target_profile_id = target_profile_id;
    cost->ordering_key = ordering_key;
    for (UINT32 i = 0; i < DSL_OPT_COST_TERM_COUNT; ++i) {
        cost->terms[i].amount = i == DSL_OPT_COST_COMPUTE ? average : 0;
        cost->terms[i].unit = DSL_OPT_COST_UNIT_NANOSECONDS;
        cost->terms[i].confidence = DSL_OPT_COST_CONFIDENCE_HIGH;
        cost->terms[i].evidence = DSL_OPT_COST_EVIDENCE_TELEMETRY;
    }
}

static BOOL
DSL_Telemetry_Copy_Candidates
        (DSL_OPT_PLAN_CONTEXT *destination,
         const DSL_OPT_PLAN_CONTEXT *source, FILE *diagnostic)
{
    for (UINT32 id = 1; id <= DSL_opt_plan_candidate_count(source); ++id) {
        DSL_OPT_CANDIDATE_RECORD record;
        DSL_OPT_CANDIDATE_INPUT input;
        DSL_OPT_CANDIDATE_ID copied_id;
        if (!DSL_opt_plan_get_candidate(source, id, &record))
            return FALSE;
        memset(&input, 0, sizeof(input));
        input.kind = record.kind;
        input.semantic_node_id = record.semantic_node_id;
        input.source_evolution_node_id = record.source_evolution_node_id;
        input.result_evolution_node_id = record.result_evolution_node_id;
        input.parent_candidate_id = record.parent_candidate_id;
        input.legality = record.legality;
        input.rejection_reason = record.rejection_reason;
        input.ordering_key = record.ordering_key;
        input.flags = record.flags;
        if (!DSL_opt_plan_add_candidate
                 (destination, &input, &copied_id, diagnostic) ||
            copied_id != id)
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Telemetry_Add_Measured_Plan
        (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis,
         DSL_OPT_PLAN_CONTEXT *destination,
         const DSL_OPT_PLAN_CONTEXT *source,
         const DSL_RUNTIME_VARIANT_SITE_RECORD &site,
         const DSL_RUNTIME_VARIANT_RECORD &variant,
         FILE *diagnostic)
{
    DSL_TELEMETRY_RECORD telemetry;
    DSL_OPT_PLAN_RECORD source_plan;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_COST_ID cost_id;
    DSL_OPT_PLAN_INPUT plan;
    std::vector<DSL_OPT_CANDIDATE_ID> members;
    DSL_OPT_PLAN_ID plan_id;
    if (!DSL_Telemetry_Find_Record
             (analysis->profile, site.id, variant.id, &telemetry) ||
        telemetry.optimization_plan_id != variant.optimization_plan_id ||
        telemetry.variant_identity != variant.identity ||
        !DSL_opt_plan_get_plan
             (source, variant.optimization_plan_id, &source_plan))
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "stale variant telemetry", variant.id);

    DSL_Telemetry_Measured_Cost
        (&cost, analysis->control.target_profile_id,
         source_plan.ordering_key, telemetry);
    if (!DSL_opt_plan_add_cost(destination, &cost, &cost_id, diagnostic))
        return FALSE;
    for (UINT32 i = 0; i < source_plan.member_count; ++i) {
        DSL_OPT_PLAN_MEMBER_RECORD member;
        if (!DSL_opt_plan_get_member(source, source_plan.id, i, &member))
            return FALSE;
        members.push_back(member.candidate_id);
    }
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &members[0];
    plan.candidate_count = members.size();
    plan.cost_id = cost_id;
    plan.fallback_plan_id = source_plan.fallback_plan_id;
    plan.legality = source_plan.legality;
    plan.rejection_reason = source_plan.rejection_reason;
    plan.ordering_key = source_plan.ordering_key;
    plan.flags = source_plan.flags;
    if (!DSL_opt_plan_add_plan
             (destination, &plan, &plan_id, diagnostic) ||
        plan_id != source_plan.id)
        return FALSE;

    VHO_DSL_TELEMETRY_VARIANT_RESULT result;
    memset(&result, 0, sizeof(result));
    result.site_id = site.id;
    result.variant_id = variant.id;
    result.source_plan_id = source_plan.id;
    result.feedback_plan_id = plan_id;
    result.average_latency_ns =
        telemetry.latency_ns_total / telemetry.sample_count;
    if (telemetry.latency_ns_total % telemetry.sample_count != 0)
        ++result.average_latency_ns;
    if (telemetry.guard_evaluation_count != 0) {
        UINT64 whole = telemetry.guard_pass_count /
                       telemetry.guard_evaluation_count;
        UINT64 remainder = telemetry.guard_pass_count %
                           telemetry.guard_evaluation_count;
        result.guard_hit_rate_ppm =
            (UINT32)(whole * DSL_TELEMETRY_OCCUPANCY_SCALE +
                     (remainder * DSL_TELEMETRY_OCCUPANCY_SCALE) /
                         telemetry.guard_evaluation_count);
    }
    result.originally_selected =
        variant.id == site.selected_variant_id ? 1 : 0;
    analysis->variant_results.push_back(result);
    return TRUE;
}

static BOOL
DSL_Telemetry_Build_Site
        (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis,
         const DSL_RUNTIME_VARIANT_SITE_RECORD &site, FILE *diagnostic)
{
    const DSL_OPT_PLAN_CONTEXT *source =
        VHO_DSL_Runtime_Variant_Get_Plan_Context
            (analysis->runtime, site.id);
    if (source == NULL)
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "missing source plan", site.id);
    DSL_OPT_PLAN_BUDGET budget;
    budget.max_candidates = DSL_opt_plan_candidate_count(source);
    budget.max_plans = DSL_opt_plan_plan_count(source);
    DSL_OPT_PLAN_CONTEXT *destination = DSL_opt_plan_create
        (analysis->pu,
         VHO_DSL_Runtime_Variant_Get_Evolution_Graph(analysis->runtime),
         &budget, diagnostic);
    if (destination == NULL)
        return FALSE;
    analysis->plan_contexts.push_back(destination);
    if (!DSL_Telemetry_Copy_Candidates(destination, source, diagnostic))
        return FALSE;
    UINT32 first_result = analysis->variant_results.size();
    for (UINT32 i = 0; i < site.variant_count; ++i) {
        DSL_RUNTIME_VARIANT_RECORD variant;
        if (!DSL_runtime_variant_ir_get_variant
                 (analysis->runtime_ir, site.first_variant_id + i,
                  &variant) ||
            !DSL_Telemetry_Add_Measured_Plan
                 (analysis, destination, source, site, variant, diagnostic))
            return FALSE;
    }
    DSL_OPT_SELECTION_RESULT selection;
    if (!VHO_DSL_Opt_Plan_Select
             (destination, analysis->control.target_profile_id,
              &selection, diagnostic))
        return FALSE;

    VHO_DSL_TELEMETRY_SITE_RESULT site_result;
    memset(&site_result, 0, sizeof(site_result));
    site_result.site_id = site.id;
    site_result.original_variant_id = site.selected_variant_id;
    site_result.recommended_plan_id = selection.selected_plan_id;
    for (UINT32 i = first_result;
         i < analysis->variant_results.size(); ++i) {
        VHO_DSL_TELEMETRY_VARIANT_RESULT &result =
            analysis->variant_results[i];
        if (result.feedback_plan_id == selection.selected_plan_id) {
            result.recommended = 1;
            site_result.recommended_variant_id = result.variant_id;
        }
    }
    if (site_result.recommended_variant_id == 0)
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "selection has no variant", site.id);
    site_result.profitability_changed =
        site_result.original_variant_id !=
            site_result.recommended_variant_id ? 1 : 0;
    site_result.legality_changed = 0;
    analysis->site_results.push_back(site_result);
    return TRUE;
}

BOOL
VHO_DSL_Telemetry_Feedback_Build
        (DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Telemetry_Feedback_Active(analysis) || analysis->built)
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "inactive analysis", 0);
    if (!analysis->control.consume_profile || analysis->profile == NULL) {
        analysis->status = VHO_DSL_TELEMETRY_STATUS_NO_PROFILE;
        analysis->built = TRUE;
        return VHO_DSL_Telemetry_Feedback_Verify(analysis, diagnostic);
    }
    DSL_TELEMETRY_PROFILE_HEADER header;
    UINT32 runtime_count =
        DSL_runtime_variant_ir_variant_count(analysis->runtime_ir);
    if (!DSL_telemetry_profile_verify(analysis->profile, diagnostic) ||
        !DSL_telemetry_profile_get_header(analysis->profile, &header) ||
        header.owner_pu_st != analysis->owner_pu_st ||
        header.target_profile_id != analysis->control.target_profile_id ||
        header.profile_generation !=
            analysis->control.expected_profile_generation ||
        header.record_count > analysis->control.max_records ||
        (analysis->control.require_complete_profile &&
         header.record_count != runtime_count))
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "stale or incomplete profile", 0);
    for (UINT32 id = 1;
         id <= DSL_runtime_variant_ir_site_count(analysis->runtime_ir); ++id) {
        DSL_RUNTIME_VARIANT_SITE_RECORD site;
        if (!DSL_runtime_variant_ir_get_site
                 (analysis->runtime_ir, id, &site) ||
            !DSL_Telemetry_Build_Site(analysis, site, diagnostic))
            return FALSE;
    }
    analysis->status = VHO_DSL_TELEMETRY_STATUS_APPLIED;
    analysis->built = TRUE;
    return VHO_DSL_Telemetry_Feedback_Verify(analysis, diagnostic);
}

BOOL
VHO_DSL_Telemetry_Feedback_Verify
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Telemetry_Feedback_Active(analysis) || !analysis->built ||
        !DSL_Telemetry_Feedback_Control_Valid(analysis->control) ||
        !VHO_DSL_Runtime_Variant_Verify(analysis->runtime, diagnostic) ||
        analysis->runtime_ir !=
            VHO_DSL_Runtime_Variant_Get_IR(analysis->runtime))
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "invalid analysis", 0);
    if (analysis->status == VHO_DSL_TELEMETRY_STATUS_NO_PROFILE)
        return analysis->plan_contexts.empty() &&
               analysis->variant_results.empty() &&
               analysis->site_results.empty();
    if (analysis->status != VHO_DSL_TELEMETRY_STATUS_APPLIED ||
        analysis->profile == NULL ||
        !DSL_telemetry_profile_verify(analysis->profile, diagnostic) ||
        analysis->variant_results.size() !=
            DSL_runtime_variant_ir_variant_count(analysis->runtime_ir) ||
        analysis->site_results.size() !=
            DSL_runtime_variant_ir_site_count(analysis->runtime_ir) ||
        analysis->plan_contexts.size() != analysis->site_results.size())
        return DSL_Telemetry_Feedback_Report
                   (diagnostic, "invalid applied profile", 0);
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i) {
        if (!DSL_opt_plan_verify(analysis->plan_contexts[i], diagnostic) ||
            analysis->site_results[i].site_id != i + 1 ||
            analysis->site_results[i].recommended_variant_id == 0 ||
            analysis->site_results[i].legality_changed != 0)
            return DSL_Telemetry_Feedback_Report
                       (diagnostic, "invalid feedback result", i + 1);
    }
    return TRUE;
}

void
VHO_DSL_Telemetry_Feedback_Print
        (FILE *file, const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL || !analysis->built)
        return;
    const char *status =
        analysis->status == VHO_DSL_TELEMETRY_STATUS_NO_PROFILE ?
            "no_profile" :
        analysis->status == VHO_DSL_TELEMETRY_STATUS_APPLIED ?
            "applied" : "unknown";
    fprintf(file,
            "VHODSLTelemetryFeedback: stage=G16 owner=0x%x status=%s "
            "sites=%u variants=%u whirl_mutation=no legality_mutation=no\n",
            analysis->owner_pu_st, status,
            (UINT32)analysis->site_results.size(),
            (UINT32)analysis->variant_results.size());
    if (analysis->status == VHO_DSL_TELEMETRY_STATUS_NO_PROFILE)
        return;
    DSL_telemetry_profile_print(file, analysis->profile);
    for (UINT32 i = 0; i < analysis->plan_contexts.size(); ++i)
        DSL_opt_plan_print(file, analysis->plan_contexts[i]);
    for (UINT32 i = 0; i < analysis->variant_results.size(); ++i) {
        const VHO_DSL_TELEMETRY_VARIANT_RESULT &result =
            analysis->variant_results[i];
        fprintf(file,
                "  feedback-variant site=%u variant=%u source_plan=%u "
                "feedback_plan=%u average_latency_ns=%llu "
                "guard_hit_ppm=%u original=%s recommended=%s "
                "cost_evidence=telemetry\n",
                result.site_id, result.variant_id, result.source_plan_id,
                result.feedback_plan_id,
                (unsigned long long)result.average_latency_ns,
                result.guard_hit_rate_ppm,
                result.originally_selected ? "yes" : "no",
                result.recommended ? "yes" : "no");
    }
    for (UINT32 i = 0; i < analysis->site_results.size(); ++i) {
        const VHO_DSL_TELEMETRY_SITE_RESULT &result =
            analysis->site_results[i];
        fprintf(file,
                "  feedback-site id=%u original_variant=%u "
                "recommended_variant=%u recommended_plan=%u "
                "profitability_changed=%s legality_changed=no\n",
                result.site_id, result.original_variant_id,
                result.recommended_variant_id,
                result.recommended_plan_id,
                result.profitability_changed ? "yes" : "no");
    }
}

UINT32
VHO_DSL_Telemetry_Feedback_Status
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    return analysis == NULL ? VHO_DSL_TELEMETRY_STATUS_UNKNOWN :
           analysis->status;
}

UINT32
VHO_DSL_Telemetry_Feedback_Variant_Count
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->variant_results.size();
}

UINT32
VHO_DSL_Telemetry_Feedback_Site_Count
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->site_results.size();
}

BOOL
VHO_DSL_Telemetry_Feedback_Get_Variant
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis, UINT32 ordinal,
         VHO_DSL_TELEMETRY_VARIANT_RESULT *result)
{
    if (analysis == NULL || result == NULL || ordinal == 0 ||
        ordinal > analysis->variant_results.size())
        return FALSE;
    *result = analysis->variant_results[ordinal - 1];
    return TRUE;
}

BOOL
VHO_DSL_Telemetry_Feedback_Get_Site
        (const DSL_TELEMETRY_FEEDBACK_ANALYSIS *analysis,
         DSL_RUNTIME_VARIANT_SITE_ID site_id,
         VHO_DSL_TELEMETRY_SITE_RESULT *result)
{
    if (analysis == NULL || result == NULL || site_id == 0 ||
        site_id > analysis->site_results.size())
        return FALSE;
    *result = analysis->site_results[site_id - 1];
    return TRUE;
}
