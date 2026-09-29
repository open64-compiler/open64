/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Implements the AIO-2 VHO plan-selection policy. Selection considers only
 * proven plans with complete comparable cost for the requested target. Equal
 * costs retain the earlier stable plan ID. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO2-PLAN-COST.md.
 */

#include <string.h>

#include "dsl_opt_plan_opt.h"

static BOOL
DSL_Opt_Select_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL optimization selection error: %s\n", message);
    return FALSE;
}

BOOL
VHO_DSL_Opt_Plan_Select
        (DSL_OPT_PLAN_CONTEXT *context, UINT32 target_profile_id,
         DSL_OPT_SELECTION_RESULT *result, FILE *diagnostic)
{
    DSL_OPT_SELECTION_RESULT selection;
    UINT64 best_cost = ~(UINT64)0;
    memset(&selection, 0, sizeof(selection));
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (context == NULL || target_profile_id == 0 ||
        !DSL_opt_plan_verify(context, diagnostic))
        return FALSE;

    if (DSL_opt_plan_get_selection(context, &selection)) {
        if (selection.target_profile_id != target_profile_id)
            return DSL_Opt_Select_Report
                       (diagnostic, "selection target changed");
        if (result != NULL)
            *result = selection;
        return TRUE;
    }

    selection.target_profile_id = target_profile_id;
    for (DSL_OPT_PLAN_ID id = 1;
         id <= DSL_opt_plan_plan_count(context); ++id) {
        DSL_OPT_PLAN_RECORD plan;
        DSL_OPT_COST_RECORD cost;
        if (!DSL_opt_plan_get_plan(context, id, &plan) ||
            !DSL_opt_plan_get_cost(context, plan.cost_id, &cost))
            return DSL_Opt_Select_Report
                       (diagnostic, "inconsistent plan records");
        if (plan.legality != DSL_OPT_LEGALITY_PROVEN) {
            ++selection.rejected_plan_count;
            continue;
        }
        ++selection.legal_plan_count;
        if (cost.target_profile_id != target_profile_id) {
            ++selection.target_mismatch_count;
            continue;
        }
        if (!cost.complete) {
            ++selection.incomplete_cost_count;
            continue;
        }
        ++selection.complete_cost_count;
        if (selection.selected_plan_id == 0 || cost.total < best_cost) {
            selection.selected_plan_id = plan.id;
            best_cost = cost.total;
        }
    }
    if (selection.selected_plan_id == 0) {
        if (result != NULL)
            *result = selection;
        return DSL_Opt_Select_Report(diagnostic, "no selectable plan");
    }
    if (!DSL_opt_plan_record_selection(context, &selection, diagnostic))
        return FALSE;
    if (result != NULL)
        *result = selection;
    return TRUE;
}
