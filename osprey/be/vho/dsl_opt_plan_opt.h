/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-2 plan selection policy. Common/com owns immutable
 * candidate, cost, plan, and recorded-selection IR; VHO chooses among those
 * records for the active target profile.
 */

#ifndef dsl_opt_plan_opt_INCLUDED
#define dsl_opt_plan_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"

extern BOOL VHO_DSL_Opt_Plan_Select
                                (DSL_OPT_PLAN_CONTEXT *context,
                                 UINT32 target_profile_id,
                                 DSL_OPT_SELECTION_RESULT *result,
                                 FILE *diagnostic);

#endif /* dsl_opt_plan_opt_INCLUDED */
