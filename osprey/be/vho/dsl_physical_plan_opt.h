/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-11 physical implementation discovery, capability and
 * legality checks, costing, selection, and semantic verification. Common/com
 * owns only CommonPhysicalPlanIR and the provider capability registry.
 */

#ifndef dsl_physical_plan_opt_INCLUDED
#define dsl_physical_plan_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_fetch_pipeline_opt.h"
#include "dsl_physical_plan.h"
#include "dsl_tile_candidate_opt.h"

struct pu_info;
struct DSL_PHYSICAL_PLAN_ANALYSIS;

typedef struct DSL_PHYSICAL_PLAN_ANALYSIS DSL_PHYSICAL_PLAN_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plan;
    UINT32 apply_selected_plan;
    UINT32 optimization_level;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 enabled_provider_mask;
    UINT32 available_provider_mask;
    UINT32 max_sites;
    UINT32 max_implementations_per_site;
    UINT32 reserved0;
    UINT32 reserved1;
} VHO_DSL_PHYSICAL_PLAN_CONTROL;

extern void VHO_DSL_Physical_Plan_Control_Init
                                (VHO_DSL_PHYSICAL_PLAN_CONTROL *control);
extern DSL_PHYSICAL_PLAN_ANALYSIS *VHO_DSL_Physical_Plan_Create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TILE_ANALYSIS *tile,
                                 const DSL_FETCH_PIPELINE_ANALYSIS *pipeline,
                                 const VHO_DSL_PHYSICAL_PLAN_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Physical_Plan_Destroy
                                (DSL_PHYSICAL_PLAN_ANALYSIS *analysis);
extern BOOL VHO_DSL_Physical_Plan_Build
                                (DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Physical_Plan_Verify
                                (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Physical_Plan_Print
                                (FILE *file,
                                 const DSL_PHYSICAL_PLAN_ANALYSIS *analysis);
extern const DSL_PHYSICAL_PLAN_IR *VHO_DSL_Physical_Plan_Get_IR
                                (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Physical_Plan_Get_Plan_Context
                                (const DSL_PHYSICAL_PLAN_ANALYSIS *analysis,
                                 DSL_PHYSICAL_SITE_ID id);

#endif /* dsl_physical_plan_opt_INCLUDED */
