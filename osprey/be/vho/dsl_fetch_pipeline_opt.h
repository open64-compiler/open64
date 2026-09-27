/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-10 movement-plan generation, overlap estimates,
 * barrier/resource legality, costing, selection, and semantic verification.
 */

#ifndef dsl_fetch_pipeline_opt_INCLUDED
#define dsl_fetch_pipeline_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_distributed_candidate.h"
#include "dsl_fetch_pipeline.h"
#include "dsl_tensor_locality.h"
#include "dsl_tile_candidate_opt.h"

struct pu_info;
struct DSL_FETCH_PIPELINE_ANALYSIS;

typedef struct DSL_FETCH_PIPELINE_ANALYSIS DSL_FETCH_PIPELINE_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 max_sites;
    UINT32 max_plans_per_site;
    UINT32 maximum_buffer_stages;
    UINT32 prefetch_distance_hint;
    UINT32 enable_vector;
    UINT32 enable_async_copy;
    UINT32 enable_multidimensional_async;
    UINT32 reserved;
} VHO_DSL_FETCH_PIPELINE_CONTROL;

extern void VHO_DSL_Fetch_Pipeline_Control_Init
                                (VHO_DSL_FETCH_PIPELINE_CONTROL *control);
extern DSL_FETCH_PIPELINE_ANALYSIS *VHO_DSL_Fetch_Pipeline_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_DISTRIBUTED_ANALYSIS *distributed,
                                 const DSL_TILE_ANALYSIS *tile,
                                 const VHO_DSL_FETCH_PIPELINE_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Fetch_Pipeline_Destroy
                                (DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern BOOL VHO_DSL_Fetch_Pipeline_Build
                                (DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Fetch_Pipeline_Verify
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Fetch_Pipeline_Print
                                (FILE *file,
                                 const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern const DSL_FETCH_PIPELINE_IR *VHO_DSL_Fetch_Pipeline_Get_IR
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Fetch_Pipeline_Get_Plan_Context
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_FETCH_SITE_ID id);

#endif /* dsl_fetch_pipeline_opt_INCLUDED */
