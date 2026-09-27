/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-9 shape capture, tile-family generation, legality,
 * costing, selection, and semantic verification.
 */

#ifndef dsl_tile_candidate_opt_INCLUDED
#define dsl_tile_candidate_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_residency_candidate_opt.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"
#include "dsl_tile_candidate.h"

struct pu_info;
struct DSL_TILE_ANALYSIS;

typedef struct DSL_TILE_ANALYSIS DSL_TILE_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 maximum_phase;
    UINT32 max_sites;
    UINT32 max_plans_per_site;
    UINT32 enable_cuda_64;
    UINT32 enable_cuda_128;
    UINT32 enable_blackwell_wide;
    UINT32 reserved;
} VHO_DSL_TILE_CONTROL;

extern void VHO_DSL_Tile_Control_Init (VHO_DSL_TILE_CONTROL *control);
extern DSL_TILE_ANALYSIS *VHO_DSL_Tile_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_RESIDENCY_ANALYSIS *residency,
                                 const VHO_DSL_TILE_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Tile_Destroy (DSL_TILE_ANALYSIS *analysis);
extern BOOL VHO_DSL_Tile_Build
                                (DSL_TILE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Tile_Verify
                                (const DSL_TILE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Tile_Print
                                (FILE *file,
                                 const DSL_TILE_ANALYSIS *analysis);
extern const DSL_TILE_PLAN_IR *VHO_DSL_Tile_Get_IR
                                (const DSL_TILE_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Tile_Get_Plan_Context
                                (const DSL_TILE_ANALYSIS *analysis,
                                 DSL_TILE_SITE_ID id);

#endif /* dsl_tile_candidate_opt_INCLUDED */
