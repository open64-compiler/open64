/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-6 logical-layout planning policy. */

#ifndef dsl_layout_candidate_opt_INCLUDED
#define dsl_layout_candidate_opt_INCLUDED

#include "dsl_layout_candidate.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_LOGICAL_LAYOUT_ANALYSIS;
typedef struct DSL_LOGICAL_LAYOUT_ANALYSIS DSL_LOGICAL_LAYOUT_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 max_sites;
    UINT32 max_alternatives_per_site;
    UINT32 default_block_size;
    UINT32 reserved;
} VHO_DSL_LOGICAL_LAYOUT_CONTROL;

extern void VHO_DSL_Logical_Layout_Control_Init
                                (VHO_DSL_LOGICAL_LAYOUT_CONTROL *control);
extern DSL_LOGICAL_LAYOUT_ANALYSIS *VHO_DSL_Logical_Layout_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const VHO_DSL_LOGICAL_LAYOUT_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Logical_Layout_Destroy
                                (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern BOOL VHO_DSL_Logical_Layout_Build
                                (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Logical_Layout_Verify
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Logical_Layout_Print
                                (FILE *file,
                                 const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern const DSL_LOGICAL_LAYOUT_IR *VHO_DSL_Logical_Layout_Get_IR
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Logical_Layout_Get_Plan_Context
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_SITE_ID id);

#endif
