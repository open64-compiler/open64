/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-5 fusion discovery and planning policy. */

#ifndef dsl_fusion_candidate_opt_INCLUDED
#define dsl_fusion_candidate_opt_INCLUDED

#include "dsl_fusion_candidate.h"
#include "dsl_layout_candidate_opt.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_FUSION_CANDIDATE_ANALYSIS;
typedef struct DSL_FUSION_CANDIDATE_ANALYSIS DSL_FUSION_CANDIDATE_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    UINT64 resource_limit_bytes;
    UINT32 max_sites;
    UINT32 enable_semantic_patterns;
    UINT32 enable_generic_clusters;
    UINT32 max_cluster_members;
    UINT32 reserved;
} VHO_DSL_FUSION_CONTROL;

extern void VHO_DSL_Fusion_Control_Init (VHO_DSL_FUSION_CONTROL *control);
extern DSL_FUSION_CANDIDATE_ANALYSIS *VHO_DSL_Fusion_Create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const VHO_DSL_FUSION_CONTROL *control,
                                 FILE *diagnostic);
extern DSL_FUSION_CANDIDATE_ANALYSIS *VHO_DSL_Fusion_Create_With_Layout
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_LOGICAL_LAYOUT_ANALYSIS *layout,
                                 const VHO_DSL_FUSION_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Fusion_Destroy
                                (DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern BOOL VHO_DSL_Fusion_Build
                                (DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Fusion_Verify
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Fusion_Print
                                (FILE *file,
                                 const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern const DSL_FUSION_PLAN_IR *VHO_DSL_Fusion_Get_IR
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Fusion_Get_Plan_Context
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 DSL_FUSION_SITE_ID id);

#endif
