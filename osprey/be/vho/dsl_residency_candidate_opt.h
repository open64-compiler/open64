/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-8 residency planning policy. */

#ifndef dsl_residency_candidate_opt_INCLUDED
#define dsl_residency_candidate_opt_INCLUDED

#include "dsl_residency_candidate.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_RESIDENCY_ANALYSIS;
typedef struct DSL_RESIDENCY_ANALYSIS DSL_RESIDENCY_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 max_sites;
    UINT32 max_alternatives_per_site;
    UINT32 enable_system;
    UINT32 enable_pinned_host;
    UINT32 enable_hbm;
    UINT32 enable_l2;
    UINT32 enable_shared;
    UINT32 enable_register;
    UINT32 reserved;
} VHO_DSL_RESIDENCY_CONTROL;

extern void VHO_DSL_Residency_Control_Init
                                (VHO_DSL_RESIDENCY_CONTROL *control);
extern DSL_RESIDENCY_ANALYSIS *VHO_DSL_Residency_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const VHO_DSL_RESIDENCY_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Residency_Destroy (DSL_RESIDENCY_ANALYSIS *analysis);
extern BOOL VHO_DSL_Residency_Build
                                (DSL_RESIDENCY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Residency_Verify
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Residency_Print
                                (FILE *file,
                                 const DSL_RESIDENCY_ANALYSIS *analysis);
extern const DSL_RESIDENCY_PLAN_IR *VHO_DSL_Residency_Get_IR
                                (const DSL_RESIDENCY_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Residency_Get_Plan_Context
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 DSL_RESIDENCY_SITE_ID id);

#endif
