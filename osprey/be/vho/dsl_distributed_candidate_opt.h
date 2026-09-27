/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-7 distributed planning policy. */

#ifndef dsl_distributed_candidate_opt_INCLUDED
#define dsl_distributed_candidate_opt_INCLUDED

#include "dsl_distributed_candidate.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_DISTRIBUTED_ANALYSIS;
typedef struct DSL_DISTRIBUTED_ANALYSIS DSL_DISTRIBUTED_ANALYSIS;

typedef struct {
    UINT32 generate_candidates;
    UINT32 derive_communication;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 device_count;
    UINT32 max_sites;
    UINT32 max_alternatives_per_site;
    UINT32 enable_replication;
    UINT32 enable_axis_sharding;
    UINT32 enable_partial_reduction;
    UINT32 enable_migration;
    UINT32 reserved;
} VHO_DSL_DISTRIBUTED_CONTROL;

extern void VHO_DSL_Distributed_Control_Init
                                (VHO_DSL_DISTRIBUTED_CONTROL *control);
extern DSL_DISTRIBUTED_ANALYSIS *VHO_DSL_Distributed_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const VHO_DSL_DISTRIBUTED_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Distributed_Destroy (DSL_DISTRIBUTED_ANALYSIS *analysis);
extern BOOL VHO_DSL_Distributed_Build
                                (DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Distributed_Verify
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void VHO_DSL_Distributed_Print
                                (FILE *file,
                                 const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern const DSL_DISTRIBUTED_PLAN_IR *VHO_DSL_Distributed_Get_IR
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Distributed_Get_Plan_Context
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_SITE_ID id);

#endif
