/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO-10 movement-plan generation, overlap estimates,
 * barrier/resource legality, costing, selection, and semantic verification.
 * Scope and joint contract: doc/AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md.
 * Direct extension: doc/AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md.
 * Resources: doc/AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md.
 * H1c fixed-pair binding: doc/AI-COMPILER-H1C-PLANNER-BINDINGS-v1.0.md.
 */

#ifndef dsl_fetch_pipeline_opt_INCLUDED
#define dsl_fetch_pipeline_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_distributed_candidate_opt.h"
#include "dsl_fetch_pipeline.h"
#include "dsl_tensor_locality.h"
#include "dsl_tile_candidate_opt.h"
#include "dsl_gemm_occupancy.h"

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
    /* Runtime-only controls: full-GEMM costing and deferred tile override. */
    UINT32 joint_cost;
    UINT32 readiness_cost;
    UINT32 memory_latency;
    UINT32 memory_latency_set;
    UINT32 hardware_arch_id;
    UINT32 enable_direct;
    DSL_TILE_PLAN_ID tile_plan_id;
    /* H4 bounded staged siblings; zero preserves the historical catalog. */
    UINT32 enable_b_retention;
    UINT32 reserved;
} VHO_DSL_FETCH_PIPELINE_CONTROL;

/* Adapt a complete PU-local tile/movement pair to the assumed analytical
 * profile. Both output pointers are required. No IR mutation; FALSE leaves
 * outputs unused and prevents selection. */
extern BOOL VHO_DSL_fetch_readiness_cost
    (const DSL_TILE_PLAN_RECORD &tile, const DSL_FETCH_PLAN_RECORD &fetch,
     const DSL_HARDWARE_DESCRIPTION_RECORD &hardware, DSL_GEMM_READINESS_INPUT *input,
     DSL_GEMM_READINESS_RESULT *result);

/* PU planner's resource eligibility gate, before dependent fetch records are
 * built. Zero estimated residency rejects only this sibling. Invalid facts
 * leave the caller's record untouched; no executable WHIRL mutation. */
extern BOOL VHO_DSL_fetch_classify_occupancy
    (const DSL_TILE_PLAN_RECORD &tile, const DSL_HARDWARE_DESCRIPTION_RECORD &hardware,
     DSL_FETCH_PLAN_RECORD *fetch);

/* Resolve common table data into a caller-owned PU-local description and
 * apply only explicit diagnostic overrides; never mutate built-in tables. */
extern BOOL VHO_DSL_fetch_hardware_description
    (const VHO_DSL_FETCH_PIPELINE_CONTROL &control,
     DSL_HARDWARE_DESCRIPTION_RECORD *hardware);

/* Match owned semantic fetch specifications across temporary analysis rebuilds.
 * IDs and padding are excluded; resources, schedule points and cost evidence
 * must match. Read-only, with no native context or selection authority. */
inline BOOL VHO_DSL_fetch_same_specification(const DSL_FETCH_PLAN_RECORD &a,
    const DSL_FETCH_PLAN_RECORD &b)
{
    const DSL_FETCH_RESOURCE_RECORD &x=a.resources,&y=b.resources;
    return a.target_profile_id==b.target_profile_id && a.engine==b.engine &&
        a.fetch_count==b.fetch_count && a.stage_count==b.stage_count && a.prefetch_distance==b.prefetch_distance &&
        a.issue_point==b.issue_point && a.arrival_barrier==b.arrival_barrier && a.wait_point==b.wait_point &&
        a.edge_policy==b.edge_policy && a.buffering_bytes==b.buffering_bytes &&
        a.buffering_capacity_bytes==b.buffering_capacity_bytes && a.raw_movement_cost==b.raw_movement_cost &&
        a.hidden_movement_cost==b.hidden_movement_cost && a.unhidden_movement_cost==b.unhidden_movement_cost &&
        a.barrier_count==b.barrier_count && a.legality==b.legality && a.rejection_reason==b.rejection_reason &&
        a.flags==b.flags && x.model==y.model && x.register_bytes_per_thread==y.register_bytes_per_thread &&
        x.register_capacity_per_thread==y.register_capacity_per_thread && x.threads_per_cta==y.threads_per_cta &&
        x.thread_limit==y.thread_limit && x.shared_bytes==y.shared_bytes && x.shared_limit_bytes==y.shared_limit_bytes;
}

/* Freeze a fixed joint winner in the rebuilt active-PU analysis using the
 * existing common selection service and complete census; never re-rank. No
 * WHIRL mutation. Caller discards this temporary analysis on any failure and
 * must reacquire IR/site views after successful rematerialization. */
extern BOOL VHO_DSL_fetch_finalize_joint_selection(DSL_FETCH_PIPELINE_ANALYSIS *,
    DSL_FETCH_SITE_ID, DSL_FETCH_PLAN_ID, FILE *);

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

/* PU-local executable joint-plan preflight shared by planning and lowering;
 * reject unsupported schedules without modifying WHIRL or plan records. */
extern BOOL VHO_DSL_fetch_pipeline_can_lower
                                (const DSL_TILE_PLAN_RECORD &tile,
                                 const DSL_FETCH_PLAN_RECORD &fetch);

/* Derive a per-movement value-storage/capacity ledger without changing the
 * supplied tile/fetch. FALSE denotes malformed geometry or arithmetic;
 * capacity excess is represented in the returned ledger, not hidden. */
extern BOOL VHO_DSL_fetch_resource_requirements
                                (const DSL_TILE_PLAN_RECORD &tile,
                                 const DSL_FETCH_PLAN_RECORD &fetch,
                                 DSL_FETCH_RESOURCE_RECORD *resources);

/* Prove signed-I4 index and padded loop bounds before either plan selection
 * or source/device construction; do not mutate the supplied runtime record. */
extern BOOL VHO_DSL_cuda_i4_geometry_safe (const DSL_TILE_PLAN_RECORD &tile);

/* Count whole-kernel warp-issued A/B sectors for the direct register map,
 * including tails and worst F32 base alignment; no cache-hit credit or IR mutation. */
extern BOOL VHO_DSL_direct_gmem_sectors
                                (const DSL_TILE_PLAN_RECORD &tile, UINT64 bytes[2]);

#endif /* dsl_fetch_pipeline_opt_INCLUDED */
