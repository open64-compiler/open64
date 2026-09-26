/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-10 PU-local, check-only fetch and async-pipeline planning. The analysis
 * consumes AIO-9 tile plans and records runtime-only movement alternatives;
 * it does not emit prefetch WNs, barriers, target instructions, or binary IR.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
 */

#ifndef dsl_fetch_pipeline_INCLUDED
#define dsl_fetch_pipeline_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_distributed_candidate.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_locality.h"
#include "dsl_tile_candidate.h"

struct pu_info;
struct DSL_FETCH_PIPELINE_ANALYSIS;

typedef struct DSL_FETCH_PIPELINE_ANALYSIS DSL_FETCH_PIPELINE_ANALYSIS;
typedef UINT32 DSL_FETCH_SITE_ID;
typedef UINT32 DSL_FETCH_PLAN_ID;
typedef UINT32 DSL_FETCH_RECORD_ID;
typedef UINT32 DSL_PIPELINE_STAGE_ID;

#define DSL_FETCH_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_FETCH_ISSUE_UNKNOWN = 0,
    DSL_FETCH_ISSUE_CONSUMER = 1,
    DSL_FETCH_ISSUE_PREVIOUS_K_TILE = 2,
    DSL_FETCH_ISSUE_PROLOGUE = 3
} DSL_FETCH_ISSUE_POINT;

typedef enum {
    DSL_FETCH_BARRIER_NONE = 0,
    DSL_FETCH_BARRIER_CTA = 1,
    DSL_FETCH_BARRIER_ARRIVAL = 2
} DSL_FETCH_BARRIER_KIND;

typedef enum {
    DSL_FETCH_WAIT_NONE = 0,
    DSL_FETCH_WAIT_BEFORE_CONSUMER = 1,
    DSL_FETCH_WAIT_PIPELINE_STAGE = 2
} DSL_FETCH_WAIT_POINT;

enum {
    DSL_FETCH_PLAN_FLAG_NONE = 0,
    DSL_FETCH_PLAN_FLAG_BASELINE = 0x00000001,
    DSL_FETCH_PLAN_FLAG_PROVISIONAL = 0x00000002,
    DSL_FETCH_PLAN_FLAG_ASYNC = 0x00000004,
    DSL_FETCH_PLAN_FLAG_SEMANTICS_PRESERVING = 0x00000008
};

enum {
    DSL_FETCH_RECORD_FLAG_NONE = 0,
    DSL_FETCH_RECORD_FLAG_STAGED = 0x00000001,
    DSL_FETCH_RECORD_FLAG_ASYNC = 0x00000002,
    DSL_FETCH_RECORD_FLAG_REQUIRES_BARRIER = 0x00000004
};

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
} DSL_FETCH_PIPELINE_CONTROL;

typedef struct {
    DSL_FETCH_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TILE_SITE_ID tile_site_id;
    DSL_TILE_PLAN_ID selected_tile_plan_id;
    DSL_FETCH_PLAN_ID first_pipeline_plan_id;
    UINT32 pipeline_plan_count;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_FETCH_SITE_RECORD;

typedef struct {
    DSL_FETCH_PLAN_ID id;
    DSL_FETCH_SITE_ID site_id;
    DSL_TILE_PLAN_ID tile_plan_id;
    UINT32 target_profile_id;
    UINT32 engine;
    DSL_FETCH_RECORD_ID first_fetch_id;
    UINT32 fetch_count;
    DSL_PIPELINE_STAGE_ID first_stage_id;
    UINT32 stage_count;
    UINT32 prefetch_distance;
    UINT32 issue_point;
    UINT32 arrival_barrier;
    UINT32 wait_point;
    UINT32 edge_policy;
    UINT64 buffering_bytes;
    UINT64 buffering_capacity_bytes;
    UINT64 raw_movement_cost;
    UINT64 hidden_movement_cost;
    UINT64 unhidden_movement_cost;
    UINT32 barrier_count;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_FETCH_PLAN_RECORD;

typedef struct {
    DSL_FETCH_RECORD_ID id;
    DSL_FETCH_PLAN_ID pipeline_plan_id;
    UINT32 operand_ordinal;
    DSL_IR_VALUE_ID value_id;
    TY_IDX descriptor_ty;
    UINT32 source_tier_kind;
    UINT32 destination_tier_kind;
    UINT32 engine;
    UINT32 transaction_bytes;
    UINT32 minimum_alignment;
    UINT64 bytes_per_stage;
    UINT64 raw_movement_cost;
    UINT64 hidden_movement_cost;
    UINT64 unhidden_movement_cost;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT32 flags;
    UINT32 reserved;
} DSL_FETCH_RECORD;

typedef struct {
    DSL_PIPELINE_STAGE_ID id;
    DSL_FETCH_PLAN_ID pipeline_plan_id;
    UINT32 ordinal;
    UINT32 buffer_slot;
    UINT32 prefetch_distance;
    UINT32 issue_point;
    UINT32 arrival_barrier;
    UINT32 wait_point;
    UINT32 reserved;
} DSL_PIPELINE_STAGE_RECORD;

extern void DSL_fetch_pipeline_control_init
                                (DSL_FETCH_PIPELINE_CONTROL *control);
extern DSL_FETCH_PIPELINE_ANALYSIS *DSL_fetch_pipeline_create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_DISTRIBUTED_ANALYSIS *distributed,
                                 const DSL_TILE_ANALYSIS *tile,
                                 const DSL_FETCH_PIPELINE_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_fetch_pipeline_destroy
                                (DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern BOOL DSL_fetch_pipeline_build
                                (DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_fetch_pipeline_verify
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_fetch_pipeline_print
                                (FILE *file,
                                 const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern UINT32 DSL_fetch_pipeline_site_count
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern UINT32 DSL_fetch_pipeline_plan_count
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern UINT32 DSL_fetch_pipeline_fetch_count
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern UINT32 DSL_fetch_pipeline_stage_count
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis);
extern BOOL DSL_fetch_pipeline_get_site
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_FETCH_SITE_ID id,
                                 DSL_FETCH_SITE_RECORD *record);
extern BOOL DSL_fetch_pipeline_get_plan
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_FETCH_PLAN_ID id,
                                 DSL_FETCH_PLAN_RECORD *record);
extern BOOL DSL_fetch_pipeline_get_fetch
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_FETCH_RECORD_ID id,
                                 DSL_FETCH_RECORD *record);
extern BOOL DSL_fetch_pipeline_get_stage
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_PIPELINE_STAGE_ID id,
                                 DSL_PIPELINE_STAGE_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_fetch_pipeline_get_plan_context
                                (const DSL_FETCH_PIPELINE_ANALYSIS *analysis,
                                 DSL_FETCH_SITE_ID id);
extern const char *DSL_fetch_issue_point_name (UINT32 point);
extern const char *DSL_fetch_barrier_name (UINT32 barrier);
extern const char *DSL_fetch_wait_point_name (UINT32 point);

#endif /* dsl_fetch_pipeline_INCLUDED */
