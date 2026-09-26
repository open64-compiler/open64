/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-9 check-only hierarchical tile-plan construction.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO9-HIERARCHICAL-TILING.md.
 */

#ifndef dsl_tile_candidate_INCLUDED
#define dsl_tile_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"
#include "dsl_residency_candidate.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct dsl_tile_analysis;

typedef struct dsl_tile_analysis DSL_TILE_ANALYSIS;
typedef UINT32 DSL_TILE_SITE_ID;
typedef UINT32 DSL_TILE_PLAN_ID;
typedef UINT32 DSL_TILE_STAGE_ID;

#define DSL_TILE_INVALID_ID ((UINT32)0)
#define DSL_TILE_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_TILE_PHASE_P7_0_BASELINE = 0,
    DSL_TILE_PHASE_P7_1_OUTPUT = 1,
    DSL_TILE_PHASE_P7_2_COALESCING = 2,
    DSL_TILE_PHASE_P7_3_REDUCTION = 3,
    DSL_TILE_PHASE_P7_4_SHARED = 4,
    DSL_TILE_PHASE_P7_5_RESOURCE = 5,
    DSL_TILE_PHASE_P7_6_THREAD = 6,
    DSL_TILE_PHASE_P7_7_REGISTER_REUSE = 7,
    DSL_TILE_PHASE_P7_8_VECTOR = 8,
    DSL_TILE_PHASE_P7_9_FAMILY = 9,
    DSL_TILE_PHASE_P7_10_WARP = 10,
    DSL_TILE_PHASE_P7_11_INSTRUCTION = 11,
    DSL_TILE_PHASE_COUNT = 12
} DSL_TILE_PHASE;

typedef enum {
    DSL_TILE_LEVEL_UNKNOWN = 0,
    DSL_TILE_LEVEL_PROBLEM = 1,
    DSL_TILE_LEVEL_OUTPUT = 2,
    DSL_TILE_LEVEL_COALESCING = 3,
    DSL_TILE_LEVEL_REDUCTION = 4,
    DSL_TILE_LEVEL_SHARED = 5,
    DSL_TILE_LEVEL_RESOURCE = 6,
    DSL_TILE_LEVEL_THREAD = 7,
    DSL_TILE_LEVEL_REGISTER_REUSE = 8,
    DSL_TILE_LEVEL_VECTOR = 9,
    DSL_TILE_LEVEL_FAMILY = 10,
    DSL_TILE_LEVEL_WARP = 11,
    DSL_TILE_LEVEL_INSTRUCTION = 12
} DSL_TILE_LEVEL_KIND;

typedef enum {
    DSL_TILE_FAMILY_UNKNOWN = 0,
    DSL_TILE_FAMILY_BASELINE = 1,
    DSL_TILE_FAMILY_CUDA_64 = 2,
    DSL_TILE_FAMILY_CUDA_128 = 3,
    DSL_TILE_FAMILY_BLACKWELL_WIDE = 4
} DSL_TILE_FAMILY_KIND;

typedef enum {
    DSL_TILE_EDGE_UNKNOWN = 0,
    DSL_TILE_EDGE_EXACT = 1,
    DSL_TILE_EDGE_PREDICATED = 2
} DSL_TILE_EDGE_POLICY;

typedef enum {
    DSL_TILE_INSTRUCTION_UNKNOWN = 0,
    DSL_TILE_INSTRUCTION_SCALAR_FMA = 1,
    DSL_TILE_INSTRUCTION_VECTOR_FMA = 2
} DSL_TILE_INSTRUCTION_FAMILY;

enum {
    DSL_TILE_PLAN_FLAG_NONE = 0,
    DSL_TILE_PLAN_FLAG_BASELINE = 0x00000001,
    DSL_TILE_PLAN_FLAG_PROVISIONAL = 0x00000002,
    DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING = 0x00000004,
    DSL_TILE_PLAN_FLAG_PREFETCH_HINT = 0x00000008,
    DSL_TILE_PLAN_FLAG_TMA_CANDIDATE = 0x00000010
};

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
} DSL_TILE_CONTROL;

typedef struct {
    DSL_TILE_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TILE_PLAN_ID first_tile_plan_id;
    UINT32 tile_plan_count;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_TILE_SITE_RECORD;

typedef struct {
    DSL_TILE_PLAN_ID id;
    DSL_TILE_SITE_ID site_id;
    UINT32 target_profile_id;
    UINT32 family;
    TY_IDX source_descriptor_ty;
    DSL_TILE_STAGE_ID first_stage_id;
    UINT32 stage_count;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT64 problem_m;
    UINT64 problem_n;
    UINT64 problem_k;
    UINT32 cta_m;
    UINT32 cta_n;
    UINT32 cta_k;
    UINT32 warp_m;
    UINT32 warp_n;
    UINT32 thread_m;
    UINT32 thread_n;
    UINT32 instruction_m;
    UINT32 instruction_n;
    UINT32 instruction_k;
    UINT32 vector_width;
    UINT32 transaction_bytes;
    UINT64 gmem_read_bytes;
    UINT64 gmem_write_bytes;
    UINT64 operation_count;
    UINT64 shared_bytes_per_stage;
    UINT64 shared_capacity_bytes;
    UINT32 maximum_buffer_stages;
    UINT32 registers_per_thread;
    UINT32 register_capacity_per_thread;
    UINT32 threads_per_cta;
    UINT32 warps_per_cta;
    UINT32 barrier_count;
    UINT32 edge_policy;
    UINT32 instruction_family;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_TILE_PLAN_RECORD;

typedef struct {
    DSL_TILE_STAGE_ID id;
    DSL_TILE_PLAN_ID tile_plan_id;
    UINT32 ordinal;
    UINT32 phase;
    UINT32 level_kind;
    UINT64 extent_m;
    UINT64 extent_n;
    UINT64 extent_k;
    UINT32 flags;
    UINT32 reserved;
} DSL_TILE_STAGE_RECORD;

extern void DSL_Tile_Control_Init (DSL_TILE_CONTROL *control);
extern DSL_TILE_ANALYSIS *DSL_Tile_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_RESIDENCY_ANALYSIS *residency,
                                 const DSL_TILE_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_Tile_Destroy (DSL_TILE_ANALYSIS *analysis);
extern BOOL DSL_Tile_Build
                                (DSL_TILE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_Tile_Verify
                                (const DSL_TILE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_Tile_Print
                                (FILE *file,
                                 const DSL_TILE_ANALYSIS *analysis);
extern UINT32 DSL_Tile_Site_Count (const DSL_TILE_ANALYSIS *analysis);
extern UINT32 DSL_Tile_Plan_Count (const DSL_TILE_ANALYSIS *analysis);
extern UINT32 DSL_Tile_Stage_Count (const DSL_TILE_ANALYSIS *analysis);
extern BOOL DSL_Tile_Get_Site
                                (const DSL_TILE_ANALYSIS *analysis,
                                 DSL_TILE_SITE_ID id,
                                 DSL_TILE_SITE_RECORD *record);
extern BOOL DSL_Tile_Get_Plan
                                (const DSL_TILE_ANALYSIS *analysis,
                                 DSL_TILE_PLAN_ID id,
                                 DSL_TILE_PLAN_RECORD *record);
extern BOOL DSL_Tile_Get_Stage
                                (const DSL_TILE_ANALYSIS *analysis,
                                 DSL_TILE_STAGE_ID id,
                                 DSL_TILE_STAGE_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_Tile_Get_Plan_Context
                                (const DSL_TILE_ANALYSIS *analysis,
                                 DSL_TILE_SITE_ID id);
extern const char *DSL_Tile_Phase_Name (UINT32 phase);
extern const char *DSL_Tile_Level_Name (UINT32 level);
extern const char *DSL_Tile_Family_Name (UINT32 family);
extern const char *DSL_Tile_Edge_Policy_Name (UINT32 policy);
extern const char *DSL_Tile_Instruction_Name (UINT32 family);

#endif /* dsl_tile_candidate_INCLUDED */
