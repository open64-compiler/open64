/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-8 PU-local, check-only memory residency alternatives. The contract does
 * not allocate target memory or rewrite executable WHIRL.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
 */

#ifndef dsl_residency_candidate_INCLUDED
#define dsl_residency_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_RESIDENCY_ANALYSIS;

typedef struct DSL_RESIDENCY_ANALYSIS DSL_RESIDENCY_ANALYSIS;
typedef UINT32 DSL_RESIDENCY_DESCRIPTOR_ID;
typedef UINT32 DSL_RESIDENCY_SITE_ID;
typedef UINT32 DSL_RESIDENCY_ALTERNATIVE_ID;

#define DSL_RESIDENCY_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_RESIDENCY_PROMOTION_UNKNOWN = 0,
    DSL_RESIDENCY_PROMOTION_NONE = 1,
    DSL_RESIDENCY_PROMOTION_ON_DEMAND = 2,
    DSL_RESIDENCY_PROMOTION_PREFETCH = 3
} DSL_RESIDENCY_PROMOTION_POLICY;

typedef enum {
    DSL_RESIDENCY_DEMOTION_UNKNOWN = 0,
    DSL_RESIDENCY_DEMOTION_NONE = 1,
    DSL_RESIDENCY_DEMOTION_LAST_USE = 2,
    DSL_RESIDENCY_DEMOTION_PRESSURE = 3
} DSL_RESIDENCY_DEMOTION_POLICY;

typedef enum {
    DSL_RESIDENCY_SPILL_UNKNOWN = 0,
    DSL_RESIDENCY_SPILL_NONE = 1,
    DSL_RESIDENCY_SPILL_LOWER_TIER = 2,
    DSL_RESIDENCY_SPILL_SYSTEM = 3
} DSL_RESIDENCY_SPILL_POLICY;

typedef enum {
    DSL_RESIDENCY_EVICTION_UNKNOWN = 0,
    DSL_RESIDENCY_EVICTION_NONE = 1,
    DSL_RESIDENCY_EVICTION_LAST_USE = 2,
    DSL_RESIDENCY_EVICTION_PRESSURE = 3
} DSL_RESIDENCY_EVICTION_POLICY;

enum {
    DSL_RESIDENCY_FLAG_NONE = 0,
    DSL_RESIDENCY_FLAG_PROVISIONAL = 0x00000001,
    DSL_RESIDENCY_FLAG_SEMANTICS_PRESERVING = 0x00000002,
    DSL_RESIDENCY_FLAG_CAPACITY_CHECKED = 0x00000004,
    DSL_RESIDENCY_FLAG_LIFETIME_CHECKED = 0x00000008
};

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
} DSL_RESIDENCY_CONTROL;

typedef struct {
    DSL_RESIDENCY_DESCRIPTOR_ID id;
    TY_IDX source_descriptor_ty;
    UINT32 target_profile_id;
    DSL_MEMORY_TIER_ID tier_id;
    UINT32 tier_kind;
    UINT32 tier_scope;
    UINT64 required_bytes;
    UINT64 capacity_bytes;
    UINT64 allocation_granularity;
    UINT32 minimum_alignment;
    UINT32 promotion_policy;
    UINT32 demotion_policy;
    UINT32 spill_policy;
    UINT32 eviction_policy;
    UINT32 flags;
    UINT32 reserved;
} DSL_RESIDENCY_DESCRIPTOR_RECORD;

typedef struct {
    DSL_RESIDENCY_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_RESIDENCY_ALTERNATIVE_ID first_alternative_id;
    UINT32 alternative_count;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_RESIDENCY_SITE_RECORD;

typedef struct {
    DSL_RESIDENCY_ALTERNATIVE_ID id;
    DSL_RESIDENCY_SITE_ID site_id;
    DSL_RESIDENCY_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID plan_id;
    UINT32 reserved;
} DSL_RESIDENCY_ALTERNATIVE_RECORD;

extern void DSL_residency_control_init (DSL_RESIDENCY_CONTROL *control);
extern DSL_RESIDENCY_ANALYSIS *DSL_residency_create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_RESIDENCY_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_residency_destroy (DSL_RESIDENCY_ANALYSIS *analysis);
extern BOOL DSL_residency_build
                                (DSL_RESIDENCY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_residency_verify
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_residency_print
                                (FILE *file,
                                 const DSL_RESIDENCY_ANALYSIS *analysis);
extern UINT32 DSL_residency_descriptor_count
                                (const DSL_RESIDENCY_ANALYSIS *analysis);
extern UINT32 DSL_residency_site_count
                                (const DSL_RESIDENCY_ANALYSIS *analysis);
extern UINT32 DSL_residency_alternative_count
                                (const DSL_RESIDENCY_ANALYSIS *analysis);
extern BOOL DSL_residency_get_descriptor
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 DSL_RESIDENCY_DESCRIPTOR_ID id,
                                 DSL_RESIDENCY_DESCRIPTOR_RECORD *record);
extern BOOL DSL_residency_get_site
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 DSL_RESIDENCY_SITE_ID id,
                                 DSL_RESIDENCY_SITE_RECORD *record);
extern BOOL DSL_residency_get_alternative
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 DSL_RESIDENCY_ALTERNATIVE_ID id,
                                 DSL_RESIDENCY_ALTERNATIVE_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_residency_get_plan_context
                                (const DSL_RESIDENCY_ANALYSIS *analysis,
                                 DSL_RESIDENCY_SITE_ID id);
extern const char *DSL_residency_promotion_name (UINT32 policy);
extern const char *DSL_residency_demotion_name (UINT32 policy);
extern const char *DSL_residency_spill_name (UINT32 policy);
extern const char *DSL_residency_eviction_name (UINT32 policy);

#endif /* dsl_residency_candidate_INCLUDED */
