/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-5 PU-local, check-only semantic and generic fusion candidate discovery.
 * It exposes planning evidence and performs no executable WHIRL rewrite.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md.
 */

#ifndef dsl_fusion_candidate_INCLUDED
#define dsl_fusion_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_layout_candidate.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_FUSION_CANDIDATE_ANALYSIS;

typedef struct DSL_FUSION_CANDIDATE_ANALYSIS DSL_FUSION_CANDIDATE_ANALYSIS;
typedef UINT32 DSL_FUSION_SITE_ID;
typedef UINT32 DSL_FUSION_MEMBER_ID;
typedef UINT32 DSL_FUSION_BOUNDARY_ID;

#define DSL_FUSION_INVALID_ID ((UINT32)0)
#define DSL_FUSION_UNKNOWN_U64 (~(UINT64)0)
#define DSL_FUSION_BOUNDARY_NO_OPERAND (~(UINT32)0)

typedef enum {
    DSL_FUSION_PATTERN_UNKNOWN = 0,
    DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION = 1,
    DSL_FUSION_PATTERN_RESIDUAL_ACTIVATION = 2,
    DSL_FUSION_PATTERN_GENERIC_CLUSTER = 3
} DSL_FUSION_PATTERN;

typedef enum {
    DSL_FUSION_MEMBER_UNKNOWN = 0,
    DSL_FUSION_MEMBER_MATMUL = 1,
    DSL_FUSION_MEMBER_BIAS_ADD = 2,
    DSL_FUSION_MEMBER_RESIDUAL_ADD = 3,
    DSL_FUSION_MEMBER_ACTIVATION = 4,
    DSL_FUSION_MEMBER_GENERIC_CONTRACTION = 5,
    DSL_FUSION_MEMBER_GENERIC_POINTWISE = 6
} DSL_FUSION_MEMBER_ROLE;

typedef enum {
    DSL_FUSION_BOUNDARY_UNKNOWN = 0,
    DSL_FUSION_BOUNDARY_INPUT = 1,
    DSL_FUSION_BOUNDARY_OUTPUT = 2,
    DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT = 3
} DSL_FUSION_BOUNDARY_KIND;

typedef enum {
    DSL_FUSION_FACT_UNKNOWN = 0,
    DSL_FUSION_FACT_PROVEN = 1,
    DSL_FUSION_FACT_REJECTED = 2
} DSL_FUSION_FACT_STATE;

enum {
    DSL_FUSION_REPRESENTATION_NONE = 0,
    DSL_FUSION_REPRESENTATION_EXACT_INTERMEDIATE = 0x00000001,
    DSL_FUSION_REPRESENTATION_BROADCAST_BIAS = 0x00000002,
    DSL_FUSION_REPRESENTATION_PRESERVE_REGION = 0x00000004
};

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
} DSL_FUSION_CONTROL;

typedef struct {
    DSL_FUSION_SITE_ID id;
    ST_IDX owner_pu_st;
    UINT32 pattern;
    DSL_IR_NODE_ID root_node_id;
    DSL_IR_VALUE_ID result_value_id;
    DSL_FUSION_MEMBER_ID first_member_id;
    UINT32 member_count;
    DSL_FUSION_BOUNDARY_ID first_boundary_id;
    UINT32 boundary_count;
    UINT32 eliminated_materialization_count;
    UINT32 representation_constraints;
    UINT32 semantic_state;
    UINT32 descriptor_state;
    UINT32 effect_state;
    UINT32 resource_state;
    UINT32 layout_state;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 eliminated_materialization_bytes;
    UINT64 live_range_growth_bytes;
    UINT64 live_range_growth_statements;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_CANDIDATE_ID fusion_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID fusion_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_FUSION_SITE_RECORD;

typedef struct {
    DSL_FUSION_MEMBER_ID id;
    DSL_FUSION_SITE_ID site_id;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID result_value_id;
    UINT32 role;
    UINT32 ordinal;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_FUSION_MEMBER_RECORD;

typedef struct {
    DSL_FUSION_BOUNDARY_ID id;
    DSL_FUSION_SITE_ID site_id;
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    DSL_IR_NODE_ID consumer_node_id;
    UINT32 kind;
    UINT32 operand_ordinal;
    UINT32 reserved;
} DSL_FUSION_BOUNDARY_RECORD;

extern void DSL_fusion_control_init (DSL_FUSION_CONTROL *control);
extern DSL_FUSION_CANDIDATE_ANALYSIS *DSL_fusion_candidates_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_FUSION_CONTROL *control,
                                 FILE *diagnostic);
extern DSL_FUSION_CANDIDATE_ANALYSIS *DSL_fusion_candidates_create_with_layout
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_LOGICAL_LAYOUT_ANALYSIS *layout,
                                 const DSL_FUSION_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_fusion_candidates_destroy
                                (DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern BOOL DSL_fusion_candidates_build
                                (DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_fusion_candidates_verify
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_fusion_candidates_print
                                (FILE *file,
                                 const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern UINT32 DSL_fusion_candidates_site_count
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern UINT32 DSL_fusion_candidates_member_count
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern UINT32 DSL_fusion_candidates_boundary_count
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis);
extern BOOL DSL_fusion_candidates_get_site
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 DSL_FUSION_SITE_ID id,
                                 DSL_FUSION_SITE_RECORD *record);
extern BOOL DSL_fusion_candidates_get_member
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 DSL_FUSION_MEMBER_ID id,
                                 DSL_FUSION_MEMBER_RECORD *record);
extern BOOL DSL_fusion_candidates_get_boundary
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 DSL_FUSION_BOUNDARY_ID id,
                                 DSL_FUSION_BOUNDARY_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_fusion_candidates_get_plan_context
                                (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
                                 DSL_FUSION_SITE_ID id);
extern const char *DSL_fusion_pattern_name (UINT32 pattern);
extern const char *DSL_fusion_member_role_name (UINT32 role);
extern const char *DSL_fusion_boundary_kind_name (UINT32 kind);
extern const char *DSL_fusion_fact_state_name (UINT32 state);

#endif /* dsl_fusion_candidate_INCLUDED */
