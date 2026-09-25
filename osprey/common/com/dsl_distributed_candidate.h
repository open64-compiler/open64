/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-7 placement, sharding, ownership, and derived communication plans.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md.
 */

#ifndef dsl_distributed_candidate_INCLUDED
#define dsl_distributed_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct dsl_distributed_analysis;

typedef struct dsl_distributed_analysis DSL_DISTRIBUTED_ANALYSIS;
typedef UINT32 DSL_DISTRIBUTED_DESCRIPTOR_ID;
typedef UINT32 DSL_DISTRIBUTED_RANGE_ID;
typedef UINT32 DSL_DISTRIBUTED_ALIAS_ID;
typedef UINT32 DSL_DISTRIBUTED_SITE_ID;
typedef UINT32 DSL_DISTRIBUTED_ALTERNATIVE_ID;
typedef UINT32 DSL_COMMUNICATION_EPOCH_ID;
typedef UINT32 DSL_COMMUNICATION_INTENT_ID;

#define DSL_DISTRIBUTED_INVALID_ID ((UINT32)0)
#define DSL_DISTRIBUTED_NO_AXIS (~(UINT32)0)
#define DSL_DISTRIBUTED_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_PLACEMENT_UNKNOWN = 0,
    DSL_PLACEMENT_REPLICATED = 1,
    DSL_PLACEMENT_PARTITIONED = 2,
    DSL_PLACEMENT_REMOTE_SINGLE = 3
} DSL_PLACEMENT_KIND;

typedef enum {
    DSL_SHARDING_UNKNOWN = 0,
    DSL_SHARDING_REPLICATED = 1,
    DSL_SHARDING_AXIS = 2,
    DSL_SHARDING_PARTIAL_REDUCTION = 3,
    DSL_SHARDING_MIGRATED = 4
} DSL_SHARDING_KIND;

typedef enum {
    DSL_DISTRIBUTED_OWNERSHIP_UNKNOWN = 0,
    DSL_DISTRIBUTED_OWNERSHIP_DISJOINT = 1,
    DSL_DISTRIBUTED_OWNERSHIP_REPLICATED = 2,
    DSL_DISTRIBUTED_OWNERSHIP_REDUCED = 3,
    DSL_DISTRIBUTED_OWNERSHIP_MIGRATED = 4
} DSL_DISTRIBUTED_OWNERSHIP;

typedef enum {
    DSL_DISTRIBUTED_RANGE_UNKNOWN = 0,
    DSL_DISTRIBUTED_RANGE_EXACT = 1
} DSL_DISTRIBUTED_RANGE_STATE;

typedef enum {
    DSL_DISTRIBUTED_DISJOINT_UNKNOWN = 0,
    DSL_DISTRIBUTED_DISJOINT_PROVEN = 1,
    DSL_DISTRIBUTED_DISJOINT_OVERLAP = 2
} DSL_DISTRIBUTED_DISJOINT_STATE;

typedef enum {
    DSL_COMMUNICATION_UNKNOWN = 0,
    DSL_COMMUNICATION_ALL_GATHER = 1,
    DSL_COMMUNICATION_SCATTER = 2,
    DSL_COMMUNICATION_ALL_REDUCE = 3,
    DSL_COMMUNICATION_REDUCE_SCATTER = 4,
    DSL_COMMUNICATION_ALL_TO_ALL = 5,
    DSL_COMMUNICATION_PEER_COPY = 6
} DSL_COMMUNICATION_KIND;

enum {
    DSL_DISTRIBUTED_FLAG_NONE = 0,
    DSL_DISTRIBUTED_FLAG_PROVISIONAL = 0x00000001,
    DSL_DISTRIBUTED_FLAG_SEMANTICS_PRESERVING = 0x00000002
};

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
} DSL_DISTRIBUTED_CONTROL;

typedef struct {
    DSL_DISTRIBUTED_DESCRIPTOR_ID id;
    TY_IDX source_descriptor_ty;
    UINT32 placement_kind;
    UINT32 sharding_kind;
    UINT32 ownership;
    UINT32 device_count;
    UINT32 shard_axis;
    DSL_DISTRIBUTED_ALIAS_ID alias_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_DISTRIBUTED_DESCRIPTOR_RECORD;

typedef struct {
    DSL_DISTRIBUTED_ALIAS_ID id;
    DSL_DISTRIBUTED_DESCRIPTOR_ID descriptor_id;
    DSL_DISTRIBUTED_RANGE_ID first_range_id;
    UINT32 range_count;
    UINT32 ownership;
    UINT32 disjoint_state;
    UINT32 visibility_epoch;
    UINT32 reserved;
} DSL_DISTRIBUTED_ALIAS_RECORD;

typedef struct {
    DSL_DISTRIBUTED_RANGE_ID id;
    DSL_DISTRIBUTED_ALIAS_ID alias_id;
    UINT32 device_ordinal;
    UINT32 axis;
    UINT64 lower;
    UINT64 upper;
    UINT32 state;
    UINT32 reserved;
} DSL_DISTRIBUTED_RANGE_RECORD;

typedef struct {
    DSL_DISTRIBUTED_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_DISTRIBUTED_ALTERNATIVE_ID first_alternative_id;
    UINT32 alternative_count;
    DSL_COMMUNICATION_EPOCH_ID epoch_id;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_DISTRIBUTED_SITE_RECORD;

typedef struct {
    DSL_DISTRIBUTED_ALTERNATIVE_ID id;
    DSL_DISTRIBUTED_SITE_ID site_id;
    DSL_DISTRIBUTED_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    DSL_COMMUNICATION_INTENT_ID communication_intent_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 communication_bytes;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID plan_id;
    UINT32 reserved;
} DSL_DISTRIBUTED_ALTERNATIVE_RECORD;

typedef struct {
    DSL_COMMUNICATION_EPOCH_ID id;
    DSL_DISTRIBUTED_SITE_ID site_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_IR_NODE_ID producer_node_id;
    DSL_IR_NODE_ID first_consumer_node_id;
    DSL_IR_NODE_ID last_consumer_node_id;
    UINT32 consumer_count;
    UINT32 reserved;
} DSL_COMMUNICATION_EPOCH_RECORD;

typedef struct {
    DSL_COMMUNICATION_INTENT_ID id;
    DSL_DISTRIBUTED_ALTERNATIVE_ID alternative_id;
    DSL_COMMUNICATION_EPOCH_ID epoch_id;
    UINT32 kind;
    UINT32 source_count;
    UINT32 destination_count;
    UINT64 bytes;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_COMMUNICATION_INTENT_RECORD;

extern void DSL_Distributed_Control_Init
                                (DSL_DISTRIBUTED_CONTROL *control);
extern DSL_DISTRIBUTED_ANALYSIS *DSL_Distributed_Create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_DISTRIBUTED_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_Distributed_Destroy (DSL_DISTRIBUTED_ANALYSIS *analysis);
extern BOOL DSL_Distributed_Build
                                (DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_Distributed_Verify
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_Distributed_Print
                                (FILE *file,
                                 const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Distributed_Descriptor_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Distributed_Alias_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Distributed_Range_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Distributed_Site_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Distributed_Alternative_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Communication_Epoch_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern UINT32 DSL_Communication_Intent_Count
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis);
extern BOOL DSL_Distributed_Get_Descriptor
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_DESCRIPTOR_ID id,
                                 DSL_DISTRIBUTED_DESCRIPTOR_RECORD *record);
extern BOOL DSL_Distributed_Get_Alias
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_ALIAS_ID id,
                                 DSL_DISTRIBUTED_ALIAS_RECORD *record);
extern BOOL DSL_Distributed_Get_Range
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_RANGE_ID id,
                                 DSL_DISTRIBUTED_RANGE_RECORD *record);
extern BOOL DSL_Distributed_Get_Site
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_SITE_ID id,
                                 DSL_DISTRIBUTED_SITE_RECORD *record);
extern BOOL DSL_Distributed_Get_Alternative
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_ALTERNATIVE_ID id,
                                 DSL_DISTRIBUTED_ALTERNATIVE_RECORD *record);
extern BOOL DSL_Communication_Get_Epoch
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_COMMUNICATION_EPOCH_ID id,
                                 DSL_COMMUNICATION_EPOCH_RECORD *record);
extern BOOL DSL_Communication_Get_Intent
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_COMMUNICATION_INTENT_ID id,
                                 DSL_COMMUNICATION_INTENT_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_Distributed_Get_Plan_Context
                                (const DSL_DISTRIBUTED_ANALYSIS *analysis,
                                 DSL_DISTRIBUTED_SITE_ID id);
extern const char *DSL_Placement_Kind_Name (UINT32 kind);
extern const char *DSL_Sharding_Kind_Name (UINT32 kind);
extern const char *DSL_Distributed_Ownership_Name (UINT32 kind);
extern const char *DSL_Distributed_Range_State_Name (UINT32 state);
extern const char *DSL_Distributed_Disjoint_State_Name (UINT32 state);
extern const char *DSL_Communication_Kind_Name (UINT32 kind);

#endif /* dsl_distributed_candidate_INCLUDED */
