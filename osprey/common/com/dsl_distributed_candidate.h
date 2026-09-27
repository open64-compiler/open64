/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Policy-free AIO-7 CommonDistributedPlanIR records and structural services.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO7-DISTRIBUTED.md.
 */

#ifndef dsl_distributed_candidate_INCLUDED
#define dsl_distributed_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_evolution.h"

struct DSL_DISTRIBUTED_PLAN_IR;
typedef struct DSL_DISTRIBUTED_PLAN_IR DSL_DISTRIBUTED_PLAN_IR;
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

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_DISTRIBUTED_DESCRIPTOR_RECORD *descriptors;
    UINT32 descriptor_count;
    const DSL_DISTRIBUTED_ALIAS_RECORD *aliases;
    UINT32 alias_count;
    const DSL_DISTRIBUTED_RANGE_RECORD *ranges;
    UINT32 range_count;
    const DSL_DISTRIBUTED_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_DISTRIBUTED_ALTERNATIVE_RECORD *alternatives;
    UINT32 alternative_count;
    const DSL_COMMUNICATION_EPOCH_RECORD *epochs;
    UINT32 epoch_count;
    const DSL_COMMUNICATION_INTENT_RECORD *intents;
    UINT32 intent_count;
} DSL_DISTRIBUTED_PLAN_IR_CREATE_INFO;

extern DSL_DISTRIBUTED_PLAN_IR *DSL_distributed_plan_ir_create
                                (const DSL_DISTRIBUTED_PLAN_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
extern void DSL_distributed_plan_ir_destroy (DSL_DISTRIBUTED_PLAN_IR *ir);
extern BOOL DSL_distributed_plan_ir_verify
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 FILE *diagnostic);
extern void DSL_distributed_plan_ir_print
                                (FILE *file,
                                 const DSL_DISTRIBUTED_PLAN_IR *ir);
extern ST_IDX DSL_distributed_plan_ir_owner
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_descriptor_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_alias_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_range_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_site_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_alternative_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_epoch_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern UINT32 DSL_distributed_plan_ir_intent_count
                                (const DSL_DISTRIBUTED_PLAN_IR *ir);
extern BOOL DSL_distributed_plan_ir_get_descriptor
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_DISTRIBUTED_DESCRIPTOR_ID id,
                                 DSL_DISTRIBUTED_DESCRIPTOR_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_alias
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_DISTRIBUTED_ALIAS_ID id,
                                 DSL_DISTRIBUTED_ALIAS_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_range
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_DISTRIBUTED_RANGE_ID id,
                                 DSL_DISTRIBUTED_RANGE_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_site
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_DISTRIBUTED_SITE_ID id,
                                 DSL_DISTRIBUTED_SITE_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_alternative
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_DISTRIBUTED_ALTERNATIVE_ID id,
                                 DSL_DISTRIBUTED_ALTERNATIVE_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_epoch
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_COMMUNICATION_EPOCH_ID id,
                                 DSL_COMMUNICATION_EPOCH_RECORD *record);
extern BOOL DSL_distributed_plan_ir_get_intent
                                (const DSL_DISTRIBUTED_PLAN_IR *ir,
                                 DSL_COMMUNICATION_INTENT_ID id,
                                 DSL_COMMUNICATION_INTENT_RECORD *record);
extern const char *DSL_placement_kind_name (UINT32 kind);
extern const char *DSL_sharding_kind_name (UINT32 kind);
extern const char *DSL_distributed_ownership_name (UINT32 kind);
extern const char *DSL_distributed_range_state_name (UINT32 state);
extern const char *DSL_distributed_disjoint_state_name (UINT32 state);
extern const char *DSL_communication_kind_name (UINT32 kind);

#endif /* dsl_distributed_candidate_INCLUDED */
