/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-4 PU-local lifetime, reuse, locality, and copied control-flow evidence.
 * The common contract never retains WOPT-owned objects or changes WHIRL.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#ifndef dsl_tensor_locality_INCLUDED
#define dsl_tensor_locality_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_tensor_analysis.h"

struct pu_info;
struct DSL_TENSOR_CONTROL_SNAPSHOT;
struct DSL_TENSOR_LOCALITY_ANALYSIS;

typedef struct DSL_TENSOR_CONTROL_SNAPSHOT DSL_TENSOR_CONTROL_SNAPSHOT;
typedef struct DSL_TENSOR_LOCALITY_ANALYSIS DSL_TENSOR_LOCALITY_ANALYSIS;
typedef UINT32 DSL_TENSOR_LOCALITY_FACT_ID;
typedef UINT32 DSL_TENSOR_LOCALITY_USE_ID;

#define DSL_TENSOR_LOCALITY_INVALID_ID ((UINT32)0)
#define DSL_TENSOR_LOCALITY_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_TENSOR_CONTROL_NONE = 0,
    DSL_TENSOR_CONTROL_BRANCH = 0x0001,
    DSL_TENSOR_CONTROL_LOOP = 0x0002,
    DSL_TENSOR_CONTROL_REGION = 0x0004,
    DSL_TENSOR_CONTROL_EFFECT_BARRIER = 0x0008
} DSL_TENSOR_CONTROL_FLAG;

typedef enum {
    DSL_TENSOR_SIZE_UNKNOWN = 0,
    DSL_TENSOR_SIZE_STATIC = 1,
    DSL_TENSOR_SIZE_SYMBOLIC = 2,
    DSL_TENSOR_SIZE_OVERFLOW = 3
} DSL_TENSOR_SIZE_STATE;

typedef enum {
    DSL_TENSOR_LIFETIME_UNKNOWN = 0,
    DSL_TENSOR_LIFETIME_EXACT_BLOCK = 1,
    DSL_TENSOR_LIFETIME_DOMINATED = 2,
    DSL_TENSOR_LIFETIME_BRANCH = 3,
    DSL_TENSOR_LIFETIME_LOOP = 4,
    DSL_TENSOR_LIFETIME_REGION = 5,
    DSL_TENSOR_LIFETIME_EFFECT = 6,
    DSL_TENSOR_LIFETIME_ALIAS = 7
} DSL_TENSOR_LIFETIME_STATE;

typedef enum {
    DSL_TENSOR_DISTANCE_UNKNOWN = 0,
    DSL_TENSOR_DISTANCE_EXACT = 1,
    DSL_TENSOR_DISTANCE_CONSERVATIVE = 2
} DSL_TENSOR_DISTANCE_STATE;

typedef enum {
    DSL_TENSOR_ACCESS_UNKNOWN = 0,
    DSL_TENSOR_ACCESS_ELEMENTWISE = 1,
    DSL_TENSOR_ACCESS_CONTRACTION = 2,
    DSL_TENSOR_ACCESS_REDUCTION = 3,
    DSL_TENSOR_ACCESS_VIEW = 4,
    DSL_TENSOR_ACCESS_INDEXED = 5,
    DSL_TENSOR_ACCESS_MIXED = 6
} DSL_TENSOR_ACCESS_PATTERN;

typedef enum {
    DSL_TENSOR_RESIDENCY_UNKNOWN = 0,
    DSL_TENSOR_RESIDENCY_NONE = 1,
    DSL_TENSOR_RESIDENCY_LOW = 2,
    DSL_TENSOR_RESIDENCY_MEDIUM = 3,
    DSL_TENSOR_RESIDENCY_HIGH = 4
} DSL_TENSOR_RESIDENCY_BENEFIT;

typedef enum {
    DSL_TENSOR_CRITICAL_PATH_UNKNOWN = 0,
    DSL_TENSOR_CRITICAL_PATH_OFF = 1,
    DSL_TENSOR_CRITICAL_PATH_ON = 2
} DSL_TENSOR_CRITICAL_PATH_STATE;

typedef enum {
    DSL_TENSOR_ALIAS_UNKNOWN = 0,
    DSL_TENSOR_ALIAS_CONSERVATIVE = 1,
    DSL_TENSOR_ALIAS_PROVEN_UNIQUE = 2
} DSL_TENSOR_ALIAS_STATE;

typedef struct {
    UINT32 block_id;
    UINT32 reverse_postorder;
    UINT32 immediate_dominator;
    UINT32 immediate_postdominator;
    UINT32 loop_depth;
    UINT32 region_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_TENSOR_CONTROL_BLOCK;

typedef struct {
    DSL_IR_NODE_ID node_id;
    UINT32 block_id;
    UINT32 statement_order;
    UINT32 reverse_postorder;
    UINT32 reserved;
} DSL_TENSOR_CONTROL_POSITION;

typedef struct {
    DSL_TENSOR_LOCALITY_FACT_ID id;
    DSL_TENSOR_FACT_ID tensor_fact_id;
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    DSL_IR_NODE_ID last_consumer_node_id;
    DSL_TENSOR_LOCALITY_USE_ID first_use_id;
    UINT32 use_count;
    UINT32 size_state;
    UINT32 lifetime_state;
    UINT32 reuse_distance_state;
    UINT32 access_pattern;
    UINT32 residency_benefit;
    UINT32 critical_path_state;
    UINT32 alias_state;
    UINT32 producer_path_position;
    UINT32 last_use_path_position;
    UINT64 object_bytes;
    UINT64 reuse_distance_statements;
    UINT64 working_set_bytes;
    UINT64 estimated_read_bytes;
    UINT64 estimated_write_bytes;
    UINT32 flags;
    UINT32 reserved;
} DSL_TENSOR_LOCALITY_FACT_RECORD;

typedef struct {
    DSL_TENSOR_LOCALITY_USE_ID id;
    DSL_TENSOR_LOCALITY_FACT_ID locality_fact_id;
    DSL_TENSOR_USE_FACT_ID tensor_use_fact_id;
    DSL_IR_NODE_ID consumer_node_id;
    UINT32 block_id;
    UINT32 statement_order;
    UINT32 path_position;
    UINT32 loop_depth;
    UINT32 region_id;
    UINT32 control_flags;
    UINT32 reserved;
} DSL_TENSOR_LOCALITY_USE_RECORD;

extern DSL_TENSOR_CONTROL_SNAPSHOT *DSL_tensor_control_snapshot_create
                                (struct pu_info *pu,
                                 FILE *diagnostic);
extern void DSL_tensor_control_snapshot_destroy
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot);
extern BOOL DSL_tensor_control_snapshot_add_block
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 const DSL_TENSOR_CONTROL_BLOCK *block,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_control_snapshot_add_position
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 const DSL_TENSOR_CONTROL_POSITION *position,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_control_snapshot_seal
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_control_snapshot_verify
                                (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
extern void DSL_tensor_control_snapshot_print
                                (FILE *file,
                                 const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot);

extern DSL_TENSOR_LOCALITY_ANALYSIS *DSL_tensor_locality_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
extern void DSL_tensor_locality_destroy
                                (DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
extern BOOL DSL_tensor_locality_build
                                (DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_locality_verify
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_tensor_locality_print
                                (FILE *file,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
extern UINT32 DSL_tensor_locality_fact_count
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
extern UINT32 DSL_tensor_locality_use_count
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis);
extern BOOL DSL_tensor_locality_get_fact
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_TENSOR_LOCALITY_FACT_ID id,
                                 DSL_TENSOR_LOCALITY_FACT_RECORD *record);
extern BOOL DSL_tensor_locality_get_use
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_TENSOR_LOCALITY_USE_ID id,
                                 DSL_TENSOR_LOCALITY_USE_RECORD *record);
extern BOOL DSL_tensor_locality_find_fact
                                (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_TENSOR_LOCALITY_FACT_RECORD *record);

extern const char *DSL_tensor_size_state_name (UINT32 state);
extern const char *DSL_tensor_lifetime_state_name (UINT32 state);
extern const char *DSL_tensor_distance_state_name (UINT32 state);
extern const char *DSL_tensor_access_pattern_name (UINT32 pattern);
extern const char *DSL_tensor_residency_benefit_name (UINT32 benefit);
extern const char *DSL_tensor_critical_path_state_name (UINT32 state);
extern const char *DSL_tensor_alias_state_name (UINT32 state);

#endif /* dsl_tensor_locality_INCLUDED */
