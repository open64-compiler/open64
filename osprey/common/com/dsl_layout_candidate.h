/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-6 PU-local immutable logical-layout alternatives and compatibility
 * evidence. It preserves tensor type identity and adds no binary IR state.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md.
 */

#ifndef dsl_layout_candidate_INCLUDED
#define dsl_layout_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_analysis.h"
#include "dsl_tensor_locality.h"

struct pu_info;
struct DSL_LOGICAL_LAYOUT_ANALYSIS;

typedef struct DSL_LOGICAL_LAYOUT_ANALYSIS DSL_LOGICAL_LAYOUT_ANALYSIS;
typedef UINT32 DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_AXIS_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_BLOCK_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_SITE_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID;

#define DSL_LOGICAL_LAYOUT_INVALID_ID ((UINT32)0)
#define DSL_LOGICAL_LAYOUT_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_LOGICAL_LAYOUT_UNKNOWN = 0,
    DSL_LOGICAL_LAYOUT_PERMUTED = 1,
    DSL_LOGICAL_LAYOUT_BLOCKED = 2,
    DSL_LOGICAL_LAYOUT_PACKED_HEAD = 3,
    DSL_LOGICAL_LAYOUT_DOMAIN = 4
} DSL_LOGICAL_LAYOUT_KIND;

typedef enum {
    DSL_LAYOUT_COMPATIBILITY_UNKNOWN = 0,
    DSL_LAYOUT_COMPATIBILITY_PROVEN = 1,
    DSL_LAYOUT_COMPATIBILITY_REJECTED = 2
} DSL_LAYOUT_COMPATIBILITY_STATE;

typedef enum {
    DSL_LAYOUT_CONVERSION_UNKNOWN = 0,
    DSL_LAYOUT_CONVERSION_KNOWN = 1
} DSL_LAYOUT_CONVERSION_STATE;

enum {
    DSL_LOGICAL_LAYOUT_FLAG_NONE = 0,
    DSL_LOGICAL_LAYOUT_FLAG_SEMANTICS_PRESERVING = 0x00000001,
    DSL_LOGICAL_LAYOUT_FLAG_LOGICAL_ONLY = 0x00000002
};

typedef struct {
    UINT32 generate_candidates;
    UINT32 select_plans;
    UINT32 apply_transformation;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 max_sites;
    UINT32 max_alternatives_per_site;
    UINT32 default_block_size;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_CONTROL;

typedef struct {
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID id;
    TY_IDX source_descriptor_ty;
    UINT32 kind;
    UINT32 rank;
    DSL_LOGICAL_LAYOUT_AXIS_ID first_axis_id;
    UINT32 axis_count;
    DSL_LOGICAL_LAYOUT_BLOCK_ID first_block_id;
    UINT32 block_count;
    UINT32 minimum_alignment;
    UINT32 flags;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_AXIS_ID id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    UINT32 ordinal;
    UINT32 source_axis;
    UINT32 result_axis;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_AXIS_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_BLOCK_ID id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    UINT32 axis;
    UINT32 factor;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_LOGICAL_LAYOUT_BLOCK_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID first_alternative_id;
    UINT32 alternative_count;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_SITE_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID id;
    DSL_LOGICAL_LAYOUT_SITE_ID site_id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 compatibility_state;
    UINT32 conversion_state;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 conversion_bytes;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID plan_id;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD;

extern void DSL_logical_layout_control_init
                                (DSL_LOGICAL_LAYOUT_CONTROL *control);
extern DSL_LOGICAL_LAYOUT_ANALYSIS *DSL_logical_layout_create
                                (struct pu_info *pu,
                                 DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_TENSOR_ANALYSIS *tensor_analysis,
                                 const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
                                 const DSL_LOGICAL_LAYOUT_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_logical_layout_destroy
                                (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern BOOL DSL_logical_layout_build
                                (DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_logical_layout_verify
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern void DSL_logical_layout_print
                                (FILE *file,
                                 const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern UINT32 DSL_logical_layout_descriptor_count
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern UINT32 DSL_logical_layout_axis_count
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern UINT32 DSL_logical_layout_block_count
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern UINT32 DSL_logical_layout_site_count
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern UINT32 DSL_logical_layout_alternative_count
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis);
extern BOOL DSL_logical_layout_get_descriptor
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID id,
                                 DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD *record);
extern BOOL DSL_logical_layout_get_axis
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_AXIS_ID id,
                                 DSL_LOGICAL_LAYOUT_AXIS_RECORD *record);
extern BOOL DSL_logical_layout_get_block
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_BLOCK_ID id,
                                 DSL_LOGICAL_LAYOUT_BLOCK_RECORD *record);
extern BOOL DSL_logical_layout_get_site
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_SITE_ID id,
                                 DSL_LOGICAL_LAYOUT_SITE_RECORD *record);
extern BOOL DSL_logical_layout_find_site
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_IR_VALUE_ID semantic_value_id,
                                 DSL_LOGICAL_LAYOUT_SITE_RECORD *record);
extern BOOL DSL_logical_layout_get_alternative
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID id,
                                 DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_logical_layout_get_plan_context
                                (const DSL_LOGICAL_LAYOUT_ANALYSIS *analysis,
                                 DSL_LOGICAL_LAYOUT_SITE_ID id);
extern const char *DSL_logical_layout_kind_name (UINT32 kind);
extern const char *DSL_layout_compatibility_name (UINT32 state);
extern const char *DSL_layout_conversion_name (UINT32 state);

#endif /* dsl_layout_candidate_INCLUDED */
