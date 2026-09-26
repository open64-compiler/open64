/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-12 PU-local certified runtime variants and guards. The first slice
 * selects between a reviewed provider implementation and its unconditional
 * direct fallback using operand-alignment observations. It is runtime-only
 * and does not rewrite or extend binary WHIRL. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md.
 */

#ifndef dsl_runtime_variant_INCLUDED
#define dsl_runtime_variant_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_physical_plan.h"

struct pu_info;
struct DSL_RUNTIME_VARIANT_ANALYSIS;

typedef struct DSL_RUNTIME_VARIANT_ANALYSIS DSL_RUNTIME_VARIANT_ANALYSIS;
typedef UINT32 DSL_RUNTIME_VARIANT_SITE_ID;
typedef UINT32 DSL_RUNTIME_VARIANT_ID;
typedef UINT32 DSL_RUNTIME_GUARD_ID;

#define DSL_RUNTIME_VARIANT_INVALID_ID ((UINT32)0)
#define DSL_RUNTIME_GUARD_INVALID_ID ((UINT32)0)
#define DSL_RUNTIME_GUARD_NO_DIMENSION (~(UINT32)0)

typedef enum {
    DSL_RUNTIME_GUARD_UNKNOWN = 0,
    DSL_RUNTIME_GUARD_OPERAND_ALIGNMENT = 1,
    DSL_RUNTIME_GUARD_SHAPE_DIMENSION_EQUAL = 2,
    DSL_RUNTIME_GUARD_SHAPE_DIMENSION_MULTIPLE = 3
} DSL_RUNTIME_GUARD_KIND;

typedef enum {
    DSL_RUNTIME_GUARD_COMPARE_UNKNOWN = 0,
    DSL_RUNTIME_GUARD_COMPARE_AT_LEAST = 1,
    DSL_RUNTIME_GUARD_COMPARE_EQUAL = 2,
    DSL_RUNTIME_GUARD_COMPARE_MULTIPLE_OF = 3
} DSL_RUNTIME_GUARD_COMPARISON;

typedef enum {
    DSL_RUNTIME_GUARD_FAILURE_UNKNOWN = 0,
    DSL_RUNTIME_GUARD_FAILURE_FALLBACK = 1
} DSL_RUNTIME_GUARD_FAILURE_ACTION;

typedef enum {
    DSL_RUNTIME_SELECTION_UNKNOWN = 0,
    DSL_RUNTIME_SELECTION_FIRST_MATCH = 1
} DSL_RUNTIME_SELECTION_POLICY;

enum {
    DSL_RUNTIME_VARIANT_FLAG_NONE = 0,
    DSL_RUNTIME_VARIANT_FLAG_BASELINE = 0x00000001,
    DSL_RUNTIME_VARIANT_FLAG_GUARDED = 0x00000002,
    DSL_RUNTIME_VARIANT_FLAG_CERTIFIED = 0x00000004,
    DSL_RUNTIME_VARIANT_FLAG_SELECTED = 0x00000008
};

enum {
    DSL_RUNTIME_GUARD_FLAG_NONE = 0,
    DSL_RUNTIME_GUARD_FLAG_REQUIRED = 0x00000001
};

typedef struct {
    UINT32 generate_variants;
    UINT32 select_policy;
    UINT32 apply_selected_variant;
    UINT32 optimization_level;
    UINT32 target_profile_id;
    DSL_IR_VALUE_ID focus_value_id;
    UINT32 required_operand_alignment;
    UINT32 guard_evaluation_cost;
    UINT32 max_sites;
    UINT32 max_variants_per_site;
    UINT32 max_guards_per_variant;
    UINT32 reserved;
} DSL_RUNTIME_VARIANT_CONTROL;

typedef struct {
    DSL_RUNTIME_VARIANT_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_PHYSICAL_SITE_ID physical_site_id;
    DSL_RUNTIME_VARIANT_ID first_variant_id;
    UINT32 variant_count;
    DSL_RUNTIME_VARIANT_ID baseline_variant_id;
    DSL_RUNTIME_VARIANT_ID selected_variant_id;
    UINT32 selection_policy;
    UINT32 reserved;
} DSL_RUNTIME_VARIANT_SITE_RECORD;

typedef struct {
    DSL_RUNTIME_VARIANT_ID id;
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_PHYSICAL_IMPLEMENTATION_ID physical_implementation_id;
    DSL_RUNTIME_GUARD_ID first_guard_id;
    UINT32 guard_count;
    DSL_RUNTIME_VARIANT_ID fallback_variant_id;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT64 physical_cost;
    UINT64 guard_cost;
    UINT64 total_cost;
    UINT64 identity;
    UINT32 flags;
    UINT32 reserved;
} DSL_RUNTIME_VARIANT_RECORD;

typedef struct {
    DSL_RUNTIME_GUARD_ID id;
    DSL_RUNTIME_VARIANT_ID variant_id;
    UINT32 kind;
    UINT32 comparison;
    UINT32 operand_ordinal;
    UINT32 dimension_ordinal;
    UINT64 required_value;
    UINT64 evaluation_cost;
    UINT32 failure_action;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_RUNTIME_GUARD_RECORD;

/* Borrowed runtime facts used only for one guard evaluation call. */
typedef struct {
    UINT32 operand_ordinal;
    UINT32 rank;
    const INT64 *dimensions;
    UINT32 observed_alignment;
    UINT32 reserved;
} DSL_RUNTIME_GUARD_OBSERVATION;

typedef struct {
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_RUNTIME_VARIANT_ID selected_variant_id;
    DSL_PHYSICAL_IMPLEMENTATION_ID selected_implementation_id;
    UINT32 evaluated_guard_count;
    UINT64 evaluation_cost;
    UINT32 guard_passed;
    UINT32 fallback_taken;
} DSL_RUNTIME_VARIANT_EVALUATION_RESULT;

extern void DSL_runtime_variant_control_init
                                (DSL_RUNTIME_VARIANT_CONTROL *control);
extern DSL_RUNTIME_VARIANT_ANALYSIS *DSL_runtime_variant_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_PHYSICAL_PLAN_ANALYSIS *physical,
                                 const DSL_RUNTIME_VARIANT_CONTROL *control,
                                 FILE *diagnostic);
extern void DSL_runtime_variant_destroy
                                (DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern BOOL DSL_runtime_variant_build
                                (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_runtime_variant_verify
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_runtime_variant_evaluate
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID site_id,
                                 const DSL_RUNTIME_GUARD_OBSERVATION
                                     *observations,
                                 UINT32 observation_count,
                                 DSL_RUNTIME_VARIANT_EVALUATION_RESULT *result,
                                 FILE *diagnostic);
extern void DSL_runtime_variant_print
                                (FILE *file,
                                 const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern void DSL_runtime_variant_print_evaluation
                                (FILE *file, const char *label,
                                 const DSL_RUNTIME_VARIANT_EVALUATION_RESULT
                                     *result);
extern UINT32 DSL_runtime_variant_site_count
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern UINT32 DSL_runtime_variant_count
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern UINT32 DSL_runtime_guard_count
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern BOOL DSL_runtime_variant_get_site
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID id,
                                 DSL_RUNTIME_VARIANT_SITE_RECORD *record);
extern BOOL DSL_runtime_variant_get_variant
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_ID id,
                                 DSL_RUNTIME_VARIANT_RECORD *record);
extern BOOL DSL_runtime_variant_get_guard
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_GUARD_ID id,
                                 DSL_RUNTIME_GUARD_RECORD *record);
extern const DSL_OPT_PLAN_CONTEXT *DSL_runtime_variant_get_plan_context
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID site_id);
extern const char *DSL_runtime_guard_kind_name (UINT32 kind);
extern const char *DSL_runtime_guard_comparison_name (UINT32 comparison);

#endif /* dsl_runtime_variant_INCLUDED */
