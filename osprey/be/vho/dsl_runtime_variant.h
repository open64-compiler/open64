/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO owns transient runtime-variant, guard, and site records plus copied
 * construction services. These PU-local plans do not evaluate guards, rewrite
 * WHIRL, or add .B rows; shared plan identities remain in dsl_plan_identity.h.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md.
 */

#ifndef dsl_runtime_variant_INCLUDED
#define dsl_runtime_variant_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_plan_identity.h"
#include "dsl_ir_image.h"
#include "dsl_opt_plan.h"
#include "symtab_idx.h"

struct DSL_RUNTIME_VARIANT_IR;

typedef struct DSL_RUNTIME_VARIANT_IR DSL_RUNTIME_VARIANT_IR;
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
    DSL_RUNTIME_VARIANT_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    UINT32 physical_site_id;
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
    UINT32 physical_implementation_id;
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

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_RUNTIME_VARIANT_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_RUNTIME_VARIANT_RECORD *variants;
    UINT32 variant_count;
    const DSL_RUNTIME_GUARD_RECORD *guards;
    UINT32 guard_count;
} DSL_RUNTIME_VARIANT_IR_CREATE_INFO;

/* Copy counted site/variant/guard arrays for a nonzero owner into caller-owned
 * storage. Return NULL after invalid input or structural failure, freeing the
 * rejected copy; WHIRL and inputs stay unchanged. Allocation exceptions propagate. */
extern DSL_RUNTIME_VARIANT_IR *DSL_runtime_variant_ir_create
                                (const DSL_RUNTIME_VARIANT_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release owned plan storage and copied rows; NULL is allowed. No WHIRL
 * or provider state is modified. */
extern void DSL_runtime_variant_ir_destroy (DSL_RUNTIME_VARIANT_IR *ir);
/* Read-only structural check of IDs, owner, contiguous ranges, fallback bounds,
 * selection flags, guard fields and exact costs. Return FALSE with optional
 * diagnostics; semantic guard legality, cycles and runtime evaluation are phase-owned. */
extern BOOL DSL_runtime_variant_ir_verify
                                (const DSL_RUNTIME_VARIANT_IR *ir,
                                 FILE *diagnostic);
/* Print a structurally valid borrowed plan to a caller-owned stream without
 * mutation. NULL inputs are no-ops; stream errors are not reported here. */
extern void DSL_runtime_variant_ir_print
                                (FILE *file,
                                 const DSL_RUNTIME_VARIANT_IR *ir);
/* Read the copied owner without mutation; NULL returns ST_IDX_ZERO. */
extern ST_IDX DSL_runtime_variant_ir_owner
                                (const DSL_RUNTIME_VARIANT_IR *ir);
/* Read the copied site count without mutation; NULL returns zero. */
extern UINT32 DSL_runtime_variant_ir_site_count
                                (const DSL_RUNTIME_VARIANT_IR *ir);
/* Read the copied variant count without mutation; NULL returns zero. */
extern UINT32 DSL_runtime_variant_ir_variant_count
                                (const DSL_RUNTIME_VARIANT_IR *ir);
/* Read the copied guard count without mutation; NULL returns zero. */
extern UINT32 DSL_runtime_variant_ir_guard_count
                                (const DSL_RUNTIME_VARIANT_IR *ir);
/* Copy a one-based site to caller storage; invalid plan, output, or ID returns
 * FALSE without writing output or changing the plan. */
extern BOOL DSL_runtime_variant_ir_get_site
                                (const DSL_RUNTIME_VARIANT_IR *ir,
                                 DSL_RUNTIME_VARIANT_SITE_ID id,
                                 DSL_RUNTIME_VARIANT_SITE_RECORD *record);
/* Copy a one-based variant to caller storage; invalid plan, output, or ID
 * returns FALSE without writing output or changing the plan. */
extern BOOL DSL_runtime_variant_ir_get_variant
                                (const DSL_RUNTIME_VARIANT_IR *ir,
                                 DSL_RUNTIME_VARIANT_ID id,
                                 DSL_RUNTIME_VARIANT_RECORD *record);
/* Copy a one-based guard to caller storage; invalid plan, output, or ID returns
 * FALSE without writing output or changing the plan. */
extern BOOL DSL_runtime_variant_ir_get_guard
                                (const DSL_RUNTIME_VARIANT_IR *ir,
                                 DSL_RUNTIME_GUARD_ID id,
                                 DSL_RUNTIME_GUARD_RECORD *record);
/* Borrow a static guard-kind spelling; out-of-range values return "unknown". */
extern const char *DSL_runtime_guard_kind_name (UINT32 kind);
/* Borrow a static comparison spelling; out-of-range values return "unknown". */
extern const char *DSL_runtime_guard_comparison_name (UINT32 comparison);

#endif /* dsl_runtime_variant_INCLUDED */
