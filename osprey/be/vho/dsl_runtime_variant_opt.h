/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO ownership for AIO runtime-variant fact capture, costing, selection, and
 * guard evaluation. Common/com owns only RuntimeVariantIR records and their
 * construction. See
 * doc/AI-COMPILER-OPTIMIZATION-AIO12-RUNTIME-VARIANT.md.
 */

#ifndef dsl_runtime_variant_opt_INCLUDED
#define dsl_runtime_variant_opt_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_physical_plan.h"
#include "dsl_runtime_variant.h"

struct pu_info;
struct DSL_RUNTIME_VARIANT_ANALYSIS;

typedef struct DSL_RUNTIME_VARIANT_ANALYSIS DSL_RUNTIME_VARIANT_ANALYSIS;

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
} VHO_DSL_RUNTIME_VARIANT_CONTROL;

/* Borrowed runtime facts used only for one guard evaluation call. */
typedef struct {
    UINT32 operand_ordinal;
    UINT32 rank;
    const INT64 *dimensions;
    UINT32 observed_alignment;
    UINT32 reserved;
} VHO_DSL_RUNTIME_GUARD_OBSERVATION;

typedef struct {
    DSL_RUNTIME_VARIANT_SITE_ID site_id;
    DSL_RUNTIME_VARIANT_ID selected_variant_id;
    UINT32 selected_implementation_id;
    UINT32 evaluated_guard_count;
    UINT64 evaluation_cost;
    UINT32 guard_passed;
    UINT32 fallback_taken;
} VHO_DSL_RUNTIME_VARIANT_EVALUATION_RESULT;

extern void VHO_DSL_Runtime_Variant_Control_Init
                                (VHO_DSL_RUNTIME_VARIANT_CONTROL *control);
extern DSL_RUNTIME_VARIANT_ANALYSIS *VHO_DSL_Runtime_Variant_Create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 const DSL_PHYSICAL_PLAN_ANALYSIS *physical,
                                 const VHO_DSL_RUNTIME_VARIANT_CONTROL *control,
                                 FILE *diagnostic);
extern void VHO_DSL_Runtime_Variant_Destroy
                                (DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern BOOL VHO_DSL_Runtime_Variant_Build
                                (DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Runtime_Variant_Verify
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Runtime_Variant_Evaluate
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID site_id,
                                 const VHO_DSL_RUNTIME_GUARD_OBSERVATION
                                     *observations,
                                 UINT32 observation_count,
                                 VHO_DSL_RUNTIME_VARIANT_EVALUATION_RESULT
                                     *result,
                                 FILE *diagnostic);
extern void VHO_DSL_Runtime_Variant_Print
                                (FILE *file,
                                 const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern void VHO_DSL_Runtime_Variant_Print_Evaluation
                                (FILE *file, const char *label,
                                 const VHO_DSL_RUNTIME_VARIANT_EVALUATION_RESULT
                                     *result);
extern const DSL_RUNTIME_VARIANT_IR *VHO_DSL_Runtime_Variant_Get_IR
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis);
extern const DSL_OPT_PLAN_CONTEXT *VHO_DSL_Runtime_Variant_Get_Plan_Context
                                (const DSL_RUNTIME_VARIANT_ANALYSIS *analysis,
                                 DSL_RUNTIME_VARIANT_SITE_ID site_id);

#endif /* dsl_runtime_variant_opt_INCLUDED */
