/*
 * Copyright (C) 2026 Open64 Project
 *
 * Program-scope CKKS PU specialization requests. The FHE pass chooses
 * executable signatures and approved bounds; this interface validates their
 * existing WHIRL identities before any clone or call rewrite is attempted.
 * See doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md.
 */

#ifndef dsl_pu_specialize_INCLUDED
#define dsl_pu_specialize_INCLUDED

#include <stdio.h>

#include "dsl_ir_image.h"
#include "fhe_plan.h"
#include "pu_info.h"

typedef struct {
    ST_IDX source_pu_st;
    const char *clone_name;
    const char *signature_sha256;
} DSL_PU_SPECIALIZATION_VARIANT;

typedef struct {
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 variant_index;
} DSL_PU_SPECIALIZATION_ROUTE;

typedef struct {
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 bound_slot;
    DSL_IR_VALUE_ID source_relu_value_id;
    DSL_FHE_CONTEXT_RANGE_ID context_range_id;
    TCON_IDX positive_bound_tcon;
} DSL_PU_SPECIALIZATION_BOUND;

typedef struct {
    const DSL_PU_SPECIALIZATION_VARIANT *variants;
    UINT32 variant_count;
    const DSL_PU_SPECIALIZATION_ROUTE *routes;
    UINT32 route_count;
    const DSL_PU_SPECIALIZATION_BOUND *bounds;
    UINT32 bound_count;
} DSL_PU_SPECIALIZATION_PLAN;

extern BOOL VHO_DSL_PU_Specialization_Plan_Validate
                (PU_Info *program, const DSL_PU_SPECIALIZATION_PLAN *plan,
                 FILE *diagnostic);

#endif /* dsl_pu_specialize_INCLUDED */
