/*
 * Copyright (C) 2026 Open64 Project
 *
 * Owner-PU transaction for replacing one native DSL value with ordered
 * executable CKKS events. See
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef dsl_ckks_expand_INCLUDED
#define dsl_ckks_expand_INCLUDED

#include <stdio.h>

#include "dsl_ckks_event.h"
#include "pu_info.h"
#include "srcpos.h"

typedef enum {
    DSL_CKKS_EXPANSION_EXISTING_VALUE = 1,
    DSL_CKKS_EXPANSION_PRIOR_STEP = 2
} DSL_CKKS_EXPANSION_OPERAND_KIND;

typedef struct {
    DSL_CKKS_EXPANSION_OPERAND_KIND kind;
    DSL_IR_VALUE_ID value_id;
    UINT32 step_index;
} DSL_CKKS_EXPANSION_OPERAND;

typedef struct {
    const char *name;
    const char *value;
} DSL_CKKS_EXPANSION_ATTRIBUTE;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    const DSL_CKKS_EXPANSION_OPERAND *operands;
    UINT32 operand_count;
    const DSL_CKKS_EXPANSION_ATTRIBUTE *attributes;
    UINT32 attribute_count;
    const char *result_name;
    TY_IDX result_ty;
    SRCPOS source_position;
} DSL_CKKS_EXPANSION_STEP;

typedef struct {
    UINT32 source_static_ordinal;
    UINT32 origin_static_ordinal;
    UINT32 first_step;
    UINT32 step_count;
} DSL_CKKS_EXPANSION_GROUP;

typedef struct {
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    ST_IDX origin_owner_pu_st;
    DSL_IR_VALUE_ID origin_source_value_id;
} DSL_CKKS_EXPANSION_CONTEXT;

typedef struct {
    WN *source_definition;
    DSL_IR_VALUE_ID source_value_id;
    DSL_OPERATOR expected_source_operator;
    UINT16 expected_source_version;
    const DSL_CKKS_EXPANSION_GROUP *groups;
    UINT32 group_count;
    const DSL_CKKS_EXPANSION_STEP *steps;
    UINT32 step_count;
    const DSL_CKKS_EXPANSION_CONTEXT *contexts;
    UINT32 context_count;
    UINT32 final_step_index;
} DSL_CKKS_EXPANSION_REQUEST;

typedef struct {
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID value_id;
    ST_IDX result_st;
} DSL_CKKS_EXPANSION_STEP_RESULT;

/* Read-only preflight. It never reserves names or changes the active PU. */
extern BOOL DSL_IR_Can_Expand_Native_Value_To_CKKS_Events
                                (PU_Info *pu_info,
                                 const DSL_CKKS_EXPANSION_REQUEST *request,
                                 FILE *diagnostic);

/* An accepted request publishes all steps or restores the active PU. */
extern BOOL DSL_IR_Expand_Native_Value_To_CKKS_Events
                                (PU_Info *pu_info,
                                 const DSL_CKKS_EXPANSION_REQUEST *request,
                                 FILE *diagnostic,
                                 DSL_CKKS_EXPANSION_STEP_RESULT *results);

#endif /* dsl_ckks_expand_INCLUDED */
