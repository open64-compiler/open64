/*
 * Copyright (C) 2026 Open64 Project
 *
 * Owner-PU transaction for replacing one native DSL value with ordered
 * executable CKKS events. Caller owns plan/policy, active-PU locals and shared
 * image lifetime; these request/result structures are not persisted rows and
 * add no mapped-layout change. Reported commit rejection uses bounded journals,
 * not general allocation-exception recovery. See
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef dsl_ckks_expand_INCLUDED
#define dsl_ckks_expand_INCLUDED

#include <stdio.h>

#include "dsl_ckks_event.h"
#include "pu_info.h"
#include "srcpos.h"

class WN;

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

/* Preflight a complete borrowed request against the exact Current_PU_Info with
 * resident locals, coherent shared images/strings/types and live source WNs.
 * Check source identity/purity/use closure, ordered step groups, exact static
 * attribute names, contexts, existing/prior-step operands and restricted physical
 * dominance. No request/name reservation, file/IR writes or PU activation;
 * TRUE does not prove numerical/cryptographic correctness or reserve future
 * acceptance. FALSE may diagnose (NULL is silent); scratch allocations escape. */
extern BOOL DSL_IR_Can_Expand_Native_Value_To_CKKS_Events
                                (PU_Info *pu_info,
                                 const DSL_CKKS_EXPANSION_REQUEST *request,
                                 FILE *diagnostic);

/* Recheck complete preflight, save core/string/event/symbol/metadata watermarks,
 * build steps/events and redirect source WN/ABI/REGION/runtime/image uses.
 * Caller supplies step_count result slots and stable borrowed arrays/strings.
 * Success deletes the original source definition, retains PU-owned step WNs
 * and publishes every scalar result after image/event/ABI/REGION postchecks.
 * Entry/preflight/savepoint FALSE preserves result slots; later reported failure
 * reverses recorded edits, removes appends and zeroes results. Failed restoration
 * is a fatal compiler assertion, not a recoverable partial program. Allocation
 * exceptions are not caught; no general exception rollback, PU switch, payload
 * computation or canonical-TY mutation is promised. NULL diagnostic is silent
 * except failed-rollback diagnostics may fall back to stderr. */
extern BOOL DSL_IR_Expand_Native_Value_To_CKKS_Events
                                (PU_Info *pu_info,
                                 const DSL_CKKS_EXPANSION_REQUEST *request,
                                 FILE *diagnostic,
                                 DSL_CKKS_EXPANSION_STEP_RESULT *results);

#endif /* dsl_ckks_expand_INCLUDED */
