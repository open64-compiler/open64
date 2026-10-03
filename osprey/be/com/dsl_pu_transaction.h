/*
 * Copyright (C) 2026 Open64 Project
 *
 * Program-scope DSL PU cloning support.  Policy selects signatures and
 * routes; this service owns physical and managed-image construction.
 * See doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md.
 */

#ifndef dsl_pu_transaction_INCLUDED
#define dsl_pu_transaction_INCLUDED

#include <stdio.h>

#include "dsl_pu_specialize_internal.h"
#include "pu_info.h"
#include "srcpos.h"

typedef struct {
    const char *name;
    const char *semantic_role;
    TY_IDX ty;
    SRCPOS source_position;
} DSL_PU_TRANSACTION_FORMAL_REQUEST;

typedef struct {
    UINT32 callee_formal_ordinal;
    TY_IDX ty;
    TCON_IDX source_tcon;
    const char *semantic_role;
} DSL_PU_TRANSACTION_ACTUAL_REQUEST;

typedef struct {
    ST_IDX source_pu_st;
    const char *clone_name;
    const unsigned char *signature_bytes;
    UINT32 signature_size;
    /* Producer-supplied diagnostic fingerprint. Generic equality uses the
     * complete canonical bytes, not this unauthenticated digest. */
    const char *signature_sha256;
    BOOL use_existing_pu;
    const DSL_PU_TRANSACTION_FORMAL_REQUEST *formals;
    UINT32 formal_count;
} DSL_PU_TRANSACTION_VARIANT_REQUEST;

typedef struct {
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 variant_index;
    const DSL_PU_TRANSACTION_ACTUAL_REQUEST *actuals;
    UINT32 actual_count;
} DSL_PU_TRANSACTION_ROUTE_REQUEST;

typedef struct {
    const DSL_PU_TRANSACTION_VARIANT_REQUEST *variants;
    UINT32 variant_count;
    const DSL_PU_TRANSACTION_ROUTE_REQUEST *routes;
    UINT32 route_count;
} DSL_PU_TRANSACTION_PLAN;

struct DSL_PU_TRANSACTION_RESULT;

typedef struct {
    BOOL (*build_plan)(PU_Info *program,
                       DSL_PU_TRANSACTION_PLAN *plan,
                       void **policy_state, FILE *diagnostic);
    BOOL (*after_apply)(PU_Info *program,
                        const DSL_PU_TRANSACTION_RESULT *result,
                        void *policy_state, FILE *diagnostic);
    void (*release)(void *policy_state);
} DSL_PU_TRANSACTION_POLICY;

extern BOOL DSL_PU_Transaction_Register_Policy
                (const DSL_PU_TRANSACTION_POLICY *policy);
extern BOOL DSL_PU_Transaction_Get_Policy
                (DSL_PU_TRANSACTION_POLICY *policy);

/* All affected PU trees/local tables/map tables must be resident.  This
 * read-only image preflight switches the active PU scope but allocates no
 * WHIRL nodes, symbols, or mapped rows. */
extern BOOL DSL_PU_Transaction_Preflight_Resident
                (PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
                 FILE *diagnostic);

/* The plan is preflighted before the first mapped-image allocation.  Once
 * applying starts, any failure is terminal for the checkpoint process. */
extern BOOL DSL_PU_Transaction_Apply_Resident
                (PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
                 DSL_PU_TRANSACTION_RESULT **result, FILE *diagnostic);
extern ST_IDX DSL_PU_Transaction_Variant_PU_ST
                (const DSL_PU_TRANSACTION_RESULT *result,
                 UINT32 variant_index);
extern BOOL DSL_PU_Transaction_Cloned_Value
                (const DSL_PU_TRANSACTION_RESULT *result,
                 UINT32 variant_index, DSL_IR_VALUE_ID source_value_id,
                 DSL_IR_VALUE_ID *clone_value_id);
extern void DSL_PU_Transaction_Result_Delete
                (DSL_PU_TRANSACTION_RESULT *result);

/*
 * Source local state must already be resident and selected.  The caller
 * retains its pool until all PUs have been written.  A failure after the
 * first allocation is terminal for this checkpoint process, not retryable.
 */
extern BOOL DSL_PU_Transaction_Clone_Active
                (PU_Info *source, const char *clone_name,
                 DSL_PU_CLONE_VALUE_PAIR *value_map,
                 UINT32 value_map_capacity, UINT32 *value_map_count,
                 PU_Info **cloned, FILE *diagnostic);

/* Insert exact scalar inputs before any hidden result formals.  The caller
 * must select the cloned PU and may not retry after a commit-stage failure. */
extern BOOL DSL_PU_Transaction_Insert_Formals_Active
                (PU_Info *pu,
                 const DSL_PU_TRANSACTION_FORMAL_REQUEST *requests,
                 UINT32 request_count, ST_IDX *formal_sts,
                 DSL_IR_VALUE_ID *formal_values, FILE *diagnostic);

/* Route one managed call to a variant and insert exact scalar TCON actuals.
 * All callsites in a program plan must be preflighted before the first call. */
extern BOOL DSL_PU_Transaction_Route_Call_Active
                (PU_Info *caller, DSL_CALLSITE_METADATA_ID callsite_id,
                 ST_IDX variant_pu_st,
                 const DSL_PU_TRANSACTION_ACTUAL_REQUEST *requests,
                 UINT32 request_count, FILE *diagnostic);

/* Materialize an entry-owned scalar TCON before the exact source result's
 * physical definition.  There is no synthetic callsite or formal. */
extern BOOL DSL_PU_Transaction_Root_Constant_Active
                (PU_Info *owner, DSL_IR_VALUE_ID anchor_value_id,
                 const char *name, TY_IDX ty, TCON_IDX source_tcon,
                 ST_IDX *created_st, DSL_IR_VALUE_ID *created_value,
                 FILE *diagnostic);

/* Reopen-safe proof for a uniquely initialized, non-address-taken scalar
 * value.  The result is the exact TCON referenced by physical WHIRL. */
extern BOOL DSL_PU_Transaction_Scalar_TCON_Active
                (PU_Info *owner, DSL_IR_VALUE_ID value_id,
                 TCON_IDX *source_tcon, FILE *diagnostic);

#endif /* dsl_pu_transaction_INCLUDED */
