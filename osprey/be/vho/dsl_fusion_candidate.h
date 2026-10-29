/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned CommonFusionPlanIR runtime record and copied-store contract for
 * caller-scoped PU planning. Discovery, semantic legality, costing and selection
 * remain in the companion *_opt phase implementation. These records are not
 * mapped-image rows; relocation changes no .B layout, reader/writer or public IDs.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_fusion_candidate_INCLUDED
#define dsl_fusion_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
struct DSL_FUSION_PLAN_IR;
typedef struct DSL_FUSION_PLAN_IR DSL_FUSION_PLAN_IR;
typedef UINT32 DSL_FUSION_SITE_ID;
typedef UINT32 DSL_FUSION_MEMBER_ID;
typedef UINT32 DSL_FUSION_BOUNDARY_ID;

#define DSL_FUSION_INVALID_ID ((UINT32)0)
#define DSL_FUSION_UNKNOWN_U64 (~(UINT64)0)
#define DSL_FUSION_BOUNDARY_NO_OPERAND (~(UINT32)0)

typedef enum {
    DSL_FUSION_PATTERN_UNKNOWN = 0,
    DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION = 1,
    DSL_FUSION_PATTERN_RESIDUAL_ACTIVATION = 2,
    DSL_FUSION_PATTERN_GENERIC_CLUSTER = 3
} DSL_FUSION_PATTERN;

typedef enum {
    DSL_FUSION_MEMBER_UNKNOWN = 0,
    DSL_FUSION_MEMBER_MATMUL = 1,
    DSL_FUSION_MEMBER_BIAS_ADD = 2,
    DSL_FUSION_MEMBER_RESIDUAL_ADD = 3,
    DSL_FUSION_MEMBER_ACTIVATION = 4,
    DSL_FUSION_MEMBER_GENERIC_CONTRACTION = 5,
    DSL_FUSION_MEMBER_GENERIC_POINTWISE = 6
} DSL_FUSION_MEMBER_ROLE;

typedef enum {
    DSL_FUSION_BOUNDARY_UNKNOWN = 0,
    DSL_FUSION_BOUNDARY_INPUT = 1,
    DSL_FUSION_BOUNDARY_OUTPUT = 2,
    DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT = 3
} DSL_FUSION_BOUNDARY_KIND;

typedef enum {
    DSL_FUSION_FACT_UNKNOWN = 0,
    DSL_FUSION_FACT_PROVEN = 1,
    DSL_FUSION_FACT_REJECTED = 2
} DSL_FUSION_FACT_STATE;

enum {
    DSL_FUSION_REPRESENTATION_NONE = 0,
    DSL_FUSION_REPRESENTATION_EXACT_INTERMEDIATE = 0x00000001,
    DSL_FUSION_REPRESENTATION_BROADCAST_BIAS = 0x00000002,
    DSL_FUSION_REPRESENTATION_PRESERVE_REGION = 0x00000004
};

typedef struct {
    DSL_FUSION_SITE_ID id;
    ST_IDX owner_pu_st;
    UINT32 pattern;
    DSL_IR_NODE_ID root_node_id;
    DSL_IR_VALUE_ID result_value_id;
    DSL_FUSION_MEMBER_ID first_member_id;
    UINT32 member_count;
    DSL_FUSION_BOUNDARY_ID first_boundary_id;
    UINT32 boundary_count;
    UINT32 eliminated_materialization_count;
    UINT32 representation_constraints;
    UINT32 semantic_state;
    UINT32 descriptor_state;
    UINT32 effect_state;
    UINT32 resource_state;
    UINT32 layout_state;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 eliminated_materialization_bytes;
    UINT64 live_range_growth_bytes;
    UINT64 live_range_growth_statements;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_CANDIDATE_ID fusion_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID fusion_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_FUSION_SITE_RECORD;

typedef struct {
    DSL_FUSION_MEMBER_ID id;
    DSL_FUSION_SITE_ID site_id;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID result_value_id;
    UINT32 role;
    UINT32 ordinal;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_FUSION_MEMBER_RECORD;

typedef struct {
    DSL_FUSION_BOUNDARY_ID id;
    DSL_FUSION_SITE_ID site_id;
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    DSL_IR_NODE_ID consumer_node_id;
    UINT32 kind;
    UINT32 operand_ordinal;
    UINT32 reserved;
} DSL_FUSION_BOUNDARY_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_FUSION_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_FUSION_MEMBER_RECORD *members;
    UINT32 member_count;
    const DSL_FUSION_BOUNDARY_RECORD *boundaries;
    UINT32 boundary_count;
} DSL_FUSION_PLAN_IR_CREATE_INFO;

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
extern DSL_FUSION_PLAN_IR *DSL_fusion_plan_ir_create
                                (const DSL_FUSION_PLAN_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
extern void DSL_fusion_plan_ir_destroy (DSL_FUSION_PLAN_IR *ir);
/* Check the copied owner, dense site/member/boundary ranges, enum bounds,
 * required IDs and selected-plan membership without mutating the store. Reject
 * the first structural defect with an optional diagnostic; recorded facts are
 * not semantic, effect, resource or fusion-legality proofs. */
extern BOOL DSL_fusion_plan_ir_verify
                                (const DSL_FUSION_PLAN_IR *ir,
                                 FILE *diagnostic);
/* Print the stored fusion counts and site summaries to the borrowed stream;
 * null stream or store is a no-op. Do not verify facts, select a plan, mutate
 * WHIRL or publish mapped-image data. */
extern void DSL_fusion_plan_ir_print
                                (FILE *file, const DSL_FUSION_PLAN_IR *ir);
/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
extern ST_IDX DSL_fusion_plan_ir_owner (const DSL_FUSION_PLAN_IR *ir);
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fusion_plan_ir_site_count (const DSL_FUSION_PLAN_IR *ir);
/* Read the copied member table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fusion_plan_ir_member_count (const DSL_FUSION_PLAN_IR *ir);
/* Read the copied boundary table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fusion_plan_ir_boundary_count
                                (const DSL_FUSION_PLAN_IR *ir);
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fusion_plan_ir_get_site
                                (const DSL_FUSION_PLAN_IR *ir,
                                 DSL_FUSION_SITE_ID id,
                                 DSL_FUSION_SITE_RECORD *record);
/* Copy the member row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fusion_plan_ir_get_member
                                (const DSL_FUSION_PLAN_IR *ir,
                                 DSL_FUSION_MEMBER_ID id,
                                 DSL_FUSION_MEMBER_RECORD *record);
/* Copy the boundary row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fusion_plan_ir_get_boundary
                                (const DSL_FUSION_PLAN_IR *ir,
                                 DSL_FUSION_BOUNDARY_ID id,
                                 DSL_FUSION_BOUNDARY_RECORD *record);
/* Borrow a static fusion pattern spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fusion_pattern_name (UINT32 pattern);
/* Borrow a static member role spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fusion_member_role_name (UINT32 role);
/* Borrow a static boundary kind spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fusion_boundary_kind_name (UINT32 kind);
/* Borrow a static fact state spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fusion_fact_state_name (UINT32 state);

#endif /* dsl_fusion_candidate_INCLUDED */
