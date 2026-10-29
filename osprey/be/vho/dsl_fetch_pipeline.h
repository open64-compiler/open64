/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned CommonFetchPlanIR/CommonPipelineIR runtime record and copied-store contract for
 * caller-scoped PU planning. Discovery, semantic legality, costing and selection
 * remain in the companion *_opt phase implementation. These records are not
 * mapped-image rows; relocation changes no .B layout, reader/writer or public IDs.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_fetch_pipeline_INCLUDED
#define dsl_fetch_pipeline_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"
#include "dsl_tile_candidate.h"

struct DSL_FETCH_PIPELINE_IR;

typedef struct DSL_FETCH_PIPELINE_IR DSL_FETCH_PIPELINE_IR;
typedef UINT32 DSL_FETCH_SITE_ID;
typedef UINT32 DSL_FETCH_PLAN_ID;
typedef UINT32 DSL_FETCH_RECORD_ID;
typedef UINT32 DSL_PIPELINE_STAGE_ID;

#define DSL_FETCH_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_FETCH_ISSUE_UNKNOWN = 0,
    DSL_FETCH_ISSUE_CONSUMER = 1,
    DSL_FETCH_ISSUE_PREVIOUS_K_TILE = 2,
    DSL_FETCH_ISSUE_PROLOGUE = 3
} DSL_FETCH_ISSUE_POINT;

typedef enum {
    DSL_FETCH_BARRIER_NONE = 0,
    DSL_FETCH_BARRIER_CTA = 1,
    DSL_FETCH_BARRIER_ARRIVAL = 2
} DSL_FETCH_BARRIER_KIND;

typedef enum {
    DSL_FETCH_WAIT_NONE = 0,
    DSL_FETCH_WAIT_BEFORE_CONSUMER = 1,
    DSL_FETCH_WAIT_PIPELINE_STAGE = 2
} DSL_FETCH_WAIT_POINT;

enum {
    DSL_FETCH_PLAN_FLAG_NONE = 0,
    DSL_FETCH_PLAN_FLAG_BASELINE = 0x00000001,
    DSL_FETCH_PLAN_FLAG_PROVISIONAL = 0x00000002,
    DSL_FETCH_PLAN_FLAG_ASYNC = 0x00000004,
    DSL_FETCH_PLAN_FLAG_SEMANTICS_PRESERVING = 0x00000008,
    /* A logical demand stage with no physical software-managed buffer. */
    DSL_FETCH_PLAN_FLAG_DIRECT = 0x00000010
};

enum {
    DSL_FETCH_RECORD_FLAG_NONE = 0,
    DSL_FETCH_RECORD_FLAG_STAGED = 0x00000001,
    DSL_FETCH_RECORD_FLAG_ASYNC = 0x00000002,
    DSL_FETCH_RECORD_FLAG_REQUIRES_BARRIER = 0x00000004
};

typedef struct {
    DSL_FETCH_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TILE_SITE_ID tile_site_id;
    DSL_TILE_PLAN_ID selected_tile_plan_id;
    DSL_FETCH_PLAN_ID first_pipeline_plan_id;
    UINT32 pipeline_plan_count;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_FETCH_SITE_RECORD;

typedef struct {
    /* Version zero means absent (legacy/check-only planning). Version one
     * is a generic provisional value-storage/capacity ledger, not occupancy. */
    UINT32 model;
    UINT32 register_bytes_per_thread;
    UINT32 register_capacity_per_thread;
    UINT32 threads_per_cta;
    UINT32 thread_limit;
    UINT32 reserved;
    UINT64 shared_bytes;
    UINT64 shared_limit_bytes;
} DSL_FETCH_RESOURCE_RECORD;

typedef struct {
    DSL_FETCH_PLAN_ID id;
    DSL_FETCH_SITE_ID site_id;
    DSL_TILE_PLAN_ID tile_plan_id;
    UINT32 target_profile_id;
    UINT32 engine;
    DSL_FETCH_RECORD_ID first_fetch_id;
    UINT32 fetch_count;
    DSL_PIPELINE_STAGE_ID first_stage_id;
    UINT32 stage_count;
    UINT32 prefetch_distance;
    UINT32 issue_point;
    UINT32 arrival_barrier;
    UINT32 wait_point;
    UINT32 edge_policy;
    UINT64 buffering_bytes;
    UINT64 buffering_capacity_bytes;
    UINT64 raw_movement_cost;
    UINT64 hidden_movement_cost;
    UINT64 unhidden_movement_cost;
    UINT32 barrier_count;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT32 flags;
    UINT32 reserved;
    DSL_FETCH_RESOURCE_RECORD resources;
} DSL_FETCH_PLAN_RECORD;

typedef struct {
    DSL_FETCH_RECORD_ID id;
    DSL_FETCH_PLAN_ID pipeline_plan_id;
    UINT32 operand_ordinal;
    DSL_IR_VALUE_ID value_id;
    TY_IDX descriptor_ty;
    UINT32 source_tier_kind;
    UINT32 destination_tier_kind;
    UINT32 engine;
    UINT32 transaction_bytes;
    UINT32 minimum_alignment;
    UINT64 bytes_per_stage;
    UINT64 raw_movement_cost;
    UINT64 hidden_movement_cost;
    UINT64 unhidden_movement_cost;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT32 flags;
    UINT32 reserved;
} DSL_FETCH_RECORD;

typedef struct {
    DSL_PIPELINE_STAGE_ID id;
    DSL_FETCH_PLAN_ID pipeline_plan_id;
    UINT32 ordinal;
    UINT32 buffer_slot;
    UINT32 prefetch_distance;
    UINT32 issue_point;
    UINT32 arrival_barrier;
    UINT32 wait_point;
    UINT32 reserved;
} DSL_PIPELINE_STAGE_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_FETCH_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_FETCH_PLAN_RECORD *plans;
    UINT32 plan_count;
    const DSL_FETCH_RECORD *fetches;
    UINT32 fetch_count;
    const DSL_PIPELINE_STAGE_RECORD *stages;
    UINT32 stage_count;
} DSL_FETCH_PIPELINE_IR_CREATE_INFO;

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
extern DSL_FETCH_PIPELINE_IR *DSL_fetch_pipeline_ir_create
                                (const DSL_FETCH_PIPELINE_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
extern void DSL_fetch_pipeline_ir_destroy (DSL_FETCH_PIPELINE_IR *ir);
/* Read-only structural check of copied site/plan/fetch/stage ranges,
 * selection and tile joins, direct-versus-staged fields, stored cost sums and
 * optional resource ledgers. A version-one ledger must fit its recorded limits
 * for a proven plan; target formulas and semantic legality are not re-proved.
 * Return FALSE at the first detected defect with optional diagnostics, leaving
 * caller records and the store unchanged. */
extern BOOL DSL_fetch_pipeline_ir_verify
                                (const DSL_FETCH_PIPELINE_IR *ir,
                                 FILE *diagnostic);
/* Print stored runtime movement evidence to the borrowed stream; absent
 * resource ledgers stay absent. A nonnull store must have valid structural
 * ranges because nested rows are indexed without re-verification; null stream
 * or store is a no-op. Do not derive limits, validate schedules, select plans
 * or change WHIRL/mapped-image state. */
extern void DSL_fetch_pipeline_ir_print
                                (FILE *file,
                                 const DSL_FETCH_PIPELINE_IR *ir);
/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
extern ST_IDX DSL_fetch_pipeline_ir_owner (const DSL_FETCH_PIPELINE_IR *ir);
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fetch_pipeline_ir_site_count
                                (const DSL_FETCH_PIPELINE_IR *ir);
/* Read the copied plan table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fetch_pipeline_ir_plan_count
                                (const DSL_FETCH_PIPELINE_IR *ir);
/* Read the copied fetch table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fetch_pipeline_ir_fetch_count
                                (const DSL_FETCH_PIPELINE_IR *ir);
/* Read the copied stage table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_fetch_pipeline_ir_stage_count
                                (const DSL_FETCH_PIPELINE_IR *ir);
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fetch_pipeline_ir_get_site
                                (const DSL_FETCH_PIPELINE_IR *ir,
                                 DSL_FETCH_SITE_ID id,
                                 DSL_FETCH_SITE_RECORD *record);
/* Copy the plan row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fetch_pipeline_ir_get_plan
                                (const DSL_FETCH_PIPELINE_IR *ir,
                                 DSL_FETCH_PLAN_ID id,
                                 DSL_FETCH_PLAN_RECORD *record);
/* Copy the fetch row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fetch_pipeline_ir_get_fetch
                                (const DSL_FETCH_PIPELINE_IR *ir,
                                 DSL_FETCH_RECORD_ID id,
                                 DSL_FETCH_RECORD *record);
/* Copy the stage row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_fetch_pipeline_ir_get_stage
                                (const DSL_FETCH_PIPELINE_IR *ir,
                                 DSL_PIPELINE_STAGE_ID id,
                                 DSL_PIPELINE_STAGE_RECORD *record);
/* Borrow a static issue point spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fetch_issue_point_name (UINT32 point);
/* Borrow a static barrier kind spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fetch_barrier_name (UINT32 barrier);
/* Borrow a static wait point spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_fetch_wait_point_name (UINT32 point);

#endif /* dsl_fetch_pipeline_INCLUDED */
