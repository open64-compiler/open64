/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned CommonResidencyPlanIR runtime record and copied-store contract for
 * caller-scoped PU planning. Discovery, semantic legality, costing and selection
 * remain in the companion *_opt phase implementation. These records are not
 * mapped-image rows; relocation changes no .B layout, reader/writer or public IDs.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_residency_candidate_INCLUDED
#define dsl_residency_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_evolution.h"

struct DSL_RESIDENCY_PLAN_IR;
typedef struct DSL_RESIDENCY_PLAN_IR DSL_RESIDENCY_PLAN_IR;
typedef UINT32 DSL_RESIDENCY_DESCRIPTOR_ID;
typedef UINT32 DSL_RESIDENCY_SITE_ID;
typedef UINT32 DSL_RESIDENCY_ALTERNATIVE_ID;

#define DSL_RESIDENCY_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_RESIDENCY_PROMOTION_UNKNOWN = 0,
    DSL_RESIDENCY_PROMOTION_NONE = 1,
    DSL_RESIDENCY_PROMOTION_ON_DEMAND = 2,
    DSL_RESIDENCY_PROMOTION_PREFETCH = 3
} DSL_RESIDENCY_PROMOTION_POLICY;

typedef enum {
    DSL_RESIDENCY_DEMOTION_UNKNOWN = 0,
    DSL_RESIDENCY_DEMOTION_NONE = 1,
    DSL_RESIDENCY_DEMOTION_LAST_USE = 2,
    DSL_RESIDENCY_DEMOTION_PRESSURE = 3
} DSL_RESIDENCY_DEMOTION_POLICY;

typedef enum {
    DSL_RESIDENCY_SPILL_UNKNOWN = 0,
    DSL_RESIDENCY_SPILL_NONE = 1,
    DSL_RESIDENCY_SPILL_LOWER_TIER = 2,
    DSL_RESIDENCY_SPILL_SYSTEM = 3
} DSL_RESIDENCY_SPILL_POLICY;

typedef enum {
    DSL_RESIDENCY_EVICTION_UNKNOWN = 0,
    DSL_RESIDENCY_EVICTION_NONE = 1,
    DSL_RESIDENCY_EVICTION_LAST_USE = 2,
    DSL_RESIDENCY_EVICTION_PRESSURE = 3
} DSL_RESIDENCY_EVICTION_POLICY;

enum {
    DSL_RESIDENCY_FLAG_NONE = 0,
    DSL_RESIDENCY_FLAG_PROVISIONAL = 0x00000001,
    DSL_RESIDENCY_FLAG_SEMANTICS_PRESERVING = 0x00000002,
    DSL_RESIDENCY_FLAG_CAPACITY_CHECKED = 0x00000004,
    DSL_RESIDENCY_FLAG_LIFETIME_CHECKED = 0x00000008
};

typedef struct {
    DSL_RESIDENCY_DESCRIPTOR_ID id;
    TY_IDX source_descriptor_ty;
    UINT32 target_profile_id;
    DSL_MEMORY_TIER_ID tier_id;
    UINT32 tier_kind;
    UINT32 tier_scope;
    UINT64 required_bytes;
    UINT64 capacity_bytes;
    UINT64 allocation_granularity;
    UINT32 minimum_alignment;
    UINT32 promotion_policy;
    UINT32 demotion_policy;
    UINT32 spill_policy;
    UINT32 eviction_policy;
    UINT32 flags;
    UINT32 reserved;
} DSL_RESIDENCY_DESCRIPTOR_RECORD;

typedef struct {
    DSL_RESIDENCY_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_RESIDENCY_ALTERNATIVE_ID first_alternative_id;
    UINT32 alternative_count;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_RESIDENCY_SITE_RECORD;

typedef struct {
    DSL_RESIDENCY_ALTERNATIVE_ID id;
    DSL_RESIDENCY_SITE_ID site_id;
    DSL_RESIDENCY_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID plan_id;
    UINT32 reserved;
} DSL_RESIDENCY_ALTERNATIVE_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_RESIDENCY_DESCRIPTOR_RECORD *descriptors;
    UINT32 descriptor_count;
    const DSL_RESIDENCY_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_RESIDENCY_ALTERNATIVE_RECORD *alternatives;
    UINT32 alternative_count;
} DSL_RESIDENCY_PLAN_IR_CREATE_INFO;

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
extern DSL_RESIDENCY_PLAN_IR *DSL_residency_plan_ir_create
                                (const DSL_RESIDENCY_PLAN_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
extern void DSL_residency_plan_ir_destroy (DSL_RESIDENCY_PLAN_IR *ir);
/* Check copied owner, dense descriptor/site/alternative IDs, required IDs,
 * policy/flag bounds, alternative joins and selection membership without mutation.
 * This does not resolve hardware tiers or prove capacity, lifetime or legality.
 * Return FALSE on defects, usually with diagnostics; leftover alternatives can
 * fail the final table-size check without a diagnostic. */
extern BOOL DSL_residency_plan_ir_verify
                                (const DSL_RESIDENCY_PLAN_IR *ir,
                                 FILE *diagnostic);
/* Print stored residency counts to the borrowed stream; null stream or
 * store is a no-op. Neither recheck target capacity nor allocate storage,
 * choose a residency policy or modify WHIRL. */
extern void DSL_residency_plan_ir_print
                                (FILE *file,
                                 const DSL_RESIDENCY_PLAN_IR *ir);
/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
extern ST_IDX DSL_residency_plan_ir_owner
                                (const DSL_RESIDENCY_PLAN_IR *ir);
/* Read the copied descriptor table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_residency_plan_ir_descriptor_count
                                (const DSL_RESIDENCY_PLAN_IR *ir);
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_residency_plan_ir_site_count
                                (const DSL_RESIDENCY_PLAN_IR *ir);
/* Read the copied alternative table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_residency_plan_ir_alternative_count
                                (const DSL_RESIDENCY_PLAN_IR *ir);
/* Copy the descriptor row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_residency_plan_ir_get_descriptor
                                (const DSL_RESIDENCY_PLAN_IR *ir,
                                 DSL_RESIDENCY_DESCRIPTOR_ID id,
                                 DSL_RESIDENCY_DESCRIPTOR_RECORD *record);
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_residency_plan_ir_get_site
                                (const DSL_RESIDENCY_PLAN_IR *ir,
                                 DSL_RESIDENCY_SITE_ID id,
                                 DSL_RESIDENCY_SITE_RECORD *record);
/* Copy the alternative row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_residency_plan_ir_get_alternative
                                (const DSL_RESIDENCY_PLAN_IR *ir,
                                 DSL_RESIDENCY_ALTERNATIVE_ID id,
                                 DSL_RESIDENCY_ALTERNATIVE_RECORD *record);
/* Borrow a static promotion policy spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_residency_promotion_name (UINT32 policy);
/* Borrow a static demotion policy spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_residency_demotion_name (UINT32 policy);
/* Borrow a static spill policy spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_residency_spill_name (UINT32 policy);
/* Borrow a static eviction policy spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_residency_eviction_name (UINT32 policy);

#endif /* dsl_residency_candidate_INCLUDED */
