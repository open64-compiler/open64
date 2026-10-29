/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned CommonLogicalLayoutIR runtime record and copied-store contract for
 * caller-scoped PU planning. Discovery, semantic legality, costing and selection
 * remain in the companion *_opt phase implementation. These records are not
 * mapped-image rows; relocation changes no .B layout, reader/writer or public IDs.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO6-LOGICAL-LAYOUT.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_layout_candidate_INCLUDED
#define dsl_layout_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_evolution.h"

struct DSL_LOGICAL_LAYOUT_IR;
typedef struct DSL_LOGICAL_LAYOUT_IR DSL_LOGICAL_LAYOUT_IR;
typedef UINT32 DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_AXIS_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_BLOCK_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_SITE_ID;
typedef UINT32 DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID;

#define DSL_LOGICAL_LAYOUT_INVALID_ID ((UINT32)0)
#define DSL_LOGICAL_LAYOUT_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_LOGICAL_LAYOUT_UNKNOWN = 0,
    DSL_LOGICAL_LAYOUT_PERMUTED = 1,
    DSL_LOGICAL_LAYOUT_BLOCKED = 2,
    DSL_LOGICAL_LAYOUT_PACKED_HEAD = 3,
    DSL_LOGICAL_LAYOUT_DOMAIN = 4
} DSL_LOGICAL_LAYOUT_KIND;

typedef enum {
    DSL_LAYOUT_COMPATIBILITY_UNKNOWN = 0,
    DSL_LAYOUT_COMPATIBILITY_PROVEN = 1,
    DSL_LAYOUT_COMPATIBILITY_REJECTED = 2
} DSL_LAYOUT_COMPATIBILITY_STATE;

typedef enum {
    DSL_LAYOUT_CONVERSION_UNKNOWN = 0,
    DSL_LAYOUT_CONVERSION_KNOWN = 1
} DSL_LAYOUT_CONVERSION_STATE;

enum {
    DSL_LOGICAL_LAYOUT_FLAG_NONE = 0,
    DSL_LOGICAL_LAYOUT_FLAG_SEMANTICS_PRESERVING = 0x00000001,
    DSL_LOGICAL_LAYOUT_FLAG_LOGICAL_ONLY = 0x00000002
};

typedef struct {
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID id;
    TY_IDX source_descriptor_ty;
    UINT32 kind;
    UINT32 rank;
    DSL_LOGICAL_LAYOUT_AXIS_ID first_axis_id;
    UINT32 axis_count;
    DSL_LOGICAL_LAYOUT_BLOCK_ID first_block_id;
    UINT32 block_count;
    UINT32 minimum_alignment;
    UINT32 flags;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_AXIS_ID id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    UINT32 ordinal;
    UINT32 source_axis;
    UINT32 result_axis;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_AXIS_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_BLOCK_ID id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    UINT32 axis;
    UINT32 factor;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_LOGICAL_LAYOUT_BLOCK_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID first_alternative_id;
    UINT32 alternative_count;
    DSL_OPT_CANDIDATE_ID baseline_candidate_id;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_SITE_RECORD;

typedef struct {
    DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID id;
    DSL_LOGICAL_LAYOUT_SITE_ID site_id;
    DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID descriptor_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT32 compatibility_state;
    UINT32 conversion_state;
    UINT32 legality;
    UINT32 rejection_reason;
    UINT64 conversion_bytes;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID plan_id;
    UINT32 reserved;
} DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD *descriptors;
    UINT32 descriptor_count;
    const DSL_LOGICAL_LAYOUT_AXIS_RECORD *axes;
    UINT32 axis_count;
    const DSL_LOGICAL_LAYOUT_BLOCK_RECORD *blocks;
    UINT32 block_count;
    const DSL_LOGICAL_LAYOUT_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD *alternatives;
    UINT32 alternative_count;
} DSL_LOGICAL_LAYOUT_IR_CREATE_INFO;

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
extern DSL_LOGICAL_LAYOUT_IR *DSL_logical_layout_ir_create
                                (const DSL_LOGICAL_LAYOUT_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
extern void DSL_logical_layout_ir_destroy (DSL_LOGICAL_LAYOUT_IR *ir);
/* Read-only structural check of owner, dense descriptor/detail ranges,
 * permutations, distinct blocked axes, alternative joins and selected-plan
 * membership. This does not resolve TY/evolution IDs or prove conversions legal.
 * Return FALSE on a defect, usually with a diagnostic; the final orphan-
 * alternative check can fail silently. */
extern BOOL DSL_logical_layout_ir_verify
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 FILE *diagnostic);
/* Print stored layout table counts and site summaries to the borrowed stream;
 * null stream or store is a no-op. Neither validate compatibility nor select
 * layouts, intern tensor types or change mapped-image state. */
extern void DSL_logical_layout_ir_print
                                (FILE *file, const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
extern ST_IDX DSL_logical_layout_ir_owner (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the copied descriptor table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_logical_layout_ir_descriptor_count
                                (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the copied axis table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_logical_layout_ir_axis_count
                                (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the copied block table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_logical_layout_ir_block_count
                                (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_logical_layout_ir_site_count
                                (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Read the copied alternative table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_logical_layout_ir_alternative_count
                                (const DSL_LOGICAL_LAYOUT_IR *ir);
/* Copy the descriptor row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_logical_layout_ir_get_descriptor
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_LOGICAL_LAYOUT_DESCRIPTOR_ID id,
                                 DSL_LOGICAL_LAYOUT_DESCRIPTOR_RECORD *record);
/* Copy the axis row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_logical_layout_ir_get_axis
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_LOGICAL_LAYOUT_AXIS_ID id,
                                 DSL_LOGICAL_LAYOUT_AXIS_RECORD *record);
/* Copy the block row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_logical_layout_ir_get_block
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_LOGICAL_LAYOUT_BLOCK_ID id,
                                 DSL_LOGICAL_LAYOUT_BLOCK_RECORD *record);
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_logical_layout_ir_get_site
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_LOGICAL_LAYOUT_SITE_ID id,
                                 DSL_LOGICAL_LAYOUT_SITE_RECORD *record);
/* Copy the first site with the requested nonzero semantic value ID into
 * caller-owned record. Return FALSE for a null store/output, zero value or no
 * match, preserving any supplied output and leaving the store unchanged. */
extern BOOL DSL_logical_layout_ir_find_site
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_IR_VALUE_ID semantic_value_id,
                                 DSL_LOGICAL_LAYOUT_SITE_RECORD *record);
/* Copy the alternative row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_logical_layout_ir_get_alternative
                                (const DSL_LOGICAL_LAYOUT_IR *ir,
                                 DSL_LOGICAL_LAYOUT_ALTERNATIVE_ID id,
                                 DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD *record);
/* Borrow a static logical layout kind spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_logical_layout_kind_name (UINT32 kind);
/* Borrow a static compatibility state spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_layout_compatibility_name (UINT32 state);
/* Borrow a static conversion state spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_layout_conversion_name (UINT32 state);

#endif /* dsl_layout_candidate_INCLUDED */
