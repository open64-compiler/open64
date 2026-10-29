/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned CommonTilePlanIR runtime record and copied-store contract for
 * caller-scoped PU planning. Discovery, semantic legality, costing and selection
 * remain in the companion *_opt phase implementation. These records are not
 * mapped-image rows; relocation changes no .B layout, reader/writer or public IDs.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO9-HIERARCHICAL-TILING.md.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_tile_candidate_INCLUDED
#define dsl_tile_candidate_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_opt_plan.h"
#include "dsl_tensor_evolution.h"

struct DSL_TILE_PLAN_IR;

typedef struct DSL_TILE_PLAN_IR DSL_TILE_PLAN_IR;
typedef UINT32 DSL_TILE_SITE_ID;
typedef UINT32 DSL_TILE_PLAN_ID;
typedef UINT32 DSL_TILE_STAGE_ID;

#define DSL_TILE_INVALID_ID ((UINT32)0)
#define DSL_TILE_UNKNOWN_U64 (~(UINT64)0)

typedef enum {
    DSL_TILE_PHASE_P7_0_BASELINE = 0,
    DSL_TILE_PHASE_P7_1_OUTPUT = 1,
    DSL_TILE_PHASE_P7_2_COALESCING = 2,
    DSL_TILE_PHASE_P7_3_REDUCTION = 3,
    DSL_TILE_PHASE_P7_4_SHARED = 4,
    DSL_TILE_PHASE_P7_5_RESOURCE = 5,
    DSL_TILE_PHASE_P7_6_THREAD = 6,
    DSL_TILE_PHASE_P7_7_REGISTER_REUSE = 7,
    DSL_TILE_PHASE_P7_8_VECTOR = 8,
    DSL_TILE_PHASE_P7_9_FAMILY = 9,
    DSL_TILE_PHASE_P7_10_WARP = 10,
    DSL_TILE_PHASE_P7_11_INSTRUCTION = 11,
    DSL_TILE_PHASE_COUNT = 12
} DSL_TILE_PHASE;

typedef enum {
    DSL_TILE_LEVEL_UNKNOWN = 0,
    DSL_TILE_LEVEL_PROBLEM = 1,
    DSL_TILE_LEVEL_OUTPUT = 2,
    DSL_TILE_LEVEL_COALESCING = 3,
    DSL_TILE_LEVEL_REDUCTION = 4,
    DSL_TILE_LEVEL_SHARED = 5,
    DSL_TILE_LEVEL_RESOURCE = 6,
    DSL_TILE_LEVEL_THREAD = 7,
    DSL_TILE_LEVEL_REGISTER_REUSE = 8,
    DSL_TILE_LEVEL_VECTOR = 9,
    DSL_TILE_LEVEL_FAMILY = 10,
    DSL_TILE_LEVEL_WARP = 11,
    DSL_TILE_LEVEL_INSTRUCTION = 12
} DSL_TILE_LEVEL_KIND;

typedef enum {
    DSL_TILE_FAMILY_UNKNOWN = 0,
    DSL_TILE_FAMILY_BASELINE = 1,
    DSL_TILE_FAMILY_CUDA_64 = 2,
    DSL_TILE_FAMILY_CUDA_128 = 3,
    DSL_TILE_FAMILY_BLACKWELL_WIDE = 4,
    DSL_TILE_FAMILY_CUDA_128_COALESCED = 5
} DSL_TILE_FAMILY_KIND;

typedef enum {
    DSL_TILE_EDGE_UNKNOWN = 0,
    DSL_TILE_EDGE_EXACT = 1,
    DSL_TILE_EDGE_PREDICATED = 2
} DSL_TILE_EDGE_POLICY;

typedef enum {
    DSL_TILE_A_LAYOUT_ROW_MAJOR = 0,
    DSL_TILE_A_LAYOUT_TRANSPOSED_PADDED = 1
} DSL_TILE_A_SHARED_LAYOUT;

typedef enum {
    DSL_TILE_B_LAYOUT_ROW_MAJOR = 0,
    DSL_TILE_B_LAYOUT_XOR_COLUMNS = 1
} DSL_TILE_B_SHARED_LAYOUT;

typedef enum {
    DSL_TILE_INSTRUCTION_UNKNOWN = 0,
    DSL_TILE_INSTRUCTION_SCALAR_FMA = 1,
    DSL_TILE_INSTRUCTION_VECTOR_FMA = 2
} DSL_TILE_INSTRUCTION_FAMILY;

enum {
    DSL_TILE_PLAN_FLAG_NONE = 0,
    DSL_TILE_PLAN_FLAG_BASELINE = 0x00000001,
    DSL_TILE_PLAN_FLAG_PROVISIONAL = 0x00000002,
    DSL_TILE_PLAN_FLAG_SEMANTICS_PRESERVING = 0x00000004,
    DSL_TILE_PLAN_FLAG_PREFETCH_HINT = 0x00000008,
    DSL_TILE_PLAN_FLAG_TMA_CANDIDATE = 0x00000010,
    /* Eligible topology only: each movement must prove its own resources.
     * Design: doc/AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md. */
    DSL_TILE_PLAN_FLAG_DEFERRED_RESOURCES = 0x00000020
};

typedef struct {
    DSL_TILE_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TILE_PLAN_ID first_tile_plan_id;
    UINT32 tile_plan_count;
    DSL_OPT_PLAN_ID baseline_plan_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_TILE_SITE_RECORD;

typedef struct {
    DSL_TILE_PLAN_ID id;
    DSL_TILE_SITE_ID site_id;
    UINT32 target_profile_id;
    UINT32 family;
    TY_IDX source_descriptor_ty;
    DSL_TILE_STAGE_ID first_stage_id;
    UINT32 stage_count;
    DSL_TENSOR_EVOLUTION_NODE_ID source_evolution_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_evolution_node_id;
    DSL_TENSOR_EVOLUTION_EDGE_ID evolution_edge_id;
    UINT64 problem_m;
    UINT64 problem_n;
    UINT64 problem_k;
    UINT32 cta_m;
    UINT32 cta_n;
    UINT32 cta_k;
    UINT32 warp_m;
    UINT32 warp_n;
    UINT32 thread_m;
    UINT32 thread_n;
    UINT32 instruction_m;
    UINT32 instruction_n;
    UINT32 instruction_k;
    UINT32 vector_width;
    UINT32 transaction_bytes;
    UINT64 gmem_read_bytes;
    UINT64 gmem_write_bytes;
    UINT64 gmem_a_sector_bytes;
    UINT64 gmem_b_sector_bytes;
    UINT64 gmem_c_sector_bytes;
    UINT64 gmem_aligned_sector_bytes;
    UINT64 operation_count;
    UINT64 shared_bytes_per_stage;
    UINT64 smem_bank_conflict_work;
    UINT64 smem_a_load_issue_work;
    UINT64 smem_b_load_issue_work;
    UINT64 smem_a_copy_conflicts;
    UINT64 smem_b_copy_conflicts;
    UINT64 smem_a_read_conflicts;
    UINT64 smem_b_read_conflicts;
    UINT64 smem_address_work;
    UINT64 smem_a_allocation_bytes;
    UINT64 smem_b_allocation_bytes;
    UINT64 shared_capacity_bytes;
    UINT32 a_shared_layout;
    UINT32 a_shared_stride;
    UINT32 b_shared_layout;
    UINT32 smem_bank_count;
    UINT32 smem_bank_word_bytes;
    UINT32 maximum_buffer_stages;
    UINT32 registers_per_thread;
    UINT32 register_capacity_per_thread;
    UINT32 threads_per_cta;
    UINT32 warps_per_cta;
    UINT32 barrier_count;
    UINT32 edge_policy;
    UINT32 instruction_family;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_TILE_PLAN_RECORD;

typedef struct {
    DSL_TILE_STAGE_ID id;
    DSL_TILE_PLAN_ID tile_plan_id;
    UINT32 ordinal;
    UINT32 phase;
    UINT32 level_kind;
    UINT64 extent_m;
    UINT64 extent_n;
    UINT64 extent_k;
    UINT32 flags;
    UINT32 reserved;
} DSL_TILE_STAGE_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_TILE_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_TILE_PLAN_RECORD *plans;
    UINT32 plan_count;
    const DSL_TILE_STAGE_RECORD *stages;
    UINT32 stage_count;
} DSL_TILE_PLAN_IR_CREATE_INFO;

/* Copy the caller's readable, count-sized record arrays into a new owned
 * runtime store for owner_pu_st; nonempty arrays must be nonnull. Return NULL
 * on invalid input or structural verification failure, deleting a rejected
 * copy without changing caller arrays. The caller destroys success; allocation
 * exceptions are not caught. */
extern DSL_TILE_PLAN_IR *DSL_tile_plan_ir_create
                                (const DSL_TILE_PLAN_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release this caller-owned runtime store and its copied vectors; NULL is
 * safe. Other aliases become invalid; referenced WHIRL, types and plan IDs are
 * not owned or destroyed here. */
extern void DSL_tile_plan_ir_destroy (DSL_TILE_PLAN_IR *ir);
/* Check copied site/plan/stage topology, required lineage IDs, enum/flag
 * bounds, baseline identity, recorded shared-layout consistency and selection
 * membership without mutation. Deferred resources are nonbaseline-only. Return
 * FALSE on the first structural defect with optional diagnostics; this is not
 * target legality, capacity admission or a proof of evolution-graph semantics. */
extern BOOL DSL_tile_plan_ir_verify
                                (const DSL_TILE_PLAN_IR *ir,
                                 FILE *diagnostic);
/* Print stored tile sites, plans and stages to the borrowed stream. A
 * nonnull store must have valid structural ranges because nested records are
 * indexed without re-verification; null stream or store is a no-op. Do not
 * select tiles, derive resources, transform loops or modify WHIRL. */
extern void DSL_tile_plan_ir_print
                                (FILE *file, const DSL_TILE_PLAN_IR *ir);
/* Read the recorded PU symbol identity without consulting or activating a
 * PU; return ST_IDX_ZERO for NULL and leave the runtime store unchanged. */
extern ST_IDX DSL_tile_plan_ir_owner (const DSL_TILE_PLAN_IR *ir);
/* Read the copied site table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_tile_plan_ir_site_count (const DSL_TILE_PLAN_IR *ir);
/* Read the copied plan table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_tile_plan_ir_plan_count (const DSL_TILE_PLAN_IR *ir);
/* Read the copied stage table size, returning zero for NULL; no
 * verification, ownership transfer or store mutation occurs. */
extern UINT32 DSL_tile_plan_ir_stage_count (const DSL_TILE_PLAN_IR *ir);
/* Copy the site row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_tile_plan_ir_get_site
                                (const DSL_TILE_PLAN_IR *ir,
                                 DSL_TILE_SITE_ID id,
                                 DSL_TILE_SITE_RECORD *record);
/* Copy the plan row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_tile_plan_ir_get_plan
                                (const DSL_TILE_PLAN_IR *ir,
                                 DSL_TILE_PLAN_ID id,
                                 DSL_TILE_PLAN_RECORD *record);
/* Copy the stage row at one-based table ID id into caller-owned record.
 * Return FALSE for a null store/output, zero ID or out-of-range ID, preserving
 * any supplied output on failure. This lookup does not resolve external IDs. */
extern BOOL DSL_tile_plan_ir_get_stage
                                (const DSL_TILE_PLAN_IR *ir,
                                 DSL_TILE_STAGE_ID id,
                                 DSL_TILE_STAGE_RECORD *record);
/* Borrow a static tile phase spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_tile_phase_name (UINT32 phase);
/* Borrow a static tile level spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_tile_level_name (UINT32 level);
/* Borrow a static tile family spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_tile_family_name (UINT32 family);
/* Borrow a static edge policy spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_tile_edge_policy_name (UINT32 policy);
/* Borrow a static instruction family spelling indexed by its numeric value;
 * return "unknown" outside the name table without allocation or state changes. */
extern const char *DSL_tile_instruction_name (UINT32 family);

#endif /* dsl_tile_candidate_INCLUDED */
