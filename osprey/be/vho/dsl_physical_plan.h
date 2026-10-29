/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO owns transient physical-plan records, copied storage, and software-provider
 * capabilities. Selection and lowering consume these PU-local descriptions;
 * they are not hardware descriptions or .B rows and change no binary layout.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO11-PHYSICAL-PLAN.md.
 */

#ifndef dsl_physical_plan_INCLUDED
#define dsl_physical_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_fetch_pipeline.h"
#include "dsl_opt_plan.h"
#include "dsl_tile_candidate.h"

struct DSL_PHYSICAL_PLAN_IR;

typedef struct DSL_PHYSICAL_PLAN_IR DSL_PHYSICAL_PLAN_IR;
typedef UINT32 DSL_PHYSICAL_SITE_ID;
typedef UINT32 DSL_PHYSICAL_IMPLEMENTATION_ID;
typedef UINT32 DSL_PROVIDER_CAPABILITY_ID;

#define DSL_PHYSICAL_INVALID_ID ((UINT32)0)
/* Form a provider bit; the caller supplies a shift smaller than unsigned width. */
#define DSL_PHYSICAL_PROVIDER_MASK(provider) (1U << (provider))

typedef enum {
    DSL_PHYSICAL_PROVIDER_UNKNOWN = 0,
    DSL_PHYSICAL_PROVIDER_OPEN64_DIRECT = 1,
    DSL_PHYSICAL_PROVIDER_OPEN64_GENERATED = 2,
    DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT = 3,
    DSL_PHYSICAL_PROVIDER_NVIDIA_CUDNN = 4,
    DSL_PHYSICAL_PROVIDER_TRITON = 5,
    DSL_PHYSICAL_PROVIDER_EXISTING_PTX = 6,
    DSL_PHYSICAL_PROVIDER_COUNT = 7
} DSL_PHYSICAL_PROVIDER_KIND;

typedef enum {
    DSL_PHYSICAL_IMPLEMENTATION_UNKNOWN = 0,
    DSL_PHYSICAL_IMPLEMENTATION_BASELINE_DIRECT = 1,
    DSL_PHYSICAL_IMPLEMENTATION_GENERATED_KERNEL = 2,
    DSL_PHYSICAL_IMPLEMENTATION_LIBRARY = 3,
    DSL_PHYSICAL_IMPLEMENTATION_EXISTING_KERNEL = 4
} DSL_PHYSICAL_IMPLEMENTATION_KIND;

typedef enum {
    DSL_PHYSICAL_SCHEDULE_UNKNOWN = 0,
    DSL_PHYSICAL_SCHEDULE_DIRECT = 1,
    DSL_PHYSICAL_SCHEDULE_TILED_PIPELINE = 2,
    DSL_PHYSICAL_SCHEDULE_PROVIDER_OWNED = 3
} DSL_PHYSICAL_SCHEDULE_KIND;

enum {
    DSL_PROVIDER_CAPABILITY_FLAG_NONE = 0,
    DSL_PROVIDER_CAPABILITY_FLAG_BUILTIN = 0x00000001,
    DSL_PROVIDER_CAPABILITY_FLAG_REQUIRES_RUNTIME = 0x00000002,
    DSL_PROVIDER_CAPABILITY_FLAG_REVIEWED = 0x00000004
};

enum {
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_NONE = 0,
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_BASELINE = 0x00000001,
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVISIONAL = 0x00000002,
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_SELECTED = 0x00000004,
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_PROVIDER_AVAILABLE = 0x00000008,
    DSL_PHYSICAL_IMPLEMENTATION_FLAG_SEMANTICS_PRESERVING = 0x00000010
};

typedef struct {
    DSL_PROVIDER_CAPABILITY_ID id;
    UINT32 provider;
    UINT32 implementation_kind;
    UINT32 target_profile_id;
    UINT32 logical_operator;
    UINT32 operator_version;
    UINT32 element_mtype;
    UINT32 rank;
    UINT32 schedule_kind;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_PROVIDER_CAPABILITY_RECORD;

typedef struct {
    DSL_PHYSICAL_SITE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID semantic_node_id;
    DSL_IR_VALUE_ID semantic_value_id;
    DSL_TILE_PLAN_ID selected_tile_plan_id;
    DSL_FETCH_PLAN_ID selected_fetch_plan_id;
    DSL_PHYSICAL_IMPLEMENTATION_ID first_implementation_id;
    UINT32 implementation_count;
    DSL_PHYSICAL_IMPLEMENTATION_ID baseline_implementation_id;
    DSL_PHYSICAL_IMPLEMENTATION_ID selected_implementation_id;
    DSL_OPT_PLAN_ID selected_plan_id;
    UINT32 reserved;
} DSL_PHYSICAL_SITE_RECORD;

typedef struct {
    DSL_PHYSICAL_IMPLEMENTATION_ID id;
    DSL_PHYSICAL_SITE_ID site_id;
    UINT32 implementation_kind;
    UINT32 provider;
    DSL_PROVIDER_CAPABILITY_ID capability_id;
    UINT32 target_profile_id;
    DSL_TILE_PLAN_ID tile_plan_id;
    DSL_FETCH_PLAN_ID fetch_plan_id;
    UINT32 schedule_kind;
    UINT64 identity;
    UINT64 compute_cost;
    UINT64 memory_cost;
    UINT64 synchronization_cost;
    UINT64 launch_cost;
    UINT64 total_cost;
    UINT32 legality;
    UINT32 rejection_reason;
    DSL_OPT_CANDIDATE_ID candidate_id;
    DSL_OPT_PLAN_ID optimization_plan_id;
    DSL_PHYSICAL_IMPLEMENTATION_ID fallback_implementation_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_PHYSICAL_IMPLEMENTATION_RECORD;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_PHYSICAL_SITE_RECORD *sites;
    UINT32 site_count;
    const DSL_PHYSICAL_IMPLEMENTATION_RECORD *implementations;
    UINT32 implementation_count;
} DSL_PHYSICAL_PLAN_IR_CREATE_INFO;

/* Copy the caller's counted site/implementation arrays for a nonzero owner.
 * Return caller-owned storage to destroy, or NULL after input/structural rejection;
 * inputs and WHIRL stay unchanged. Allocation exceptions are not caught. */
extern DSL_PHYSICAL_PLAN_IR *DSL_physical_plan_ir_create
                                (const DSL_PHYSICAL_PLAN_IR_CREATE_INFO *info,
                                 FILE *diagnostic);
/* Release the owned plan and its copied arrays; NULL is allowed. Borrowed
 * pointers to the plan expire; no WHIRL or provider inventory is changed. */
extern void DSL_physical_plan_ir_destroy (DSL_PHYSICAL_PLAN_IR *ir);
/* Read-only structural check of IDs, ownership, ranges, costs, capability joins
 * and selection/fallback consistency; optionally diagnose and return FALSE.
 * This does not prove active-PU legality or provider availability. */
extern BOOL DSL_physical_plan_ir_verify
                                (const DSL_PHYSICAL_PLAN_IR *ir,
                                 FILE *diagnostic);
/* Print a structurally valid borrowed plan to the caller's stream without
 * mutation. NULL inputs are no-ops; the caller handles stream errors. */
extern void DSL_physical_plan_ir_print
                                (FILE *file, const DSL_PHYSICAL_PLAN_IR *ir);
/* Read the copied owner without mutation; NULL returns ST_IDX_ZERO. */
extern ST_IDX DSL_physical_plan_ir_owner (const DSL_PHYSICAL_PLAN_IR *ir);
/* Read the copied site count without mutation; NULL returns zero. */
extern UINT32 DSL_physical_plan_ir_site_count
                                (const DSL_PHYSICAL_PLAN_IR *ir);
/* Read the copied implementation count without mutation; NULL returns zero. */
extern UINT32 DSL_physical_plan_ir_implementation_count
                                (const DSL_PHYSICAL_PLAN_IR *ir);
/* Copy a one-based site into caller storage; invalid plan, output, or ID
 * returns FALSE without writing the output or changing the plan. */
extern BOOL DSL_physical_plan_ir_get_site
                                (const DSL_PHYSICAL_PLAN_IR *ir,
                                 DSL_PHYSICAL_SITE_ID id,
                                 DSL_PHYSICAL_SITE_RECORD *record);
/* Copy a one-based implementation into caller storage; invalid plan, output,
 * or ID returns FALSE without writing the output or changing the plan. */
extern BOOL DSL_physical_plan_ir_get_implementation
                                (const DSL_PHYSICAL_PLAN_IR *ir,
                                 DSL_PHYSICAL_IMPLEMENTATION_ID id,
                                 DSL_PHYSICAL_IMPLEMENTATION_RECORD *record);
/* Find the first matching semantic node in a borrowed plan. Copy its site
 * before looking up the selected implementation: FALSE may leave the site
 * written when selection is absent; the implementation stays unchanged on failure. */
extern BOOL DSL_physical_plan_ir_find_selected
                                (const DSL_PHYSICAL_PLAN_IR *ir,
                                 DSL_IR_NODE_ID semantic_node_id,
                                 DSL_PHYSICAL_SITE_RECORD *site,
                                 DSL_PHYSICAL_IMPLEMENTATION_RECORD
                                     *implementation);
/* Read the immutable VHO software-provider inventory size; no availability
 * probe or target discovery is performed. */
extern UINT32 DSL_provider_capability_count (void);
/* Copy a one-based software capability from immutable inventory. Invalid ID
 * or NULL output returns FALSE without writes; this does not probe a runtime. */
extern BOOL DSL_provider_capability_get
                                (DSL_PROVIDER_CAPABILITY_ID id,
                                 DSL_PROVIDER_CAPABILITY_RECORD *record);
/* Borrow a static provider spelling; out-of-range values return "unknown". */
extern const char *DSL_physical_provider_name (UINT32 provider);
/* Borrow a static implementation spelling; out-of-range values return "unknown". */
extern const char *DSL_physical_implementation_name (UINT32 implementation);
/* Borrow a static schedule spelling; out-of-range values return "unknown". */
extern const char *DSL_physical_schedule_name (UINT32 schedule);

#endif /* dsl_physical_plan_INCLUDED */
