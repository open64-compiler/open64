/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Common targ_info/access memory-hierarchy and hardware-description APIs.
 * Immutable family/architecture envelopes are copied into caller-owned runtime
 * records, not WHIRL images. No optimizer policy or runtime probing is owned
 * here; estimates and exact architecture suffixes retain their meaning.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
 * Quantitative target lookup:
 * doc/AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md.
 */

#ifndef dsl_memory_hierarchy_INCLUDED
#define dsl_memory_hierarchy_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_hardware_performance.h"

typedef UINT32 DSL_MEMORY_TIER_ID;
typedef UINT32 DSL_MEMORY_MOVEMENT_CAPABILITY_ID;

#define DSL_MEMORY_TIER_INVALID_ID ((UINT32)0)
#define DSL_MEMORY_CAPACITY_UNKNOWN (~(UINT64)0)

typedef enum {
    DSL_TARGET_PROFILE_UNKNOWN = 0,
    DSL_TARGET_PROFILE_CPU_BASELINE = 1,
    DSL_TARGET_PROFILE_NVIDIA_HOPPER = 2,
    DSL_TARGET_PROFILE_NVIDIA_BLACKWELL = 3
} DSL_TARGET_PROFILE_KIND;

typedef enum {
    DSL_HARDWARE_ARCH_UNKNOWN = 0,
    DSL_HARDWARE_ARCH_SM_90 = 1,
    DSL_HARDWARE_ARCH_SM_90A = 2,
    DSL_HARDWARE_ARCH_SM_100 = 3,
    DSL_HARDWARE_ARCH_SM_100A = 4
} DSL_HARDWARE_ARCH_KIND;

typedef struct {
    UINT32 arch_id;
    UINT32 target_profile_id;
    UINT32 revision;
    UINT32 latency_override;
    const char *architecture;
    const char *table_name;
    const char *unit;
    const char *provenance;
    DSL_HARDWARE_PERFORMANCE_RECORD performance;
    DSL_HARDWARE_RESOURCE_RECORD resources;
} DSL_HARDWARE_DESCRIPTION_RECORD;

/* Exact public-name lookup. Output is caller-owned runtime data, never a
 * persisted WHIRL table; unknown names fail without changing the output. */
extern BOOL DSL_hardware_description_find_arch
    (const char *architecture, DSL_HARDWARE_DESCRIPTION_RECORD *description);

/* Copy a structurally validated immutable row by ID to caller storage.
 * Null output, unknown ID or invalid built-in row returns FALSE without changing
 * output; strings remain borrowed. No nearest-architecture fallback, probing,
 * profitability decision or mutation of global tables. */
extern BOOL DSL_hardware_description_get_arch
    (UINT32 arch_id, DSL_HARDWARE_DESCRIPTION_RECORD *description);

typedef enum {
    DSL_MEMORY_TIER_UNKNOWN = 0,
    DSL_MEMORY_TIER_SYSTEM = 1,
    DSL_MEMORY_TIER_PINNED_HOST = 2,
    DSL_MEMORY_TIER_HBM = 3,
    DSL_MEMORY_TIER_L2 = 4,
    DSL_MEMORY_TIER_SHARED = 5,
    DSL_MEMORY_TIER_REGISTER = 6,
    DSL_MEMORY_TIER_REMOTE = 7
} DSL_MEMORY_TIER_KIND;

typedef enum {
    DSL_MEMORY_SCOPE_UNKNOWN = 0,
    DSL_MEMORY_SCOPE_SYSTEM = 1,
    DSL_MEMORY_SCOPE_DEVICE = 2,
    DSL_MEMORY_SCOPE_SM = 3,
    DSL_MEMORY_SCOPE_CTA = 4,
    DSL_MEMORY_SCOPE_THREAD = 5
} DSL_MEMORY_SCOPE_KIND;

typedef enum {
    DSL_MEMORY_MOVEMENT_UNKNOWN = 0,
    DSL_MEMORY_MOVEMENT_DEMAND = 1,
    DSL_MEMORY_MOVEMENT_VECTOR = 2,
    DSL_MEMORY_MOVEMENT_ASYNC_COPY = 3,
    DSL_MEMORY_MOVEMENT_MULTIDIMENSIONAL_ASYNC = 4
} DSL_MEMORY_MOVEMENT_ENGINE;

enum {
    DSL_MEMORY_TIER_FLAG_NONE = 0,
    DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT = 0x00000001,
    DSL_MEMORY_TIER_FLAG_CACHE = 0x00000002,
    DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED = 0x00000004,
    DSL_MEMORY_TIER_FLAG_SPILLABLE = 0x00000008,
    DSL_MEMORY_TIER_FLAG_PROFILE_ASSUMPTION = 0x00000010
};

enum {
    DSL_MEMORY_MOVEMENT_FLAG_NONE = 0,
    DSL_MEMORY_MOVEMENT_FLAG_ASYNC = 0x00000001,
    DSL_MEMORY_MOVEMENT_FLAG_REQUIRES_BARRIER = 0x00000002,
    DSL_MEMORY_MOVEMENT_FLAG_MULTIDIMENSIONAL = 0x00000004,
    DSL_MEMORY_MOVEMENT_FLAG_PROFILE_ASSUMPTION = 0x00000008
};

typedef struct {
    UINT32 id;
    const char *name;
    UINT32 tier_count;
    UINT32 flags;
} DSL_MEMORY_HIERARCHY_PROFILE;

typedef struct {
    DSL_MEMORY_TIER_ID id;
    UINT32 kind;
    UINT32 scope;
    UINT64 capacity_bytes;
    UINT64 allocation_granularity;
    UINT32 minimum_alignment;
    UINT32 latency_class;
    UINT32 flags;
    UINT32 reserved;
} DSL_MEMORY_TIER_RECORD;

typedef struct {
    UINT32 bank_count;
    UINT32 word_bytes;
    UINT32 warp_lanes;
} DSL_SHARED_BANK_MODEL_RECORD;

/* Copy the built-in 32-bank, four-byte, 32-lane model for Hopper or
 * Blackwell. CPU, unknown profile or null output returns FALSE without
 * changing caller storage; no runtime measurement or bank-cost choice. */
extern BOOL DSL_memory_hierarchy_shared_bank_model
                                (UINT32 profile_id,
                                 DSL_SHARED_BANK_MODEL_RECORD *model);

typedef struct {
    DSL_MEMORY_MOVEMENT_CAPABILITY_ID id;
    UINT32 engine;
    UINT32 source_tier_kind;
    UINT32 destination_tier_kind;
    UINT32 transaction_bytes;
    UINT32 minimum_alignment;
    UINT32 maximum_stages;
    UINT32 latency_class;
    UINT32 flags;
    UINT32 reserved;
} DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD;

/* Copy a recognized family ID, borrowed static name and tier count to caller
 * storage; zero the successful output's remaining fields. Null output or
 * unknown profile returns FALSE without mutation or target inference. */
extern BOOL DSL_memory_hierarchy_get_profile
                                (UINT32 profile_id,
                                 DSL_MEMORY_HIERARCHY_PROFILE *profile);
/* Copy one immutable tier selected by a one-based ID within the exact
 * family. Null output, zero/out-of-range ID or unknown family preserves
 * caller output and returns FALSE; capacity unknown remains explicit. */
extern BOOL DSL_memory_hierarchy_get_tier
                                (UINT32 profile_id,
                                 DSL_MEMORY_TIER_ID tier_id,
                                 DSL_MEMORY_TIER_RECORD *tier);
/* Copy the first tier of the requested kind in the exact built-in family.
 * Null output, unknown family or absent kind returns FALSE without changing
 * caller storage; no nearest-tier fallback or allocation. */
extern BOOL DSL_memory_hierarchy_find_tier
                                (UINT32 profile_id, UINT32 kind,
                                 DSL_MEMORY_TIER_RECORD *tier);
/* Return the built-in family movement-row count, or zero for an unknown
 * profile. This read-only query does not probe devices or select an engine. */
extern UINT32 DSL_memory_hierarchy_movement_count (UINT32 profile_id);
/* Copy a one-based movement capability from the exact family inventory. Null
 * output, invalid ID or unknown profile returns FALSE without changing
 * caller storage; the row is not lowering permission. */
extern BOOL DSL_memory_hierarchy_get_movement
                                (UINT32 profile_id,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_ID id,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD *record);
/* Copy the first capability matching the engine in the exact family. Missing
 * engine/profile or null output returns FALSE and preserves output; success
 * does not prove instruction or synchronization legality. */
extern BOOL DSL_memory_hierarchy_find_movement
                                (UINT32 profile_id, UINT32 engine,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD *record);
/* Check immutable tier IDs/kinds/scopes, required allocation fields and
 * uniqueness, then movement IDs/fields/flags and endpoint-tier existence.
 * Return FALSE without repair on rejection; tier failures may diagnose to
 * the borrowed stream, while some movement failures return FALSE silently. */
extern BOOL DSL_memory_hierarchy_validate
                                (UINT32 profile_id, FILE *diagnostic);
/* Print the recognized family's tiers and movements to a borrowed stream,
 * spelling unknown capacity explicitly. Null stream or unknown family emits
 * nothing. Output is sequential, not atomic; no I/O status is returned. */
extern void DSL_memory_hierarchy_print
                                (FILE *file, UINT32 profile_id);
/* Return a borrowed static family label, or "unknown" for an out-of-range
 * value. The label neither validates a device nor changes a description. */
extern const char *DSL_target_profile_name (UINT32 profile_id);
/* Return the borrowed static tier-kind label, or "unknown" outside the
 * vocabulary. This read-only spelling query performs no tier lookup. */
extern const char *DSL_memory_tier_name (UINT32 kind);
/* Return a borrowed static allocation-scope label, or "unknown" outside the
 * vocabulary; spelling confers no allocation or ownership rights. */
extern const char *DSL_memory_scope_name (UINT32 scope);
/* Return the borrowed static engine label, or "unknown" outside the
 * vocabulary. Naming an engine does not certify its executable
 * implementation. */
extern const char *DSL_memory_movement_name (UINT32 engine);

#endif /* dsl_memory_hierarchy_INCLUDED */
