/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-8 target-independent memory hierarchy with immutable typed target
 * adapters. These runtime planning profiles add no WHIRL image state.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO8-RESIDENCY.md.
 */

#ifndef dsl_memory_hierarchy_INCLUDED
#define dsl_memory_hierarchy_INCLUDED

#include <stdio.h>

#include "defs.h"

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

extern BOOL DSL_memory_hierarchy_get_profile
                                (UINT32 profile_id,
                                 DSL_MEMORY_HIERARCHY_PROFILE *profile);
extern BOOL DSL_memory_hierarchy_get_tier
                                (UINT32 profile_id,
                                 DSL_MEMORY_TIER_ID tier_id,
                                 DSL_MEMORY_TIER_RECORD *tier);
extern BOOL DSL_memory_hierarchy_find_tier
                                (UINT32 profile_id, UINT32 kind,
                                 DSL_MEMORY_TIER_RECORD *tier);
extern UINT32 DSL_memory_hierarchy_movement_count (UINT32 profile_id);
extern BOOL DSL_memory_hierarchy_get_movement
                                (UINT32 profile_id,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_ID id,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD *record);
extern BOOL DSL_memory_hierarchy_find_movement
                                (UINT32 profile_id, UINT32 engine,
                                 DSL_MEMORY_MOVEMENT_CAPABILITY_RECORD *record);
extern BOOL DSL_memory_hierarchy_validate
                                (UINT32 profile_id, FILE *diagnostic);
extern void DSL_memory_hierarchy_print
                                (FILE *file, UINT32 profile_id);
extern const char *DSL_target_profile_name (UINT32 profile_id);
extern const char *DSL_memory_tier_name (UINT32 kind);
extern const char *DSL_memory_scope_name (UINT32 scope);
extern const char *DSL_memory_movement_name (UINT32 engine);

#endif /* dsl_memory_hierarchy_INCLUDED */
