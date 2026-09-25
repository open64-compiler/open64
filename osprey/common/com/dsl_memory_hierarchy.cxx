/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_memory_hierarchy.h"

static const char *DSL_target_profile_name[] = {
    "unknown", "cpu_baseline", "nvidia_hopper", "nvidia_blackwell"
};

static const char *DSL_memory_tier_name[] = {
    "unknown", "system", "pinned_host", "hbm", "l2", "shared",
    "register", "remote"
};

static const char *DSL_memory_scope_name[] = {
    "unknown", "system", "device", "sm", "cta", "thread"
};

static const DSL_MEMORY_TIER_RECORD DSL_cpu_tiers[] = {
    { 1, DSL_MEMORY_TIER_SYSTEM, DSL_MEMORY_SCOPE_SYSTEM,
      DSL_MEMORY_CAPACITY_UNKNOWN, 64, 64, 24,
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 2, DSL_MEMORY_TIER_L2, DSL_MEMORY_SCOPE_DEVICE,
      1024ULL * 1024ULL, 64, 64, 6,
      DSL_MEMORY_TIER_FLAG_CACHE |
      DSL_MEMORY_TIER_FLAG_PROFILE_ASSUMPTION, 0 },
    { 3, DSL_MEMORY_TIER_REGISTER, DSL_MEMORY_SCOPE_THREAD,
      256, 8, 8, 1,
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED |
      DSL_MEMORY_TIER_FLAG_PROFILE_ASSUMPTION, 0 }
};

static const DSL_MEMORY_TIER_RECORD DSL_hopper_tiers[] = {
    { 1, DSL_MEMORY_TIER_SYSTEM, DSL_MEMORY_SCOPE_SYSTEM,
      DSL_MEMORY_CAPACITY_UNKNOWN, 64, 64, 24,
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 2, DSL_MEMORY_TIER_PINNED_HOST, DSL_MEMORY_SCOPE_SYSTEM,
      DSL_MEMORY_CAPACITY_UNKNOWN, 64, 64, 18,
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED |
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 3, DSL_MEMORY_TIER_HBM, DSL_MEMORY_SCOPE_DEVICE,
      80ULL * 1024ULL * 1024ULL * 1024ULL, 256, 256, 10,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED |
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 4, DSL_MEMORY_TIER_L2, DSL_MEMORY_SCOPE_DEVICE,
      50ULL * 1024ULL * 1024ULL, 128, 128, 5,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_CACHE, 0 },
    { 5, DSL_MEMORY_TIER_SHARED, DSL_MEMORY_SCOPE_CTA,
      227ULL * 1024ULL, 256, 16, 2,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED, 0 },
    { 6, DSL_MEMORY_TIER_REGISTER, DSL_MEMORY_SCOPE_THREAD,
      255ULL * 4ULL, 4, 4, 1,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED, 0 }
};

static const DSL_MEMORY_TIER_RECORD DSL_blackwell_tiers[] = {
    { 1, DSL_MEMORY_TIER_SYSTEM, DSL_MEMORY_SCOPE_SYSTEM,
      DSL_MEMORY_CAPACITY_UNKNOWN, 64, 64, 24,
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 2, DSL_MEMORY_TIER_PINNED_HOST, DSL_MEMORY_SCOPE_SYSTEM,
      DSL_MEMORY_CAPACITY_UNKNOWN, 64, 64, 18,
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED |
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 3, DSL_MEMORY_TIER_HBM, DSL_MEMORY_SCOPE_DEVICE,
      180ULL * 1024ULL * 1024ULL * 1024ULL, 256, 256, 9,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED |
      DSL_MEMORY_TIER_FLAG_SPILLABLE, 0 },
    { 4, DSL_MEMORY_TIER_L2, DSL_MEMORY_SCOPE_DEVICE,
      126ULL * 1024ULL * 1024ULL, 128, 128, 4,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_CACHE, 0 },
    { 5, DSL_MEMORY_TIER_SHARED, DSL_MEMORY_SCOPE_CTA,
      227ULL * 1024ULL, 256, 16, 2,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED, 0 },
    { 6, DSL_MEMORY_TIER_REGISTER, DSL_MEMORY_SCOPE_THREAD,
      255ULL * 4ULL, 4, 4, 1,
      DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT |
      DSL_MEMORY_TIER_FLAG_SOFTWARE_MANAGED, 0 }
};

static BOOL
DSL_Memory_Hierarchy_Rows
        (UINT32 profile_id, const DSL_MEMORY_TIER_RECORD **rows,
         UINT32 *count)
{
    if (rows == NULL || count == NULL)
        return FALSE;
    switch (profile_id) {
    case DSL_TARGET_PROFILE_CPU_BASELINE:
        *rows = DSL_cpu_tiers;
        *count = sizeof(DSL_cpu_tiers) / sizeof(DSL_cpu_tiers[0]);
        return TRUE;
    case DSL_TARGET_PROFILE_NVIDIA_HOPPER:
        *rows = DSL_hopper_tiers;
        *count = sizeof(DSL_hopper_tiers) / sizeof(DSL_hopper_tiers[0]);
        return TRUE;
    case DSL_TARGET_PROFILE_NVIDIA_BLACKWELL:
        *rows = DSL_blackwell_tiers;
        *count = sizeof(DSL_blackwell_tiers) /
                 sizeof(DSL_blackwell_tiers[0]);
        return TRUE;
    default:
        return FALSE;
    }
}

const char *
DSL_Target_Profile_Name (UINT32 profile_id)
{
    return profile_id < sizeof(DSL_target_profile_name) /
                            sizeof(DSL_target_profile_name[0]) ?
           DSL_target_profile_name[profile_id] : "unknown";
}

const char *
DSL_Memory_Tier_Name (UINT32 kind)
{
    return kind < sizeof(DSL_memory_tier_name) /
                      sizeof(DSL_memory_tier_name[0]) ?
           DSL_memory_tier_name[kind] : "unknown";
}

const char *
DSL_Memory_Scope_Name (UINT32 scope)
{
    return scope < sizeof(DSL_memory_scope_name) /
                       sizeof(DSL_memory_scope_name[0]) ?
           DSL_memory_scope_name[scope] : "unknown";
}

BOOL
DSL_Memory_Hierarchy_Get_Profile
        (UINT32 profile_id, DSL_MEMORY_HIERARCHY_PROFILE *profile)
{
    const DSL_MEMORY_TIER_RECORD *rows;
    UINT32 count;
    if (profile == NULL ||
        !DSL_Memory_Hierarchy_Rows(profile_id, &rows, &count))
        return FALSE;
    memset(profile, 0, sizeof(*profile));
    profile->id = profile_id;
    profile->name = DSL_Target_Profile_Name(profile_id);
    profile->tier_count = count;
    return TRUE;
}

BOOL
DSL_Memory_Hierarchy_Get_Tier
        (UINT32 profile_id, DSL_MEMORY_TIER_ID tier_id,
         DSL_MEMORY_TIER_RECORD *tier)
{
    const DSL_MEMORY_TIER_RECORD *rows;
    UINT32 count;
    if (tier == NULL || tier_id == 0 ||
        !DSL_Memory_Hierarchy_Rows(profile_id, &rows, &count) ||
        tier_id > count)
        return FALSE;
    *tier = rows[tier_id - 1];
    return TRUE;
}

BOOL
DSL_Memory_Hierarchy_Find_Tier
        (UINT32 profile_id, UINT32 kind, DSL_MEMORY_TIER_RECORD *tier)
{
    const DSL_MEMORY_TIER_RECORD *rows;
    UINT32 count;
    if (tier == NULL ||
        !DSL_Memory_Hierarchy_Rows(profile_id, &rows, &count))
        return FALSE;
    for (UINT32 i = 0; i < count; ++i) {
        if (rows[i].kind == kind) {
            *tier = rows[i];
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_Memory_Hierarchy_Validate (UINT32 profile_id, FILE *diagnostic)
{
    const DSL_MEMORY_TIER_RECORD *rows;
    UINT32 count;
    if (!DSL_Memory_Hierarchy_Rows(profile_id, &rows, &count)) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL memory hierarchy error: unknown profile id=%u\n",
                    profile_id);
        return FALSE;
    }
    for (UINT32 i = 0; i < count; ++i) {
        const DSL_MEMORY_TIER_RECORD &tier = rows[i];
        if (tier.id != i + 1 || tier.kind == DSL_MEMORY_TIER_UNKNOWN ||
            tier.kind > DSL_MEMORY_TIER_REMOTE ||
            tier.scope == DSL_MEMORY_SCOPE_UNKNOWN ||
            tier.scope > DSL_MEMORY_SCOPE_THREAD ||
            tier.allocation_granularity == 0 ||
            tier.minimum_alignment == 0 || tier.latency_class == 0 ||
            tier.reserved != 0 ||
            ((tier.flags & DSL_MEMORY_TIER_FLAG_CAPACITY_EXACT) != 0 &&
             tier.capacity_bytes == DSL_MEMORY_CAPACITY_UNKNOWN)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL memory hierarchy error: invalid tier id=%u\n",
                        tier.id);
            return FALSE;
        }
        for (UINT32 j = 0; j < i; ++j) {
            if (rows[j].kind == tier.kind) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "DSL memory hierarchy error: duplicate tier "
                            "kind=%u\n", tier.kind);
                return FALSE;
            }
        }
    }
    return TRUE;
}

void
DSL_Memory_Hierarchy_Print (FILE *file, UINT32 profile_id)
{
    DSL_MEMORY_HIERARCHY_PROFILE profile;
    if (file == NULL ||
        !DSL_Memory_Hierarchy_Get_Profile(profile_id, &profile))
        return;
    fprintf(file, "MemoryHierarchyDescriptorIR: profile=%u name=%s "
                  "tiers=%u\n", profile.id, profile.name,
            profile.tier_count);
    for (UINT32 i = 1; i <= profile.tier_count; ++i) {
        DSL_MEMORY_TIER_RECORD tier;
        if (!DSL_Memory_Hierarchy_Get_Tier(profile_id, i, &tier))
            return;
        fprintf(file, "  tier[%u] kind=%s scope=%s capacity=", tier.id,
                DSL_Memory_Tier_Name(tier.kind),
                DSL_Memory_Scope_Name(tier.scope));
        if (tier.capacity_bytes == DSL_MEMORY_CAPACITY_UNKNOWN)
            fprintf(file, "<unknown>");
        else
            fprintf(file, "%llu", (unsigned long long)tier.capacity_bytes);
        fprintf(file, " granularity=%llu alignment=%u latency_class=%u "
                      "flags=0x%x\n",
                (unsigned long long)tier.allocation_granularity,
                tier.minimum_alignment, tier.latency_class, tier.flags);
    }
}
