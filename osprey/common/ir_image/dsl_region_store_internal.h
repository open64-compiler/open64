/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Private transient REGION views shared by the image-store owner and be/com
 * physical services. The single authoritative registry is static in
 * common/ir_image/dsl_region_image.cxx; these are definitions/hidden lookup
 * bridges, never per-TU state or .B records. Stores own runtime wrappers and
 * interface vectors, while PU/WN/string/symbol lifetimes remain external.
 * The existing dsl_region_runtime tag is retained for opaque-handle identity.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md.
 */

#ifndef DSL_REGION_STORE_INTERNAL_INCLUDED
#define DSL_REGION_STORE_INTERNAL_INCLUDED

#include <vector>
#include "../../be/com/dsl_region.h"

struct dsl_region_runtime {
    DSL_REGION_IMAGE_RECORD image;
    WN *wn;
};

typedef struct {
    PU_Info *pu;
    std::vector<dsl_region_runtime *> regions;
    std::vector<DSL_REGION_INTERFACE_RECORD> interfaces;
} DSL_REGION_STORE;

/*
 * Find the first PU-pointer match in the sole image-owned registry without
 * creating or activating a PU. Return a borrowed store or NULL; its lifetime
 * ends on reset, discard, or consumption of its last region.
 */
extern DSL_REGION_STORE *
DSL_Region_Find_Store (PU_Info *pu)
    __attribute__((visibility("hidden")));

/*
 * Return an existing borrowed store, or NULL when absent and creation is
 * disabled. Creation requires a valid PU, registers an empty owned store,
 * clears its overlapping regions pointer and marks WT_REGIONS in memory;
 * it neither owns the PU/tree nor catches allocation failures.
 */
extern DSL_REGION_STORE *
DSL_Region_Get_Store (PU_Info *pu, BOOL create)
    __attribute__((visibility("hidden")));

/*
 * Find the registered store containing this exact borrowed runtime handle.
 * NULL or no match returns NULL without dereferencing the handle or mutating
 * the registry; returned storage remains owned by the image service.
 */
extern DSL_REGION_STORE *
DSL_Region_Find_Owning_Store (DSL_REGION region)
    __attribute__((visibility("hidden")));

#endif /* DSL_REGION_STORE_INTERNAL_INCLUDED */
