/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_lower_INCLUDED
#define dsl_lower_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_physical_plan_opt.h"

class WN;
struct pu_info;

typedef struct {
    UINT32 native_node_count;
    UINT32 compatibility_node_count;
    UINT32 lowered_node_count;
    UINT32 unsupported_node_count;
    UINT32 malformed_node_count;
    UINT32 remaining_executable_carrier_count;
    UINT32 state_read_count;
    UINT32 state_modify_count;
    UINT32 physical_plan_site_count;
    UINT32 physical_provider_site_count;
    UINT32 physical_provider_lowered_count;
} VHO_DSL_LOWER_RESULT;

/* DSL-specific peer of the existing language-oriented VHO_Lower_Driver. */
extern WN *VHO_DSL_Lower_Driver (struct pu_info *pu_info, WN *tree);

/* The driver owns the constructed device PUs and their program-boundary image. */
extern UINT32 VHO_DSL_CUDA_Program_Kernel_Count(void);
extern BOOL VHO_DSL_CUDA_Take_Program_Kernel
    (UINT32 ordinal, struct pu_info **device_pu, UINT32 *plan_id,
     UINT32 *tile_id, ST_IDX *owner,
     FILE *diagnostic);

extern BOOL VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile_Bound
    (struct pu_info *pu_info, WN **tree,
     const DSL_TILE_PLAN_RECORD *tile, UINT32 device_entry_ordinal,
     FILE *diagnostic, VHO_DSL_LOWER_RESULT *result);

/* Testable engine for a tree that has already passed the DSL gatekeeper. */
extern BOOL VHO_DSL_Lower_Verified_Program_Unit
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic,
                                 VHO_DSL_LOWER_RESULT *result);
/*
 * Apply a reviewed physical plan to a copied tree. The owning PU and caller's
 * tree pointer change only after lowering and canonical verification succeed.
 */
extern BOOL VHO_DSL_Lower_Verified_Program_Unit_With_Physical_Plan
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 const DSL_PHYSICAL_PLAN_ANALYSIS
                                     *physical_plan,
                                 FILE *diagnostic,
                                 VHO_DSL_LOWER_RESULT *result);

/*
 * Apply one proven generated F32 tile to a copied PU. The caller must own the
 * selected AIO-9 record and matching emitted kernel source. Failure leaves
 * the original tree and PU binding unchanged.
 */
extern BOOL VHO_DSL_Lower_Verified_Program_Unit_With_Generated_Tile
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 const DSL_TILE_PLAN_RECORD *tile,
                                 FILE *diagnostic,
                                 VHO_DSL_LOWER_RESULT *result);

/* Retained DSL comments are inspection evidence, not executable carriers. */
extern BOOL VHO_DSL_Lowered_Tree_Is_Canonical
                                (WN *tree,
                                 FILE *diagnostic,
                                 UINT32 *remaining_carrier_count);

#endif /* dsl_lower_INCLUDED */
