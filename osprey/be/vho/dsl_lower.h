/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_lower_INCLUDED
#define dsl_lower_INCLUDED

#include <stdio.h>

#include "defs.h"

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
} VHO_DSL_LOWER_RESULT;

/* DSL-specific peer of the existing language-oriented VHO_Lower_Driver. */
extern WN *VHO_DSL_Lower_Driver (struct pu_info *pu_info, WN *tree);

/* Testable engine for a tree that has already passed the DSL gatekeeper. */
extern BOOL VHO_DSL_Lower_Verified_Program_Unit
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic,
                                 VHO_DSL_LOWER_RESULT *result);

/* Retained DSL comments are inspection evidence, not executable carriers. */
extern BOOL VHO_DSL_Lowered_Tree_Is_Canonical
                                (WN *tree,
                                 FILE *diagnostic,
                                 UINT32 *remaining_carrier_count);

#endif /* dsl_lower_INCLUDED */
