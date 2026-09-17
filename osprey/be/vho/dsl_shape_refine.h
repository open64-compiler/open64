/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_shape_refine_INCLUDED
#define dsl_shape_refine_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_shape.h"

class WN;
struct pu_info;

typedef struct {
    DSL_SHAPE_SOLVER_RESULT solver;
    UINT32 requested_value_count;
    UINT32 created_type_count;
    UINT32 reused_type_count;
    UINT32 retyped_value_count;
    UINT32 updated_st_count;
    UINT32 updated_wn_count;
    UINT32 rollback_count;
    UINT32 diagnostic_count;
} VHO_DSL_SHAPE_REFINE_RESULT;

extern BOOL VHO_DSL_Shape_Refine_Program_Unit
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 BOOL enable_refinement,
                                 FILE *diagnostic,
                                 VHO_DSL_SHAPE_REFINE_RESULT *result);
extern WN *VHO_DSL_Shape_Refine_Driver
                                (struct pu_info *pu_info, WN *tree);
extern BOOL VHO_DSL_Shape_Refinement_Invalidate
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 const char *reason,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Shape_Refinement_Is_Current
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic);

#endif /* dsl_shape_refine_INCLUDED */
