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

typedef enum {
    VHO_DSL_SHAPE_TRIGGER_NONE = 0,
    VHO_DSL_SHAPE_TRIGGER_PU_ADMISSION = 1,
    VHO_DSL_SHAPE_TRIGGER_PU_IDENTITY_CHANGE = 2,
    VHO_DSL_SHAPE_TRIGGER_SEED_REFINEMENT = 3,
    VHO_DSL_SHAPE_TRIGGER_OPERATOR_CONSTRAINT_CHANGE = 4,
    VHO_DSL_SHAPE_TRIGGER_VALUE_RELATIONSHIP_CHANGE = 5,
    VHO_DSL_SHAPE_TRIGGER_STRUCTURAL_TRANSFORMATION = 6,
    VHO_DSL_SHAPE_TRIGGER_PU_REGION_RESTRUCTURING = 7,
    VHO_DSL_SHAPE_TRIGGER_SYMBOLIC_RESOLUTION = 8,
    VHO_DSL_SHAPE_TRIGGER_DSL_WOPT = 9,
    VHO_DSL_SHAPE_TRIGGER_FHE_CONVERSION = 10,
    VHO_DSL_SHAPE_TRIGGER_VHO_DSL_OPTIMIZATION = 11,
    VHO_DSL_SHAPE_TRIGGER_COUNT = 12
} VHO_DSL_SHAPE_TRIGGER;

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
                                 VHO_DSL_SHAPE_TRIGGER trigger,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Shape_Refinement_Is_Current
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic);
extern const char *VHO_DSL_Shape_Trigger_Name
                                (VHO_DSL_SHAPE_TRIGGER trigger);

#endif /* dsl_shape_refine_INCLUDED */
