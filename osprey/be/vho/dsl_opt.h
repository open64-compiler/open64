/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_opt_INCLUDED
#define dsl_opt_INCLUDED

#include <stdio.h>

#include "defs.h"

class WN;
struct pu_info;

/* Fixed execution order.  Append new stages; never reorder released stages. */
typedef enum {
    VHO_DSL_OPT_CANONICALIZATION = 0,
    VHO_DSL_OPT_DESCRIPTOR_PROPAGATION = 1,
    VHO_DSL_OPT_CONSTANT_PROPAGATION = 2,
    VHO_DSL_OPT_ALGEBRAIC_SIMPLIFICATION = 3,
    VHO_DSL_OPT_DEAD_RESULT_ELIMINATION = 4,
    VHO_DSL_OPT_COMMON_SUBEXPRESSION = 5,
    VHO_DSL_OPT_SSA_PRE = 6,
    VHO_DSL_OPT_QUANTIZATION = 7,
    VHO_DSL_OPT_FUSION = 8,
    VHO_DSL_OPT_PARALLELIZATION = 9,
    VHO_DSL_OPT_IMPLEMENTATION_SELECTION = 10,
    VHO_DSL_OPT_STAGE_COUNT = 11
} VHO_DSL_OPT_STAGE;

typedef BOOL (*VHO_DSL_OPT_PASS)
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 FILE *diagnostic);

typedef struct {
    UINT32 enabled_stage_count;
    UINT32 executed_stage_count;
    UINT32 missing_stage_count;
    VHO_DSL_OPT_STAGE failed_stage;
} VHO_DSL_OPT_RESULT;

extern const char *VHO_DSL_Opt_Stage_Name (VHO_DSL_OPT_STAGE stage);
extern BOOL VHO_DSL_Opt_Stage_Enabled (VHO_DSL_OPT_STAGE stage);
extern BOOL VHO_DSL_Opt_Register_Pass
                                (VHO_DSL_OPT_STAGE stage,
                                 VHO_DSL_OPT_PASS pass);
extern void VHO_DSL_Opt_Reset_Passes (void);
extern BOOL VHO_DSL_Optimize_Program_Unit
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 FILE *diagnostic,
                                 VHO_DSL_OPT_RESULT *result);

#endif /* dsl_opt_INCLUDED */
