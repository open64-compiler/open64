/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_opt.h"
#include "config.h"
#include "config_dsl.h"

static VHO_DSL_OPT_PASS VHO_DSL_opt_pass[VHO_DSL_OPT_STAGE_COUNT];

static const char *VHO_DSL_opt_stage_name[VHO_DSL_OPT_STAGE_COUNT] = {
    "canonicalization",
    "descriptor_propagation",
    "constant_propagation",
    "algebraic_simplification",
    "dead_result_elimination",
    "common_subexpression",
    "ssa_pre",
    "quantization",
    "fusion",
    "parallelization",
    "implementation_selection"
};

const char *
VHO_DSL_Opt_Stage_Name (VHO_DSL_OPT_STAGE stage)
{
    return stage >= VHO_DSL_OPT_CANONICALIZATION &&
           stage < VHO_DSL_OPT_STAGE_COUNT ?
           VHO_DSL_opt_stage_name[stage] : "unknown";
}

BOOL
VHO_DSL_Opt_Stage_Enabled (VHO_DSL_OPT_STAGE stage)
{
    switch (stage) {
    case VHO_DSL_OPT_CANONICALIZATION:
        return VHO_DSL_Enable_Canonicalization;
    case VHO_DSL_OPT_DESCRIPTOR_PROPAGATION:
        return VHO_DSL_Enable_Descriptor_Propagation;
    case VHO_DSL_OPT_CONSTANT_PROPAGATION:
        return VHO_DSL_Enable_Constant_Propagation;
    case VHO_DSL_OPT_ALGEBRAIC_SIMPLIFICATION:
        return VHO_DSL_Enable_Algebraic_Simplification;
    case VHO_DSL_OPT_DEAD_RESULT_ELIMINATION:
        return VHO_DSL_Enable_Dead_Result_Elimination;
    case VHO_DSL_OPT_COMMON_SUBEXPRESSION:
        return VHO_DSL_Enable_Common_Subexpression;
    case VHO_DSL_OPT_SSA_PRE:
        return VHO_DSL_Enable_SSA_PRE;
    case VHO_DSL_OPT_QUANTIZATION:
        return VHO_DSL_Enable_Quantization;
    case VHO_DSL_OPT_FUSION:
        return VHO_DSL_Enable_Fusion;
    case VHO_DSL_OPT_PARALLELIZATION:
        return VHO_DSL_Enable_Parallelization;
    case VHO_DSL_OPT_IMPLEMENTATION_SELECTION:
        return VHO_DSL_Enable_Implementation_Selection;
    default:
        return FALSE;
    }
}

BOOL
VHO_DSL_Opt_Register_Pass
        (VHO_DSL_OPT_STAGE stage,
         VHO_DSL_OPT_PASS pass)
{
    if (stage < VHO_DSL_OPT_CANONICALIZATION ||
        stage >= VHO_DSL_OPT_STAGE_COUNT ||
        pass == NULL || VHO_DSL_opt_pass[stage] != NULL)
        return FALSE;
    VHO_DSL_opt_pass[stage] = pass;
    return TRUE;
}

void
VHO_DSL_Opt_Reset_Passes (void)
{
    memset(VHO_DSL_opt_pass, 0, sizeof(VHO_DSL_opt_pass));
}

BOOL
VHO_DSL_Optimize_Program_Unit
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic,
         VHO_DSL_OPT_RESULT *result)
{
    VHO_DSL_OPT_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    local_result.failed_stage = VHO_DSL_OPT_STAGE_COUNT;

    if (pu_info == NULL || tree == NULL || *tree == NULL)
        return FALSE;

    for (UINT32 ordinal = 0; ordinal < VHO_DSL_OPT_STAGE_COUNT; ++ordinal) {
        VHO_DSL_OPT_STAGE stage = (VHO_DSL_OPT_STAGE)ordinal;
        if (!VHO_DSL_Opt_Stage_Enabled(stage))
            continue;

        ++local_result.enabled_stage_count;
        if (VHO_DSL_opt_pass[stage] == NULL) {
            ++local_result.missing_stage_count;
            local_result.failed_stage = stage;
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL VHO optimization stage %s is enabled but has "
                        "no implementation\n",
                        VHO_DSL_Opt_Stage_Name(stage));
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }

        if (!VHO_DSL_opt_pass[stage](pu_info, tree, diagnostic)) {
            local_result.failed_stage = stage;
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }
        ++local_result.executed_stage_count;
    }

    if (result != NULL)
        *result = local_result;
    return TRUE;
}
