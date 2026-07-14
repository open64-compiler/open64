/*
 * Contract test for fixed-order, independently controlled DSL VHO passes.
 */

#include <stdio.h>

#include "dsl_opt.h"
#include "config.h"
#include "config_dsl.h"
#include "erglob.h"
#include "errors.h"
#include "mempool.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

static VHO_DSL_OPT_STAGE observed[8];
static UINT32 observed_count;

static BOOL
Observe_Canonicalization(struct pu_info *pu_info, WN **tree, FILE *diagnostic)
{
    observed[observed_count++] = VHO_DSL_OPT_CANONICALIZATION;
    return TRUE;
}

static BOOL
Observe_Fusion(struct pu_info *pu_info, WN **tree, FILE *diagnostic)
{
    observed[observed_count++] = VHO_DSL_OPT_FUSION;
    return TRUE;
}

static BOOL
Observe_Implementation_Selection
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic)
{
    observed[observed_count++] = VHO_DSL_OPT_IMPLEMENTATION_SELECTION;
    return TRUE;
}

static void
Disable_All_Stages(void)
{
    VHO_DSL_Enable_Canonicalization = FALSE;
    VHO_DSL_Enable_Descriptor_Propagation = FALSE;
    VHO_DSL_Enable_Constant_Propagation = FALSE;
    VHO_DSL_Enable_Algebraic_Simplification = FALSE;
    VHO_DSL_Enable_Dead_Result_Elimination = FALSE;
    VHO_DSL_Enable_Common_Subexpression = FALSE;
    VHO_DSL_Enable_SSA_PRE = FALSE;
    VHO_DSL_Enable_Quantization = FALSE;
    VHO_DSL_Enable_Fusion = FALSE;
    VHO_DSL_Enable_Parallelization = FALSE;
    VHO_DSL_Enable_Implementation_Selection = FALSE;
}

int
main(void)
{
    struct pu_info *pu_info = (struct pu_info *)1;
    WN *tree = (WN *)1;
    VHO_DSL_OPT_RESULT result;

    Disable_All_Stages();
    VHO_DSL_Opt_Reset_Passes();
    if (!VHO_DSL_Optimize_Program_Unit
             (pu_info, &tree, NULL, &result) ||
        result.enabled_stage_count != 0 ||
        result.executed_stage_count != 0) {
        fprintf(stderr, "disabled DSL VHO pipeline changed behavior\n");
        return 1;
    }

    if (!VHO_DSL_Opt_Register_Pass
             (VHO_DSL_OPT_CANONICALIZATION, Observe_Canonicalization) ||
        !VHO_DSL_Opt_Register_Pass
             (VHO_DSL_OPT_FUSION, Observe_Fusion) ||
        !VHO_DSL_Opt_Register_Pass
             (VHO_DSL_OPT_IMPLEMENTATION_SELECTION,
              Observe_Implementation_Selection) ||
        VHO_DSL_Opt_Register_Pass
             (VHO_DSL_OPT_FUSION, Observe_Fusion)) {
        fprintf(stderr, "DSL VHO pass registration contract changed\n");
        return 1;
    }

    VHO_DSL_Enable_Canonicalization = TRUE;
    VHO_DSL_Enable_Fusion = TRUE;
    VHO_DSL_Enable_Implementation_Selection = TRUE;
    observed_count = 0;
    if (!VHO_DSL_Optimize_Program_Unit
             (pu_info, &tree, NULL, &result) ||
        result.enabled_stage_count != 3 ||
        result.executed_stage_count != 3 ||
        observed_count != 3 ||
        observed[0] != VHO_DSL_OPT_CANONICALIZATION ||
        observed[1] != VHO_DSL_OPT_FUSION ||
        observed[2] != VHO_DSL_OPT_IMPLEMENTATION_SELECTION) {
        fprintf(stderr, "DSL VHO pipeline order changed\n");
        return 1;
    }

    VHO_DSL_Enable_Fusion = FALSE;
    observed_count = 0;
    if (!VHO_DSL_Optimize_Program_Unit
             (pu_info, &tree, NULL, &result) ||
        result.enabled_stage_count != 2 ||
        result.executed_stage_count != 2 ||
        observed_count != 2 ||
        observed[0] != VHO_DSL_OPT_CANONICALIZATION ||
        observed[1] != VHO_DSL_OPT_IMPLEMENTATION_SELECTION) {
        fprintf(stderr, "DSL VHO per-stage disable control changed\n");
        return 1;
    }

    VHO_DSL_Enable_Quantization = TRUE;
    observed_count = 0;
    if (VHO_DSL_Optimize_Program_Unit
            (pu_info, &tree, NULL, &result) ||
        result.missing_stage_count != 1 ||
        result.failed_stage != VHO_DSL_OPT_QUANTIZATION ||
        observed_count != 1) {
        fprintf(stderr, "unimplemented enabled stage was silently ignored\n");
        return 1;
    }

    Disable_All_Stages();
    VHO_DSL_Opt_Reset_Passes();
    return 0;
}
