/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "fhe_convert.h"
#include "config_fhe.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "errors.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "tracing.h"
#include "wn.h"

static VHO_FHE_SEMANTIC_GATEKEEPER VHO_FHE_semantic_gatekeeper;
static VHO_FHE_CONVERSION_PASS VHO_FHE_conversion_pass;

static BOOL
VHO_FHE_Convert_Report
        (FILE *diagnostic,
         VHO_FHE_CONVERT_RESULT *result,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHE-CONVERT-001: %s\n", message);
    if (result != NULL)
        ++result->error_count;
    return FALSE;
}

static BOOL
VHO_FHE_Convert_Structural_Gatekeeper
        (struct pu_info *pu_info,
         FILE *diagnostic,
         VHO_FHE_CONVERT_RESULT *result)
{
    DSL_GATEKEEPER_RESULT dsl_result;
    BOOL valid = DSL_Gatekeeper_Verify_PU
                     (pu_info, diagnostic, &dsl_result);
    if (!DSL_FHE_Image_Validate(diagnostic))
        valid = FALSE;
    if (!DSL_FHE_Plan_Image_Validate(diagnostic))
        valid = FALSE;
    if (!valid && result != NULL)
        ++result->error_count;
    return valid;
}

BOOL
VHO_FHE_Convert_Register_Semantic_Gatekeeper
        (VHO_FHE_SEMANTIC_GATEKEEPER gatekeeper)
{
    if (gatekeeper == NULL || VHO_FHE_semantic_gatekeeper != NULL)
        return FALSE;
    VHO_FHE_semantic_gatekeeper = gatekeeper;
    return TRUE;
}

BOOL
VHO_FHE_Convert_Register_Pass (VHO_FHE_CONVERSION_PASS pass)
{
    if (pass == NULL || VHO_FHE_conversion_pass != NULL)
        return FALSE;
    VHO_FHE_conversion_pass = pass;
    return TRUE;
}

void
VHO_FHE_Convert_Reset_Passes (void)
{
    VHO_FHE_semantic_gatekeeper = NULL;
    VHO_FHE_conversion_pass = NULL;
}

BOOL
VHO_FHE_Convert_Program_Unit
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic,
         VHO_FHE_CONVERT_RESULT *result)
{
    VHO_FHE_CONVERT_RESULT local_result;
    VHO_FHE_CONVERT_OPTIONS options;
    memset(&local_result, 0, sizeof(local_result));
    options.strict_o0 = VHO_FHE_Strict_O0;

    if (pu_info == NULL || tree == NULL || *tree == NULL) {
        VHO_FHE_Convert_Report
            (diagnostic, &local_result,
             "program unit or WHIRL tree is missing");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    if (!VHO_FHE_Enable_Conversion ||
        (!DSL_FHE_Image_Has_Records() &&
         !DSL_FHE_Plan_Image_Has_Records())) {
        if (result != NULL)
            *result = local_result;
        return TRUE;
    }

    if (!VHO_FHE_Convert_Structural_Gatekeeper
             (pu_info, diagnostic, &local_result)) {
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (VHO_FHE_semantic_gatekeeper == NULL ||
        VHO_FHE_conversion_pass == NULL) {
        VHO_FHE_Convert_Report
            (diagnostic, &local_result,
             "FHE semantic conversion support is not registered");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_semantic_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Convert_Report
            (diagnostic, &local_result,
             "FHE semantic gatekeeper rejected input WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.conversion_pass_count;
    if (!VHO_FHE_conversion_pass
             (pu_info, tree, &options, diagnostic, &local_result) ||
        *tree == NULL) {
        VHO_FHE_Convert_Report
            (diagnostic, &local_result,
             "FHE semantic conversion failed");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    Set_PU_Info_tree_ptr(pu_info, *tree);

    if (!VHO_FHE_Convert_Structural_Gatekeeper
             (pu_info, diagnostic, &local_result)) {
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_semantic_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Convert_Report
            (diagnostic, &local_result,
             "FHE conversion produced semantically invalid WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    if (result != NULL)
        *result = local_result;
    return TRUE;
}

WN *
VHO_FHE_Convert_Driver
        (struct pu_info *pu_info,
         WN *tree)
{
    if (VHO_FHE_Dump_Before_Conversion && TFile != NULL) {
        fprintf(TFile,
                "\n\n========== WHIRL before VHO FHE Conversion "
                "==========\n");
        fdump_tree(TFile, tree);
        fflush(TFile);
    }

    VHO_FHE_CONVERT_RESULT result;
    BOOL valid = VHO_FHE_Convert_Program_Unit
                     (pu_info, &tree, stderr, &result);
    FmtAssert(valid, ("FHE VHO conversion failed"));

    if (VHO_FHE_Dump_After_Conversion && TFile != NULL) {
        fprintf(TFile,
                "\n\n========== WHIRL after VHO FHE Conversion "
                "==========\n");
        fdump_tree(TFile, tree);
        fflush(TFile);
    }
    return tree;
}
