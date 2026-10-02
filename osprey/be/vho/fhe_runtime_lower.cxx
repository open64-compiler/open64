/*
 * Copyright (C) 2026 Open64 Project
 *
 * Callback-only FHE runtime lowering.  Runtime ABI and schedule decisions are
 * supplied by the registered FHE consumer, never by this shared phase shell.
 */

#include <string.h>

#include "fhe_runtime_lower.h"
#include "fhe_checkpoint.h"
#include "fhe_unlowered_gate.h"
#include "config_fhe.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "errors.h"
#include "pu_info.h"
#include "wn.h"

static VHO_FHE_RUNTIME_LOWER_GATEKEEPER VHO_FHE_runtime_lower_gatekeeper;
static VHO_FHE_RUNTIME_LOWER_PASS VHO_FHE_runtime_lower_pass;
static VHO_FHE_RUNTIME_LOWER_FINALIZER VHO_FHE_runtime_lower_finalizer;
static VHO_FHE_RUNTIME_LOWER_COMPLETION VHO_FHE_runtime_lower_completion;

static BOOL
VHO_FHE_Runtime_Lower_Report
        (FILE *diagnostic, VHO_FHE_RUNTIME_LOWER_RESULT *result,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHELOWER-002: %s\n", message);
    if (result != NULL)
        ++result->error_count;
    return FALSE;
}

static BOOL
VHO_FHE_Runtime_Lower_Structural_Gate
        (struct pu_info *pu_info, FILE *diagnostic,
         VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    DSL_GATEKEEPER_RESULT dsl_result;
    BOOL valid = DSL_Gatekeeper_Verify_PU_Mode
                     (pu_info, DSL_GATEKEEPER_PROJECTED,
                      diagnostic, &dsl_result);
    if (!DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Effect_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_Runtime_Interface_Image_Validate(diagnostic) ||
        !DSL_Program_Interface_Image_Validate(diagnostic) ||
        !DSL_FHE_Image_Validate(diagnostic) ||
        !DSL_FHE_Plan_Image_Validate(diagnostic) ||
        !DSL_FHE_Approx_Profile_Image_Validate(diagnostic) ||
        !DSL_FHE_Context_State_Image_Validate(diagnostic) ||
        !DSL_FHE_Materialization_Image_Validate(diagnostic))
        valid = FALSE;
    if (!valid && result != NULL)
        ++result->error_count;
    return valid;
}

void
VHO_FHE_Runtime_Lower_Result_Init (VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

void
VHO_FHE_Runtime_Lower_Result_Accumulate
        (VHO_FHE_RUNTIME_LOWER_RESULT *aggregate,
         const VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    if (aggregate == NULL || result == NULL)
        return;
    aggregate->semantic_gatekeeper_count += result->semantic_gatekeeper_count;
    aggregate->runtime_lowering_pass_count +=
        result->runtime_lowering_pass_count;
    aggregate->standard_call_count += result->standard_call_count;
    aggregate->output_handle_count += result->output_handle_count;
    aggregate->status_check_count += result->status_check_count;
    aggregate->error_count += result->error_count;
}

BOOL
VHO_FHE_Runtime_Lower_Register_Semantic_Gatekeeper
        (VHO_FHE_RUNTIME_LOWER_GATEKEEPER gatekeeper)
{
    if (gatekeeper == NULL || VHO_FHE_runtime_lower_gatekeeper != NULL)
        return FALSE;
    VHO_FHE_runtime_lower_gatekeeper = gatekeeper;
    return TRUE;
}

BOOL
VHO_FHE_Runtime_Lower_Register_Pass (VHO_FHE_RUNTIME_LOWER_PASS pass)
{
    if (pass == NULL || VHO_FHE_runtime_lower_pass != NULL)
        return FALSE;
    VHO_FHE_runtime_lower_pass = pass;
    return TRUE;
}

static BOOL
VHO_FHE_Runtime_Lower_Finalizer_Adapter
        (const void *aggregate, FILE *diagnostic)
{
    return VHO_FHE_runtime_lower_finalizer != NULL &&
           VHO_FHE_runtime_lower_finalizer
               ((const VHO_FHE_RUNTIME_LOWER_RESULT *)aggregate, diagnostic);
}

static void
VHO_FHE_Runtime_Lower_Completion_Adapter (BOOL committed)
{
    VHO_FHE_RUNTIME_LOWER_COMPLETION completion =
        VHO_FHE_runtime_lower_completion;
    VHO_FHE_runtime_lower_finalizer = NULL;
    VHO_FHE_runtime_lower_completion = NULL;
    if (completion != NULL)
        completion(committed);
}

BOOL
VHO_FHE_Runtime_Lower_Register_Checkpoint_Lifecycle
        (VHO_FHE_RUNTIME_LOWER_FINALIZER finalizer,
         VHO_FHE_RUNTIME_LOWER_COMPLETION completion)
{
    if (finalizer == NULL || completion == NULL ||
        VHO_FHE_runtime_lower_finalizer != NULL ||
        VHO_FHE_runtime_lower_completion != NULL)
        return FALSE;
    VHO_FHE_runtime_lower_finalizer = finalizer;
    VHO_FHE_runtime_lower_completion = completion;
    return TRUE;
}

void
VHO_FHE_Runtime_Lower_Reset_Passes (void)
{
    VHO_FHE_Checkpoint_Abort();
    VHO_FHE_runtime_lower_finalizer = NULL;
    VHO_FHE_runtime_lower_completion = NULL;
    VHO_FHE_runtime_lower_gatekeeper = NULL;
    VHO_FHE_runtime_lower_pass = NULL;
}

BOOL
VHO_FHE_Runtime_Lower_Program_Unit
        (struct pu_info *pu_info, WN **tree, FILE *diagnostic,
         VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    VHO_FHE_RUNTIME_LOWER_RESULT local_result;
    VHO_FHE_RUNTIME_LOWER_OPTIONS options;
    VHO_FHE_Runtime_Lower_Result_Init(&local_result);
    options.provider_manifest_path = VHO_FHE_Provider_Manifest_Path;
    options.provider_manifest_sha256 = VHO_FHE_Provider_Manifest_SHA256;
    options.checkpoint_output_path =
        VHO_FHE_Runtime_Lowering_Checkpoint_Output;

    if (pu_info == NULL || tree == NULL || *tree == NULL) {
        VHO_FHE_Runtime_Lower_Report
            (diagnostic, &local_result, "program unit or WHIRL tree is missing");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (!VHO_FHE_Enable_Runtime_Lowering) {
        if (result != NULL)
            *result = local_result;
        return TRUE;
    }
    if (!VHO_FHE_Runtime_Lower_Structural_Gate
             (pu_info, diagnostic, &local_result) ||
        VHO_FHE_runtime_lower_gatekeeper == NULL ||
        VHO_FHE_runtime_lower_pass == NULL) {
        if (VHO_FHE_runtime_lower_gatekeeper == NULL ||
            VHO_FHE_runtime_lower_pass == NULL)
            VHO_FHE_Runtime_Lower_Report
                (diagnostic, &local_result,
                 "FHE runtime lowering support is not registered");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_runtime_lower_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Runtime_Lower_Report
            (diagnostic, &local_result,
             "FHE runtime-lowering gatekeeper rejected input WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.runtime_lowering_pass_count;
    if (!VHO_FHE_runtime_lower_pass
             (pu_info, tree, &options, diagnostic, &local_result) ||
        *tree == NULL) {
        VHO_FHE_Runtime_Lower_Report
            (diagnostic, &local_result, "FHE runtime-lowering pass failed");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    Set_PU_Info_tree_ptr(pu_info, *tree);

    VHO_FHE_UNLOWERED_GATE_RESULT gate_result;
    if (!VHO_FHE_Unlowered_Gate_Program_Unit
             (pu_info, *tree, diagnostic, &gate_result)) {
        local_result.error_count += gate_result.error_count;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (!DSL_Program_Interface_Validate_Lowered_PU
             (pu_info, diagnostic)) {
        VHO_FHE_Runtime_Lower_Report
            (diagnostic, &local_result,
             "lowered program interface is inconsistent");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_runtime_lower_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Runtime_Lower_Report
            (diagnostic, &local_result,
             "FHE runtime lowering produced invalid standard WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    if (result != NULL)
        *result = local_result;
    return TRUE;
}

BOOL
VHO_FHE_Runtime_Lower_Driver_Try
        (struct pu_info *pu_info, WN **tree,
         VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    return VHO_FHE_Runtime_Lower_Program_Unit
               (pu_info, tree, stderr, result);
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Validate
        (UINT32 expected_pu_count, UINT32 lowered_pu_count,
         const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate, FILE *diagnostic)
{
    BOOL valid = aggregate != NULL && expected_pu_count != 0 &&
                 lowered_pu_count == expected_pu_count &&
                 aggregate->error_count == 0;
    if (!valid && diagnostic != NULL)
        fprintf(diagnostic,
                "CFHELOWER-CHECKPOINT-001: lowered PU count %u does not "
                "match expected count %u or errors were reported\n",
                lowered_pu_count, expected_pu_count);
    return valid;
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Begin
        (const char *temporary_binary_path, const char *final_binary_path,
         FILE *diagnostic)
{
    if (VHO_FHE_runtime_lower_finalizer == NULL ||
        VHO_FHE_runtime_lower_completion == NULL ||
        !VHO_FHE_Checkpoint_Register_Lifecycle
             (VHO_FHE_Runtime_Lower_Finalizer_Adapter,
              VHO_FHE_Runtime_Lower_Completion_Adapter))
        return FALSE;
    if (!VHO_FHE_Checkpoint_Begin
             (temporary_binary_path, final_binary_path,
              "CFHELOWER-CHECKPOINT", diagnostic)) {
        VHO_FHE_Checkpoint_Abort();
        return FALSE;
    }
    return TRUE;
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Register_Artifact
        (const char *temporary_path, const char *final_path)
{
    return VHO_FHE_Checkpoint_Register_Artifact
               (temporary_path, final_path);
}

UINT32
VHO_FHE_Runtime_Lower_Checkpoint_Artifact_Count (void)
{
    return VHO_FHE_Checkpoint_Artifact_Count();
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Finalize
        (const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate, FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Finalize(aggregate, diagnostic);
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Publish_Artifacts (FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Publish_Artifacts(diagnostic);
}

BOOL
VHO_FHE_Runtime_Lower_Checkpoint_Publish_Binary (FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Publish_Binary(diagnostic);
}

void
VHO_FHE_Runtime_Lower_Checkpoint_Complete (void)
{
    VHO_FHE_Checkpoint_Complete();
}

void
VHO_FHE_Runtime_Lower_Checkpoint_Abort (void)
{
    VHO_FHE_Checkpoint_Abort();
}
