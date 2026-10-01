/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE schedule materialization runs in the active PU scope.  Common/com owns
 * fixed records and structural validation; registered VHO callbacks own
 * provider policy and the actual logical materialization decision.
 */

#include <string.h>

#include "fhe_materialize.h"
#include "fhe_checkpoint.h"
#include "config_fhe.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "errors.h"
#include "pu_info.h"
#include "wn.h"

static VHO_FHE_MATERIALIZATION_GATEKEEPER
    VHO_FHE_materialization_gatekeeper;
static VHO_FHE_MATERIALIZATION_PASS VHO_FHE_materialization_pass;
static VHO_FHE_MATERIALIZATION_FINALIZER
    VHO_FHE_materialization_finalizer;
static VHO_FHE_MATERIALIZATION_COMPLETION
    VHO_FHE_materialization_completion;

static BOOL
VHO_FHE_Materialize_Report
        (FILE *diagnostic, VHO_FHE_MATERIALIZE_RESULT *result,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHEMAT-001: %s\n", message);
    if (result != NULL)
        ++result->error_count;
    return FALSE;
}

static BOOL
VHO_FHE_Materialize_SHA256_Valid (const char *sha256)
{
    if (sha256 == NULL || strlen(sha256) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!((sha256[i] >= '0' && sha256[i] <= '9') ||
              (sha256[i] >= 'a' && sha256[i] <= 'f')))
            return FALSE;
    }
    return TRUE;
}

static BOOL
VHO_FHE_Materialize_Options_Valid
        (FILE *diagnostic, VHO_FHE_MATERIALIZE_RESULT *result)
{
    const char *mode = VHO_FHE_Bootstrap_Mode;
    const char *path = VHO_FHE_Provider_Manifest_Path;
    const char *sha256 = VHO_FHE_Provider_Manifest_SHA256;
    BOOL has_path = path != NULL && path[0] != '\0';
    BOOL has_sha256 = sha256 != NULL && sha256[0] != '\0';

    if (mode == NULL ||
        (strcmp(mode, "auto") != 0 && strcmp(mode, "on") != 0 &&
         strcmp(mode, "manual") != 0 && strcmp(mode, "off") != 0))
        return VHO_FHE_Materialize_Report
                   (diagnostic, result,
                    "bootstrap mode must be auto, on, manual, or off");
    if (has_path != has_sha256)
        return VHO_FHE_Materialize_Report
                   (diagnostic, result,
                    "provider manifest and SHA-256 must be specified "
                    "together");
    if ((strcmp(mode, "auto") == 0 || strcmp(mode, "on") == 0) &&
        !has_path)
        return VHO_FHE_Materialize_Report
                   (diagnostic, result,
                    "auto/on materialization requires an authenticated "
                    "provider manifest");
    if (has_sha256 && !VHO_FHE_Materialize_SHA256_Valid(sha256))
        return VHO_FHE_Materialize_Report
                   (diagnostic, result,
                    "provider manifest SHA-256 must contain exactly 64 "
                    "lowercase hexadecimal characters");
    return TRUE;
}

static BOOL
VHO_FHE_Materialize_Structural_Gatekeeper
        (struct pu_info *pu_info, FILE *diagnostic,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    DSL_GATEKEEPER_RESULT dsl_result;
    BOOL valid = DSL_Gatekeeper_Verify_PU
                     (pu_info, diagnostic, &dsl_result);
    if (!DSL_FHE_Image_Validate(diagnostic) ||
        !DSL_FHE_Plan_Image_Validate_Partial(diagnostic) ||
        !DSL_FHE_Approx_Profile_Image_Validate(diagnostic) ||
        !DSL_FHE_Context_State_Image_Validate(diagnostic) ||
        !DSL_FHE_Materialization_Image_Validate_Partial(diagnostic))
        valid = FALSE;
    if (!valid && result != NULL)
        ++result->error_count;
    return valid;
}

void
VHO_FHE_Materialize_Result_Init (VHO_FHE_MATERIALIZE_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

void
VHO_FHE_Materialize_Result_Accumulate
        (VHO_FHE_MATERIALIZE_RESULT *aggregate,
         const VHO_FHE_MATERIALIZE_RESULT *result)
{
    if (aggregate == NULL || result == NULL)
        return;
    aggregate->semantic_gatekeeper_count += result->semantic_gatekeeper_count;
    aggregate->materialization_pass_count +=
        result->materialization_pass_count;
    aggregate->context_count += result->context_count;
    aggregate->operation_count += result->operation_count;
    aggregate->refresh_count += result->refresh_count;
    aggregate->error_count += result->error_count;
}

BOOL
VHO_FHE_Materialize_Register_Semantic_Gatekeeper
        (VHO_FHE_MATERIALIZATION_GATEKEEPER gatekeeper)
{
    if (gatekeeper == NULL || VHO_FHE_materialization_gatekeeper != NULL)
        return FALSE;
    VHO_FHE_materialization_gatekeeper = gatekeeper;
    return TRUE;
}

BOOL
VHO_FHE_Materialize_Register_Pass (VHO_FHE_MATERIALIZATION_PASS pass)
{
    if (pass == NULL || VHO_FHE_materialization_pass != NULL)
        return FALSE;
    VHO_FHE_materialization_pass = pass;
    return TRUE;
}

static BOOL
VHO_FHE_Materialize_Finalizer_Adapter
        (const void *aggregate, FILE *diagnostic)
{
    return VHO_FHE_materialization_finalizer != NULL &&
           VHO_FHE_materialization_finalizer
               ((const VHO_FHE_MATERIALIZE_RESULT *)aggregate, diagnostic);
}

static void
VHO_FHE_Materialize_Completion_Adapter (BOOL committed)
{
    VHO_FHE_MATERIALIZATION_COMPLETION completion =
        VHO_FHE_materialization_completion;
    VHO_FHE_materialization_finalizer = NULL;
    VHO_FHE_materialization_completion = NULL;
    if (completion != NULL)
        completion(committed);
}

BOOL
VHO_FHE_Materialize_Register_Checkpoint_Lifecycle
        (VHO_FHE_MATERIALIZATION_FINALIZER finalizer,
         VHO_FHE_MATERIALIZATION_COMPLETION completion)
{
    if (finalizer == NULL || completion == NULL ||
        VHO_FHE_materialization_finalizer != NULL ||
        VHO_FHE_materialization_completion != NULL)
        return FALSE;
    VHO_FHE_materialization_finalizer = finalizer;
    VHO_FHE_materialization_completion = completion;
    return TRUE;
}

void
VHO_FHE_Materialize_Reset_Passes (void)
{
    VHO_FHE_Checkpoint_Abort();
    VHO_FHE_materialization_finalizer = NULL;
    VHO_FHE_materialization_completion = NULL;
    VHO_FHE_materialization_gatekeeper = NULL;
    VHO_FHE_materialization_pass = NULL;
}

BOOL
VHO_FHE_Materialize_Program_Unit
        (struct pu_info *pu_info, WN **tree, FILE *diagnostic,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    VHO_FHE_MATERIALIZE_RESULT local_result;
    VHO_FHE_MATERIALIZE_OPTIONS options;
    VHO_FHE_Materialize_Result_Init(&local_result);
    options.bootstrap_mode = VHO_FHE_Bootstrap_Mode;
    options.provider_manifest_path = VHO_FHE_Provider_Manifest_Path;
    options.provider_manifest_sha256 = VHO_FHE_Provider_Manifest_SHA256;

    if (pu_info == NULL || tree == NULL || *tree == NULL) {
        VHO_FHE_Materialize_Report
            (diagnostic, &local_result,
             "program unit or WHIRL tree is missing");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (!VHO_FHE_Enable_Materialization) {
        if (result != NULL)
            *result = local_result;
        return TRUE;
    }
    if (!VHO_FHE_Materialize_Options_Valid(diagnostic, &local_result) ||
        !VHO_FHE_Materialize_Structural_Gatekeeper
             (pu_info, diagnostic, &local_result)) {
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (VHO_FHE_materialization_gatekeeper == NULL ||
        VHO_FHE_materialization_pass == NULL) {
        VHO_FHE_Materialize_Report
            (diagnostic, &local_result,
             "FHE materialization support is not registered");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_materialization_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Materialize_Report
            (diagnostic, &local_result,
             "FHE materialization gatekeeper rejected input WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    ++local_result.materialization_pass_count;
    if (!VHO_FHE_materialization_pass
             (pu_info, tree, &options, diagnostic, &local_result) ||
        *tree == NULL) {
        VHO_FHE_Materialize_Report
            (diagnostic, &local_result,
             "FHE materialization pass failed");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    Set_PU_Info_tree_ptr(pu_info, *tree);

    if (!VHO_FHE_Materialize_Structural_Gatekeeper
             (pu_info, diagnostic, &local_result)) {
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    ++local_result.semantic_gatekeeper_count;
    if (!VHO_FHE_materialization_gatekeeper
             (pu_info, *tree, &options, diagnostic)) {
        VHO_FHE_Materialize_Report
            (diagnostic, &local_result,
             "FHE materialization produced semantically invalid WHIRL");
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    if (result != NULL)
        *result = local_result;
    return TRUE;
}

BOOL
VHO_FHE_Materialize_Driver_Try
        (struct pu_info *pu_info, WN **tree,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    return VHO_FHE_Materialize_Program_Unit
               (pu_info, tree, stderr, result);
}

WN *
VHO_FHE_Materialize_Driver_With_Result
        (struct pu_info *pu_info, WN *tree,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    BOOL valid = VHO_FHE_Materialize_Driver_Try(pu_info, &tree, result);
    FmtAssert(valid, ("FHE VHO materialization failed"));
    return tree;
}

BOOL
VHO_FHE_Materialize_Checkpoint_Validate
        (UINT32 expected_pu_count, UINT32 materialized_pu_count,
         const VHO_FHE_MATERIALIZE_RESULT *aggregate, FILE *diagnostic)
{
    BOOL valid = aggregate != NULL && expected_pu_count != 0 &&
                 materialized_pu_count == expected_pu_count &&
                 aggregate->error_count == 0;
    if (!valid && diagnostic != NULL)
        fprintf(diagnostic,
                "CFHEMAT-CHECKPOINT-001: materialized PU count %u does not "
                "match expected count %u or errors were reported\n",
                materialized_pu_count, expected_pu_count);
    if (!DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Effect_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_FHE_Image_Validate(diagnostic) ||
        !DSL_FHE_Plan_Image_Validate(diagnostic) ||
        !DSL_FHE_Approx_Profile_Image_Validate(diagnostic) ||
        !DSL_FHE_Context_State_Image_Validate(diagnostic) ||
        !DSL_FHE_Materialization_Image_Validate(diagnostic)) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHEMAT-CHECKPOINT-003: complete managed image is "
                    "invalid\n");
        valid = FALSE;
    }
    return valid;
}

BOOL
VHO_FHE_Materialize_Checkpoint_Begin
        (const char *temporary_binary_path, const char *final_binary_path,
         FILE *diagnostic)
{
    if (VHO_FHE_materialization_finalizer == NULL ||
        VHO_FHE_materialization_completion == NULL ||
        !VHO_FHE_Checkpoint_Register_Lifecycle
             (VHO_FHE_Materialize_Finalizer_Adapter,
              VHO_FHE_Materialize_Completion_Adapter))
        return FALSE;
    if (!VHO_FHE_Checkpoint_Begin
             (temporary_binary_path, final_binary_path,
              "CFHEMAT-CHECKPOINT", diagnostic)) {
        VHO_FHE_Checkpoint_Abort();
        return FALSE;
    }
    return TRUE;
}

BOOL
VHO_FHE_Materialize_Checkpoint_Register_Artifact
        (const char *temporary_path, const char *final_path)
{
    return VHO_FHE_Checkpoint_Register_Artifact
               (temporary_path, final_path);
}

UINT32
VHO_FHE_Materialize_Checkpoint_Artifact_Count (void)
{
    return VHO_FHE_Checkpoint_Artifact_Count();
}

BOOL
VHO_FHE_Materialize_Checkpoint_Finalize
        (const VHO_FHE_MATERIALIZE_RESULT *aggregate, FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Finalize(aggregate, diagnostic);
}

BOOL
VHO_FHE_Materialize_Checkpoint_Publish_Artifacts (FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Publish_Artifacts(diagnostic);
}

BOOL
VHO_FHE_Materialize_Checkpoint_Publish_Binary (FILE *diagnostic)
{
    return VHO_FHE_Checkpoint_Publish_Binary(diagnostic);
}

void
VHO_FHE_Materialize_Checkpoint_Complete (void)
{
    VHO_FHE_Checkpoint_Complete();
}

void
VHO_FHE_Materialize_Checkpoint_Abort (void)
{
    VHO_FHE_Checkpoint_Abort();
}
