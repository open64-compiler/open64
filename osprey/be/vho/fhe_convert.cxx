/*
 * Copyright (C) 2026 Open64 Project
 */

#include <algorithm>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <string>
#include <vector>

#include "fhe_convert.h"
#include "config_fhe.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "errors.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "tracing.h"
#include "wn.h"

static VHO_FHE_SEMANTIC_GATEKEEPER VHO_FHE_semantic_gatekeeper;
static VHO_FHE_CONVERSION_PASS VHO_FHE_conversion_pass;
static VHO_FHE_CHECKPOINT_FINALIZER VHO_FHE_checkpoint_finalizer;
static VHO_FHE_CHECKPOINT_COMPLETION VHO_FHE_checkpoint_completion;

typedef struct {
    std::string temporary_path;
    std::string final_path;
    BOOL published;
} VHO_FHE_CHECKPOINT_ARTIFACT;

static std::vector<VHO_FHE_CHECKPOINT_ARTIFACT>
    VHO_FHE_checkpoint_artifacts;
static std::string VHO_FHE_checkpoint_binary_temporary_path;
static std::string VHO_FHE_checkpoint_binary_final_path;
static BOOL VHO_FHE_checkpoint_active;
static BOOL VHO_FHE_checkpoint_finalized;

static BOOL
VHO_FHE_Checkpoint_Artifact_Order
        (const VHO_FHE_CHECKPOINT_ARTIFACT &left,
         const VHO_FHE_CHECKPOINT_ARTIFACT &right)
{
    return left.final_path < right.final_path;
}

static void
VHO_FHE_Convert_Checkpoint_Clear_State (void)
{
    VHO_FHE_checkpoint_artifacts.clear();
    VHO_FHE_checkpoint_binary_temporary_path.clear();
    VHO_FHE_checkpoint_binary_final_path.clear();
    VHO_FHE_checkpoint_active = FALSE;
    VHO_FHE_checkpoint_finalizer = NULL;
    VHO_FHE_checkpoint_completion = NULL;
    VHO_FHE_checkpoint_finalized = FALSE;
}

void
VHO_FHE_Convert_Result_Init (VHO_FHE_CONVERT_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

void
VHO_FHE_Convert_Result_Accumulate
        (VHO_FHE_CONVERT_RESULT *aggregate,
         const VHO_FHE_CONVERT_RESULT *result)
{
    if (aggregate == NULL || result == NULL)
        return;
    aggregate->semantic_gatekeeper_count += result->semantic_gatekeeper_count;
    aggregate->conversion_pass_count += result->conversion_pass_count;
    aggregate->source_disposition_count += result->source_disposition_count;
    aggregate->converted_disposition_count +=
        result->converted_disposition_count;
    aggregate->rewritten_value_count += result->rewritten_value_count;
    aggregate->folded_batch_norm_count += result->folded_batch_norm_count;
    aggregate->approximation_contract_count +=
        result->approximation_contract_count;
    aggregate->error_count += result->error_count;
}

BOOL
VHO_FHE_Convert_Checkpoint_Validate
        (UINT32 expected_pu_count,
         UINT32 converted_pu_count,
         const VHO_FHE_CONVERT_RESULT *aggregate,
         FILE *diagnostic)
{
    BOOL valid = TRUE;

    if (aggregate == NULL || expected_pu_count == 0 ||
        converted_pu_count != expected_pu_count) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-001: converted PU count %u does not "
                    "match expected count %u\n",
                    converted_pu_count, expected_pu_count);
        valid = FALSE;
    } else if (aggregate->error_count != 0) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-002: conversion reported %u errors\n",
                    aggregate->error_count);
        valid = FALSE;
    }

    if (!DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Effect_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_FHE_Image_Validate(diagnostic) ||
        !DSL_FHE_Plan_Image_Validate(diagnostic)) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-003: complete managed image is invalid\n");
        valid = FALSE;
    }
    return valid;
}

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

BOOL
VHO_FHE_Convert_Register_Checkpoint_Lifecycle
        (VHO_FHE_CHECKPOINT_FINALIZER finalizer,
         VHO_FHE_CHECKPOINT_COMPLETION completion)
{
    if (finalizer == NULL || completion == NULL ||
        VHO_FHE_checkpoint_finalizer != NULL ||
        VHO_FHE_checkpoint_completion != NULL ||
        !VHO_FHE_checkpoint_artifacts.empty() ||
        VHO_FHE_checkpoint_finalized)
        return FALSE;
    VHO_FHE_checkpoint_finalizer = finalizer;
    VHO_FHE_checkpoint_completion = completion;
    return TRUE;
}

BOOL
VHO_FHE_Convert_Checkpoint_Begin
        (const char *temporary_binary_path,
         const char *final_binary_path,
         FILE *diagnostic)
{
    if (VHO_FHE_checkpoint_active || VHO_FHE_checkpoint_finalized ||
        !VHO_FHE_checkpoint_artifacts.empty() ||
        temporary_binary_path == NULL ||
        temporary_binary_path[0] == '\0' || final_binary_path == NULL ||
        final_binary_path[0] == '\0' ||
        strcmp(temporary_binary_path, final_binary_path) == 0) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-006: invalid checkpoint output "
                    "reservation\n");
        return FALSE;
    }

    errno = 0;
    if (access(final_binary_path, F_OK) == 0 || errno != ENOENT) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-006: checkpoint destination already "
                    "exists or cannot be inspected: %s\n",
                    final_binary_path);
        return FALSE;
    }

    VHO_FHE_checkpoint_binary_temporary_path = temporary_binary_path;
    VHO_FHE_checkpoint_binary_final_path = final_binary_path;
    VHO_FHE_checkpoint_active = TRUE;
    return TRUE;
}

BOOL
VHO_FHE_Convert_Checkpoint_Register_Artifact
        (const char *temporary_path, const char *final_path)
{
    if (VHO_FHE_checkpoint_finalizer == NULL ||
        !VHO_FHE_checkpoint_active || VHO_FHE_checkpoint_finalized ||
        temporary_path == NULL ||
        temporary_path[0] == '\0' || final_path == NULL ||
        final_path[0] == '\0' || strcmp(temporary_path, final_path) == 0)
        return FALSE;
    if (VHO_FHE_checkpoint_binary_temporary_path == temporary_path ||
        VHO_FHE_checkpoint_binary_temporary_path == final_path ||
        VHO_FHE_checkpoint_binary_final_path == temporary_path ||
        VHO_FHE_checkpoint_binary_final_path == final_path)
        return FALSE;
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        const VHO_FHE_CHECKPOINT_ARTIFACT &artifact =
            VHO_FHE_checkpoint_artifacts[i];
        if (artifact.temporary_path == temporary_path ||
            artifact.temporary_path == final_path ||
            artifact.final_path == temporary_path ||
            artifact.final_path == final_path)
            return FALSE;
    }

    VHO_FHE_CHECKPOINT_ARTIFACT artifact;
    artifact.temporary_path = temporary_path;
    artifact.final_path = final_path;
    artifact.published = FALSE;
    VHO_FHE_checkpoint_artifacts.push_back(artifact);
    return TRUE;
}

UINT32
VHO_FHE_Convert_Checkpoint_Artifact_Count (void)
{
    return (UINT32)VHO_FHE_checkpoint_artifacts.size();
}

BOOL
VHO_FHE_Convert_Checkpoint_Finalize
        (const VHO_FHE_CONVERT_RESULT *aggregate, FILE *diagnostic)
{
    if (!VHO_FHE_checkpoint_active || aggregate == NULL ||
        VHO_FHE_checkpoint_finalized) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-004: invalid checkpoint finalization "
                    "state\n");
        return FALSE;
    }
    if (VHO_FHE_checkpoint_finalizer != NULL &&
        !VHO_FHE_checkpoint_finalizer(aggregate, diagnostic)) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-004: registered checkpoint finalizer "
                    "failed\n");
        return FALSE;
    }
    VHO_FHE_checkpoint_finalized = TRUE;
    return TRUE;
}

static BOOL
VHO_FHE_Checkpoint_Publish_No_Replace
        (VHO_FHE_CHECKPOINT_ARTIFACT *artifact, FILE *diagnostic)
{
    sigset_t all_signals;
    sigset_t previous_signals;
    if (sigfillset(&all_signals) != 0 ||
        sigprocmask(SIG_BLOCK, &all_signals, &previous_signals) != 0) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-005: could not protect auxiliary "
                    "artifact publication: %s\n", strerror(errno));
        return FALSE;
    }

    BOOL published = FALSE;
    INT publish_error = 0;
    if (link(artifact->temporary_path.c_str(),
             artifact->final_path.c_str()) != 0) {
        publish_error = errno;
    }
    else {
        artifact->published = TRUE;
        if (unlink(artifact->temporary_path.c_str()) != 0)
            publish_error = errno;
        else
            published = TRUE;
    }

    INT restore_error = 0;
    if (sigprocmask(SIG_SETMASK, &previous_signals, NULL) != 0)
        restore_error = errno;
    if (!published || restore_error != 0) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-005: could not publish auxiliary "
                    "artifact %s without replacement: %s\n",
                    artifact->final_path.c_str(),
                    strerror(restore_error != 0 ? restore_error :
                             publish_error));
        return FALSE;
    }
    return TRUE;
}

BOOL
VHO_FHE_Convert_Checkpoint_Publish_Artifacts (FILE *diagnostic)
{
    if (!VHO_FHE_checkpoint_active || !VHO_FHE_checkpoint_finalized) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "CFHE-CHECKPOINT-005: auxiliary artifacts were not "
                    "finalized\n");
        return FALSE;
    }

    std::sort(VHO_FHE_checkpoint_artifacts.begin(),
              VHO_FHE_checkpoint_artifacts.end(),
              VHO_FHE_Checkpoint_Artifact_Order);
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        VHO_FHE_CHECKPOINT_ARTIFACT &artifact =
            VHO_FHE_checkpoint_artifacts[i];
        errno = 0;
        if (access(artifact.temporary_path.c_str(), F_OK) != 0 ||
            access(artifact.final_path.c_str(), F_OK) == 0 ||
            errno != ENOENT) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "CFHE-CHECKPOINT-005: auxiliary artifact is not "
                        "ready for publication: %s -> %s\n",
                        artifact.temporary_path.c_str(),
                        artifact.final_path.c_str());
            return FALSE;
        }
    }
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        VHO_FHE_CHECKPOINT_ARTIFACT &artifact =
            VHO_FHE_checkpoint_artifacts[i];
        if (!VHO_FHE_Checkpoint_Publish_No_Replace
                 (&artifact, diagnostic))
            return FALSE;
    }
    return TRUE;
}

void
VHO_FHE_Convert_Checkpoint_Complete (void)
{
    VHO_FHE_CHECKPOINT_COMPLETION completion =
        VHO_FHE_checkpoint_completion;
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        if (!VHO_FHE_checkpoint_artifacts[i].published)
            remove(VHO_FHE_checkpoint_artifacts[i].temporary_path.c_str());
    }
    VHO_FHE_Convert_Checkpoint_Clear_State();
    if (completion != NULL)
        completion(TRUE);
}

void
VHO_FHE_Convert_Checkpoint_Abort (void)
{
    VHO_FHE_CHECKPOINT_COMPLETION completion =
        VHO_FHE_checkpoint_completion;
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        VHO_FHE_CHECKPOINT_ARTIFACT &artifact =
            VHO_FHE_checkpoint_artifacts[i];
        remove(artifact.temporary_path.c_str());
        if (artifact.published)
            remove(artifact.final_path.c_str());
    }
    VHO_FHE_Convert_Checkpoint_Clear_State();
    if (completion != NULL)
        completion(FALSE);
}

void
VHO_FHE_Convert_Reset_Passes (void)
{
    VHO_FHE_Convert_Checkpoint_Abort();
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

BOOL
VHO_FHE_Convert_Driver_Try
        (struct pu_info *pu_info,
         WN **tree,
         VHO_FHE_CONVERT_RESULT *result)
{
    if (tree != NULL && *tree != NULL &&
        VHO_FHE_Dump_Before_Conversion && TFile != NULL) {
        fprintf(TFile,
                "\n\n========== WHIRL before VHO FHE Conversion "
                "==========\n");
        fdump_tree(TFile, *tree);
        fflush(TFile);
    }

    VHO_FHE_CONVERT_RESULT local_result;
    BOOL valid = VHO_FHE_Convert_Program_Unit
                     (pu_info, tree, stderr, &local_result);
    if (result != NULL)
        *result = local_result;

    if (valid && VHO_FHE_Dump_After_Conversion && TFile != NULL) {
        fprintf(TFile,
                "\n\n========== WHIRL after VHO FHE Conversion "
                "==========\n");
        fdump_tree(TFile, *tree);
        fflush(TFile);
    }
    return valid;
}

WN *
VHO_FHE_Convert_Driver_With_Result
        (struct pu_info *pu_info,
         WN *tree,
         VHO_FHE_CONVERT_RESULT *result)
{
    BOOL valid = VHO_FHE_Convert_Driver_Try(pu_info, &tree, result);
    FmtAssert(valid, ("FHE VHO conversion failed"));
    return tree;
}

WN *
VHO_FHE_Convert_Driver
        (struct pu_info *pu_info,
         WN *tree)
{
    return VHO_FHE_Convert_Driver_With_Result(pu_info, tree, NULL);
}
