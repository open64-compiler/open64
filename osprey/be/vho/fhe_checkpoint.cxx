/*
 * Copyright (C) 2026 Open64 Project
 *
 * Shared atomic checkpoint publication for conversion and materialization.
 * Phase-specific semantic validation remains in the owning VHO driver.
 */

#include <algorithm>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <string>
#include <vector>

#include "fhe_checkpoint.h"

typedef struct {
    std::string temporary_path;
    std::string final_path;
    BOOL published;
} VHO_FHE_CHECKPOINT_ARTIFACT;

static std::vector<VHO_FHE_CHECKPOINT_ARTIFACT>
    VHO_FHE_checkpoint_artifacts;
static std::string VHO_FHE_checkpoint_binary_temporary_path;
static std::string VHO_FHE_checkpoint_binary_final_path;
static std::string VHO_FHE_checkpoint_diagnostic_prefix;
static VHO_FHE_CHECKPOINT_GENERIC_FINALIZER VHO_FHE_checkpoint_finalizer;
static VHO_FHE_CHECKPOINT_GENERIC_COMPLETION VHO_FHE_checkpoint_completion;
static BOOL VHO_FHE_checkpoint_active;
static BOOL VHO_FHE_checkpoint_finalized;
static BOOL VHO_FHE_checkpoint_binary_published;

static const char *
VHO_FHE_Checkpoint_Diagnostic_Prefix (void)
{
    return VHO_FHE_checkpoint_diagnostic_prefix.empty() ?
               "CFHE-CHECKPOINT" :
               VHO_FHE_checkpoint_diagnostic_prefix.c_str();
}

static void
VHO_FHE_Checkpoint_Report
        (FILE *diagnostic, UINT32 code, const char *message,
         const char *path)
{
    if (diagnostic == NULL)
        return;
    fprintf(diagnostic, "%s-%03u: %s", VHO_FHE_Checkpoint_Diagnostic_Prefix(),
            code, message);
    if (path != NULL)
        fprintf(diagnostic, ": %s", path);
    fprintf(diagnostic, "\n");
}

static BOOL
VHO_FHE_Checkpoint_Artifact_Order
        (const VHO_FHE_CHECKPOINT_ARTIFACT &left,
         const VHO_FHE_CHECKPOINT_ARTIFACT &right)
{
    return left.final_path < right.final_path;
}

static void
VHO_FHE_Checkpoint_Clear_State (void)
{
    VHO_FHE_checkpoint_artifacts.clear();
    VHO_FHE_checkpoint_binary_temporary_path.clear();
    VHO_FHE_checkpoint_binary_final_path.clear();
    VHO_FHE_checkpoint_diagnostic_prefix.clear();
    VHO_FHE_checkpoint_finalizer = NULL;
    VHO_FHE_checkpoint_completion = NULL;
    VHO_FHE_checkpoint_active = FALSE;
    VHO_FHE_checkpoint_finalized = FALSE;
    VHO_FHE_checkpoint_binary_published = FALSE;
}

BOOL
VHO_FHE_Checkpoint_Register_Lifecycle
        (VHO_FHE_CHECKPOINT_GENERIC_FINALIZER finalizer,
         VHO_FHE_CHECKPOINT_GENERIC_COMPLETION completion)
{
    if (finalizer == NULL || completion == NULL ||
        VHO_FHE_checkpoint_finalizer != NULL ||
        VHO_FHE_checkpoint_completion != NULL ||
        !VHO_FHE_checkpoint_artifacts.empty() ||
        VHO_FHE_checkpoint_active || VHO_FHE_checkpoint_finalized)
        return FALSE;
    VHO_FHE_checkpoint_finalizer = finalizer;
    VHO_FHE_checkpoint_completion = completion;
    return TRUE;
}

BOOL
VHO_FHE_Checkpoint_Begin
        (const char *temporary_binary_path,
         const char *final_binary_path,
         const char *diagnostic_prefix,
         FILE *diagnostic)
{
    if (VHO_FHE_checkpoint_active || VHO_FHE_checkpoint_finalized ||
        !VHO_FHE_checkpoint_artifacts.empty() ||
        temporary_binary_path == NULL || temporary_binary_path[0] == '\0' ||
        final_binary_path == NULL || final_binary_path[0] == '\0' ||
        diagnostic_prefix == NULL || diagnostic_prefix[0] == '\0' ||
        strcmp(temporary_binary_path, final_binary_path) == 0) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 6, "invalid checkpoint output reservation", NULL);
        return FALSE;
    }

    VHO_FHE_checkpoint_diagnostic_prefix = diagnostic_prefix;
    errno = 0;
    if (access(final_binary_path, F_OK) == 0 || errno != ENOENT) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 6,
             "checkpoint destination already exists or cannot be inspected",
             final_binary_path);
        VHO_FHE_checkpoint_diagnostic_prefix.clear();
        return FALSE;
    }

    VHO_FHE_checkpoint_binary_temporary_path = temporary_binary_path;
    VHO_FHE_checkpoint_binary_final_path = final_binary_path;
    VHO_FHE_checkpoint_active = TRUE;
    return TRUE;
}

BOOL
VHO_FHE_Checkpoint_Register_Artifact
        (const char *temporary_path, const char *final_path)
{
    if (VHO_FHE_checkpoint_finalizer == NULL ||
        !VHO_FHE_checkpoint_active || VHO_FHE_checkpoint_finalized ||
        temporary_path == NULL || temporary_path[0] == '\0' ||
        final_path == NULL || final_path[0] == '\0' ||
        strcmp(temporary_path, final_path) == 0)
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
VHO_FHE_Checkpoint_Artifact_Count (void)
{
    return (UINT32)VHO_FHE_checkpoint_artifacts.size();
}

BOOL
VHO_FHE_Checkpoint_Finalize (const void *aggregate, FILE *diagnostic)
{
    if (!VHO_FHE_checkpoint_active || aggregate == NULL ||
        VHO_FHE_checkpoint_finalized) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 4, "invalid checkpoint finalization state", NULL);
        return FALSE;
    }
    if (VHO_FHE_checkpoint_finalizer != NULL &&
        !VHO_FHE_checkpoint_finalizer(aggregate, diagnostic)) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 4, "registered checkpoint finalizer failed", NULL);
        return FALSE;
    }
    VHO_FHE_checkpoint_finalized = TRUE;
    return TRUE;
}

static BOOL
VHO_FHE_Checkpoint_Publish_No_Replace
        (VHO_FHE_CHECKPOINT_ARTIFACT *artifact,
         FILE *diagnostic, const char *description)
{
    sigset_t all_signals;
    sigset_t previous_signals;
    if (sigfillset(&all_signals) != 0 ||
        sigprocmask(SIG_BLOCK, &all_signals, &previous_signals) != 0) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 5, "could not protect artifact publication", NULL);
        return FALSE;
    }

    BOOL published = FALSE;
    INT publish_error = 0;
    if (link(artifact->temporary_path.c_str(),
             artifact->final_path.c_str()) != 0) {
        publish_error = errno;
    } else {
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
            fprintf(diagnostic, "%s-005: could not publish %s %s without "
                    "replacement: %s\n",
                    VHO_FHE_Checkpoint_Diagnostic_Prefix(), description,
                    artifact->final_path.c_str(),
                    strerror(restore_error != 0 ? restore_error :
                             publish_error));
        return FALSE;
    }
    return TRUE;
}

BOOL
VHO_FHE_Checkpoint_Publish_Artifacts (FILE *diagnostic)
{
    if (!VHO_FHE_checkpoint_active || !VHO_FHE_checkpoint_finalized) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 5, "auxiliary artifacts were not finalized", NULL);
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
            VHO_FHE_Checkpoint_Report
                (diagnostic, 5,
                 "auxiliary artifact is not ready for publication",
                 artifact.final_path.c_str());
            return FALSE;
        }
    }
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        if (!VHO_FHE_Checkpoint_Publish_No_Replace
                 (&VHO_FHE_checkpoint_artifacts[i], diagnostic,
                  "auxiliary artifact"))
            return FALSE;
    }
    return TRUE;
}

BOOL
VHO_FHE_Checkpoint_Publish_Binary (FILE *diagnostic)
{
    if (!VHO_FHE_checkpoint_active || !VHO_FHE_checkpoint_finalized ||
        VHO_FHE_checkpoint_binary_published) {
        VHO_FHE_Checkpoint_Report
            (diagnostic, 5, "binary checkpoint is not ready", NULL);
        return FALSE;
    }
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        if (!VHO_FHE_checkpoint_artifacts[i].published) {
            VHO_FHE_Checkpoint_Report
                (diagnostic, 5,
                 "binary checkpoint cannot precede auxiliary artifacts",
                 NULL);
            return FALSE;
        }
    }
    VHO_FHE_CHECKPOINT_ARTIFACT binary;
    binary.temporary_path = VHO_FHE_checkpoint_binary_temporary_path;
    binary.final_path = VHO_FHE_checkpoint_binary_final_path;
    binary.published = FALSE;
    if (!VHO_FHE_Checkpoint_Publish_No_Replace
             (&binary, diagnostic, "binary checkpoint"))
        return FALSE;
    VHO_FHE_checkpoint_binary_published = TRUE;
    return TRUE;
}

void
VHO_FHE_Checkpoint_Complete (void)
{
    if (!VHO_FHE_checkpoint_binary_published) {
        VHO_FHE_Checkpoint_Abort();
        return;
    }
    VHO_FHE_CHECKPOINT_GENERIC_COMPLETION completion =
        VHO_FHE_checkpoint_completion;
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        if (!VHO_FHE_checkpoint_artifacts[i].published)
            remove(VHO_FHE_checkpoint_artifacts[i].temporary_path.c_str());
    }
    VHO_FHE_Checkpoint_Clear_State();
    if (completion != NULL)
        completion(TRUE);
}

void
VHO_FHE_Checkpoint_Abort (void)
{
    VHO_FHE_CHECKPOINT_GENERIC_COMPLETION completion =
        VHO_FHE_checkpoint_completion;
    for (size_t i = 0; i < VHO_FHE_checkpoint_artifacts.size(); ++i) {
        VHO_FHE_CHECKPOINT_ARTIFACT &artifact =
            VHO_FHE_checkpoint_artifacts[i];
        remove(artifact.temporary_path.c_str());
        if (artifact.published)
            remove(artifact.final_path.c_str());
    }
    if (!VHO_FHE_checkpoint_binary_temporary_path.empty())
        remove(VHO_FHE_checkpoint_binary_temporary_path.c_str());
    if (VHO_FHE_checkpoint_binary_published)
        remove(VHO_FHE_checkpoint_binary_final_path.c_str());
    VHO_FHE_Checkpoint_Clear_State();
    if (completion != NULL)
        completion(FALSE);
}
