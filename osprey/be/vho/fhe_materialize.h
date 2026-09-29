/*
 * Copyright (C) 2026 Open64 Project
 *
 * Per-PU FHE materialization phase boundary.  The registered FHE consumer
 * owns semantic policy; this interface owns backend phase and checkpoint
 * coordination.  See doc/FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md.
 */

#ifndef fhe_materialize_INCLUDED
#define fhe_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"

class WN;
struct pu_info;

typedef struct {
    const char *bootstrap_mode;
    const char *provider_manifest_path;
    const char *provider_manifest_sha256;
} VHO_FHE_MATERIALIZE_OPTIONS;

typedef struct {
    UINT32 semantic_gatekeeper_count;
    UINT32 materialization_pass_count;
    UINT32 context_count;
    UINT32 operation_count;
    UINT32 refresh_count;
    UINT32 error_count;
} VHO_FHE_MATERIALIZE_RESULT;

typedef BOOL (*VHO_FHE_MATERIALIZATION_GATEKEEPER)
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 const VHO_FHE_MATERIALIZE_OPTIONS *options,
                                 FILE *diagnostic);
typedef BOOL (*VHO_FHE_MATERIALIZATION_PASS)
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 const VHO_FHE_MATERIALIZE_OPTIONS *options,
                                 FILE *diagnostic,
                                 VHO_FHE_MATERIALIZE_RESULT *result);
typedef BOOL (*VHO_FHE_MATERIALIZATION_FINALIZER)
                                (const VHO_FHE_MATERIALIZE_RESULT *aggregate,
                                 FILE *diagnostic);
typedef void (*VHO_FHE_MATERIALIZATION_COMPLETION) (BOOL committed);

extern BOOL VHO_FHE_Materialize_Register_Semantic_Gatekeeper
                                (VHO_FHE_MATERIALIZATION_GATEKEEPER
                                     gatekeeper);
extern BOOL VHO_FHE_Materialize_Register_Pass
                                (VHO_FHE_MATERIALIZATION_PASS pass);
extern BOOL VHO_FHE_Materialize_Register_Checkpoint_Lifecycle
                                (VHO_FHE_MATERIALIZATION_FINALIZER finalizer,
                                 VHO_FHE_MATERIALIZATION_COMPLETION
                                     completion);
extern void VHO_FHE_Materialize_Reset_Passes (void);

extern void VHO_FHE_Materialize_Result_Init
                                (VHO_FHE_MATERIALIZE_RESULT *result);
extern void VHO_FHE_Materialize_Result_Accumulate
                                (VHO_FHE_MATERIALIZE_RESULT *aggregate,
                                 const VHO_FHE_MATERIALIZE_RESULT *result);
extern BOOL VHO_FHE_Materialize_Program_Unit
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 FILE *diagnostic,
                                 VHO_FHE_MATERIALIZE_RESULT *result);
extern BOOL VHO_FHE_Materialize_Driver_Try
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 VHO_FHE_MATERIALIZE_RESULT *result);
extern WN *VHO_FHE_Materialize_Driver_With_Result
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 VHO_FHE_MATERIALIZE_RESULT *result);

extern BOOL VHO_FHE_Materialize_Checkpoint_Validate
                                (UINT32 expected_pu_count,
                                 UINT32 materialized_pu_count,
                                 const VHO_FHE_MATERIALIZE_RESULT *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Materialize_Checkpoint_Begin
                                (const char *temporary_binary_path,
                                 const char *final_binary_path,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Materialize_Checkpoint_Register_Artifact
                                (const char *temporary_path,
                                 const char *final_path);
extern UINT32 VHO_FHE_Materialize_Checkpoint_Artifact_Count (void);
extern BOOL VHO_FHE_Materialize_Checkpoint_Finalize
                                (const VHO_FHE_MATERIALIZE_RESULT *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Materialize_Checkpoint_Publish_Artifacts
                                (FILE *diagnostic);
extern BOOL VHO_FHE_Materialize_Checkpoint_Publish_Binary
                                (FILE *diagnostic);
extern void VHO_FHE_Materialize_Checkpoint_Complete (void);
extern void VHO_FHE_Materialize_Checkpoint_Abort (void);

#endif /* fhe_materialize_INCLUDED */
