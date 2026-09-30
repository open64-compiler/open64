/*
 * Copyright (C) 2026 Open64 Project
 *
 * Per-PU FHE runtime-lowering phase boundary.  The registered FHE consumer
 * owns ABI and schedule policy; this interface owns backend phase and
 * checkpoint coordination.  See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#ifndef fhe_runtime_lower_INCLUDED
#define fhe_runtime_lower_INCLUDED

#include <stdio.h>

#include "defs.h"

class WN;
struct pu_info;

typedef struct {
    const char *provider_manifest_path;
    const char *provider_manifest_sha256;
    const char *checkpoint_output_path;
} VHO_FHE_RUNTIME_LOWER_OPTIONS;

typedef struct {
    UINT32 semantic_gatekeeper_count;
    UINT32 runtime_lowering_pass_count;
    UINT32 standard_call_count;
    UINT32 output_handle_count;
    UINT32 status_check_count;
    UINT32 error_count;
} VHO_FHE_RUNTIME_LOWER_RESULT;

typedef BOOL (*VHO_FHE_RUNTIME_LOWER_GATEKEEPER)
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 const VHO_FHE_RUNTIME_LOWER_OPTIONS *options,
                                 FILE *diagnostic);
typedef BOOL (*VHO_FHE_RUNTIME_LOWER_PASS)
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 const VHO_FHE_RUNTIME_LOWER_OPTIONS *options,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_LOWER_RESULT *result);
typedef BOOL (*VHO_FHE_RUNTIME_LOWER_FINALIZER)
                                (const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate,
                                 FILE *diagnostic);
typedef void (*VHO_FHE_RUNTIME_LOWER_COMPLETION) (BOOL committed);

extern BOOL VHO_FHE_Runtime_Lower_Register_Semantic_Gatekeeper
                                (VHO_FHE_RUNTIME_LOWER_GATEKEEPER gatekeeper);
extern BOOL VHO_FHE_Runtime_Lower_Register_Pass
                                (VHO_FHE_RUNTIME_LOWER_PASS pass);
extern BOOL VHO_FHE_Runtime_Lower_Register_Checkpoint_Lifecycle
                                (VHO_FHE_RUNTIME_LOWER_FINALIZER finalizer,
                                 VHO_FHE_RUNTIME_LOWER_COMPLETION completion);
extern void VHO_FHE_Runtime_Lower_Reset_Passes (void);

extern void VHO_FHE_Runtime_Lower_Result_Init
                                (VHO_FHE_RUNTIME_LOWER_RESULT *result);
extern void VHO_FHE_Runtime_Lower_Result_Accumulate
                                (VHO_FHE_RUNTIME_LOWER_RESULT *aggregate,
                                 const VHO_FHE_RUNTIME_LOWER_RESULT *result);
extern BOOL VHO_FHE_Runtime_Lower_Program_Unit
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 FILE *diagnostic,
                                 VHO_FHE_RUNTIME_LOWER_RESULT *result);
extern BOOL VHO_FHE_Runtime_Lower_Driver_Try
                                (struct pu_info *pu_info,
                                 WN **tree,
                                 VHO_FHE_RUNTIME_LOWER_RESULT *result);

extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Validate
                                (UINT32 expected_pu_count,
                                 UINT32 lowered_pu_count,
                                 const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Begin
                                (const char *temporary_binary_path,
                                 const char *final_binary_path,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Register_Artifact
                                (const char *temporary_path,
                                 const char *final_path);
extern UINT32 VHO_FHE_Runtime_Lower_Checkpoint_Artifact_Count (void);
extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Finalize
                                (const VHO_FHE_RUNTIME_LOWER_RESULT *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Publish_Artifacts
                                (FILE *diagnostic);
extern BOOL VHO_FHE_Runtime_Lower_Checkpoint_Publish_Binary
                                (FILE *diagnostic);
extern void VHO_FHE_Runtime_Lower_Checkpoint_Complete (void);
extern void VHO_FHE_Runtime_Lower_Checkpoint_Abort (void);

#endif /* fhe_runtime_lower_INCLUDED */
