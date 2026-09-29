/*
 * Copyright (C) 2026 Open64 Project
 *
 * Shared atomic checkpoint publication for FHE VHO phases.  See
 * doc/FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md.
 */

#ifndef fhe_checkpoint_INCLUDED
#define fhe_checkpoint_INCLUDED

#include <stdio.h>

#include "defs.h"

typedef BOOL (*VHO_FHE_CHECKPOINT_GENERIC_FINALIZER)
                                (const void *aggregate,
                                 FILE *diagnostic);
typedef void (*VHO_FHE_CHECKPOINT_GENERIC_COMPLETION) (BOOL committed);

extern BOOL VHO_FHE_Checkpoint_Register_Lifecycle
                                (VHO_FHE_CHECKPOINT_GENERIC_FINALIZER
                                     finalizer,
                                 VHO_FHE_CHECKPOINT_GENERIC_COMPLETION
                                     completion);
extern BOOL VHO_FHE_Checkpoint_Begin
                                (const char *temporary_binary_path,
                                 const char *final_binary_path,
                                 const char *diagnostic_prefix,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Checkpoint_Register_Artifact
                                (const char *temporary_path,
                                 const char *final_path);
extern UINT32 VHO_FHE_Checkpoint_Artifact_Count (void);
extern BOOL VHO_FHE_Checkpoint_Finalize
                                (const void *aggregate,
                                 FILE *diagnostic);
extern BOOL VHO_FHE_Checkpoint_Publish_Artifacts (FILE *diagnostic);
extern BOOL VHO_FHE_Checkpoint_Publish_Binary (FILE *diagnostic);
extern void VHO_FHE_Checkpoint_Complete (void);
extern void VHO_FHE_Checkpoint_Abort (void);

#endif /* fhe_checkpoint_INCLUDED */
