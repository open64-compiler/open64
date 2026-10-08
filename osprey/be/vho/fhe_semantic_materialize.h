/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: publish the approved SYNC-4 provider authentication and semantic
 * materialization registration used again by SYNC-5 runtime lowering.
 * Design: doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-C and S5-G.
 */

#ifndef fhe_semantic_materialize_INCLUDED
#define fhe_semantic_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"

/* Verify the exact approved provider bytes and capability schema. */
extern BOOL VHO_FHE_Authenticate_Approved_Provider_Manifest
                                (const char *path, const char *sha256,
                                 FILE *diagnostic);

/* Ensure one approved ReLU context has the complete six-operation planning
 * chain needed by PU specialization and later executable materialization. */
extern BOOL VHO_FHE_Ensure_Relu_Materialization_Context
                                (UINT32 range_id, FILE *diagnostic);

/* Register the default SYNC-4 policy callbacks once per compiler process. */
extern BOOL VHO_FHE_Register_Default_Semantic_Materialization (void);

#endif /* fhe_semantic_materialize_INCLUDED */
