/*
 * Copyright (C) 2026 Open64 Project
 *
 * Final standard-WHIRL boundary for FHE runtime lowering.  Structural checks
 * stay generic; the registered consumer verifies the FHE schedule and ABI.
 */

#ifndef fhe_unlowered_gate_INCLUDED
#define fhe_unlowered_gate_INCLUDED

#include <stdio.h>

#include "defs.h"

class WN;
struct pu_info;

typedef BOOL (*VHO_FHE_UNLOWERED_SEMANTIC_GATE)
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic);

typedef struct {
    UINT32 executable_dsl_carrier_count;
    UINT32 semantic_gatekeeper_count;
    UINT32 error_count;
} VHO_FHE_UNLOWERED_GATE_RESULT;

extern BOOL VHO_FHE_Unlowered_Gate_Register_Semantic_Verifier
                                (VHO_FHE_UNLOWERED_SEMANTIC_GATE verifier);
extern void VHO_FHE_Unlowered_Gate_Reset (void);
extern BOOL VHO_FHE_Unlowered_Gate_Program_Unit
                                (struct pu_info *pu_info,
                                 WN *tree,
                                 FILE *diagnostic,
                                 VHO_FHE_UNLOWERED_GATE_RESULT *result);

#endif /* fhe_unlowered_gate_INCLUDED */
