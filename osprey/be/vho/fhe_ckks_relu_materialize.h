/*
 * Copyright (C) 2026 Open64 Project
 *
 * Materialize every approved ResNet ReLU context as explicit CKKS IR. The
 * producer consumes the pinned ACE recipe and authenticated range/profile
 * evidence, while common/com owns native value creation and source retirement.
 * See doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md and
 * doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_ckks_relu_materialize_INCLUDED
#define fhe_ckks_relu_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "fhe_materialize.h"
#include "pu_info.h"
#include "wn.h"

/* Capture the exact nineteen-context census before any source ReLU changes. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Expand every source ReLU owned by the active resident PU. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Prove full context/event coverage and register the ReLU asset and report. */
BOOL VHO_FHE_CKKS_Relu_Materialization_Finalize(FILE *diagnostic);

/* Release process-local state after checkpoint commit or rollback. */
void VHO_FHE_CKKS_Relu_Materialization_Complete(BOOL committed);

#endif /* fhe_ckks_relu_materialize_INCLUDED */
