/*
 * Copyright (C) 2026 Open64 Project
 *
 * Materialize exact-shape ResNet residual joins as executable CKKS IR after
 * their Conv producers have been expanded. This FHE-owned policy layer uses
 * common/com's native CKKS expansion transaction and does not construct or
 * rewrite WN, ST, TY, or mapped-image rows directly.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_residual_materialize_INCLUDED
#define fhe_ckks_residual_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "fhe_materialize.h"
#include "pu_info.h"
#include "wn.h"

/* Validate the specialized nine-context residual census without changing IR.
 * The pre-specialization five-definition artifact remains inactive. */
BOOL VHO_FHE_CKKS_Residual_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Expand every residual definition owned by the active PU. Operand state is
 * read after Conv expansion; a higher-level branch is explicitly modswitched
 * to its peer before the final ckks.add. */
BOOL VHO_FHE_CKKS_Residual_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Require nine adds, seven explicit level alignments, and no executable
 * common.residual_add node, then register the atomic checkpoint report. */
BOOL VHO_FHE_CKKS_Residual_Materialization_Finalize(FILE *diagnostic);

/* Release process-local census and report paths after commit or abort. */
void VHO_FHE_CKKS_Residual_Materialization_Complete(BOOL committed);

#endif /* fhe_ckks_residual_materialize_INCLUDED */
