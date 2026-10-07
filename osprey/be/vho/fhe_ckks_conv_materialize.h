/*
 * Copyright (C) 2026 Open64 Project
 *
 * Materialize every context-specialized ResNet Conv as executable CKKS IR.
 * The module is an FHE-owned semantic client of common/com's typed-external,
 * generated-external, and native CKKS expansion transactions. It is invoked
 * by the single semantic materialization checkpoint owner and never publishes
 * artifacts independently.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_conv_materialize_INCLUDED
#define fhe_ckks_conv_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "fhe_materialize.h"
#include "pu_info.h"
#include "wn.h"

/* Validate whether this program carries the fully specialized 21-Conv census.
 * A pre-specialization 13-definition artifact remains inactive so the existing
 * ReLU-only materialization contract is backward compatible. */
BOOL VHO_FHE_CKKS_Conv_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Materialize plaintext assets and replace every Conv owned by the active PU.
 * All persistent IR changes go through reviewed native transactions. */
BOOL VHO_FHE_CKKS_Conv_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Prove complete 21-context/5691-row/21-bias/104-mask coverage and register
 * the deterministic Conv report before checkpoint publication. */
BOOL VHO_FHE_CKKS_Conv_Materialization_Finalize(FILE *diagnostic);

/* Release process-local handles and paths after either commit or abort. */
void VHO_FHE_CKKS_Conv_Materialization_Complete(BOOL committed);

#endif /* fhe_ckks_conv_materialize_INCLUDED */
