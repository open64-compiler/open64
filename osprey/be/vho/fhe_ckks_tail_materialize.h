/*
 * Copyright (C) 2026 Open64 Project
 *
 * Materialize the ResNet-20 global-pool/flatten/classifier/output tail as
 * explicit CKKS IR. This FHE-owned producer consumes only reviewed native
 * transactions and checkpoint artifact services.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C8.
 */

#ifndef fhe_ckks_tail_materialize_INCLUDED
#define fhe_ckks_tail_materialize_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "fhe_materialize.h"
#include "pu_info.h"
#include "wn.h"

/* Validate the exact single-root ResNet pooling/classifier tail and retain
 * its stable values, contexts, external tensors, and source schedule. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Materialize tail plaintexts, expand pool and linear, retire the zero-copy
 * flatten view and output marker, and bind concrete value states. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic);

/* Prove the exact 15-pool plus 170-linear operation census, no live tail
 * source computations, complete state/key evidence, and register a report. */
BOOL VHO_FHE_CKKS_Tail_Materialization_Finalize(FILE *diagnostic);

/* Release process-local values, handles, and paths after commit or abort. */
void VHO_FHE_CKKS_Tail_Materialization_Complete(BOOL committed);

#endif /* fhe_ckks_tail_materialize_INCLUDED */
