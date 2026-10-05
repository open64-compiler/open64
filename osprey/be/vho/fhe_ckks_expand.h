/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned state binding for the generic native CKKS expansion transaction.
 * See doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md. This interface does not
 * construct WN nodes or change canonical tensor types.
 */

#ifndef fhe_ckks_expand_INCLUDED
#define fhe_ckks_expand_INCLUDED

#include <stdio.h>

#include "dsl_ckks_expand.h"
#include "fhe_plan.h"

/* One template per native step, in request order. IDs are assigned only
 * after the common transaction has produced the corresponding DSL value. */
typedef struct {
  DSL_FHE_CKKS_VALUE_STATE_RECORD state;
} VHO_FHE_CKKS_STEP_STATE;

/* Prove the complete state/tensor association and native request before
 * mutation. It does not reserve names or allocate mapped rows. */
BOOL VHO_FHE_CKKS_Can_Expand_And_Bind_States(
    PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
    const VHO_FHE_CKKS_STEP_STATE *states, UINT32 state_count,
    FILE *diagnostic);

/* Apply one native source expansion and bind every new value's CKKS state.
 * Once expansion succeeds, a later bind failure is terminal for this
 * checkpoint process; the caller must discard the image, never retry it. */
BOOL VHO_FHE_CKKS_Expand_And_Bind_States(
    PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
    const VHO_FHE_CKKS_STEP_STATE *states, UINT32 state_count,
    FILE *diagnostic, DSL_CKKS_EXPANSION_STEP_RESULT *results);

#endif /* fhe_ckks_expand_INCLUDED */
