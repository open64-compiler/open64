/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned read-only CKKS value-state transfer rules for executable unary
 * operations. See doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_transfer_INCLUDED
#define fhe_ckks_transfer_INCLUDED

#include <stdio.h>

#include "dsl_ckks_expand.h"
#include "fhe_plan.h"

/* Parse a whole canonical decimal attribute into one signed 32-bit value. */
BOOL VHO_FHE_CKKS_Step_Integer(
    const DSL_CKKS_EXPANSION_STEP &step, const char *name, INT32 *value);

/* Prove a unary CKKS step's input/output state transition. Both records are
 * immutable; this service neither changes TY nor inserts a managed row. */
BOOL VHO_FHE_CKKS_Verify_Unary_State_Transfer(
    const DSL_CKKS_EXPANSION_STEP &step,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &input,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &output,
    FILE *diagnostic);

/* Prove an aligned add/sub/mul transfer for ciphertext and optionally one
 * encoded plaintext operand. Multiply leaves explicit repair obligations. */
BOOL VHO_FHE_CKKS_Verify_Binary_State_Transfer(
    const DSL_CKKS_EXPANSION_STEP &step,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &left,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &right,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &output,
    FILE *diagnostic);

#endif /* fhe_ckks_transfer_INCLUDED */
