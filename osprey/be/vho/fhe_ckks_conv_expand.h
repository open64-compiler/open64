/*
 * Copyright (C) 2026 Open64 Project
 *
 * Translate a validated FHE Conv event plan into the generic atomic native
 * CKKS expansion and value-state transaction. This adapter owns no physical
 * WN, ST, TY, or mapped-image construction.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef fhe_ckks_conv_expand_INCLUDED
#define fhe_ckks_conv_expand_INCLUDED

#include <stdio.h>
#include <vector>

#include "dsl_ckks_expand.h"
#include "fhe_ckks_conv_plan.h"
#include "pu_info.h"

struct VHO_FHE_CKKS_CONV_EXPANSION_SOURCE {
  WN *source_definition;
  DSL_IR_VALUE_ID source_value_id;
  DSL_OPERATOR expected_source_operator;
  UINT16 expected_source_version;
  DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
  DSL_CALLSITE_METADATA_ID context_callsite_id;
  ST_IDX origin_owner_pu_st;
  DSL_IR_VALUE_ID origin_source_value_id;
  STR_IDX encrypted_layout_name;
};

/* Convert one complete process-local plan into native request rows, run the
 * shared preflight/mutation transaction, and bind every result CKKS state.
 * The layout STR_IDX must already exist and equal the plan's stable layout;
 * this adapter never mutates the string table before native preflight.
 * Failure leaves results unchanged. A failure after native expansion is
 * terminal for the enclosing checkpoint, as documented by the transaction. */
BOOL VHO_FHE_CKKS_Expand_Conv_Event(
    PU_Info *pu_info, const VHO_FHE_CKKS_EVENT_PLAN &plan,
    const VHO_FHE_CKKS_CONV_EXPANSION_SOURCE &source,
    FILE *diagnostic,
    std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> *results);

#endif /* fhe_ckks_conv_expand_INCLUDED */
