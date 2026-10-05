/*
 * Copyright (C) 2026 Open64 Project
 *
 * Read-only FHE VHO join from source events to approved ReLU materialization
 * and CKKS state evidence. Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_relu_plan_INCLUDED
#define fhe_ckks_relu_plan_INCLUDED

#include <stdio.h>
#include <vector>

#include "defs.h"
#include "symtab_idx.h"
#include "fhe_ckks_event_coverage.h"

typedef struct {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  UINT32 operation_id;
  UINT32 range_id;
  UINT32 output_state_id;
} VHO_FHE_CKKS_RELU_PLAN_STEP;

typedef struct {
  UINT32 owner_pu_st;
  UINT32 source_relu_value_id;
  UINT32 context_pu_identity_id;
  UINT32 context_callsite_id;
  UINT32 range_id;
  TCON_IDX positive_bound_tcon;
} VHO_FHE_CKKS_RELU_BOUND_BINDING;

/*
 * Resolve every ReLU event to exactly one six-part approved plan, including
 * its range and output state. The SYNC-5 static schedule and source event
 * array must already have been collected before any source is lowered.
 * A failure leaves the caller's plan vector unchanged.
 */
BOOL VHO_FHE_CKKS_Collect_Relu_Plan_Steps(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> *plans,
    FILE *diagnostic);

/*
 * Extract one exact typed normalization bound per verified ReLU context for
 * later PU specialization and caller-actual binding. The range image owns
 * positivity/type validation; this read-only join proves the normalize step
 * uses that same TCON. Failure leaves the output vector unchanged.
 */
BOOL VHO_FHE_CKKS_Collect_Relu_Bound_Bindings(
    const std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> &plans,
    std::vector<VHO_FHE_CKKS_RELU_BOUND_BINDING> *bindings,
    FILE *diagnostic);

#endif /* fhe_ckks_relu_plan_INCLUDED */
