/*
 * Copyright (C) 2026 Open64 Project
 *
 * Collect exact source/context Conv identities and geometry before the FHE
 * producer specializes PUs or writes plaintext assets. This is a read-only
 * process-local policy service; it defines no WHIRL or mapped-image record.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_conv_context_INCLUDED
#define fhe_ckks_conv_context_INCLUDED

#include <stdio.h>
#include <vector>

#include "defs.h"
#include "fhe_ckks_conv_recipe.h"
#include "fhe_ckks_event_coverage.h"
#include "fhe_plan.h"

struct VHO_FHE_CKKS_CONV_CONTEXT {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  DSL_IR_NODE_ID source_node_id;
  DSL_IR_VALUE_ID input_value_id;
  DSL_IR_VALUE_ID source_weight_operand_value_id;
  DSL_IR_VALUE_ID source_bias_operand_value_id;
  TY_IDX input_ty;
  TY_IDX source_result_ty;
  TCON_IDX folded_weight_tcon;
  TCON_IDX folded_bias_tcon;
  VHO_FHE_CKKS_CONV_SHAPE high_resolution_shape;
  UINT32 source_stride_height;
  UINT32 source_stride_width;
  UINT32 bn_fold_flags;
};

/* Join every source Conv event to its live node, exact BN-fold provenance,
 * canonical tensor descriptors, and source attributes. The high-resolution
 * shape always has stride one; source_stride records whether compaction is
 * required. Failure leaves contexts unchanged and performs no mutation. */
BOOL VHO_FHE_CKKS_Collect_Conv_Contexts(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    UINT32 slot_count,
    std::vector<VHO_FHE_CKKS_CONV_CONTEXT> *contexts,
    FILE *diagnostic);

#endif /* fhe_ckks_conv_context_INCLUDED */
