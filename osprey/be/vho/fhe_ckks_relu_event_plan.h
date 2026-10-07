/*
 * Copyright (C) 2026 Open64 Project
 *
 * Build an explicit provider-independent CKKS event plan from the pinned ANT
 * ACE composite-ReLU algebra. This file owns no WN mutation; the native CKKS
 * expansion transaction consumes the completed plan later. See
 * doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md and
 * doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_ckks_relu_event_plan_INCLUDED
#define fhe_ckks_relu_event_plan_INCLUDED

#include <stdio.h>

#include <string>
#include <vector>

#include "fhe_ckks_plan_bytes.h"
#include "fhe_ckks_relu_recipe.h"

typedef struct {
  UINT32 source_static_ordinal;
  UINT32 source_value_id;
  UINT32 result_ty;
  UINT32 cipher_encryption_descriptor_id;
  UINT32 plaintext_encryption_descriptor_id;
  UINT32 scheme;
  UINT32 ciphertext_value_class;
  UINT32 plaintext_value_class;
  INT32 post_refresh_level;
  INT32 scale_bits;
  INT32 component_count;
  INT32 precision_bits;
  UINT32 slot_count;
  UINT32 alignment_group;
  UINT32 pending_bootstrap_action;
  UINT32 pre_relu_bootstrap_reason;
  std::string encrypted_layout;
  std::string bootstrap_key_id;
  std::string relinearization_key_id;
  UINT32 reciprocal_bound_value_id;
  std::string reciprocal_bound_sha256;
  std::vector<UINT32> scalar_value_ids;
  std::vector<std::string> scalar_sha256;
} VHO_FHE_CKKS_RELU_EVENT_POLICY;

/*
 * Convert one exact ACE algebraic recipe into executable CKKS steps. The
 * source input is refreshed first, normalization consumes one level, and the
 * three polynomial stages consume 3+4+4 levels. Scalar IDs identify already
 * authenticated external F64 tensor constants, one per scalar recipe step.
 * Failure leaves the caller's event plan unchanged.
 */
extern BOOL VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
    const VHO_FHE_RELU_RECIPE &recipe,
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic);

#endif /* fhe_ckks_relu_event_plan_INCLUDED */
