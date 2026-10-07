/*
 * Copyright (C) 2026 Open64 Project
 *
 * Build the provider-independent, executable CKKS operation plan for the
 * fixed O0 column-first Conv recipe. This file owns no WHIRL construction;
 * the reviewed native expansion transaction consumes its validated plan.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_conv_plan_INCLUDED
#define fhe_ckks_conv_plan_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "fhe_ckks_conv_recipe.h"
#include "fhe_ckks_plan_bytes.h"

struct VHO_FHE_CKKS_CONV_PLAN_POLICY {
  uint32_t source_static_ordinal;
  uint32_t input_value_id;
  uint32_t result_ty;
  uint32_t cipher_encryption_descriptor_id;
  uint32_t plaintext_encryption_descriptor_id;
  uint32_t scheme;
  uint32_t ciphertext_value_class;
  uint32_t plaintext_value_class;
  int32_t input_level;
  int32_t scale_bits;
  int32_t component_count;
  int32_t precision_bits;
  uint32_t slot_count;
  uint32_t alignment_group;
  uint32_t rescale_pending_action;
  std::string encrypted_layout;
  std::string rotation_key_id;
  int32_t capacity_refresh_target_level;
  std::string capacity_refresh_reason;
  std::string bootstrap_key_id;
  std::vector<uint32_t> row_value_ids;
  std::vector<std::string> row_sha256;
  uint32_t bias_value_id;
  std::string bias_sha256;
};

struct VHO_FHE_CKKS_STRIDE_COMPACTION_POLICY {
  uint32_t input_width;
  uint32_t output_channels;
  uint32_t high_resolution_ty;
  std::vector<uint32_t> mask_value_ids;
  std::vector<std::string> mask_sha256;
};

/* Build every explicit input-duplication, row rotation, plaintext encode,
 * multiply, rescale, accumulation, bias encode, and final add step. The
 * result states encode the fixed O0 one-level plaintext-multiply cost.
 * Failure leaves plan unchanged and performs no compiler mutation. */
bool VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic);

/* Build a high-resolution 1x1 or 3x3 Conv and append the reviewed sequential
 * selection/bit-move compaction network. Mask order is selection first, then
 * selected/complement pairs for each bit move. A capacity refresh must be
 * explicit in conv_policy. The final step restores the source Conv result TY;
 * all preceding high-resolution and compaction steps use high_resolution_ty. */
bool VHO_FHE_CKKS_Build_Stride_Two_Conv_Event_Plan(
    const VHO_FHE_CKKS_CONV_RECIPE &high_resolution_recipe,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &conv_policy,
    const VHO_FHE_CKKS_STRIDE_COMPACTION_POLICY &compaction_policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic);

#endif /* fhe_ckks_conv_plan_INCLUDED */
