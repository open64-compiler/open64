/*
 * Copyright (C) 2026 Open64 Project
 *
 * Build provider-independent CKKS plans for the ResNet-20 global-average-pool
 * and classifier tail. This file owns semantic plans only; native WHIRL
 * mutation remains in the reviewed common/com CKKS expansion transaction.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C4.
 */

#ifndef fhe_ckks_tail_plan_INCLUDED
#define fhe_ckks_tail_plan_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "fhe_ckks_plan_bytes.h"

struct VHO_FHE_CKKS_TAIL_STATE_POLICY {
  uint32_t source_static_ordinal;
  uint32_t input_value_id;
  uint32_t input_ty;
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
};

struct VHO_FHE_CKKS_POOL_PLAN_POLICY {
  VHO_FHE_CKKS_TAIL_STATE_POLICY state;
  uint32_t scale_mask_value_id;
  std::string scale_mask_sha256;
};

struct VHO_FHE_CKKS_LINEAR_PLAN_POLICY {
  VHO_FHE_CKKS_TAIL_STATE_POLICY state;
  std::vector<uint32_t> weight_mask_value_ids;
  std::vector<std::string> weight_mask_sha256;
  uint32_t bias_value_id;
  std::string bias_sha256;
};

/* Build the exact six rotate/add spatial reduction followed by authenticated
 * scale-mask encode, ciphertext/plain multiply, and explicit rescale. */
bool VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
    const VHO_FHE_CKKS_POOL_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic);

/* Build a correctness-first sparse-channel classifier. Ten independent
 * plaintext weight masks consume the 64 channel means at slots c*64, each
 * branch performs a six-rotation reduction, and the branches are placed in
 * logits slots 0..9 before adding one authenticated bias vector. */
bool VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
    const VHO_FHE_CKKS_LINEAR_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic);

#endif /* fhe_ckks_tail_plan_INCLUDED */
