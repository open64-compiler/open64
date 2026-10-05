/*
 * Copyright (C) 2026 Open64 Project
 *
 * Canonical process-local bytes for a reviewed CKKS source-event circuit.
 * These bytes select equivalent PU variants; they are not a mapped image.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_ckks_plan_bytes_INCLUDED
#define fhe_ckks_plan_bytes_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

enum VHO_FHE_CKKS_PLAN_OPERAND_KIND {
  VHO_FHE_CKKS_PLAN_SOURCE_VALUE = 1,
  VHO_FHE_CKKS_PLAN_PRIOR_STEP = 2,
  VHO_FHE_CKKS_PLAN_BOUND_FORMAL = 3
};

struct VHO_FHE_CKKS_PLAN_OPERAND {
  VHO_FHE_CKKS_PLAN_OPERAND_KIND kind;
  uint32_t reference;
  std::string bound_role;
};

struct VHO_FHE_CKKS_PLAN_ATTRIBUTE {
  std::string name;
  std::string value;
};

struct VHO_FHE_CKKS_PLAN_STATE {
  uint32_t encryption_descriptor_id;
  uint32_t scheme;
  uint32_t value_class;
  int32_t level;
  int32_t scale_bits;
  int32_t component_count;
  int32_t precision_bits;
  uint32_t slot_count;
  uint32_t alignment_group;
  std::string encrypted_layout;
  uint32_t pending_actions;
  uint32_t pending_bootstrap_reason;
};

struct VHO_FHE_CKKS_PLAN_STEP {
  uint32_t logical_operator;
  uint32_t operator_version;
  uint32_t result_ty;
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> operands;
  std::vector<VHO_FHE_CKKS_PLAN_ATTRIBUTE> attributes;
  VHO_FHE_CKKS_PLAN_STATE result_state;
  std::vector<std::string> required_keys;
  std::vector<int32_t> signed_rotations;
  std::string plaintext_asset_sha256;
};

struct VHO_FHE_CKKS_EVENT_PLAN {
  uint32_t source_static_ordinal;
  std::vector<VHO_FHE_CKKS_PLAN_STEP> steps;
  std::vector<uint32_t> group_output_steps;
  /* UINT32_MAX means this event does not replace the complete source. */
  uint32_t source_final_step_index;
};

/* Validate one structured event and encode it in explicit little-endian v1
 * bytes. Attribute/key/rotation sets are sorted for canonical equality.
 * Source locations, names, callsites, and per-caller B TCON bytes are absent;
 * the exact B formal role is present. Failure leaves output unchanged. This
 * serialization does not prove operator-specific CKKS legality; the producer
 * must run the shared native and FHE state/key preflights before mutation. */
bool VHO_FHE_CKKS_Serialize_Event_Plan(
    const VHO_FHE_CKKS_EVENT_PLAN &plan,
    std::vector<unsigned char> *bytes, FILE *diagnostic);

#endif /* fhe_ckks_plan_bytes_INCLUDED */
