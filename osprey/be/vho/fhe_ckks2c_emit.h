/*
 * Copyright (C) 2026 Open64 Project
 *
 * Deterministic, read-only C emission for certified CKKS event plans.  The
 * emitter creates provider-private evaluator functions and does not mutate
 * WHIRL or the public ABI-v1 schedule.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_ckks2c_emit_INCLUDED
#define fhe_ckks2c_emit_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "fhe_ckks_plan_bytes.h"

struct VHO_FHE_CKKS2C_GROUP {
  std::string function_name;
  std::string identity_sha256;
  uint32_t owner_pu_st;
  uint32_t source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
  VHO_FHE_CKKS_EVENT_PLAN plan;
};

struct VHO_FHE_CKKS2C_EMIT_RESULT {
  uint32_t group_count;
  uint32_t operation_count;
  uint32_t source_value_count;
  uint32_t bound_value_count;
};

/* Validate all groups first, then replace output with one deterministic C11
 * translation unit. Failure leaves output and result unchanged. */
bool VHO_FHE_CKKS2C_Emit_Module(
    const std::string &module_name,
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    std::string *output, VHO_FHE_CKKS2C_EMIT_RESULT *result,
    FILE *diagnostic);

#endif /* fhe_ckks2c_emit_INCLUDED */
