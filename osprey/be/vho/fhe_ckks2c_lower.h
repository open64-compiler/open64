/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: materialize one active PU's certified CKKS2C groups as checked
 * standard-WHIRL calls to the frozen public FHE ABI. The implementation builds
 * one detached block per source context and commits the complete PU through
 * common/com's atomic grouped lowering transaction. ReLU's six private groups
 * remain six ABI operations inside one context block.
 *
 * Design references:
 *   doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D4
 *   doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md
 *   doc/FHE-SYNC6-D4-GROUPED-LOWERING-HANDOFF.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#ifndef fhe_ckks2c_lower_INCLUDED
#define fhe_ckks2c_lower_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <vector>

#include "fhe_ckks2c_emit.h"
#include "fhe_ckks2c_public_join.h"

struct pu_info;

struct VHO_FHE_CKKS2C_LOWER_RESULT {
  uint32_t group_count;
  uint32_t replacement_block_count;
  uint32_t retired_member_count;
  uint32_t inserted_statement_count;
  uint32_t standard_call_count;
  uint32_t output_handle_count;
  uint32_t status_check_count;
};

/*
 * Preflight and lower every supplied group owned by the current PU. The
 * function derives the PU tree, native definitions, logical operands, runtime
 * projections, and ReLU group order from authoritative compiler state. Six
 * ReLU private groups become one context block so intermediate handles remain
 * local; every other source context owns one private group. Caller output
 * changes only after the grouped transaction commits successfully.
 */
bool VHO_FHE_CKKS2C_Lower_Active_PU(
    struct pu_info *pu_info,
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> &routes,
    FILE *diagnostic, VHO_FHE_CKKS2C_LOWER_RESULT *result);

#endif /* fhe_ckks2c_lower_INCLUDED */
