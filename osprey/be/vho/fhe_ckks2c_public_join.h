/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: reconstruct the frozen public FHE ABI-v1 selector schedule from a
 * certified CKKS group family without mutating WHIRL.  Context-specialized
 * PUs are folded back to canonical source signatures through structured PU
 * identity, formal TY, logical value, and callsite evidence.  The result is
 * the FHE-owned preflight consumed by the later main/common grouped lowering
 * transaction.
 *
 * Design references:
 *   doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D4
 *   doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#ifndef fhe_ckks2c_public_join_INCLUDED
#define fhe_ckks2c_public_join_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "fhe_ckks2c_emit.h"

enum VHO_FHE_CKKS2C_PUBLIC_BINDING_KIND {
  /* One binding is backed by one certified provider-private CKKS group. */
  VHO_FHE_CKKS2C_PUBLIC_PRIVATE_GROUP = 1,
  /* One binding preserves a public view operation with no CKKS primitives. */
  VHO_FHE_CKKS2C_PUBLIC_REPRESENTATION_ONLY = 2
};

/* One static selector in a canonical source PU's frozen public ABI schedule. */
struct VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR {
  uint32_t canonical_owner_pu_st;
  uint32_t canonical_source_node_id;
  uint32_t canonical_source_value_id;
  uint32_t static_ordinal;
  uint32_t operation_kind;
  uint32_t execution_multiplicity;
};

/*
 * One context-specific realization of a public descriptor.  Private-group
 * bindings authenticate the exact collected group; representation-only
 * bindings deliberately carry no fabricated group identity.
 */
struct VHO_FHE_CKKS2C_PUBLIC_BINDING {
  uint32_t binding_kind;
  uint32_t static_ordinal;
  uint32_t operation_kind;
  uint32_t specialized_owner_pu_st;
  uint32_t specialized_source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
  uint32_t group_output_value_id;
  std::string group_identity_sha256;
};

/* One image-derived route available before any active PU is mutated. */
struct VHO_FHE_CKKS2C_PUBLIC_ROUTE {
  uint32_t binding_kind;
  uint32_t specialized_owner_pu_st;
  uint32_t specialized_source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
  uint32_t origin_static_ordinal;
  uint32_t public_static_ordinal;
  uint32_t operation_kind;
  /* Nonzero only for a representation-only source. This records the exact
   * source value whose public result is viewed by the representation step. */
  uint32_t representation_operand_value_id;
};

/* Immutable-image result consumed by the later grouped lowering transaction. */
struct VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN {
  std::vector<VHO_FHE_CKKS2C_PUBLIC_DESCRIPTOR> descriptors;
  std::vector<VHO_FHE_CKKS2C_PUBLIC_BINDING> bindings;
  std::string identity_sha256;
};

/* Review census for the canonical schedule and its specialized realizations. */
struct VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT {
  uint32_t canonical_pu_count;
  uint32_t canonical_source_count;
  uint32_t static_descriptor_count;
  uint32_t dynamic_binding_count;
  uint32_t private_group_binding_count;
  uint32_t representation_binding_count;
};

/*
 * Join all collected groups to canonical public selectors.  The mapped image
 * remains read-only.  Success atomically replaces plan/result; failure leaves
 * both caller-owned outputs unchanged.
 */
bool VHO_FHE_CKKS2C_Build_Public_Join(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN *plan,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT *result,
    FILE *diagnostic);

/* Render the authenticated join as deterministic, human-readable schedule
 * evidence. The text is suitable for appending to the transaction's group
 * report before any auxiliary artifact is published. */
bool VHO_FHE_CKKS2C_Render_Public_Join(
    const VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN &plan,
    const VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT &result,
    std::string *text, FILE *diagnostic);

/* Reconstruct the public selector route for every typed CKKS event group
 * directly from immutable mapped images. This plan is available before the
 * backend opens the first local PU and is authenticated again by the complete
 * collected-group join at checkpoint finalization. */
bool VHO_FHE_CKKS2C_Build_Public_Routes_From_Image(
    std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> *routes,
    VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT *result,
    FILE *diagnostic);

#endif /* fhe_ckks2c_public_join_INCLUDED */
