/*
 * Copyright (C) 2026 Open64 Project
 *
 * Read-only reconstruction of provider-private CKKS2C groups from the typed
 * DSL/FHE mapped images.  Collection runs once for each active PU because an
 * external tensor reference is validated against that PU's local symbol
 * table; Finish then proves whole-image event coverage and publishes no IR
 * changes.  See doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2, and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#ifndef fhe_ckks2c_collect_INCLUDED
#define fhe_ckks2c_collect_INCLUDED

#include <stdint.h>
#include <stdio.h>
#include <map>
#include <set>
#include <vector>

#include "fhe_ckks2c_emit.h"
#include "fhe_plan.h"

struct VHO_FHE_CKKS2C_COLLECT_RESULT {
  uint32_t pu_count;
  uint32_t source_count;
  uint32_t group_count;
  uint32_t event_count;
  uint32_t external_asset_count;
  uint32_t key_use_count;
};

/* Process-local transaction state. Callers may inspect neither container;
 * they exist only so standard all-PU traversal can preserve active-PU symbol
 * ownership while collecting one immutable program result. */
struct VHO_FHE_CKKS2C_COLLECTION {
  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  std::set<uint32_t> event_ids;
  std::set<uint32_t> owners;
  std::set<uint64_t> sources;
  std::map<uint32_t, DSL_FHE_CKKS_VALUE_STATE_RECORD> latest_states;
  uint32_t external_asset_count;
  uint32_t key_use_count;
  bool state_cache_ready;
  bool failed;

  VHO_FHE_CKKS2C_COLLECTION()
      : external_asset_count(0), key_use_count(0), state_cache_ready(false),
        failed(false) {}
};

/* Collect every event owned by owner_pu_st while that PU is active. Failure
 * makes the session terminal and leaves the input image untouched. */
bool VHO_FHE_CKKS2C_Collect_Active_PU(
    uint32_t owner_pu_st, VHO_FHE_CKKS2C_COLLECTION *collection,
    FILE *diagnostic);

/* Prove every event row was consumed once, order groups by semantic identity,
 * and atomically replace caller output and counters. */
bool VHO_FHE_CKKS2C_Finish_Collection(
    VHO_FHE_CKKS2C_COLLECTION *collection,
    std::vector<VHO_FHE_CKKS2C_GROUP> *groups,
    VHO_FHE_CKKS2C_COLLECT_RESULT *result, FILE *diagnostic);

#endif /* fhe_ckks2c_collect_INCLUDED */
