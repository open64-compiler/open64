/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned, process-local CKKS expansion coverage contract for VHO.
 * This does not allocate DSL opcodes, mutate WHIRL, or define a binary row.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#ifndef fhe_ckks_event_coverage_INCLUDED
#define fhe_ckks_event_coverage_INCLUDED

#include <stddef.h>
#include <stdint.h>

typedef struct {
  uint32_t owner_pu_st;
  uint32_t source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
} VHO_FHE_CKKS_EVENT_IDENTITY;

typedef struct {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  uint32_t step_ordinal;
  uint32_t result_value_id;
} VHO_FHE_CKKS_EVENT_STEP;

typedef enum {
  VHO_FHE_CKKS_COVERAGE_OK = 0,
  VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT,
  VHO_FHE_CKKS_COVERAGE_EVENT_COUNT,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_EVENT,
  VHO_FHE_CKKS_COVERAGE_UNKNOWN_EVENT,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_STEP,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_RESULT,
  VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT
} VHO_FHE_CKKS_COVERAGE_STATUS;

/*
 * Verify one-to-many expansion coverage without borrowing or changing IR.
 * Each event has exactly one owner/context identity, at least one step, and
 * dense step ordinals starting at zero. Result identities are unique within
 * an exact context; different call contexts may reuse a specialized PU value.
 * The caller supplies its independently counted expected event total. This
 * check does not establish CKKS operation legality or persisted provenance.
 */
VHO_FHE_CKKS_COVERAGE_STATUS VHO_FHE_CKKS_Verify_Event_Coverage(
    const VHO_FHE_CKKS_EVENT_IDENTITY *events,
    size_t event_count,
    size_t expected_event_count,
    const VHO_FHE_CKKS_EVENT_STEP *steps,
    size_t step_count);

#endif /* fhe_ckks_event_coverage_INCLUDED */
