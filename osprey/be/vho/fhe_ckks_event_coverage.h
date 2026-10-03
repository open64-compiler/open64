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
  uint32_t source_static_ordinal;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
} VHO_FHE_CKKS_EVENT_IDENTITY;

typedef struct {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  uint32_t step_ordinal;
  uint32_t result_value_id;
} VHO_FHE_CKKS_EVENT_STEP;

typedef struct {
  uint32_t owner_pu_st;
  uint32_t source_value_id;
  uint32_t first_static_ordinal;
  uint32_t static_evaluation_count;
  uint32_t execution_multiplicity;
} VHO_FHE_CKKS_STATIC_SOURCE;

typedef struct {
  uint32_t owner_pu_st;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;
} VHO_FHE_CKKS_CONTEXT_ROUTE;

typedef enum {
  VHO_FHE_CKKS_COVERAGE_OK = 0,
  VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT,
  VHO_FHE_CKKS_COVERAGE_EVENT_COUNT,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_EVENT,
  VHO_FHE_CKKS_COVERAGE_UNKNOWN_EVENT,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_STEP,
  VHO_FHE_CKKS_COVERAGE_DUPLICATE_RESULT,
  VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT,
  VHO_FHE_CKKS_COVERAGE_SOURCE_SCHEDULE,
  VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE,
  VHO_FHE_CKKS_COVERAGE_CAPACITY
} VHO_FHE_CKKS_COVERAGE_STATUS;

/*
 * Expand the existing static evaluation schedule across exact PU routes.
 * A route is root callsite zero or one called context. The caller must derive
 * routes from the validated DSL call image and independently compare totals.
 * A nested call path with multiplicity greater than the available exact
 * routes fails closed until a typed full-path identity is published. On any
 * failure, output events and event_count are unchanged.
 */
VHO_FHE_CKKS_COVERAGE_STATUS VHO_FHE_CKKS_Expand_Source_Events(
    const VHO_FHE_CKKS_STATIC_SOURCE *sources,
    size_t source_count,
    const VHO_FHE_CKKS_CONTEXT_ROUTE *routes,
    size_t route_count,
    VHO_FHE_CKKS_EVENT_IDENTITY *events,
    size_t event_capacity,
    size_t *event_count);

/*
 * Verify one-to-many expansion coverage without borrowing or changing IR.
 * Each event has exactly one owner/value/static-ordinal/context identity,
 * at least one step, and dense step ordinals starting at zero. The static
 * ordinal distinguishes the six evaluation events of one source ReLU value.
 * Result identities are unique within an exact context; different call
 * contexts may reuse a specialized PU value.
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
