/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused FHE-only coverage checks before native CKKS node expansion.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_event_coverage.h"

#include <assert.h>
#include <stdio.h>
#include <vector>

/* Exercise 147 synthetic events over 19 contexts, including root callsite 0. */
int main()
{
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  std::vector<VHO_FHE_CKKS_EVENT_STEP> steps;
  for (uint32_t i = 0; i < 147; ++i) {
    VHO_FHE_CKKS_EVENT_IDENTITY event = {
      10, i / 19 + 1, 20, i % 19
    };
    events.push_back(event);
    for (uint32_t ordinal = 0; ordinal < 2; ++ordinal) {
      VHO_FHE_CKKS_EVENT_STEP step = {
        event, ordinal, (i / 19) * 2 + ordinal + 1
      };
      steps.push_back(step);
    }
  }
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_OK);

  VHO_FHE_CKKS_EVENT_IDENTITY colliding_local_ids[2] = {
    { 10, 7, 20, 0 }, { 11, 7, 21, 0 }
  };
  VHO_FHE_CKKS_EVENT_STEP distinct_owners[2] = {
    { colliding_local_ids[0], 0, 44 },
    { colliding_local_ids[1], 0, 44 }
  };
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      colliding_local_ids, 2, 2, distinct_owners, 2) ==
      VHO_FHE_CKKS_COVERAGE_OK);
  distinct_owners[1].event.owner_pu_st = 10;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      colliding_local_ids, 2, 2, distinct_owners, 2) ==
      VHO_FHE_CKKS_COVERAGE_UNKNOWN_EVENT);

  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 146, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_EVENT_COUNT);

  VHO_FHE_CKKS_EVENT_IDENTITY duplicate = events[0];
  events.push_back(duplicate);
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 148, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_DUPLICATE_EVENT);
  events.pop_back();

  VHO_FHE_CKKS_EVENT_STEP changed = steps[0];
  steps[0].event.context_callsite_id = 999;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_UNKNOWN_EVENT);
  steps[0] = changed;

  steps[1].step_ordinal = 0;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_DUPLICATE_STEP);
  steps[1].step_ordinal = 1;

  steps[1].result_value_id = steps[0].result_value_id;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_DUPLICATE_RESULT);
  steps[1].result_value_id = changed.result_value_id + 1;

  uint32_t other_event_result = steps[38].result_value_id;
  steps[38].result_value_id = steps[0].result_value_id;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_DUPLICATE_RESULT);
  steps[38].result_value_id = other_event_result;

  steps[0].step_ordinal = 2;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size()) ==
      VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT);
  steps[0] = changed;

  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      &events[0], events.size(), 147, &steps[0], steps.size() - 2) ==
      VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT);
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      NULL, 1, 1, NULL, 0) ==
      VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT);
  printf("synthetic_events=147 contexts=19 steps=294 ");
  printf("owner_collision=accepted malformed_cases=rejected\n");
  return 0;
}
