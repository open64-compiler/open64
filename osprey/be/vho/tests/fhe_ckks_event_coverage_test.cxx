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
  VHO_FHE_CKKS_STATIC_SOURCE sources[2] = {
    { 10, 100, 1, 1, 1 },
    { 11, 200, 2, 6, 2 }
  };
  VHO_FHE_CKKS_CONTEXT_ROUTE routes[3] = {
    { 10, 20, 0 }, { 11, 21, 4 }, { 11, 21, 5 }
  };
  VHO_FHE_CKKS_EVENT_IDENTITY expanded[13] = {};
  expanded[0].owner_pu_st = 99;
  size_t expanded_count = 777;
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 12, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_CAPACITY);
  assert(expanded[0].owner_pu_st == 99 && expanded_count == 777);
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_OK);
  assert(expanded_count == 13 && expanded[0].context_callsite_id == 0);
  assert(expanded[1].source_static_ordinal == 2 &&
         expanded[6].source_static_ordinal == 7 &&
         expanded[7].context_callsite_id == 5 &&
         expanded[12].source_static_ordinal == 7);

  VHO_FHE_CKKS_CONTEXT_ROUTE saved_route = routes[2];
  routes[2] = routes[1];
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE);
  routes[2] = saved_route;
  routes[2].context_pu_identity_id = 22;
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE);
  routes[2] = saved_route;
  routes[2].context_callsite_id = 0;
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE);
  routes[2] = saved_route;
  sources[1].execution_multiplicity = 3;
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE);
  sources[1].execution_multiplicity = 2;
  sources[1].first_static_ordinal = 1;
  assert(VHO_FHE_CKKS_Expand_Source_Events(
      sources, 2, routes, 3, expanded, 13, &expanded_count) ==
      VHO_FHE_CKKS_COVERAGE_SOURCE_SCHEDULE);

  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  std::vector<VHO_FHE_CKKS_EVENT_STEP> steps;
  for (uint32_t i = 0; i < 147; ++i) {
    VHO_FHE_CKKS_EVENT_IDENTITY event = {
      10, i / 19 + 1, i / 19 + 1, 20, i % 19
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
    { 10, 7, 50, 20, 0 }, { 11, 7, 50, 21, 0 }
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

  VHO_FHE_CKKS_EVENT_IDENTITY relu_events[6];
  VHO_FHE_CKKS_EVENT_STEP relu_steps[6];
  for (uint32_t ordinal = 0; ordinal < 6; ++ordinal) {
    VHO_FHE_CKKS_EVENT_IDENTITY relu = {
      10, 7, 50 + ordinal, 20, 3
    };
    relu_events[ordinal] = relu;
    VHO_FHE_CKKS_EVENT_STEP step = { relu, 0, 80 + ordinal };
    relu_steps[ordinal] = step;
  }
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      relu_events, 6, 6, relu_steps, 6) ==
      VHO_FHE_CKKS_COVERAGE_OK);
  relu_events[1].source_static_ordinal = 50;
  assert(VHO_FHE_CKKS_Verify_Event_Coverage(
      relu_events, 6, 6, relu_steps, 6) ==
      VHO_FHE_CKKS_COVERAGE_DUPLICATE_EVENT);

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
  printf("source_expansion=13 synthetic_events=147 contexts=19 steps=294 ");
  printf("owner_collision=accepted relu_static_ordinals=6 ");
  printf("malformed_cases=rejected\n");
  return 0;
}
