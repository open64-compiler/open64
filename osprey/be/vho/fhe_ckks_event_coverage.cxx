/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE VHO preflight for complete source-event to CKKS-step coverage.
 * Only process-local identities are inspected; shared DSL images and WHIRL
 * remain unchanged. Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_event_coverage.h"

#include <map>
#include <set>

namespace {

struct Event_Less {
  /* Order every part of the callee/source/call-context identity. */
  bool operator()(const VHO_FHE_CKKS_EVENT_IDENTITY &left,
                  const VHO_FHE_CKKS_EVENT_IDENTITY &right) const
  {
    if (left.owner_pu_st != right.owner_pu_st)
      return left.owner_pu_st < right.owner_pu_st;
    if (left.source_value_id != right.source_value_id)
      return left.source_value_id < right.source_value_id;
    if (left.context_pu_identity_id != right.context_pu_identity_id)
      return left.context_pu_identity_id < right.context_pu_identity_id;
    return left.context_callsite_id < right.context_callsite_id;
  }
};

struct Event_Steps {
  std::set<uint32_t> ordinals;
  std::set<uint32_t> results;
};

/* Reject zero IDs while allowing callsite zero for an entry-owned event. */
bool Event_Valid(const VHO_FHE_CKKS_EVENT_IDENTITY &event)
{
  return event.owner_pu_st != 0 && event.source_value_id != 0 &&
         event.context_pu_identity_id != 0;
}

}  // namespace

/*
 * Preflight the complete event and step arrays before the common expansion
 * transaction is invoked. No failure path changes a caller-owned object.
 */
VHO_FHE_CKKS_COVERAGE_STATUS
VHO_FHE_CKKS_Verify_Event_Coverage(
    const VHO_FHE_CKKS_EVENT_IDENTITY *events,
    size_t event_count,
    size_t expected_event_count,
    const VHO_FHE_CKKS_EVENT_STEP *steps,
    size_t step_count)
{
  if ((event_count != 0 && events == NULL) ||
      (step_count != 0 && steps == NULL))
    return VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT;
  if (event_count == 0 || event_count != expected_event_count)
    return VHO_FHE_CKKS_COVERAGE_EVENT_COUNT;

  std::map<VHO_FHE_CKKS_EVENT_IDENTITY, Event_Steps, Event_Less> coverage;
  for (size_t i = 0; i < event_count; ++i) {
    if (!Event_Valid(events[i]))
      return VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT;
    if (!coverage.insert(std::make_pair(events[i], Event_Steps())).second)
      return VHO_FHE_CKKS_COVERAGE_DUPLICATE_EVENT;
  }

  for (size_t i = 0; i < step_count; ++i) {
    if (!Event_Valid(steps[i].event) || steps[i].result_value_id == 0)
      return VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT;
    std::map<VHO_FHE_CKKS_EVENT_IDENTITY, Event_Steps,
             Event_Less>::iterator found = coverage.find(steps[i].event);
    if (found == coverage.end())
      return VHO_FHE_CKKS_COVERAGE_UNKNOWN_EVENT;
    if (!found->second.ordinals.insert(steps[i].step_ordinal).second)
      return VHO_FHE_CKKS_COVERAGE_DUPLICATE_STEP;
    if (!found->second.results.insert(steps[i].result_value_id).second)
      return VHO_FHE_CKKS_COVERAGE_DUPLICATE_RESULT;
  }

  for (std::map<VHO_FHE_CKKS_EVENT_IDENTITY, Event_Steps,
                Event_Less>::const_iterator event = coverage.begin();
       event != coverage.end(); ++event) {
    if (event->second.ordinals.empty())
      return VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT;
    uint32_t expected_ordinal = 0;
    for (std::set<uint32_t>::const_iterator ordinal =
             event->second.ordinals.begin();
         ordinal != event->second.ordinals.end(); ++ordinal) {
      if (*ordinal != expected_ordinal)
        return VHO_FHE_CKKS_COVERAGE_INCOMPLETE_EVENT;
      ++expected_ordinal;
    }
  }
  return VHO_FHE_CKKS_COVERAGE_OK;
}
