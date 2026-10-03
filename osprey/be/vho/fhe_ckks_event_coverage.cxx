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
#include <tuple>
#include <vector>

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
    if (left.source_static_ordinal != right.source_static_ordinal)
      return left.source_static_ordinal < right.source_static_ordinal;
    if (left.context_pu_identity_id != right.context_pu_identity_id)
      return left.context_pu_identity_id < right.context_pu_identity_id;
    return left.context_callsite_id < right.context_callsite_id;
  }
};

struct Event_Steps {
  std::set<uint32_t> ordinals;
};

/* Reject zero IDs while allowing callsite zero for an entry-owned event. */
bool Event_Valid(const VHO_FHE_CKKS_EVENT_IDENTITY &event)
{
  return event.owner_pu_st != 0 && event.source_value_id != 0 &&
         event.source_static_ordinal != 0 &&
         event.context_pu_identity_id != 0;
}

}  // namespace

/*
 * Preflight schedule/routing completeness before writing any event. The
 * caller's independently validated call image is the authority for routes;
 * this adapter rejects multiplicities that lose call-path information.
 */
VHO_FHE_CKKS_COVERAGE_STATUS
VHO_FHE_CKKS_Expand_Source_Events(
    const VHO_FHE_CKKS_STATIC_SOURCE *sources,
    size_t source_count,
    const VHO_FHE_CKKS_CONTEXT_ROUTE *routes,
    size_t route_count,
    VHO_FHE_CKKS_EVENT_IDENTITY *events,
    size_t event_capacity,
    size_t *event_count)
{
  if (sources == NULL || source_count == 0 || routes == NULL ||
      route_count == 0 || events == NULL || event_count == NULL)
    return VHO_FHE_CKKS_COVERAGE_INVALID_ARGUMENT;

  typedef std::vector<VHO_FHE_CKKS_CONTEXT_ROUTE> Route_List;
  std::map<uint32_t, Route_List> routes_by_owner;
  std::map<uint32_t, uint32_t> identity_by_owner;
  std::set<std::tuple<uint32_t, uint32_t, uint32_t> > route_keys;
  for (size_t i = 0; i < route_count; ++i) {
    std::map<uint32_t, uint32_t>::const_iterator identity =
        identity_by_owner.find(routes[i].owner_pu_st);
    if (routes[i].owner_pu_st == 0 ||
        routes[i].context_pu_identity_id == 0 ||
        (identity != identity_by_owner.end() &&
         identity->second != routes[i].context_pu_identity_id) ||
        !route_keys.insert(std::make_tuple(
            routes[i].owner_pu_st,
            routes[i].context_pu_identity_id,
            routes[i].context_callsite_id)).second)
      return VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE;
    identity_by_owner[routes[i].owner_pu_st] =
        routes[i].context_pu_identity_id;
    routes_by_owner[routes[i].owner_pu_st].push_back(routes[i]);
  }
  for (std::map<uint32_t, Route_List>::const_iterator owner =
           routes_by_owner.begin(); owner != routes_by_owner.end(); ++owner) {
    if (owner->second.size() > 1) {
      for (size_t i = 0; i < owner->second.size(); ++i) {
        if (owner->second[i].context_callsite_id == 0)
          return VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE;
      }
    }
  }

  std::set<uint32_t> static_ordinals;
  size_t total = 0;
  for (size_t i = 0; i < source_count; ++i) {
    const VHO_FHE_CKKS_STATIC_SOURCE &source = sources[i];
    std::map<uint32_t, Route_List>::const_iterator found =
        routes_by_owner.find(source.owner_pu_st);
    if (source.owner_pu_st == 0 || source.source_value_id == 0 ||
        source.first_static_ordinal == 0 ||
        source.static_evaluation_count == 0 ||
        source.first_static_ordinal >
            UINT32_MAX - (source.static_evaluation_count - 1))
      return VHO_FHE_CKKS_COVERAGE_SOURCE_SCHEDULE;
    if (found == routes_by_owner.end() ||
        found->second.size() != source.execution_multiplicity)
      return VHO_FHE_CKKS_COVERAGE_CONTEXT_ROUTE;
    for (uint32_t offset = 0; offset < source.static_evaluation_count;
         ++offset) {
      if (!static_ordinals.insert(source.first_static_ordinal + offset).second)
        return VHO_FHE_CKKS_COVERAGE_SOURCE_SCHEDULE;
    }
    size_t contexts = found->second.size();
    if (contexts > (SIZE_MAX - total) / source.static_evaluation_count)
      return VHO_FHE_CKKS_COVERAGE_CAPACITY;
    total += contexts * source.static_evaluation_count;
  }
  if (total > event_capacity)
    return VHO_FHE_CKKS_COVERAGE_CAPACITY;

  size_t cursor = 0;
  for (size_t i = 0; i < source_count; ++i) {
    const VHO_FHE_CKKS_STATIC_SOURCE &source = sources[i];
    const Route_List &owner_routes = routes_by_owner.find(
        source.owner_pu_st)->second;
    for (size_t route = 0; route < owner_routes.size(); ++route) {
      for (uint32_t offset = 0; offset < source.static_evaluation_count;
           ++offset) {
        VHO_FHE_CKKS_EVENT_IDENTITY event = {
          source.owner_pu_st,
          source.source_value_id,
          source.first_static_ordinal + offset,
          owner_routes[route].context_pu_identity_id,
          owner_routes[route].context_callsite_id
        };
        events[cursor++] = event;
      }
    }
  }
  *event_count = cursor;
  return VHO_FHE_CKKS_COVERAGE_OK;
}

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
  std::set<std::tuple<uint32_t, uint32_t, uint32_t, uint32_t> > results;
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
    if (!results.insert(std::make_tuple(
            steps[i].event.owner_pu_st,
            steps[i].event.context_pu_identity_id,
            steps[i].event.context_callsite_id,
            steps[i].result_value_id)).second)
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
