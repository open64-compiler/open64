/*
 * Copyright (C) 2026 Open64 Project
 *
 * Reuse the certified SYNC-5 schedule and DSL call image as the source-event
 * authority for FHE CKKS expansion. No WN, TY, ST, or mapped row is changed.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_source_events.h"

#include <map>
#include <set>
#include <vector>

#include "dsl_ir_image.h"
#include "fhe_semantic_runtime_lower.h"

/* Report a failed read-only join without publishing a partial event list. */
static BOOL
VHO_FHE_CKKS_Source_Event_Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-EVENT-001: %s\n", message);
  return FALSE;
}

/*
 * Derive the dynamic event identities from the schedule once, before any
 * source definition is retired. A direct callsite represents one context;
 * the process-local adapter rejects nested multiplicity it cannot name.
 */
BOOL
VHO_FHE_CKKS_Collect_Source_Events(
    std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> *events,
    FILE *diagnostic)
{
  if (events == NULL || !VHO_FHE_Runtime_Static_Schedule_Prepare(diagnostic))
    return FALSE;

  std::map<ST_IDX, DSL_PU_SOURCE_IDENTITY_ID> identities;
  for (UINT32 id = 1; id <= DSL_Call_Image_PU_Identity_Count(); ++id) {
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    if (!DSL_Call_Image_Get_PU_Identity(id, &identity) ||
        identity.id != id || identity.owner_pu_st == ST_IDX_ZERO ||
        !identities.insert(std::make_pair(
            identity.owner_pu_st, identity.id)).second)
      return VHO_FHE_CKKS_Source_Event_Report(
          diagnostic, "PU source identities are incomplete or duplicated");
  }
  if (identities.empty())
    return VHO_FHE_CKKS_Source_Event_Report(
        diagnostic, "no PU source identities were captured");

  std::vector<VHO_FHE_CKKS_CONTEXT_ROUTE> routes;
  std::set<ST_IDX> called_owners;
  for (UINT32 id = 1; id <= DSL_Call_Image_Callsite_Count(); ++id) {
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (!DSL_Call_Image_Get_Callsite(id, &callsite) ||
        callsite.id != id ||
        identities.find(callsite.owner_pu_st) == identities.end())
      return VHO_FHE_CKKS_Source_Event_Report(
          diagnostic, "callsite owner is not a known PU");
    std::map<ST_IDX, DSL_PU_SOURCE_IDENTITY_ID>::const_iterator callee =
        identities.find(callsite.callee_pu_st);
    if (callee == identities.end())
      return VHO_FHE_CKKS_Source_Event_Report(
          diagnostic, "callsite callee is not a known PU");
    VHO_FHE_CKKS_CONTEXT_ROUTE route = {
      callsite.callee_pu_st, callee->second, id
    };
    routes.push_back(route);
    called_owners.insert(callsite.callee_pu_st);
  }
  for (std::map<ST_IDX, DSL_PU_SOURCE_IDENTITY_ID>::const_iterator owner =
           identities.begin(); owner != identities.end(); ++owner) {
    if (called_owners.find(owner->first) == called_owners.end()) {
      VHO_FHE_CKKS_CONTEXT_ROUTE route = {
        owner->first, owner->second, 0
      };
      routes.push_back(route);
    }
  }

  std::vector<VHO_FHE_CKKS_STATIC_SOURCE> sources;
  for (UINT32 index = 0;
       index < VHO_FHE_Runtime_Static_Schedule_Record_Count(); ++index) {
    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD schedule;
    if (!VHO_FHE_Runtime_Static_Schedule_Get(index, &schedule))
      return VHO_FHE_CKKS_Source_Event_Report(
          diagnostic, "static schedule row cannot be read");
    VHO_FHE_CKKS_STATIC_SOURCE source = {
      schedule.owner_pu_st,
      schedule.result_value_id,
      schedule.first_static_ordinal,
      schedule.static_evaluation_count,
      schedule.execution_multiplicity
    };
    sources.push_back(source);
  }

  UINT32 dynamic_count = VHO_FHE_Runtime_Dynamic_Evaluation_Count();
  if (sources.empty() || dynamic_count == 0)
    return VHO_FHE_CKKS_Source_Event_Report(
        diagnostic, "no executable FHE source events were scheduled");
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> collected(dynamic_count);
  size_t actual_count = 0;
  VHO_FHE_CKKS_COVERAGE_STATUS status =
      VHO_FHE_CKKS_Expand_Source_Events(
          &sources[0], sources.size(), &routes[0], routes.size(),
          &collected[0], collected.size(), &actual_count);
  if (status != VHO_FHE_CKKS_COVERAGE_OK ||
      actual_count != dynamic_count)
    return VHO_FHE_CKKS_Source_Event_Report(
        diagnostic, "static schedule and exact call routes disagree");

  events->swap(collected);
  return TRUE;
}
