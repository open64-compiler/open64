/*
 * Copyright (C) 2026 Open64 Project
 *
 * Link-test the FHE CKKS collector against read-only schedule/call-image
 * substitutes. No WHIRL image or common/com implementation is modified.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_source_events.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <vector>

#include "dsl_ir_image.h"
#include "fhe_semantic_runtime_lower.h"

static DSL_PU_SOURCE_IDENTITY_RECORD identities[2];
static DSL_CALLSITE_METADATA_RECORD callsites[2];
static VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD schedules[2];
static UINT32 dynamic_count;

/* Supply a valid two-PU root/callee identity table to the collector. */
UINT32 DSL_Call_Image_PU_Identity_Count(void)
{
  return 2;
}

/* Reject an out-of-range identity as the managed table would. */
BOOL DSL_Call_Image_Get_PU_Identity(
    DSL_PU_SOURCE_IDENTITY_ID id,
    DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
  if (id == 0 || id > 2 || record == NULL)
    return FALSE;
  *record = identities[id - 1];
  return TRUE;
}

/* Supply both direct callsites into the shared callee PU. */
UINT32 DSL_Call_Image_Callsite_Count(void)
{
  return 2;
}

/* Reject an out-of-range callsite without changing the caller's record. */
BOOL DSL_Call_Image_Get_Callsite(
    DSL_CALLSITE_METADATA_ID id,
    DSL_CALLSITE_METADATA_RECORD *record)
{
  if (id == 0 || id > 2 || record == NULL)
    return FALSE;
  *record = callsites[id - 1];
  return TRUE;
}

/* Model the existing schedule's read-only preparation boundary. */
BOOL VHO_FHE_Runtime_Static_Schedule_Prepare(FILE *)
{
  return TRUE;
}

/* Expose the two physical source definitions in this linked fixture. */
UINT32 VHO_FHE_Runtime_Static_Schedule_Record_Count(void)
{
  return 2;
}

/* Keep the ordinal range and multiplicity exactly as stored by SYNC-5. */
BOOL VHO_FHE_Runtime_Static_Schedule_Get(
    UINT32 index,
    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD *record)
{
  if (index >= 2 || record == NULL)
    return FALSE;
  *record = schedules[index];
  return TRUE;
}

/* Report the independently counted dynamic event total for comparison. */
UINT32 VHO_FHE_Runtime_Dynamic_Evaluation_Count(void)
{
  return dynamic_count;
}

/* Certify exact source identities and leave prior output on every rejection. */
int main()
{
  memset(identities, 0, sizeof(identities));
  memset(callsites, 0, sizeof(callsites));
  memset(schedules, 0, sizeof(schedules));
  identities[0].id = 1;
  identities[0].owner_pu_st = 10;
  identities[1].id = 2;
  identities[1].owner_pu_st = 11;
  for (UINT32 i = 0; i < 2; ++i) {
    callsites[i].id = i + 1;
    callsites[i].owner_pu_st = 10;
    callsites[i].callee_pu_st = 11;
  }
  schedules[0].owner_pu_st = 10;
  schedules[0].result_value_id = 100;
  schedules[0].first_static_ordinal = 1;
  schedules[0].static_evaluation_count = 1;
  schedules[0].execution_multiplicity = 1;
  schedules[1].owner_pu_st = 11;
  schedules[1].result_value_id = 200;
  schedules[1].first_static_ordinal = 2;
  schedules[1].static_evaluation_count = 6;
  schedules[1].execution_multiplicity = 2;
  dynamic_count = 13;

  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  assert(VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13 && events[0].context_callsite_id == 0);
  assert(events[1].source_value_id == 200 &&
         events[1].source_static_ordinal == 2 &&
         events[6].source_static_ordinal == 7 &&
         events[7].context_callsite_id == 2 &&
         events[12].source_static_ordinal == 7);

  callsites[1].owner_pu_st = 99;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13 && events[12].source_static_ordinal == 7);
  callsites[1].owner_pu_st = 10;

  dynamic_count = 12;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13);
  dynamic_count = 13;
  schedules[1].execution_multiplicity = 3;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13);

  printf("linked_schedule_rows=2 source_events=13 relu_static_ordinals=6 ");
  printf("partial_output=none\n");
  return 0;
}
