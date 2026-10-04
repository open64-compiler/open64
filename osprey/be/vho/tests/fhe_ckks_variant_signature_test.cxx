/*
 * Copyright (C) 2026 Open64 Project
 *
 * Exercise FHE whole-PU CKKS signature grouping without allocating WHIRL.
 * Design: doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md.
 */

#include "fhe_ckks_variant_signature.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <vector>

/* Build a 6-PU, 10-invocation, 87-static/147-dynamic source fixture.
 * The second ReLU in callsites 3/6/9 has a different CKKS target level.
 * Non-ReLU events are also represented so the policy cannot group by ReLU
 * rows alone. B is a typed formal role inside the complete plan bytes; its
 * per-caller TCON is intentionally not part of the circuit signature. */
static void
Build_Complete_Event_Plans(
    std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> *events,
    std::vector<VHO_FHE_CKKS_SIGNATURE_EVENT> *plans,
    std::vector<std::vector<unsigned char> > *bytes)
{
  const uint32_t owners[] = { 12801, 13057, 13313, 13569, 13825, 14081 };
  const uint32_t first[] = { 1, 11, 26, 42, 57, 73 };
  const uint32_t counts[] = { 10, 15, 16, 15, 16, 15 };
  const uint32_t call_first[] = { 0, 1, 4, 5, 7, 8 };
  const uint32_t call_count[] = { 1, 3, 1, 2, 1, 2 };
  events->clear();
  plans->clear();
  bytes->clear();
  events->reserve(147);
  plans->reserve(147);
  bytes->reserve(147);
  for (size_t owner = 0; owner < 6; ++owner) {
    for (uint32_t call = call_first[owner];
         call < call_first[owner] + call_count[owner]; ++call) {
      for (uint32_t offset = 0; offset < counts[owner]; ++offset) {
        uint32_t ordinal = first[owner] + offset;
        VHO_FHE_CKKS_EVENT_IDENTITY event = {
          owners[owner], ordinal + 1000, ordinal,
          static_cast<uint32_t>(owner + 1), call
        };
        events->push_back(event);
        uint32_t level = ((call == 3 || call == 6) &&
                          offset >= counts[owner] - 6) ? 18 :
                         (call == 9 && offset >= counts[owner] - 6) ? 17 : 15;
        unsigned char role = offset >= counts[owner] - 6 ? 2 : 1;
        bytes->push_back(std::vector<unsigned char>{
          role, static_cast<unsigned char>(level),
          static_cast<unsigned char>(offset), 0x42
        });
        VHO_FHE_CKKS_SIGNATURE_EVENT plan = {
          event, &bytes->back()[0], bytes->back().size()
        };
        plans->push_back(plan);
      }
    }
  }
  assert(events->size() == 147 && plans->size() == 147);
}

/* Compare route groups and prove a non-ReLU circuit change causes a split. */
static void
Check_Positive_And_Complete_Signature_Split(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    std::vector<VHO_FHE_CKKS_SIGNATURE_EVENT> *plans,
    std::vector<std::vector<unsigned char> > *bytes)
{
  std::vector<VHO_FHE_CKKS_SIGNATURE_VARIANT> variants;
  assert(VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size(),
      &variants, stderr));
  assert(variants.size() == 9);
  uint32_t existing = 0;
  for (size_t i = 0; i < variants.size(); ++i)
    existing += variants[i].use_existing_pu ? 1 : 0;
  assert(existing == 6);
  assert(variants[1].context_callsites.size() == 2 &&
         variants[1].context_callsites[0] == 1 &&
         variants[1].context_callsites[1] == 2);
  assert(variants[2].context_callsites.size() == 1 &&
         variants[2].context_callsites[0] == 3);

  /* Same source PU, same ReLU levels, different conv plan: no reuse. */
  size_t changed = 0;
  for (size_t i = 0; i < events.size(); ++i) {
    if (events[i].context_callsite_id == 2 &&
        events[i].source_static_ordinal == 11) {
      changed = i;
      break;
    }
  }
  assert(changed != 0);
  (*bytes)[changed][3] = 0x43;
  assert(VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size(),
      &variants, stderr));
  assert(variants.size() == 10);
  (*bytes)[changed][3] = 0x42;
}

/* Rejected partial, duplicate, cross-owner, and null plans preserve output. */
static void
Check_Fail_Closed(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    std::vector<VHO_FHE_CKKS_SIGNATURE_EVENT> *plans)
{
  std::vector<VHO_FHE_CKKS_SIGNATURE_VARIANT> result(1);
  result[0].source_owner_pu_st = 999;
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size() - 1,
      &result, NULL));
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 66, 147, &(*plans)[0], plans->size(),
      &result, NULL));
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);

  VHO_FHE_CKKS_SIGNATURE_EVENT saved = (*plans)[1];
  (*plans)[1] = (*plans)[0];
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size(),
      &result, NULL));
  (*plans)[1] = saved;
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);

  (*plans)[1].event.owner_pu_st = 14081;
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size(),
      &result, NULL));
  (*plans)[1] = saved;
  (*plans)[1].plan_size = 0;
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &events[0], events.size(), 87, 147, &(*plans)[0], plans->size(),
      &result, NULL));
  (*plans)[1] = saved;
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);

  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> mixed = events;
  std::vector<VHO_FHE_CKKS_SIGNATURE_EVENT> mixed_plans = *plans;
  for (size_t i = 0; i < mixed.size(); ++i) {
    if (mixed[i].owner_pu_st == 13057 &&
        mixed[i].context_callsite_id == 1) {
      mixed[i].context_callsite_id = 0;
      mixed_plans[i].event.context_callsite_id = 0;
    }
  }
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &mixed[0], mixed.size(), 87, 147, &mixed_plans[0],
      mixed_plans.size(), &result, NULL));
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);

  mixed = events;
  mixed_plans = *plans;
  mixed[1].source_static_ordinal = 87;
  mixed_plans[1].event.source_static_ordinal = 87;
  assert(!VHO_FHE_CKKS_Build_Variant_Signatures(
      &mixed[0], mixed.size(), 87, 147, &mixed_plans[0],
      mixed_plans.size(), &result, NULL));
  assert(result.size() == 1 && result[0].source_owner_pu_st == 999);
}

/* Keep the fixture explicitly below the mapped CKKS checkpoint boundary. */
int main()
{
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  std::vector<VHO_FHE_CKKS_SIGNATURE_EVENT> plans;
  std::vector<std::vector<unsigned char> > bytes;
  Build_Complete_Event_Plans(&events, &plans, &bytes);
  Check_Positive_And_Complete_Signature_Split(events, &plans, &bytes);
  Check_Fail_Closed(events, &plans);
  puts("FHE complete-signature grouping fixture passed (no WHIRL emitted)");
  return 0;
}
