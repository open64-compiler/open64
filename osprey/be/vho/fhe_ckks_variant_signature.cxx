/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned whole-PU signature grouping from complete CKKS event plans.
 * This is a read-only policy layer before the generic PU transaction.
 * Design: doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_variant_signature.h"

#include <algorithm>
#include <map>
#include <set>
#include <utility>

namespace {

typedef std::pair<uint32_t, uint32_t> Context_Key;
typedef std::pair<uint32_t, uint32_t> Source_Slot;

struct Event_Key {
  uint32_t owner;
  uint32_t value;
  uint32_t ordinal;
  uint32_t identity;
  uint32_t callsite;

  /* Establish an owner-safe total order for exact event membership. */
  bool operator<(const Event_Key &other) const
  {
    if (owner != other.owner) return owner < other.owner;
    if (value != other.value) return value < other.value;
    if (ordinal != other.ordinal) return ordinal < other.ordinal;
    if (identity != other.identity) return identity < other.identity;
    return callsite < other.callsite;
  }
};

struct Planned_Event {
  uint32_t ordinal;
  uint32_t source_value;
  const unsigned char *bytes;
  size_t size;
};

/* Convert the published event identity without dropping its context. */
Event_Key Key_Of(const VHO_FHE_CKKS_EVENT_IDENTITY &event)
{
  Event_Key key = { event.owner_pu_st, event.source_value_id,
                    event.source_static_ordinal,
                    event.context_pu_identity_id,
                    event.context_callsite_id };
  return key;
}

/* Report a policy error without changing the caller's output vector. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-SIGNATURE-001: %s\n", message);
  return false;
}

/* Append an integer in fixed little-endian form to the canonical signature. */
void Append_U32(std::vector<unsigned char> *bytes, uint32_t value)
{
  for (unsigned shift = 0; shift != 32; shift += 8)
    bytes->push_back(static_cast<unsigned char>(value >> shift));
}

/* Sort the static source positions, independent of producer enumeration. */
bool Event_Ordinal_Less(const Planned_Event &left,
                        const Planned_Event &right)
{
  return left.ordinal < right.ordinal;
}

}  // namespace

/* Compare complete event-plan bytes across all routes of one source PU.
 * The canonical bytes include source identities and length-delimited plans,
 * but deliberately exclude callsite and context IDs so equivalent calls
 * can share one physical executable PU. */
bool
VHO_FHE_CKKS_Build_Variant_Signatures(
    const VHO_FHE_CKKS_EVENT_IDENTITY *source_events,
    size_t source_event_count,
    size_t expected_static_event_count,
    size_t expected_dynamic_event_count,
    const VHO_FHE_CKKS_SIGNATURE_EVENT *plans,
    size_t plan_count,
    std::vector<VHO_FHE_CKKS_SIGNATURE_VARIANT> *variants,
    FILE *diagnostic)
{
  if (source_events == NULL || plans == NULL || variants == NULL ||
      source_event_count == 0 ||
      source_event_count != expected_dynamic_event_count ||
      source_event_count != plan_count || expected_static_event_count == 0 ||
      expected_static_event_count > UINT32_MAX)
    return Report(diagnostic, "missing or mismatched circuit event array");

  std::set<Event_Key> expected;
  std::map<Context_Key, uint32_t> identities;
  std::map<uint32_t, uint32_t> called_owner;
  std::map<uint32_t, Source_Slot> source_ordinals;
  for (size_t i = 0; i < source_event_count; ++i) {
    const VHO_FHE_CKKS_EVENT_IDENTITY &event = source_events[i];
    if (event.owner_pu_st == 0 || event.source_value_id == 0 ||
        event.source_static_ordinal == 0 ||
        event.context_pu_identity_id == 0 ||
        !expected.insert(Key_Of(event)).second)
      return Report(diagnostic, "invalid or duplicated source event");
    Context_Key context(event.owner_pu_st, event.context_callsite_id);
    std::map<Context_Key, uint32_t>::const_iterator known =
        identities.find(context);
    if (known != identities.end() &&
        known->second != event.context_pu_identity_id)
      return Report(diagnostic, "context identity changes within one route");
    identities[context] = event.context_pu_identity_id;
    if (event.context_callsite_id != 0) {
      std::map<uint32_t, uint32_t>::const_iterator prior =
          called_owner.find(event.context_callsite_id);
      if (prior != called_owner.end() && prior->second != event.owner_pu_st)
        return Report(diagnostic, "callsite resolves to two source owners");
      called_owner[event.context_callsite_id] = event.owner_pu_st;
    }
    Source_Slot slot(event.owner_pu_st, event.source_value_id);
    std::map<uint32_t, Source_Slot>::const_iterator prior =
        source_ordinals.find(event.source_static_ordinal);
    if (prior != source_ordinals.end() && prior->second != slot)
      return Report(diagnostic, "static ordinal changes source identity");
    source_ordinals[event.source_static_ordinal] = slot;
  }
  if (source_ordinals.size() != expected_static_event_count)
    return Report(diagnostic, "static source census is incomplete");
  uint32_t expected_ordinal = 1;
  for (std::map<uint32_t, Source_Slot>::const_iterator ordinal =
           source_ordinals.begin(); ordinal != source_ordinals.end();
       ++ordinal, ++expected_ordinal) {
    if (ordinal->first != expected_ordinal)
      return Report(diagnostic, "static source ordinals are not dense");
  }

  std::map<Context_Key, std::vector<Planned_Event> > contexts;
  for (size_t i = 0; i < plan_count; ++i) {
    const VHO_FHE_CKKS_SIGNATURE_EVENT &plan = plans[i];
    Event_Key key = Key_Of(plan.event);
    if (expected.erase(key) != 1 || plan.plan_bytes == NULL ||
        plan.plan_size == 0 || plan.plan_size > UINT32_MAX)
      return Report(diagnostic, "plan is absent, duplicated, or empty");
    Context_Key context(key.owner, key.callsite);
    Planned_Event item = { key.ordinal, key.value,
                           plan.plan_bytes, plan.plan_size };
    contexts[context].push_back(item);
  }
  if (!expected.empty() || contexts.size() != identities.size())
    return Report(diagnostic, "circuit plan does not cover source events");
  for (std::map<Context_Key, uint32_t>::const_iterator route =
           identities.begin(); route != identities.end(); ++route) {
    if (route->first.second == 0) {
      std::map<Context_Key, uint32_t>::const_iterator next = route;
      ++next;
      if (next != identities.end() && next->first.first == route->first.first)
        return Report(diagnostic, "root and called contexts share one PU");
    }
  }

  std::map<uint32_t, std::vector<Source_Slot> > source_shape;
  std::vector<VHO_FHE_CKKS_SIGNATURE_VARIANT> built;
  for (std::map<Context_Key, std::vector<Planned_Event> >::iterator route =
           contexts.begin(); route != contexts.end(); ++route) {
    const uint32_t owner = route->first.first;
    std::vector<Planned_Event> &ordered = route->second;
    if (ordered.size() > UINT32_MAX)
      return Report(diagnostic, "context event count is not representable");
    std::sort(ordered.begin(), ordered.end(), Event_Ordinal_Less);
    std::vector<Source_Slot> shape;
    std::vector<unsigned char> signature;
    const unsigned char magic[] = { 'F', 'H', 'E', 'C', 'K', 'K', 'S', 1 };
    signature.insert(signature.end(), magic, magic + sizeof(magic));
    Append_U32(&signature, static_cast<uint32_t>(ordered.size()));
    for (size_t i = 0; i < ordered.size(); ++i) {
      if (i != 0 && ordered[i - 1].ordinal == ordered[i].ordinal)
        return Report(diagnostic, "source ordinal repeats in one context");
      shape.push_back(Source_Slot(ordered[i].ordinal,
                                  ordered[i].source_value));
      Append_U32(&signature, ordered[i].ordinal);
      Append_U32(&signature, ordered[i].source_value);
      Append_U32(&signature, static_cast<uint32_t>(ordered[i].size));
      signature.insert(signature.end(), ordered[i].bytes,
                       ordered[i].bytes + ordered[i].size);
    }
    std::map<uint32_t, std::vector<Source_Slot> >::const_iterator prior =
        source_shape.find(owner);
    if (prior != source_shape.end() && prior->second != shape)
      return Report(diagnostic, "source PU static shape differs by context");
    source_shape[owner] = shape;

    size_t match = built.size();
    for (size_t i = 0; i < built.size(); ++i) {
      if (built[i].source_owner_pu_st == owner &&
          built[i].signature_bytes == signature) {
        match = i;
        break;
      }
    }
    if (match == built.size()) {
      VHO_FHE_CKKS_SIGNATURE_VARIANT variant;
      variant.source_owner_pu_st = owner;
      variant.use_existing_pu =
          built.empty() || built.back().source_owner_pu_st != owner;
      variant.signature_bytes.swap(signature);
      built.push_back(variant);
    }
    built[match].context_callsites.push_back(route->first.second);
  }
  variants->swap(built);
  return true;
}
