/*
 * Copyright (C) 2026 Open64 Project
 *
 * Join typed CKKS event, logical IR, CKKS state/key, call identity, and
 * external tensor evidence into deterministic CKKS2C event groups.  This
 * backend-only collector is deliberately read-only and retains no borrowed
 * image strings after each active-PU call.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2.
 */

#include "fhe_ckks2c_collect.h"

#include <algorithm>
#include <cerrno>
#include <climits>
#include <cstring>
#include <cstdlib>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "fhe_sha256.h"
#include "strtab.h"

namespace {

const size_t kMaxCollectedGroups = 4096;

struct Event_Key {
  uint32_t owner;
  uint32_t source;
  uint32_t identity;
  uint32_t callsite;
  uint32_t ordinal;

  /* Establish one portable total order over fixed-width persisted IDs. */
  bool operator<(const Event_Key &other) const
  {
    if (owner != other.owner) return owner < other.owner;
    if (source != other.source) return source < other.source;
    if (identity != other.identity) return identity < other.identity;
    if (callsite != other.callsite) return callsite < other.callsite;
    return ordinal < other.ordinal;
  }
};

/* Emit one stable collector diagnostic and make no caller-visible changes. */
bool Report(FILE *diagnostic, const char *message, uint32_t id)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHECKKS2C-002: id=%u %s\n", id, message);
  return false;
}

/* Borrow one validated nonempty string only for the current expression. */
bool String_Value(STR_IDX index, std::string *value)
{
  if (value == NULL || index == STR_IDX_ZERO || index >= STR_Table_Size())
    return false;
  const char *text = Index_To_Str(index);
  if (text == NULL || text[0] == 0)
    return false;
  *value = text;
  return true;
}

/* External assets use the same canonical lowercase digest contract as the
 * mapped tensor metadata and the provider-private facade. */
bool Lowercase_SHA256(const char *value)
{
  if (value == NULL || strlen(value) != 64)
    return false;
  for (size_t i = 0; i < 64; ++i)
    if (!((value[i] >= '0' && value[i] <= '9') ||
          (value[i] >= 'a' && value[i] <= 'f')))
      return false;
  return true;
}

/* Parse one exact signed decimal attribute for key-ledger matching. */
bool Signed_Decimal(const std::string &text, int32_t *value)
{
  if (value == NULL || text.empty())
    return false;
  errno = 0;
  char *end = NULL;
  const long parsed = strtol(text.c_str(), &end, 10);
  if (errno != 0 || end == NULL || *end != 0 ||
      parsed < INT_MIN || parsed > INT_MAX)
    return false;
  *value = static_cast<int32_t>(parsed);
  return true;
}

/* Read exact contiguous operand rows and reject a reordered owner/ordinal. */
bool Node_Operands(const DSL_IR_NODE_RECORD &node,
                   std::vector<uint32_t> *operands)
{
  if (operands == NULL ||
      (node.operand_count != 0 && node.first_operand_reference_id == 0))
    return false;
  std::vector<uint32_t> candidate;
  for (uint32_t i = 0; i < node.operand_count; ++i) {
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    if (!DSL_IR_Image_Get_Value_Reference(
            node.first_operand_reference_id + i, &reference) ||
        reference.owner_node_id != node.id || reference.ordinal != i ||
        reference.value_id == 0 || reference.flags != 0 ||
        reference.reserved != 0)
      return false;
    candidate.push_back(reference.value_id);
  }
  operands->swap(candidate);
  return true;
}

/* Copy exact logical attributes; canonical serialization later sorts names. */
bool Node_Attributes(const DSL_IR_NODE_RECORD &node,
                     std::vector<VHO_FHE_CKKS_PLAN_ATTRIBUTE> *attributes)
{
  if (attributes == NULL ||
      (node.attribute_count != 0 && node.first_attribute_id == 0))
    return false;
  std::vector<VHO_FHE_CKKS_PLAN_ATTRIBUTE> candidate;
  for (uint32_t i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    VHO_FHE_CKKS_PLAN_ATTRIBUTE copied;
    if (!DSL_IR_Image_Get_Attribute(node.first_attribute_id + i, &attribute) ||
        attribute.owner_node_id != node.id || attribute.flags != 0 ||
        !String_Value(attribute.name, &copied.name) ||
        !String_Value(attribute.value, &copied.value))
      return false;
    candidate.push_back(copied);
  }
  attributes->swap(candidate);
  return true;
}

/* Find one copied attribute by semantic name. */
bool Attribute(const VHO_FHE_CKKS_PLAN_STEP &step, const char *name,
               std::string *value)
{
  for (size_t i = 0; i < step.attributes.size(); ++i)
    if (step.attributes[i].name == name) {
      *value = step.attributes[i].value;
      return true;
    }
  return false;
}

/* Build one value-to-role map for the called group. Scanning once per group
 * avoids multiplying the global formal/argument table walk by every CKKS
 * operand in a large expanded model. */
bool Bound_Roles(uint32_t owner, uint32_t callsite,
                 std::map<uint32_t, std::string> *roles)
{
  if (roles == NULL)
    return false;
  roles->clear();
  if (callsite == 0)
    return true;
  for (uint32_t id = 1; id <= DSL_PU_Interface_Image_Formal_Count(); ++id) {
    DSL_PU_FORMAL_RECORD formal;
    if (!DSL_PU_Interface_Image_Get_Formal(id, &formal))
      return false;
    if (formal.owner_pu_st != owner)
      continue;
    for (uint32_t argument_id = 1;
         argument_id <= DSL_Call_ABI_Image_Argument_Count(); ++argument_id) {
      DSL_CALL_ARGUMENT_RECORD argument;
      std::string candidate;
      if (!DSL_Call_ABI_Image_Get_Argument(argument_id, &argument))
        return false;
      if (argument.callsite_id == callsite &&
          argument.callee_formal_ordinal == formal.formal_ordinal &&
          String_Value(argument.semantic_role, &candidate) &&
          candidate.compare(0, 11, "relu_bound_") == 0) {
        if (!roles->insert(std::make_pair(formal.formal_value_id,
                                          candidate)).second)
          return false;
      }
    }
  }
  return true;
}

/* Prove one operation's named key against the exact descriptor/config ledger. */
bool Collect_Key_Use(VHO_FHE_CKKS_PLAN_STEP *step,
                     uint32_t *key_use_count)
{
  uint32_t key_class = DSL_FHE_KEY_UNKNOWN;
  int32_t rotation = 0;
  switch (step->logical_operator) {
    case OPR_DSLCKKSROTATE: {
      std::string amount;
      if (!Attribute(*step, "attr.signed_steps", &amount) ||
          !Signed_Decimal(amount, &rotation) || rotation == 0)
        return false;
      key_class = DSL_FHE_KEY_ROTATION;
      break;
    }
    case OPR_DSLCKKSRELIN:
      key_class = DSL_FHE_KEY_RELINEARIZATION;
      break;
    case OPR_DSLCKKSBOOTSTRAP:
      key_class = DSL_FHE_KEY_BOOTSTRAP;
      break;
    default:
      return true;
  }
  std::string key_name;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
  if (!Attribute(*step, "attr.key_id", &key_name) ||
      !DSL_FHE_Get_Encryption_Descriptor(
          step->result_state.encryption_descriptor_id, &descriptor))
    return false;
  for (uint32_t id = 1; id <= DSL_FHE_Key_Requirement_Count(); ++id) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    std::string stored_name;
    if (!DSL_FHE_Get_Key_Requirement(id, &key))
      return false;
    if (key.config_id == descriptor.config_id &&
        key.key_class == key_class && key.rotation_offset == rotation &&
        String_Value(key.key_set_name, &stored_name) &&
        stored_name == key_name) {
      step->required_keys.push_back(key_name);
      if (key_class == DSL_FHE_KEY_ROTATION)
        step->signed_rotations.push_back(rotation);
      ++*key_use_count;
      return true;
    }
  }
  return false;
}

/* Copy one cached latest value state into the emitter's pointer-free view.
 * The whole state table is indexed once per collection, never once per event. */
bool Result_State(
    uint32_t value_id,
    const std::map<uint32_t, DSL_FHE_CKKS_VALUE_STATE_RECORD> &latest_states,
    VHO_FHE_CKKS_PLAN_STATE *state)
{
  std::map<uint32_t, DSL_FHE_CKKS_VALUE_STATE_RECORD>::const_iterator found =
      latest_states.find(value_id);
  std::string layout;
  if (state == NULL || found == latest_states.end() ||
      !String_Value(found->second.encrypted_layout_name, &layout))
    return false;
  const DSL_FHE_CKKS_VALUE_STATE_RECORD &record = found->second;
  state->encryption_descriptor_id = record.encryption_descriptor_id;
  state->scheme = record.scheme;
  state->value_class = record.value_class;
  state->level = record.level;
  state->scale_bits = record.scale_bits;
  state->component_count = record.component_count;
  state->precision_bits = record.precision_bits;
  state->slot_count = record.slot_count;
  state->alignment_group = record.alignment_group;
  state->encrypted_layout = layout;
  state->pending_actions = record.pending_actions;
  state->pending_bootstrap_reason = record.pending_bootstrap_reason;
  return true;
}

/* Build an authenticated semantic identity. Stable source/call strings and
 * canonical plan bytes ensure a table number is never the sole selector. */
bool Group_Identity(const DSL_CKKS_EVENT_RECORD &first,
                    const DSL_IR_VALUE_RECORD &source,
                    const VHO_FHE_CKKS_EVENT_PLAN &plan,
                    std::string *digest)
{
  DSL_PU_SOURCE_IDENTITY_RECORD identity;
  DSL_CALLSITE_METADATA_RECORD callsite;
  std::string definition, module, file, source_name;
  if (digest == NULL ||
      !DSL_Call_Image_Get_PU_Identity(
          first.context_pu_identity_id, &identity) ||
      identity.owner_pu_st != first.owner_pu_st ||
      !String_Value(identity.canonical_definition_name, &definition) ||
      !String_Value(identity.defining_module, &module) ||
      !String_Value(identity.defining_file, &file) ||
      !String_Value(source.name, &source_name))
    return false;
  std::ostringstream semantic;
  semantic << "open64.ckks2c.group.v1\n" << definition << '\n' << module
           << '\n' << file << ':' << identity.defining_line << '\n'
           << source_name << '\n' << first.source_static_ordinal << '\n'
           << first.origin_static_ordinal << '\n';
  if (first.context_callsite_id == 0)
    semantic << "root\n";
  else {
    std::string klass, path, context;
    if (!DSL_Call_Image_Get_Callsite(first.context_callsite_id, &callsite) ||
        callsite.callee_pu_st != first.owner_pu_st ||
        !String_Value(callsite.canonical_class_name, &klass) ||
        !String_Value(callsite.instance_path, &path) ||
        !String_Value(callsite.context_identity, &context))
      return false;
    semantic << klass << '\n' << path << '\n' << context << '\n'
             << callsite.source_call_ordinal << '\n';
  }
  std::vector<unsigned char> bytes;
  if (!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL))
    return false;
  const std::string prefix = semantic.str();
  std::vector<unsigned char> authenticated(prefix.begin(), prefix.end());
  authenticated.insert(authenticated.end(), bytes.begin(), bytes.end());
  *digest = VHO_FHE_SHA256(
      authenticated.empty() ? NULL : &authenticated[0], authenticated.size());
  return digest->size() == 64;
}

/* Convert one exact persisted event group after validating every typed join. */
bool Build_Group(uint32_t owner, const std::vector<DSL_CKKS_EVENT_RECORD> &rows,
                 const std::map<uint32_t,
                                DSL_FHE_CKKS_VALUE_STATE_RECORD> &latest_states,
                 VHO_FHE_CKKS2C_GROUP *group, uint32_t *external_assets,
                 uint32_t *key_uses, FILE *diagnostic)
{
  if (rows.empty() || group == NULL || external_assets == NULL ||
      key_uses == NULL)
    return Report(diagnostic, "empty event group", 0);
  const DSL_CKKS_EVENT_RECORD &first = rows[0];
  DSL_IR_VALUE_RECORD source;
  DSL_IR_NODE_RECORD source_node;
  DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
  std::string source_name;
  if (first.owner_pu_st != owner ||
      !DSL_IR_Image_Get_Value(first.source_value_id, &source) ||
      !DSL_IR_Image_Get_Node(source.producer_node_id, &source_node) ||
      !DSL_IR_Image_Get_Opcode_Descriptor(
          source_node.opcode_descriptor_id, &source_opcode) ||
      source_node.result_value_id != source.id ||
      !String_Value(source.name, &source_name))
    return Report(diagnostic, "source value join failed", first.id);

  std::map<uint32_t, uint32_t> local_steps;
  for (size_t i = 0; i < rows.size(); ++i)
    local_steps[rows[i].result_value_id] = rows[i].step_ordinal;
  std::map<uint32_t, std::string> bound_roles;
  if (!Bound_Roles(owner, first.context_callsite_id, &bound_roles))
    return Report(diagnostic, "bound-formal role join failed", first.id);

  VHO_FHE_CKKS2C_GROUP candidate;
  candidate.owner_pu_st = owner;
  candidate.source_value_id = first.source_value_id;
  candidate.source_node_id = source_node.id;
  candidate.source_logical_operator = source_opcode.logical_operator;
  candidate.source_operator_version = source_opcode.version;
  candidate.source_value_name = source_name;
  candidate.context_pu_identity_id = first.context_pu_identity_id;
  candidate.context_callsite_id = first.context_callsite_id;
  candidate.origin_owner_pu_st = first.origin_owner_pu_st;
  candidate.origin_source_value_id = first.origin_source_value_id;
  candidate.origin_static_ordinal = first.origin_static_ordinal;
  candidate.group_output_value_id = rows.back().result_value_id;
  candidate.plan.source_static_ordinal = first.source_static_ordinal;
  candidate.plan.source_final_step_index =
      std::numeric_limits<uint32_t>::max();

  uint32_t final_count = 0;
  for (size_t i = 0; i < rows.size(); ++i) {
    const DSL_CKKS_EVENT_RECORD &event = rows[i];
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_RECORD result_value;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    VHO_FHE_CKKS_PLAN_STEP step;
    if (event.owner_pu_st != first.owner_pu_st ||
        event.source_value_id != first.source_value_id ||
        event.source_static_ordinal != first.source_static_ordinal ||
        event.context_pu_identity_id != first.context_pu_identity_id ||
        event.context_callsite_id != first.context_callsite_id ||
        event.origin_owner_pu_st != first.origin_owner_pu_st ||
        event.origin_source_value_id != first.origin_source_value_id ||
        event.origin_static_ordinal != first.origin_static_ordinal ||
        event.step_ordinal != i ||
        !DSL_IR_Image_Get_Node(event.result_node_id, &node) ||
        !DSL_IR_Image_Get_Value(event.result_value_id, &result_value) ||
        node.result_value_id != event.result_value_id ||
        result_value.producer_node_id != event.result_node_id ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode) ||
        opcode.logical_operator < OPR_DSLCKKSADD ||
        opcode.logical_operator > OPR_DSLCKKSBOOTSTRAP ||
        !Node_Attributes(node, &step.attributes) ||
        !Result_State(event.result_value_id, latest_states,
                      &step.result_state))
      return Report(diagnostic, "node, opcode, or state join failed", event.id);
    candidate.event_ids.push_back(event.id);
    candidate.result_node_ids.push_back(event.result_node_id);
    candidate.result_value_ids.push_back(event.result_value_id);
    step.logical_operator = opcode.logical_operator;
    step.operator_version = opcode.version;
    step.result_ty = result_value.ty;

    std::vector<uint32_t> operands;
    if (!Node_Operands(node, &operands))
      return Report(diagnostic, "operand rows are malformed", event.id);
    for (size_t operand_index = 0; operand_index < operands.size();
         ++operand_index) {
      VHO_FHE_CKKS_PLAN_OPERAND operand;
      operand.reference = operands[operand_index];
      std::map<uint32_t, uint32_t>::const_iterator prior =
          local_steps.find(operand.reference);
      if (prior != local_steps.end() && prior->second < i) {
        operand.kind = VHO_FHE_CKKS_PLAN_PRIOR_STEP;
        operand.reference = prior->second;
      } else if (prior != local_steps.end()) {
        return Report(diagnostic, "forward or self step reference", event.id);
      } else if (bound_roles.find(operand.reference) != bound_roles.end()) {
        operand.kind = VHO_FHE_CKKS_PLAN_BOUND_FORMAL;
        operand.bound_role = bound_roles[operand.reference];
        operand.reference = 0;
      } else {
        DSL_IR_VALUE_RECORD input;
        if (!DSL_IR_Image_Get_Value(operand.reference, &input))
          return Report(diagnostic, "source operand is missing", event.id);
        operand.kind = VHO_FHE_CKKS_PLAN_SOURCE_VALUE;
      }
      step.operands.push_back(operand);
    }

    if (step.logical_operator == OPR_DSLCKKSENCODE) {
      DSL_IR_EXTERNAL_TENSOR_REFERENCE external;
      if (operands.size() != 1 ||
          !DSL_IR_Image_Get_External_Tensor_Reference(
              owner, operands[0], &external) ||
          !Lowercase_SHA256(external.checksum))
        return Report(diagnostic, "encoded asset reference is invalid", event.id);
      step.plaintext_asset_sha256 = external.checksum;
      ++*external_assets;
    }
    if (!Collect_Key_Use(&step, key_uses))
      return Report(diagnostic, "key requirement join failed", event.id);
    candidate.plan.steps.push_back(step);
    if ((event.flags & DSL_CKKS_EVENT_FINAL_RESULT) != 0) {
      candidate.plan.source_final_step_index = i;
      ++final_count;
    }
  }
  candidate.plan.group_output_steps.push_back(rows.size() - 1);
  if (final_count > 1 ||
      (final_count == 1 && candidate.plan.source_final_step_index !=
                             rows.size() - 1))
    return Report(diagnostic, "source-final row is not the group output",
                  first.id);
  if (!Group_Identity(first, source, candidate.plan,
                      &candidate.identity_sha256))
    return Report(diagnostic, "stable group identity failed", first.id);
  candidate.function_name = "open64_fhe_ckks_group_" +
                            candidate.identity_sha256.substr(0, 24);
  *group = candidate;
  return true;
}

/* Order complete groups independently of mapped row insertion order. */
struct Group_Less {
  bool operator()(const VHO_FHE_CKKS2C_GROUP &left,
                  const VHO_FHE_CKKS2C_GROUP &right) const
  {
    return left.identity_sha256 < right.identity_sha256;
  }
};

/* Keep production collection compatible with Open64's pre-C++11 build. */
struct Event_Step_Less {
  bool operator()(const DSL_CKKS_EVENT_RECORD &left,
                  const DSL_CKKS_EVENT_RECORD &right) const
  {
    return left.step_ordinal < right.step_ordinal;
  }
};

}  // namespace

/* Collect one active PU without retaining local symbol or borrowed strings. */
bool VHO_FHE_CKKS2C_Collect_Active_PU(
    uint32_t owner_pu_st, VHO_FHE_CKKS2C_COLLECTION *collection,
    FILE *diagnostic)
{
  if (collection == NULL)
    return Report(diagnostic, "collection session is absent", owner_pu_st);
  if (collection->failed)
    return Report(diagnostic, "collection session already failed",
                  owner_pu_st);
  if (owner_pu_st == 0 || collection->owners.count(owner_pu_st) != 0) {
    collection->failed = true;
    return Report(diagnostic, "invalid or repeated active PU", owner_pu_st);
  }
  std::map<Event_Key, std::vector<DSL_CKKS_EVENT_RECORD> > events;
  if (!collection->state_cache_ready) {
    for (uint32_t id = 1; id <= DSL_FHE_Plan_CKKS_Value_State_Count(); ++id) {
      DSL_FHE_CKKS_VALUE_STATE_RECORD state;
      if (!DSL_FHE_Plan_Get_CKKS_Value_State(id, &state)) {
        collection->failed = true;
        return Report(diagnostic, "CKKS value state row cannot be read", id);
      }
      std::map<uint32_t, DSL_FHE_CKKS_VALUE_STATE_RECORD>::iterator prior =
          collection->latest_states.find(state.value_id);
      if (prior == collection->latest_states.end() ||
          prior->second.state_version < state.state_version)
        collection->latest_states[state.value_id] = state;
    }
    collection->state_cache_ready = true;
  }
  for (uint32_t id = 1; id <= DSL_CKKS_Event_Image_Count(); ++id) {
    DSL_CKKS_EVENT_RECORD event;
    if (!DSL_CKKS_Event_Image_Get(id, &event)) {
      collection->failed = true;
      return Report(diagnostic, "event row cannot be read", id);
    }
    if (event.owner_pu_st != owner_pu_st)
      continue;
    Event_Key key = {event.owner_pu_st, event.source_value_id,
                     event.context_pu_identity_id,
                     event.context_callsite_id,
                     event.source_static_ordinal};
    events[key].push_back(event);
  }
  std::vector<VHO_FHE_CKKS2C_GROUP> candidate;
  uint32_t asset_count = 0;
  uint32_t key_count = 0;
  std::set<uint32_t> event_ids;
  for (std::map<Event_Key, std::vector<DSL_CKKS_EVENT_RECORD> >::iterator
           group = events.begin(); group != events.end(); ++group) {
    std::sort(group->second.begin(), group->second.end(), Event_Step_Less());
    VHO_FHE_CKKS2C_GROUP collected;
    if (!Build_Group(owner_pu_st, group->second, collection->latest_states,
                     &collected,
                     &asset_count, &key_count, diagnostic)) {
      collection->failed = true;
      return false;
    }
    for (size_t i = 0; i < group->second.size(); ++i)
      event_ids.insert(group->second[i].id);
    candidate.push_back(collected);
  }
  if (collection->groups.size() + candidate.size() > kMaxCollectedGroups) {
    collection->failed = true;
    return Report(diagnostic, "group count exceeds collector budget", 0);
  }
  collection->groups.insert(collection->groups.end(),
                            candidate.begin(), candidate.end());
  collection->event_ids.insert(event_ids.begin(), event_ids.end());
  collection->owners.insert(owner_pu_st);
  for (size_t i = 0; i < candidate.size(); ++i)
    collection->sources.insert(
        (uint64_t(candidate[i].owner_pu_st) << 32) |
        candidate[i].source_value_id);
  collection->external_asset_count += asset_count;
  collection->key_use_count += key_count;
  return true;
}

/* Finalize only when every typed event row has one collected group owner. */
bool VHO_FHE_CKKS2C_Finish_Collection(
    VHO_FHE_CKKS2C_COLLECTION *collection,
    std::vector<VHO_FHE_CKKS2C_GROUP> *groups,
    VHO_FHE_CKKS2C_COLLECT_RESULT *result, FILE *diagnostic)
{
  if (collection == NULL || groups == NULL || result == NULL)
    return Report(diagnostic, "collection output is absent", 0);
  if (collection->failed || collection->groups.empty() ||
      collection->event_ids.size() != DSL_CKKS_Event_Image_Count()) {
    collection->failed = true;
    return Report(diagnostic, "whole-image event coverage is incomplete", 0);
  }
  std::vector<VHO_FHE_CKKS2C_GROUP> candidate = collection->groups;
  std::sort(candidate.begin(), candidate.end(), Group_Less());
  std::set<std::string> identities;
  for (size_t i = 0; i < candidate.size(); ++i)
    if (!identities.insert(candidate[i].identity_sha256).second) {
      collection->failed = true;
      return Report(diagnostic, "duplicate authenticated group identity", 0);
    }
  VHO_FHE_CKKS2C_COLLECT_RESULT counts;
  counts.pu_count = collection->owners.size();
  counts.source_count = collection->sources.size();
  counts.group_count = candidate.size();
  counts.event_count = collection->event_ids.size();
  counts.external_asset_count = collection->external_asset_count;
  counts.key_use_count = collection->key_use_count;
  groups->swap(candidate);
  *result = counts;
  return true;
}
