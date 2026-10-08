/*
 * Copyright (C) 2026 Open64 Project
 *
 * Context-specialize shared ResNet block PUs for CKKS Conv. The policy uses
 * complete Conv geometry and folded tensor TCON semantics for equality, then
 * lets the generic PU transaction clone/reroute native WHIRL atomically.
 * Clone-local FHE disposition and BN provenance are appended afterward.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md.
 */

#include "fhe_ckks_conv_specialize.h"

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_pu_transaction.h"
#include "dsl_tensor_fold.h"
#include "fhe_ckks_conv_context.h"
#include "fhe_ckks_source_events.h"
#include "fhe_plan.h"
#include "fhe_plan_specialize_internal.h"
#include "fhe_semantic_materialize.h"
#include "fhe_sha256.h"
#include "strtab.h"
#include "symtab.h"

namespace {

typedef std::pair<ST_IDX, DSL_CALLSITE_METADATA_ID> Route_Key;

struct Variant_State {
  ST_IDX source_owner;
  DSL_CALLSITE_METADATA_ID context_callsite_id;
  BOOL use_existing;
  std::vector<unsigned char> signature;
  std::string signature_sha256;
  std::string clone_name;
  std::vector<VHO_FHE_CKKS_CONV_CONTEXT> contexts;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> relu_events;
  VHO_FHE_CKKS_EVENT_IDENTITY input_state_source;
};

struct Specialization_State {
  std::vector<Variant_State> variants;
  std::vector<DSL_PU_TRANSACTION_VARIANT_REQUEST> variant_requests;
  std::vector<DSL_PU_TRANSACTION_ROUTE_REQUEST> route_requests;
};

struct Context_Remap {
  VHO_FHE_CKKS_EVENT_IDENTITY source;
  VHO_FHE_CKKS_EVENT_IDENTITY target;
};

/* Emit one stable policy diagnostic without changing the program. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-CONV-SPECIALIZE-001: %s\n", message);
  return FALSE;
}

/* Append a fixed little-endian integer to canonical signature bytes. */
void Append_U32(std::vector<unsigned char> *bytes, UINT32 value)
{
  for (UINT32 shift = 0; shift != 32; shift += 8)
    bytes->push_back(static_cast<unsigned char>(value >> shift));
}

/* Append a fixed little-endian 64-bit integer to canonical bytes. */
void Append_U64(std::vector<unsigned char> *bytes, UINT64 value)
{
  for (UINT32 shift = 0; shift != 64; shift += 8)
    bytes->push_back(static_cast<unsigned char>(value >> shift));
}

/* Append one length-delimited byte string to canonical signature bytes. */
void Append_Bytes(std::vector<unsigned char> *bytes, const char *data,
                  UINT32 size)
{
  Append_U32(bytes, size);
  if (data != NULL && size != 0)
    bytes->insert(bytes->end(), data, data + size);
}

/* Append stable side-file tensor semantics without using its transient ID. */
BOOL Append_Tensor_TCON(std::vector<unsigned char> *bytes, TCON_IDX tcon)
{
  DSL_TENSOR_TCON_RECORD record;
  const char *side_path = NULL;
  UINT32 side_path_length = 0;
  if (!DSL_Tensor_TCON_Get(tcon, &record) ||
      record.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
      !DSL_Tensor_TCON_Get_Side_Path(tcon, &side_path, &side_path_length))
    return FALSE;
  Append_U32(bytes, record.storage_kind);
  Append_U32(bytes, record.element_mtype);
  Append_U32(bytes, record.element_size);
  Append_U32(bytes, record.required_alignment);
  Append_U64(bytes, record.element_count);
  Append_U64(bytes, record.logical_bytes);
  Append_U64(bytes, record.byte_offset);
  Append_U64(bytes, record.byte_length);
  Append_U64(bytes, record.checksum_hi);
  Append_U64(bytes, record.checksum_lo);
  Append_Bytes(bytes, side_path, side_path_length);
  return TRUE;
}

/* Sort Conv definitions by their source static ordinal within one route. */
BOOL Context_Less(const VHO_FHE_CKKS_CONV_CONTEXT &left,
                  const VHO_FHE_CKKS_CONV_CONTEXT &right)
{
  return left.event.source_static_ordinal <
         right.event.source_static_ordinal;
}

/* Order source events by their stable static schedule ordinal. */
BOOL Event_Less(const VHO_FHE_CKKS_EVENT_IDENTITY &left,
                const VHO_FHE_CKKS_EVENT_IDENTITY &right)
{
  return left.source_static_ordinal < right.source_static_ordinal;
}

/* Compare the persisted identity dimensions that key context state rows. */
BOOL Same_Event_Key(const VHO_FHE_CKKS_EVENT_IDENTITY &left,
                    const VHO_FHE_CKKS_EVENT_IDENTITY &right)
{
  return left.owner_pu_st == right.owner_pu_st &&
         left.source_value_id == right.source_value_id &&
         left.context_pu_identity_id == right.context_pu_identity_id &&
         left.context_callsite_id == right.context_callsite_id;
}

/* Translate a pre-specialization event to its retained or cloned context. */
BOOL Resolve_Event(const std::vector<Context_Remap> &remaps,
                   const VHO_FHE_CKKS_EVENT_IDENTITY &source,
                   VHO_FHE_CKKS_EVENT_IDENTITY *target)
{
  if (target == NULL)
    return FALSE;
  UINT32 matches = 0;
  for (size_t i = 0; i < remaps.size(); ++i) {
    if (!Same_Event_Key(remaps[i].source, source))
      continue;
    *target = remaps[i].target;
    ++matches;
  }
  if (matches == 0) {
    *target = source;
    return TRUE;
  }
  return matches == 1;
}

/* Encode the complete Conv-specific structural and folded-data signature. */
BOOL Build_Signature(std::vector<VHO_FHE_CKKS_CONV_CONTEXT> *contexts,
                     std::vector<unsigned char> *signature)
{
  if (contexts == NULL || contexts->empty() || signature == NULL)
    return FALSE;
  std::sort(contexts->begin(), contexts->end(), Context_Less);
  const unsigned char magic[8] = {'F','H','E','C','O','N','V',1};
  std::vector<unsigned char> built(magic, magic + sizeof(magic));
  Append_U32(&built, static_cast<UINT32>(contexts->size()));
  for (size_t i = 0; i < contexts->size(); ++i) {
    const VHO_FHE_CKKS_CONV_CONTEXT &context = (*contexts)[i];
    const VHO_FHE_CKKS_CONV_SHAPE &shape = context.high_resolution_shape;
    Append_U32(&built, context.event.source_static_ordinal);
    Append_U32(&built, shape.batch);
    Append_U32(&built, shape.input_channels);
    Append_U32(&built, shape.output_channels);
    Append_U32(&built, shape.height);
    Append_U32(&built, shape.width);
    Append_U32(&built, shape.kernel_height);
    Append_U32(&built, shape.kernel_width);
    Append_U32(&built, context.source_stride_height);
    Append_U32(&built, context.source_stride_width);
    Append_U32(&built, shape.pad_top);
    Append_U32(&built, shape.pad_left);
    Append_U32(&built, shape.slot_count);
    if (!Append_Tensor_TCON(&built, context.folded_weight_tcon) ||
        !Append_Tensor_TCON(&built, context.folded_bias_tcon))
      return FALSE;
  }
  signature->swap(built);
  return TRUE;
}

/* Resolve the unique result value produced by one retained logical node. */
BOOL Node_Result(DSL_IR_NODE_ID node_id, DSL_IR_VALUE_ID *value_id)
{
  if (value_id == NULL)
    return FALSE;
  *value_id = DSL_IR_VALUE_INVALID_ID;
  for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
    DSL_IR_VALUE_RECORD value;
    if (DSL_IR_Image_Get_Value(id, &value) &&
        value.producer_node_id == node_id) {
      if (*value_id != DSL_IR_VALUE_INVALID_ID)
        return FALSE;
      *value_id = id;
    }
  }
  return *value_id != DSL_IR_VALUE_INVALID_ID;
}

/* Identify the logical operator that produced one scheduled source value. */
BOOL Event_Operator(const VHO_FHE_CKKS_EVENT_IDENTITY &event,
                    UINT32 *logical_operator)
{
  DSL_IR_VALUE_RECORD value;
  DSL_IR_NODE_RECORD node;
  DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
  if (logical_operator == NULL ||
      !DSL_IR_Image_Get_Value(event.source_value_id, &value) ||
      !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
      !DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,
                                          &descriptor))
    return FALSE;
  *logical_operator = descriptor.logical_operator;
  return TRUE;
}

/* Select every ReLU source event owned by one exact direct-call route. */
BOOL Route_Relu_Events(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    ST_IDX owner_pu_st, DSL_CALLSITE_METADATA_ID callsite_id,
    std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> *relu_events)
{
  if (relu_events == NULL)
    return FALSE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> found;
  for (size_t i = 0; i < events.size(); ++i) {
    UINT32 logical_operator = OPR_DSLUNKNOWN;
    if (events[i].owner_pu_st != owner_pu_st ||
        events[i].context_callsite_id != callsite_id)
      continue;
    if (!Event_Operator(events[i], &logical_operator))
      return FALSE;
    if (logical_operator == OPR_DSLRELU)
      found.push_back(events[i]);
  }
  std::sort(found.begin(), found.end(), Event_Less);
  if (found.empty())
    return FALSE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> contexts;
  for (size_t i = 0; i < found.size(); ++i) {
    size_t match = contexts.size();
    for (size_t j = 0; j < contexts.size(); ++j) {
      if (Same_Event_Key(contexts[j], found[i])) {
        match = j;
        break;
      }
    }
    if (match == contexts.size())
      contexts.push_back(found[i]);
    else if (found[i].source_static_ordinal >
             contexts[match].source_static_ordinal)
      contexts[match] = found[i];
  }
  relu_events->swap(contexts);
  return TRUE;
}

/* Find the final ReLU event whose result feeds one routed block input. */
BOOL Input_State_Source(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    DSL_CALLSITE_METADATA_ID callsite_id,
    VHO_FHE_CKKS_EVENT_IDENTITY *source)
{
  DSL_CALL_ARGUMENT_RECORD input;
  if (source == NULL ||
      !DSL_Call_ABI_Image_Find_Argument_By_Id(callsite_id, 0, &input))
    return FALSE;

  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> candidates;
  for (size_t i = 0; i < events.size(); ++i) {
    UINT32 logical_operator = OPR_DSLUNKNOWN;
    if (events[i].source_value_id == input.argument_value_id &&
        Event_Operator(events[i], &logical_operator) &&
        logical_operator == OPR_DSLRELU)
      candidates.push_back(events[i]);
  }
  if (candidates.empty()) {
    DSL_CALLSITE_METADATA_ID producer_callsite = 0;
    for (DSL_RUNTIME_CALL_PROJECTION_ID id = 1;
         id <= DSL_Runtime_Interface_Image_Call_Count(); ++id) {
      DSL_RUNTIME_CALL_PROJECTION_RECORD projection;
      if (!DSL_Runtime_Interface_Image_Get_Call(id, &projection))
        return FALSE;
      if (projection.direction == DSL_RUNTIME_CALL_RESULT &&
          projection.source_value_id == input.argument_value_id) {
        if (producer_callsite != 0)
          return FALSE;
        producer_callsite = projection.callsite_id;
      }
    }
    DSL_CALLSITE_METADATA_RECORD producer;
    if (producer_callsite == 0 ||
        !DSL_Call_Image_Get_Callsite(producer_callsite, &producer))
      return FALSE;
    for (size_t i = 0; i < events.size(); ++i) {
      UINT32 logical_operator = OPR_DSLUNKNOWN;
      if (events[i].owner_pu_st == producer.callee_pu_st &&
          events[i].context_callsite_id == producer_callsite &&
          Event_Operator(events[i], &logical_operator) &&
          logical_operator == OPR_DSLRELU)
        candidates.push_back(events[i]);
    }
  }
  if (candidates.empty())
    return FALSE;
  std::sort(candidates.begin(), candidates.end(), Event_Less);
  *source = candidates.back();
  return TRUE;
}

/* Translate one source value through the generic transaction clone map. */
BOOL Clone_Value(const DSL_PU_TRANSACTION_RESULT *result,
                 UINT32 variant_index, DSL_IR_VALUE_ID source,
                 DSL_IR_VALUE_ID *clone)
{
  if (clone == NULL)
    return FALSE;
  if (source == DSL_IR_VALUE_INVALID_ID) {
    *clone = DSL_IR_VALUE_INVALID_ID;
    return TRUE;
  }
  return DSL_PU_Transaction_Cloned_Value(
      result, variant_index, source, clone);
}

/* Map a value to the selected variant; the reused variant is its own map. */
BOOL Variant_Value(const DSL_PU_TRANSACTION_RESULT *result,
                   UINT32 variant_index, BOOL use_existing,
                   DSL_IR_VALUE_ID source, DSL_IR_VALUE_ID *target)
{
  if (target == NULL || source == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  if (use_existing) {
    *target = source;
    return TRUE;
  }
  return Clone_Value(result, variant_index, source, target);
}

/* Convert context evidence into the next concrete value-state version. */
BOOL Bind_Context_Result_State(
    DSL_IR_VALUE_ID value_id,
    const VHO_FHE_CKKS_EVENT_IDENTITY &source_event,
    DSL_FHE_CKKS_VALUE_STATE_ID *state_id,
    FILE *diagnostic)
{
  if (state_id == NULL)
    return FALSE;
  *state_id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
  DSL_FHE_CONTEXT_CKKS_STATE_RECORD context;
  if (!DSL_FHE_Context_State_Find_Latest(
          source_event.owner_pu_st, source_event.source_value_id,
          source_event.context_pu_identity_id,
          source_event.context_callsite_id,
          DSL_FHE_CONTEXT_STATE_ROLE_RESULT, &context)) {
    if (diagnostic != NULL)
      fprintf(diagnostic,
              "CFHEIR-CONV-SPECIALIZE-001: missing RESULT owner=%u "
              "value=%u identity=%u callsite=%u ordinal=%u\n",
              source_event.owner_pu_st, source_event.source_value_id,
              source_event.context_pu_identity_id,
              source_event.context_callsite_id,
              source_event.source_static_ordinal);
    return Report(diagnostic, "context RESULT state is missing");
  }

  DSL_FHE_CKKS_VALUE_STATE_RECORD prior;
  DSL_FHE_CKKS_VALUE_STATE_RECORD state;
  DSL_FHE_CKKS_Value_State_Record_Init(&state);
  state.value_id = value_id;
  state.state_version = 1;
  if (DSL_FHE_Plan_Find_Latest_CKKS_Value_State(value_id, &prior))
    state.state_version = prior.state_version + 1;
  state.encryption_descriptor_id = context.encryption_descriptor_id;
  state.scheme = context.scheme;
  state.value_class = context.value_class;
  state.level = context.level;
  state.scale_bits = context.scale_bits;
  state.component_count = context.component_count;
  state.precision_bits = context.precision_bits;
  state.slot_count = context.slot_count;
  state.alignment_group = context.alignment_group;
  state.encrypted_layout_name = context.encrypted_layout_name;
  state.pending_actions = 0;
  state.pending_bootstrap_reason = DSL_FHE_BOOTSTRAP_REASON_NONE;
  *state_id = DSL_FHE_Plan_Add_CKKS_Value_State(&state);
  if (*state_id == DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
    return Report(diagnostic, "concrete clone-local CKKS state was rejected");
  return TRUE;
}

/* Resolve the one persisted approximation range for an exact source event. */
BOOL Find_Context_Range(
    const VHO_FHE_CKKS_EVENT_IDENTITY &event,
    DSL_FHE_CONTEXT_RANGE_RECORD *range)
{
  if (range == NULL)
    return FALSE;
  UINT32 matches = 0;
  for (DSL_FHE_CONTEXT_RANGE_ID id = 1;
       id <= DSL_FHE_Context_Range_Count(); ++id) {
    DSL_FHE_CONTEXT_RANGE_RECORD candidate;
    if (!DSL_FHE_Context_Range_Get(id, &candidate))
      return FALSE;
    if (candidate.owner_pu_st != event.owner_pu_st ||
        candidate.source_relu_value_id != event.source_value_id ||
        candidate.context_pu_identity_id !=
            event.context_pu_identity_id ||
        candidate.context_callsite_id != event.context_callsite_id)
      continue;
    *range = candidate;
    ++matches;
  }
  return matches == 1;
}

/* Translate a source logical node through its result-value clone mapping. */
BOOL Clone_Node(const DSL_PU_TRANSACTION_RESULT *result,
                UINT32 variant_index, DSL_IR_NODE_ID source_node,
                DSL_IR_NODE_ID *clone_node, DSL_IR_VALUE_ID *clone_value)
{
  DSL_IR_VALUE_ID source_value = DSL_IR_VALUE_INVALID_ID;
  DSL_IR_VALUE_RECORD value;
  if (clone_node == NULL || clone_value == NULL ||
      !Node_Result(source_node, &source_value) ||
      !Clone_Value(result, variant_index, source_value, clone_value) ||
      !DSL_IR_Image_Get_Value(*clone_value, &value))
    return FALSE;
  *clone_node = value.producer_node_id;
  return *clone_node != DSL_IR_NODE_INVALID_ID;
}

/* Build nine called-context variants from the certified 21-Conv census. */
BOOL Build_Plan(PU_Info *, DSL_PU_TRANSACTION_PLAN *plan,
                void **policy_state, FILE *diagnostic)
{
  if (plan == NULL || policy_state == NULL)
    return Report(diagnostic, "transaction output is missing");
  UINT32 prepared_contexts = 0;
  for (DSL_FHE_CONTEXT_RANGE_ID id = 1;
       id <= DSL_FHE_Context_Range_Count(); ++id) {
    if (!VHO_FHE_Ensure_Relu_Materialization_Context(id, diagnostic))
      return Report(diagnostic,
                    "ReLU planning schedule cannot be prepared");
    ++prepared_contexts;
  }
  if (prepared_contexts != 19)
    return Report(diagnostic,
                  "ResNet ReLU context census is not nineteen");
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  std::vector<VHO_FHE_CKKS_CONV_CONTEXT> contexts;
  if (!VHO_FHE_CKKS_Collect_Source_Events(&events, diagnostic) ||
      !VHO_FHE_CKKS_Collect_Conv_Contexts(
          events, 32768, &contexts, diagnostic))
    return Report(diagnostic, "source Conv census cannot be collected");

  std::set<std::pair<ST_IDX, DSL_IR_NODE_ID> > definitions;
  std::map<Route_Key, std::vector<VHO_FHE_CKKS_CONV_CONTEXT> > routes;
  UINT32 root_count = 0;
  for (size_t i = 0; i < contexts.size(); ++i) {
    definitions.insert(std::make_pair(
        contexts[i].event.owner_pu_st, contexts[i].source_node_id));
    if (contexts[i].event.context_callsite_id == 0)
      ++root_count;
    else
      routes[Route_Key(contexts[i].event.owner_pu_st,
                       contexts[i].event.context_callsite_id)].push_back(
                           contexts[i]);
  }
  if (contexts.size() != 21 || definitions.size() != 13 || root_count != 1 ||
      routes.size() != 9)
    return Report(diagnostic,
                  "ResNet Conv census is not 13 definitions/21 contexts");

  Specialization_State *state = new Specialization_State;
  std::map<ST_IDX, BOOL> owner_has_existing;
  std::map<Route_Key, UINT32> route_variant;
  for (std::map<Route_Key,
                std::vector<VHO_FHE_CKKS_CONV_CONTEXT> >::iterator route =
           routes.begin(); route != routes.end(); ++route) {
    std::vector<unsigned char> signature;
    if (!Build_Signature(&route->second, &signature)) {
      delete state;
      return Report(diagnostic, "Conv variant signature is incomplete");
    }
    UINT32 found = static_cast<UINT32>(state->variants.size());
    for (UINT32 i = 0; i < state->variants.size(); ++i)
      if (state->variants[i].source_owner == route->first.first &&
          state->variants[i].signature == signature) {
        found = i;
        break;
      }
    if (found == state->variants.size()) {
      Variant_State variant;
      variant.source_owner = route->first.first;
      variant.context_callsite_id = route->first.second;
      variant.use_existing = !owner_has_existing[route->first.first];
      owner_has_existing[route->first.first] = TRUE;
      variant.signature.swap(signature);
      variant.signature_sha256 = VHO_FHE_SHA256(
          &variant.signature[0], variant.signature.size());
      if (!variant.use_existing) {
        variant.clone_name = ST_name(St_Table[variant.source_owner]);
        variant.clone_name += "__fhe_";
        variant.clone_name += variant.signature_sha256.substr(0, 12);
      }
      state->variants.push_back(variant);
      found = static_cast<UINT32>(state->variants.size() - 1);
    }
    if (state->variants[found].context_callsite_id != route->first.second ||
        !state->variants[found].contexts.empty()) {
      delete state;
      return Report(diagnostic,
                    "distinct call routes unexpectedly share one Conv variant");
    }
    if (!Route_Relu_Events(events, route->first.first,
                           route->first.second,
                           &state->variants[found].relu_events) ||
        !Input_State_Source(events, route->first.second,
                            &state->variants[found].input_state_source)) {
      delete state;
      return Report(diagnostic,
                    "Conv route has no exact input or ReLU state source");
    }
    state->variants[found].contexts.insert(
        state->variants[found].contexts.end(), route->second.begin(),
        route->second.end());
    route_variant[route->first] = found;
  }

  state->variant_requests.resize(state->variants.size());
  for (UINT32 i = 0; i < state->variants.size(); ++i) {
    DSL_PU_TRANSACTION_VARIANT_REQUEST &request =
        state->variant_requests[i];
    memset(&request, 0, sizeof(request));
    request.source_pu_st = state->variants[i].source_owner;
    request.clone_name = state->variants[i].use_existing ? NULL :
                         state->variants[i].clone_name.c_str();
    request.signature_bytes = &state->variants[i].signature[0];
    request.signature_size = state->variants[i].signature.size();
    request.signature_sha256 =
        state->variants[i].signature_sha256.c_str();
    request.use_existing_pu = state->variants[i].use_existing;
  }
  state->route_requests.resize(route_variant.size());
  UINT32 route_index = 0;
  for (std::map<Route_Key, UINT32>::const_iterator route =
           route_variant.begin(); route != route_variant.end(); ++route) {
    DSL_PU_TRANSACTION_ROUTE_REQUEST &request =
        state->route_requests[route_index++];
    memset(&request, 0, sizeof(request));
    request.callsite_id = route->first.second;
    request.variant_index = route->second;
  }
  plan->variants = &state->variant_requests[0];
  plan->variant_count = state->variant_requests.size();
  plan->routes = &state->route_requests[0];
  plan->route_count = state->route_requests.size();
  *policy_state = state;
  return TRUE;
}

/* Append the cloned Conv planning rows that generic DSL cloning cannot own. */
BOOL After_Apply(PU_Info *, const DSL_PU_TRANSACTION_RESULT *result,
                 void *policy_state, FILE *diagnostic)
{
  Specialization_State *state =
      static_cast<Specialization_State *>(policy_state);
  if (result == NULL || state == NULL)
    return Report(diagnostic, "specialization result is missing");
  std::vector<Context_Remap> context_remaps;
  for (UINT32 variant_index = 0;
       variant_index < state->variants.size(); ++variant_index) {
    Variant_State &variant = state->variants[variant_index];
    ST_IDX clone_owner = ST_IDX_ZERO;
    DSL_PU_SOURCE_IDENTITY_RECORD clone_identity;
    memset(&clone_identity, 0, sizeof(clone_identity));
    if (!variant.use_existing) {
      clone_owner =
          DSL_PU_Transaction_Variant_PU_ST(result, variant_index);
      if (clone_owner == ST_IDX_ZERO ||
          !DSL_Call_Image_Find_PU_Identity(clone_owner, &clone_identity))
        return Report(diagnostic, "clone source identity is missing");
    }
    for (size_t relu_index = 0;
         relu_index < variant.relu_events.size(); ++relu_index) {
      const VHO_FHE_CKKS_EVENT_IDENTITY &source_event =
          variant.relu_events[relu_index];
      DSL_IR_VALUE_ID relu_value = DSL_IR_VALUE_INVALID_ID;
      DSL_FHE_CKKS_VALUE_STATE_ID relu_state_id =
          DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
      if (!Variant_Value(result, variant_index, variant.use_existing,
                         source_event.source_value_id, &relu_value) ||
          !Bind_Context_Result_State(relu_value, source_event,
                                     &relu_state_id, diagnostic))
        return Report(diagnostic,
                      "specialized ReLU result state cannot be bound");
      Context_Remap remap;
      remap.source = source_event;
      remap.target = source_event;
      if (!variant.use_existing) {
        DSL_IR_VALUE_RECORD relu_value_record;
        DSL_FHE_CONTEXT_RANGE_RECORD range;
        remap.target.owner_pu_st = clone_owner;
        remap.target.source_value_id = relu_value;
        remap.target.context_pu_identity_id = clone_identity.id;
        if (!DSL_IR_Image_Get_Value(relu_value, &relu_value_record) ||
            relu_value_record.producer_node_id ==
                DSL_IR_NODE_INVALID_ID ||
            !Find_Context_Range(source_event, &range) ||
            !DSL_FHE_Plan_Specialize_Composite_Context(
                &range, clone_owner, relu_value_record.producer_node_id,
                relu_value, clone_identity.id, relu_state_id))
          return Report(diagnostic,
                        "specialized ReLU context cannot be moved");
      }
      context_remaps.push_back(remap);
    }
  }
  std::map<DSL_IR_NODE_ID, DSL_FHE_BN_FOLD_PROVENANCE_ID>
      retained_source_folds;
  for (UINT32 variant_index = 0;
       variant_index < state->variants.size(); ++variant_index) {
    Variant_State &variant = state->variants[variant_index];
    ST_IDX clone_owner = ST_IDX_ZERO;
    DSL_PU_SOURCE_IDENTITY_RECORD clone_identity;
    memset(&clone_identity, 0, sizeof(clone_identity));
    if (!variant.use_existing) {
      clone_owner =
          DSL_PU_Transaction_Variant_PU_ST(result, variant_index);
      if (clone_owner == ST_IDX_ZERO ||
          !DSL_Call_Image_Find_PU_Identity(clone_owner, &clone_identity))
        return Report(diagnostic, "clone source identity is missing");
    }
    DSL_PU_FORMAL_RECORD source_input;
    DSL_IR_VALUE_ID input_value = DSL_IR_VALUE_INVALID_ID;
    DSL_FHE_CKKS_VALUE_STATE_ID input_state_id =
        DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
    VHO_FHE_CKKS_EVENT_IDENTITY input_state_source;
    if (!DSL_PU_Interface_Image_Find_Formal(
            variant.source_owner, 0, &source_input) ||
        !Variant_Value(result, variant_index, variant.use_existing,
                       source_input.formal_value_id, &input_value) ||
        !Resolve_Event(context_remaps, variant.input_state_source,
                       &input_state_source) ||
        !Bind_Context_Result_State(input_value,
                                   input_state_source,
                                   &input_state_id,
                                   diagnostic))
      return Report(diagnostic,
                    "specialized block input state cannot be bound");
    (void)input_state_id;
    if (variant.use_existing) {
      for (size_t i = 0; i < variant.contexts.size(); ++i) {
        const VHO_FHE_CKKS_CONV_CONTEXT &context = variant.contexts[i];
        DSL_FHE_BN_FOLD_PROVENANCE_RECORD source_fold;
        if (!DSL_FHE_Plan_Find_BN_Fold_Provenance(
                context.source_node_id,
                context.event.context_pu_identity_id,
                context.event.context_callsite_id, &source_fold) ||
            retained_source_folds.find(context.source_node_id) !=
                retained_source_folds.end())
          return Report(diagnostic,
                        "retained source BN context is not unique");
        retained_source_folds[context.source_node_id] = source_fold.id;
      }
      continue;
    }
    std::map<DSL_IR_NODE_ID,
             std::vector<VHO_FHE_CKKS_CONV_CONTEXT> > by_node;
    for (size_t i = 0; i < variant.contexts.size(); ++i)
      by_node[variant.contexts[i].source_node_id].push_back(
          variant.contexts[i]);
    for (std::map<DSL_IR_NODE_ID,
                  std::vector<VHO_FHE_CKKS_CONV_CONTEXT> >::const_iterator
             group = by_node.begin(); group != by_node.end(); ++group) {
      DSL_IR_NODE_ID clone_conv_node = DSL_IR_NODE_INVALID_ID;
      DSL_IR_VALUE_ID clone_conv_value = DSL_IR_VALUE_INVALID_ID;
      DSL_FHE_CONVERSION_DISPOSITION_RECORD source_disposition;
      if (!Clone_Node(result, variant_index, group->first,
                      &clone_conv_node, &clone_conv_value) ||
          !DSL_FHE_Plan_Find_Conversion_Disposition(
              group->first, &source_disposition))
        return Report(diagnostic, "clone Conv disposition cannot be mapped");

      DSL_FHE_BN_FOLD_PROVENANCE_ID first_fold =
          DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;
      for (size_t i = 0; i < group->second.size(); ++i) {
        const VHO_FHE_CKKS_CONV_CONTEXT &context = group->second[i];
        DSL_FHE_BN_FOLD_PROVENANCE_RECORD source_fold;
        if (!DSL_FHE_Plan_Find_BN_Fold_Provenance(
                group->first, context.event.context_pu_identity_id,
                context.event.context_callsite_id, &source_fold))
          return Report(diagnostic, "source BN-fold row is missing");
        DSL_IR_NODE_ID clone_batch_norm_node = DSL_IR_NODE_INVALID_ID;
        DSL_IR_VALUE_ID clone_batch_norm_value = DSL_IR_VALUE_INVALID_ID;
        if (!Clone_Node(result, variant_index, source_fold.batch_norm_node_id,
                        &clone_batch_norm_node,
                        &clone_batch_norm_value))
          return Report(diagnostic, "clone BatchNorm node cannot be mapped");
        /* Fold payload provenance is call-context data owned by the caller.
         * Only the callee Conv/BatchNorm definition nodes are cloned. Keeping
         * these value IDs preserves the exact external tensors selected by
         * this callsite instead of falsely rebinding them to callee formals. */
        if (!DSL_FHE_Plan_Specialize_BN_Fold(
                source_fold.id, &source_fold, clone_owner, clone_conv_node,
                clone_batch_norm_node, clone_identity.id))
          return Report(diagnostic, "clone BN-fold row was rejected");
        if (first_fold == DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID)
          first_fold = source_fold.id;
      }

      DSL_FHE_CKKS_VALUE_STATE_RECORD source_state;
      DSL_FHE_CKKS_VALUE_STATE_RECORD clone_state;
      if (!DSL_FHE_Plan_Get_CKKS_Value_State(
              source_disposition.result_ckks_value_state_id,
              &source_state))
        return Report(diagnostic, "source Conv CKKS state is missing");
      clone_state = source_state;
      clone_state.id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
      clone_state.value_id = clone_conv_value;
      const DSL_FHE_CKKS_VALUE_STATE_ID state_id =
          DSL_FHE_Plan_Add_CKKS_Value_State(&clone_state);
      if (state_id == DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
        return Report(diagnostic, "clone Conv CKKS state was rejected");

      DSL_FHE_CONVERSION_DISPOSITION_RECORD clone_disposition =
          source_disposition;
      clone_disposition.id = DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;
      clone_disposition.source_node_id = clone_conv_node;
      clone_disposition.result_value_id = clone_conv_value;
      clone_disposition.owner_pu_st = clone_owner;
      clone_disposition.result_ckks_value_state_id = state_id;
      clone_disposition.first_bn_fold_id = first_fold;
      clone_disposition.bn_fold_count = group->second.size();
      if (DSL_FHE_Plan_Add_Conversion_Disposition(&clone_disposition) ==
          DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID)
          return Report(diagnostic, "clone Conv disposition was rejected");
    }
  }
  for (std::map<DSL_IR_NODE_ID,
                DSL_FHE_BN_FOLD_PROVENANCE_ID>::const_iterator
           retained = retained_source_folds.begin();
       retained != retained_source_folds.end(); ++retained) {
    DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
    if (!DSL_FHE_Plan_Find_Conversion_Disposition(
            retained->first, &disposition) ||
        !DSL_FHE_Plan_Specialize_Disposition_BN_Range(
            disposition.id, disposition.first_bn_fold_id,
            disposition.bn_fold_count, retained->second, 1))
      return Report(diagnostic,
                    "source Conv disposition cannot be repartitioned");
  }
  return DSL_FHE_Plan_Image_Validate_Partial(diagnostic);
}

/* Release all pointer-owning request storage after the transaction. */
void Release(void *policy_state)
{
  delete static_cast<Specialization_State *>(policy_state);
}

}  // namespace

/* Install the FHE policy through the generic whole-program transaction API. */
BOOL VHO_FHE_CKKS_Register_Conv_Specialization(void)
{
  DSL_PU_TRANSACTION_POLICY policy;
  memset(&policy, 0, sizeof(policy));
  policy.build_plan = Build_Plan;
  policy.after_apply = After_Apply;
  policy.release = Release;
  return DSL_PU_Transaction_Register_Policy(&policy);
}
