/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused S6-0d D4 test for canonical public-ABI reconstruction.  The
 * fixture models one root PU, two type-equivalent block clones, fourteen
 * private CKKS groups, representative-only dispositions for the clone source
 * families, and one zero-primitive flatten event.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D4.
 */

#include "fhe_ckks2c_public_join.h"

#include <stdio.h>
#include <string.h>

#include <algorithm>
#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_plan.h"
#include "fhe_semantic_runtime_lower.h"
#include "strtab.h"

namespace {

enum Fixture_String {
  S_NONE = 0,
  S_ROOT_DEFINITION,
  S_BLOCK_DEFINITION,
  S_MODULE,
  S_FILE,
  S_FLATTEN_VALUE,
  S_CONV_VALUE,
  S_RELU_VALUE,
  S_CLASS,
  S_INSTANCE_ONE,
  S_INSTANCE_TWO,
  S_CONTEXT
};

bool omit_flatten;
std::vector<DSL_CKKS_EVENT_RECORD> route_events;

struct Source_Row {
  UINT32 owner;
  UINT32 node;
  UINT32 value;
  UINT32 opcode;
  STR_IDX name;
  UINT32 ty;
};

const Source_Row sources[] = {
  {10, 1, 1, OPR_DSLFLATTEN, S_FLATTEN_VALUE, 100},
  {11, 2, 2, OPR_DSLCONV2D, S_CONV_VALUE, 200},
  {11, 3, 3, OPR_DSLRELU, S_RELU_VALUE, 200},
  {12, 4, 4, OPR_DSLCONV2D, S_CONV_VALUE, 200},
  {12, 5, 5, OPR_DSLRELU, S_RELU_VALUE, 200}
};

/* Resolve one fixed source row by node or value identity. */
const Source_Row *Find_Source(UINT32 id, bool by_node)
{
  for (size_t index = 0; index < sizeof(sources) / sizeof(sources[0]);
       ++index) {
    if ((by_node ? sources[index].node : sources[index].value) == id)
      return &sources[index];
  }
  return NULL;
}

/* Construct one complete runtime-only private group. */
VHO_FHE_CKKS2C_GROUP Group(UINT32 owner, UINT32 source_value,
                           UINT32 source_node, UINT32 logical_operator,
                           const char *source_name, UINT32 identity,
                           UINT32 callsite, UINT32 origin_ordinal,
                           UINT32 serial)
{
  VHO_FHE_CKKS2C_GROUP group;
  group.function_name = "group";
  char digest[65];
  snprintf(digest, sizeof(digest), "%064x", serial);
  group.identity_sha256 = digest;
  group.owner_pu_st = owner;
  group.source_value_id = source_value;
  group.source_node_id = source_node;
  group.source_logical_operator = logical_operator;
  group.source_operator_version = 2;
  group.source_value_name = source_name;
  group.context_pu_identity_id = identity;
  group.context_callsite_id = callsite;
  group.origin_owner_pu_st = owner;
  group.origin_source_value_id = source_value;
  group.origin_static_ordinal = origin_ordinal;
  group.group_output_value_id = 1000 + serial;
  group.event_ids.push_back(2000 + serial);
  group.result_node_ids.push_back(3000 + serial);
  group.result_value_ids.push_back(group.group_output_value_id);
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSADD;
  step.operator_version = 1;
  step.result_ty = 200;
  group.plan.steps.push_back(step);
  group.plan.group_output_steps.push_back(0);
  group.plan.source_static_ordinal = origin_ordinal;
  group.plan.source_final_step_index = 0;
  return group;
}

/* Build the complete two-context private family in deliberately mixed order. */
std::vector<VHO_FHE_CKKS2C_GROUP> Groups()
{
  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  UINT32 serial = 1;
  groups.push_back(Group(11, 2, 2, OPR_DSLCONV2D, "conv", 2, 1, 10,
                         serial++));
  for (UINT32 offset = 0; offset < 6; ++offset)
    groups.push_back(Group(11, 3, 3, OPR_DSLRELU, "relu", 2, 1,
                           11 + offset, serial++));
  groups.push_back(Group(12, 4, 4, OPR_DSLCONV2D, "conv", 3, 2, 20,
                         serial++));
  for (UINT32 offset = 0; offset < 6; ++offset)
    groups.push_back(Group(12, 5, 5, OPR_DSLRELU, "relu", 3, 2,
                           21 + offset, serial++));
  std::reverse(groups.begin(), groups.end());
  return groups;
}

/* Require one rejected request to preserve both caller-owned outputs. */
bool Rejects(const std::vector<VHO_FHE_CKKS2C_GROUP> &groups)
{
  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN plan;
  plan.identity_sha256 = "sentinel";
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT result;
  memset(&result, 0x5a, sizeof(result));
  const VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT before = result;
  return !VHO_FHE_CKKS2C_Build_Public_Join(
             groups, &plan, &result, NULL) &&
         plan.identity_sha256 == "sentinel" &&
         plan.descriptors.empty() && plan.bindings.empty() &&
         memcmp(&result, &before, sizeof(result)) == 0;
}

}  // namespace

BOOL DSL_CKKS_Event_Image_Validate(FILE *diagnostic)
{
  (void)diagnostic;
  return !route_events.empty();
}

UINT32 DSL_CKKS_Event_Image_Count()
{
  return route_events.size();
}

BOOL DSL_CKKS_Event_Image_Get(DSL_CKKS_EVENT_ID id,
                              DSL_CKKS_EVENT_RECORD *record)
{
  if (record == NULL || id == 0 || id > route_events.size())
    return FALSE;
  *record = route_events[id - 1];
  return TRUE;
}

/* Keep string indexes independent of byte offsets. */
STR_IDX STR_Table_Size()
{
  return 32;
}

/* Resolve every fixture string copied by the join. */
char *Index_To_Str(STR_IDX id)
{
  static char empty[] = "";
  static char root[] = "fixture.Root.forward";
  static char block[] = "fixture.Block.forward";
  static char module[] = "fixture";
  static char file[] = "fixture.py";
  static char flatten[] = "flatten";
  static char conv[] = "conv";
  static char relu[] = "relu";
  static char klass[] = "Block";
  static char one[] = "layer.0";
  static char two[] = "layer.1";
  static char context[] = "same_tensor_signature";
  char *strings[] = {empty, root, block, module, file, flatten, conv, relu,
                     klass, one, two, context};
  return id < sizeof(strings) / sizeof(strings[0]) ? strings[id] : NULL;
}

/* Expose one root and two clone-equivalent block identities. */
UINT32 DSL_Call_Image_PU_Identity_Count()
{
  return 3;
}

/* Read one structured PU source identity. */
BOOL DSL_Call_Image_Get_PU_Identity(
    DSL_PU_SOURCE_IDENTITY_ID id, DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
  if (record == NULL || id == 0 || id > 3)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->owner_pu_st = 9 + id;
  record->canonical_definition_name =
      id == 1 ? S_ROOT_DEFINITION : S_BLOCK_DEFINITION;
  record->defining_module = S_MODULE;
  record->defining_file = S_FILE;
  record->defining_line = id == 1 ? 10 : 20;
  return TRUE;
}

/* Expose the root and both block formal/result signatures. */
UINT32 DSL_PU_Interface_Image_Formal_Count()
{
  return 5;
}

/* Read one dense formal row; block clones intentionally share exact TYs. */
BOOL DSL_PU_Interface_Image_Get_Formal(
    DSL_PU_FORMAL_ID id, DSL_PU_FORMAL_RECORD *record)
{
  if (record == NULL || id == 0 || id > 5)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  if (id == 1) {
    record->owner_pu_st = 10;
    record->formal_ordinal = 0;
    record->formal_ty = 100;
  } else {
    record->owner_pu_st = id <= 3 ? 11 : 12;
    record->formal_ordinal = (id - 2) % 2;
    record->formal_ty = record->formal_ordinal == 0 ? 200 : 201;
  }
  return TRUE;
}

/* Read one of the two exact root-to-clone callsites. */
BOOL DSL_Call_Image_Get_Callsite(
    DSL_CALLSITE_METADATA_ID id, DSL_CALLSITE_METADATA_RECORD *record)
{
  if (record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->owner_pu_st = 10;
  record->callee_pu_st = 10 + id;
  record->canonical_class_name = S_CLASS;
  record->instance_path = id == 1 ? S_INSTANCE_ONE : S_INSTANCE_TWO;
  record->context_identity = S_CONTEXT;
  record->source_call_ordinal = id - 1;
  return TRUE;
}

/* Count representative source dispositions; clone duplicates are optional. */
UINT32 DSL_FHE_Plan_Conversion_Disposition_Count()
{
  return omit_flatten ? 2 : 3;
}

/* Bind each retained node/value to its owner PU. */
BOOL DSL_FHE_Plan_Get_Conversion_Disposition(
    DSL_FHE_CONVERSION_DISPOSITION_ID id,
    DSL_FHE_CONVERSION_DISPOSITION_RECORD *record)
{
  if (record == NULL || id == 0 ||
      id > DSL_FHE_Plan_Conversion_Disposition_Count())
    return FALSE;
  const Source_Row &source = sources[omit_flatten ? id : id - 1];
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->source_node_id = source.node;
  record->result_value_id = source.value;
  record->owner_pu_st = source.owner;
  return TRUE;
}

/* Read one retained high-level source node. */
BOOL DSL_IR_Image_Get_Node(DSL_IR_NODE_ID id, DSL_IR_NODE_RECORD *record)
{
  const Source_Row *source = Find_Source(id, true);
  if (record == NULL || source == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = source->node;
  record->opcode_descriptor_id = source->node;
  record->result_value_id = source->value;
  return TRUE;
}

/* Read one retained high-level result value. */
BOOL DSL_IR_Image_Get_Value(DSL_IR_VALUE_ID id, DSL_IR_VALUE_RECORD *record)
{
  const Source_Row *source = Find_Source(id, false);
  if (record == NULL || source == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = source->value;
  record->producer_node_id = source->node;
  record->ty = source->ty;
  record->name = source->name;
  return TRUE;
}

/* Read one logical operator descriptor for a source node. */
BOOL DSL_IR_Image_Get_Opcode_Descriptor(
    DSL_IR_OPCODE_DESCRIPTOR_ID id,
    DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
  const Source_Row *source = Find_Source(id, true);
  if (record == NULL || source == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->logical_operator = source->opcode;
  record->version = 2;
  return TRUE;
}

/* Exercise canonical clone folding, 14+1 coverage, determinism, and rejects. */
int main()
{
  omit_flatten = false;
  std::vector<VHO_FHE_CKKS2C_GROUP> groups = Groups();
  route_events.clear();
  for (UINT32 index = 0; index < groups.size(); ++index) {
    const VHO_FHE_CKKS2C_GROUP &group = groups[index];
    DSL_CKKS_EVENT_RECORD event;
    memset(&event, 0, sizeof(event));
    event.id = index + 1;
    event.owner_pu_st = group.owner_pu_st;
    event.source_value_id = group.source_value_id;
    event.source_node_id = group.source_node_id;
    event.context_pu_identity_id = group.context_pu_identity_id;
    event.context_callsite_id = group.context_callsite_id;
    event.source_static_ordinal = group.origin_static_ordinal;
    event.result_value_id = group.group_output_value_id;
    event.result_node_id = group.result_node_ids[0];
    event.origin_owner_pu_st = group.origin_owner_pu_st;
    event.origin_source_value_id = group.origin_source_value_id;
    event.origin_static_ordinal = group.origin_static_ordinal;
    event.flags = DSL_CKKS_EVENT_FINAL_RESULT;
    route_events.push_back(event);
  }
  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN plan;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT result;
  memset(&result, 0, sizeof(result));
  if (!VHO_FHE_CKKS2C_Build_Public_Join(
          groups, &plan, &result, stderr) ||
      result.canonical_pu_count != 2 ||
      result.canonical_source_count != 3 ||
      result.static_descriptor_count != 8 ||
      result.dynamic_binding_count != 15 ||
      result.private_group_binding_count != 14 ||
      result.representation_binding_count != 1 ||
      plan.identity_sha256.size() != 64 ||
      plan.descriptors[0].operation_kind !=
          VHO_FHE_RUNTIME_OP_LAYOUT_CONVERT ||
      plan.descriptors[1].operation_kind !=
          VHO_FHE_RUNTIME_OP_CONV2D_PLAIN ||
      plan.descriptors[1].execution_multiplicity != 2 ||
      plan.descriptors[2].operation_kind != VHO_FHE_RUNTIME_OP_BOOTSTRAP ||
      plan.descriptors[7].operation_kind !=
          VHO_FHE_RUNTIME_OP_RELU_RECONSTRUCT)
    return 1;

  std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> routes;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT route_result;
  if (!VHO_FHE_CKKS2C_Build_Public_Routes_From_Image(
          &routes, &route_result, stderr) ||
      routes.size() != result.private_group_binding_count ||
      route_result.canonical_pu_count != result.canonical_pu_count ||
      route_result.canonical_source_count != result.canonical_source_count ||
      route_result.static_descriptor_count !=
          result.static_descriptor_count ||
      route_result.dynamic_binding_count != result.dynamic_binding_count)
    return 7;
  for (UINT32 route_index = 0; route_index < routes.size(); ++route_index) {
    UINT32 matches = 0;
    for (UINT32 binding_index = 0; binding_index < plan.bindings.size();
         ++binding_index) {
      const VHO_FHE_CKKS2C_PUBLIC_BINDING &binding =
          plan.bindings[binding_index];
      if (binding.binding_kind == VHO_FHE_CKKS2C_PUBLIC_PRIVATE_GROUP &&
          binding.specialized_owner_pu_st ==
              routes[route_index].specialized_owner_pu_st &&
          binding.specialized_source_value_id ==
              routes[route_index].specialized_source_value_id &&
          binding.context_pu_identity_id ==
              routes[route_index].context_pu_identity_id &&
          binding.context_callsite_id ==
              routes[route_index].context_callsite_id &&
          binding.static_ordinal ==
              routes[route_index].public_static_ordinal &&
          binding.operation_kind == routes[route_index].operation_kind)
        ++matches;
    }
    if (matches != 1)
      return 8;
  }

  std::reverse(groups.begin(), groups.end());
  VHO_FHE_CKKS2C_PUBLIC_JOIN_PLAN reordered;
  VHO_FHE_CKKS2C_PUBLIC_JOIN_RESULT reordered_result;
  if (!VHO_FHE_CKKS2C_Build_Public_Join(
          groups, &reordered, &reordered_result, stderr) ||
      reordered.identity_sha256 != plan.identity_sha256)
    return 2;

  std::vector<VHO_FHE_CKKS2C_GROUP> malformed = groups;
  malformed.push_back(malformed[0]);
  if (!Rejects(malformed))
    return 3;
  malformed = groups;
  /* Shift one member of the first six-group ReLU expansion. */
  malformed[1].origin_static_ordinal += 100;
  if (!Rejects(malformed))
    return 4;
  malformed = groups;
  /* Cross the first clone's source identity with the second clone's callsite. */
  malformed[0].context_callsite_id = 2;
  if (!Rejects(malformed))
    return 5;
  omit_flatten = true;
  if (!Rejects(groups))
    return 6;
  return 0;
}
