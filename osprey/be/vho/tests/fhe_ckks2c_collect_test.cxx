/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused two-PU certification for the read-only D2 CKKS2C collector.  The
 * fixture supplies typed image-table substitutes and proves exact joins,
 * insertion-order independence, and fail-closed malformed evidence without
 * constructing or mutating WHIRL.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D2.
 */

#include "fhe_ckks2c_collect.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <algorithm>
#include <iterator>
#include <map>
#include <string>
#include <vector>

#include "dsl_ckks_event.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "strtab.h"

static std::vector<DSL_CKKS_EVENT_RECORD> events;
static std::map<UINT32, DSL_IR_NODE_RECORD> nodes;
static std::map<UINT32, DSL_IR_VALUE_RECORD> values;
static std::map<UINT32, DSL_IR_VALUE_REFERENCE_RECORD> references;
static std::map<UINT32, DSL_IR_ATTRIBUTE_RECORD> attributes;
static std::map<UINT32, DSL_IR_OPCODE_DESCRIPTOR_RECORD> opcodes;
static std::map<UINT32, DSL_FHE_CKKS_VALUE_STATE_RECORD> states;
static UINT32 active_owner;
static BOOL missing_state;
static BOOL missing_key;
static BOOL bad_checksum;

enum Fixture_String {
  S_NONE = 0,
  S_ROOT = 1,
  S_CALLEE,
  S_MODULE,
  S_FILE,
  S_ROOT_SOURCE,
  S_CALLEE_SOURCE,
  S_CLASS,
  S_PATH,
  S_CONTEXT,
  S_LAYOUT,
  S_KEY,
  S_ROLE_INPUT,
  S_ROLE_BOUND,
  S_SIGNED_STEPS,
  S_ONE,
  S_KEY_ID,
  S_OP_ENCODE,
  S_OP_ROTATE,
  S_OP_ADD,
  S_OP_MUL,
  S_OP_RELIN
};

/* Keep string IDs intentionally unrelated to their textual byte offsets. */
STR_IDX STR_Table_Size()
{
  return 64;
}

/* Resolve every stable semantic string copied by the collector. */
char *Index_To_Str(STR_IDX id)
{
  static char empty[] = "";
  static char root[] = "Root";
  static char callee[] = "Block";
  static char module[] = "fixture";
  static char file[] = "fixture.py";
  static char root_source[] = "root.conv";
  static char callee_source[] = "block.relu";
  static char klass[] = "ResNet20Block";
  static char path[] = "layer1.0";
  static char context[] = "shape_16x32x32";
  static char layout[] = "ckks.packed";
  static char key[] = "request_key";
  static char role_input[] = "activation";
  static char role_bound[] = "relu_bound_0";
  static char signed_steps[] = "attr.signed_steps";
  static char one[] = "1";
  static char key_id[] = "attr.key_id";
  static char encode[] = "ckks.encode";
  static char rotate[] = "ckks.rotate";
  static char add[] = "ckks.add";
  static char mul[] = "ckks.mul";
  static char relin[] = "ckks.relin";
  char *table[] = {empty, root, callee, module, file, root_source,
                   callee_source, klass, path, context, layout, key,
                   role_input, role_bound, signed_steps, one, key_id,
                   encode, rotate, add, mul, relin};
  return id < sizeof(table) / sizeof(table[0]) ? table[id] : NULL;
}

/* Expose the current fixture event table. */
UINT32 DSL_CKKS_Event_Image_Count()
{
  return events.size();
}

/* Copy one event without exposing fixture storage. */
BOOL DSL_CKKS_Event_Image_Get(DSL_CKKS_EVENT_ID id,
                              DSL_CKKS_EVENT_RECORD *record)
{
  if (id == 0 || id > events.size() || record == NULL)
    return FALSE;
  *record = events[id - 1];
  return TRUE;
}

/* Resolve the root and callee source identities. */
BOOL DSL_Call_Image_Get_PU_Identity(
    DSL_PU_SOURCE_IDENTITY_ID id, DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
  if (record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->owner_pu_st = id == 1 ? 10 : 11;
  record->canonical_definition_name = id == 1 ? S_ROOT : S_CALLEE;
  record->defining_module = S_MODULE;
  record->defining_file = S_FILE;
  record->defining_line = id == 1 ? 10 : 20;
  return TRUE;
}

/* Resolve the sole called context into PU 11. */
BOOL DSL_Call_Image_Get_Callsite(
    DSL_CALLSITE_METADATA_ID id, DSL_CALLSITE_METADATA_RECORD *record)
{
  if (record == NULL || id != 1)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = 1;
  record->owner_pu_st = 10;
  record->callee_pu_st = 11;
  record->canonical_class_name = S_CLASS;
  record->instance_path = S_PATH;
  record->context_identity = S_CONTEXT;
  record->source_call_ordinal = 1;
  return TRUE;
}

/* Supply the callee activation and bound formal identities. */
UINT32 DSL_PU_Interface_Image_Formal_Count()
{
  return 2;
}

BOOL DSL_PU_Interface_Image_Get_Formal(
    DSL_PU_FORMAL_ID id, DSL_PU_FORMAL_RECORD *record)
{
  if (record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->owner_pu_st = 11;
  record->formal_value_id = id == 1 ? 40 : 41;
  record->formal_ordinal = id - 1;
  record->formal_ty = 500;
  return TRUE;
}

/* Supply semantic roles for both callee formals. */
UINT32 DSL_Call_ABI_Image_Argument_Count()
{
  return 2;
}

BOOL DSL_Call_ABI_Image_Get_Argument(
    DSL_CALL_ARGUMENT_ID id, DSL_CALL_ARGUMENT_RECORD *record)
{
  if (record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->callsite_id = 1;
  record->argument_value_id = id == 1 ? 30 : 31;
  record->actual_ordinal = id - 1;
  record->callee_formal_ordinal = id - 1;
  record->semantic_role = id == 1 ? S_ROLE_INPUT : S_ROLE_BOUND;
  return TRUE;
}

/* Read one logical node from the fixture. */
BOOL DSL_IR_Image_Get_Node(DSL_IR_NODE_ID id, DSL_IR_NODE_RECORD *record)
{
  if (record == NULL || nodes.count(id) == 0)
    return FALSE;
  *record = nodes[id];
  return TRUE;
}

/* Read one logical value from the fixture. */
BOOL DSL_IR_Image_Get_Value(DSL_IR_VALUE_ID id, DSL_IR_VALUE_RECORD *record)
{
  if (record == NULL || values.count(id) == 0)
    return FALSE;
  *record = values[id];
  return TRUE;
}

/* Read one direct operand relation from the fixture. */
BOOL DSL_IR_Image_Get_Value_Reference(
    DSL_IR_VALUE_REFERENCE_ID id, DSL_IR_VALUE_REFERENCE_RECORD *record)
{
  if (record == NULL || references.count(id) == 0)
    return FALSE;
  *record = references[id];
  return TRUE;
}

/* Read one exact operation attribute from the fixture. */
BOOL DSL_IR_Image_Get_Attribute(
    DSL_IR_ATTRIBUTE_ID id, DSL_IR_ATTRIBUTE_RECORD *record)
{
  if (record == NULL || attributes.count(id) == 0)
    return FALSE;
  *record = attributes[id];
  return TRUE;
}

/* Read one CKKS logical opcode descriptor. */
BOOL DSL_IR_Image_Get_Opcode_Descriptor(
    DSL_IR_OPCODE_DESCRIPTOR_ID id,
    DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
  if (record == NULL || opcodes.count(id) == 0)
    return FALSE;
  *record = opcodes[id];
  return TRUE;
}

/* Expose the fixed-row state count used to build the collector index. */
UINT32 DSL_FHE_Plan_CKKS_Value_State_Count()
{
  return states.size();
}

/* Read one state row in stable value-ID order for the collector index. */
BOOL DSL_FHE_Plan_Get_CKKS_Value_State(
    DSL_FHE_CKKS_VALUE_STATE_ID id,
    DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{
  if (missing_state || record == NULL || id == 0 || id > states.size())
    return FALSE;
  std::map<UINT32, DSL_FHE_CKKS_VALUE_STATE_RECORD>::const_iterator state =
      states.begin();
  std::advance(state, id - 1);
  *record = state->second;
  return TRUE;
}

/* Validate the sole encoded asset against the active owner PU. */
BOOL DSL_IR_Image_Get_External_Tensor_Reference(
    ST_IDX owner, DSL_IR_VALUE_ID value_id,
    DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference)
{
  static const char digest[] =
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  static const char bad[] = "bad";
  if (reference == NULL || owner != active_owner || owner != 10 ||
      value_id != 20)
    return FALSE;
  memset(reference, 0, sizeof(*reference));
  reference->value_id = value_id;
  reference->descriptor_ty = 600;
  reference->checksum = bad_checksum ? bad : digest;
  return TRUE;
}

/* Supply plaintext/ciphertext descriptors sharing one config. */
BOOL DSL_FHE_Get_Encryption_Descriptor(
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID id,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{
  if (record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->config_id = 1;
  record->scheme = DSL_FHE_SCHEME_CKKS;
  record->value_class = id == 1 ?
      DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT :
      DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  record->key_set_name = id == 1 ? 0 : S_KEY;
  return TRUE;
}

/* Expose rotation plus relinearization key requirements. */
UINT32 DSL_FHE_Key_Requirement_Count()
{
  return 2;
}

BOOL DSL_FHE_Get_Key_Requirement(
    DSL_FHE_KEY_REQUIREMENT_ID id, DSL_FHE_KEY_REQUIREMENT_RECORD *record)
{
  if (missing_key || record == NULL || id == 0 || id > 2)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->config_id = 1;
  record->key_set_name = S_KEY;
  record->key_class = id == 1 ? DSL_FHE_KEY_ROTATION :
                               DSL_FHE_KEY_RELINEARIZATION;
  record->rotation_offset = id == 1 ? 1 : 0;
  return TRUE;
}

/* Install one value with a stable name and optional executable producer. */
static void Add_Value(UINT32 id, UINT32 producer, UINT32 ty, STR_IDX name)
{
  DSL_IR_VALUE_RECORD value;
  memset(&value, 0, sizeof(value));
  value.id = id;
  value.value_kind = producer == 0 ? DSL_IR_VALUE_SYMBOL :
                                    DSL_IR_VALUE_OPERATOR_RESULT;
  value.producer_node_id = producer;
  value.ty = ty;
  value.name = name;
  values[id] = value;
}

/* Install one CKKS node and contiguous typed operand/attribute rows. */
static void Add_Node(UINT32 id, UINT32 opcode_id, UINT32 result,
                     const std::vector<UINT32> &operands,
                     const std::vector<std::pair<STR_IDX, STR_IDX> > &attrs)
{
  DSL_IR_NODE_RECORD node;
  memset(&node, 0, sizeof(node));
  node.id = id;
  node.opcode_descriptor_id = opcode_id;
  node.result_value_id = result;
  node.first_operand_reference_id = references.size() + 1;
  node.operand_count = operands.size();
  for (UINT32 i = 0; i < operands.size(); ++i) {
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    memset(&reference, 0, sizeof(reference));
    reference.id = references.size() + 1;
    reference.owner_node_id = id;
    reference.ordinal = i;
    reference.value_id = operands[i];
    references[reference.id] = reference;
  }
  node.first_attribute_id = attrs.empty() ? 0 : attributes.size() + 1;
  node.attribute_count = attrs.size();
  for (UINT32 i = 0; i < attrs.size(); ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    memset(&attribute, 0, sizeof(attribute));
    attribute.id = attributes.size() + 1;
    attribute.owner_node_id = id;
    attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attribute.name = attrs[i].first;
    attribute.value = attrs[i].second;
    attributes[attribute.id] = attribute;
  }
  nodes[id] = node;
  Add_Value(result, id, 500, result < 200 ? S_ROOT_SOURCE : S_CALLEE_SOURCE);
}

/* Install one concrete value state with explicit plaintext/ciphertext class. */
static void Add_State(UINT32 value, BOOL plaintext)
{
  DSL_FHE_CKKS_VALUE_STATE_RECORD state;
  memset(&state, 0, sizeof(state));
  state.id = value;
  state.value_id = value;
  state.encryption_descriptor_id = plaintext ? 1 : 2;
  state.state_version = 1;
  state.scheme = DSL_FHE_SCHEME_CKKS;
  state.value_class = plaintext ? DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT :
                                  DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  state.level = 18;
  state.scale_bits = 56;
  state.component_count = plaintext ? 1 : 2;
  state.precision_bits = 30;
  state.slot_count = 32768;
  state.encrypted_layout_name = S_LAYOUT;
  states[value] = state;
}

/* Rebuild the complete fixture so each negative starts from clean evidence. */
static void Initialize_Fixture()
{
  events.clear(); nodes.clear(); values.clear(); references.clear();
  attributes.clear(); opcodes.clear(); states.clear();
  active_owner = 0; missing_state = FALSE; missing_key = FALSE;
  bad_checksum = FALSE;
  const UINT32 logical_ops[] = {OPR_DSLCKKSENCODE, OPR_DSLCKKSROTATE,
                                OPR_DSLCKKSADD, OPR_DSLCKKSMUL,
                                OPR_DSLCKKSRELIN};
  const STR_IDX names[] = {S_OP_ENCODE, S_OP_ROTATE, S_OP_ADD,
                           S_OP_MUL, S_OP_RELIN};
  for (UINT32 i = 0; i < 5; ++i) {
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    memset(&opcode, 0, sizeof(opcode));
    opcode.id = i + 1;
    opcode.logical_operator = logical_ops[i];
    opcode.version = 1;
    opcode.stable_name = names[i];
    opcodes[opcode.id] = opcode;
  }
  Add_Value(20, 0, 600, S_ROOT_SOURCE);
  Add_Value(30, 0, 500, S_ROOT_SOURCE);
  Add_Value(40, 0, 500, S_CALLEE_SOURCE);
  Add_Value(41, 0, 500, S_CALLEE_SOURCE);
  Add_Value(50, 0, 500, S_ROOT_SOURCE);
  Add_Value(51, 0, 500, S_CALLEE_SOURCE);
  Add_Node(1000, 1, 100, std::vector<UINT32>(1, 20), {});
  Add_Node(1001, 2, 101, std::vector<UINT32>(1, 100),
           {{S_SIGNED_STEPS, S_ONE}, {S_KEY_ID, S_KEY}});
  Add_Node(1002, 3, 102, {101, 30}, {});
  Add_Node(2000, 4, 200, {40, 41}, {});
  Add_Node(2001, 5, 201, std::vector<UINT32>(1, 200),
           {{S_KEY_ID, S_KEY}});
  Add_State(100, TRUE); Add_State(101, FALSE); Add_State(102, FALSE);
  Add_State(200, FALSE); Add_State(201, FALSE);
  for (UINT32 i = 0; i < 5; ++i) {
    DSL_CKKS_EVENT_RECORD event;
    memset(&event, 0, sizeof(event));
    event.id = i + 1;
    event.owner_pu_st = i < 3 ? 10 : 11;
    event.source_value_id = i < 3 ? 50 : 51;
    event.source_node_id = i < 3 ? 5000 : 5001;
    event.context_pu_identity_id = i < 3 ? 1 : 2;
    event.context_callsite_id = i < 3 ? 0 : 1;
    event.source_static_ordinal = i < 3 ? 1 : 2;
    event.step_ordinal = i < 3 ? i : i - 3;
    event.result_value_id = i < 3 ? 100 + i : 200 + i - 3;
    event.result_node_id = i < 3 ? 1000 + i : 2000 + i - 3;
    event.origin_owner_pu_st = event.owner_pu_st;
    event.origin_source_value_id = event.source_value_id;
    event.origin_static_ordinal = event.source_static_ordinal;
    if (i == 4)
      event.flags = DSL_CKKS_EVENT_FINAL_RESULT;
    events.push_back(event);
  }
}

/* Collect both active PUs and preserve output objects on any rejection. */
static BOOL Collect(std::vector<VHO_FHE_CKKS2C_GROUP> *groups,
                    VHO_FHE_CKKS2C_COLLECT_RESULT *result)
{
  VHO_FHE_CKKS2C_COLLECTION collection;
  active_owner = 10;
  if (!VHO_FHE_CKKS2C_Collect_Active_PU(10, &collection, stderr))
    return FALSE;
  active_owner = 11;
  if (!VHO_FHE_CKKS2C_Collect_Active_PU(11, &collection, stderr))
    return FALSE;
  return VHO_FHE_CKKS2C_Finish_Collection(
      &collection, groups, result, stderr);
}

/* Certify positive collection plus representative malformed joins. */
int main()
{
  Initialize_Fixture();
  std::vector<VHO_FHE_CKKS2C_GROUP> groups;
  VHO_FHE_CKKS2C_COLLECT_RESULT result = {};
  assert(Collect(&groups, &result));
  assert(result.pu_count == 2 && result.source_count == 2 &&
         result.group_count == 2 && result.event_count == 5 &&
         result.external_asset_count == 1 && result.key_use_count == 2);
  assert(groups.size() == 2);
  const std::string first_identity = groups[0].identity_sha256;
  const std::string second_identity = groups[1].identity_sha256;
  BOOL saw_nonterminal = FALSE;
  BOOL saw_bound = FALSE;
  for (UINT32 i = 0; i < groups.size(); ++i) {
    saw_nonterminal |= groups[i].plan.source_final_step_index == UINT32_MAX;
    for (UINT32 step = 0; step < groups[i].plan.steps.size(); ++step)
      for (UINT32 operand = 0;
           operand < groups[i].plan.steps[step].operands.size(); ++operand)
        saw_bound |= groups[i].plan.steps[step].operands[operand].kind ==
                     VHO_FHE_CKKS_PLAN_BOUND_FORMAL;
  }
  assert(saw_nonterminal && saw_bound);

  /* Event allocation order cannot select a generated function. */
  std::reverse(events.begin(), events.end());
  for (UINT32 i = 0; i < events.size(); ++i)
    events[i].id = i + 1;
  std::vector<VHO_FHE_CKKS2C_GROUP> reordered;
  VHO_FHE_CKKS2C_COLLECT_RESULT reordered_result = {};
  assert(Collect(&reordered, &reordered_result));
  assert(reordered[0].identity_sha256 == first_identity &&
         reordered[1].identity_sha256 == second_identity);

  const std::vector<VHO_FHE_CKKS2C_GROUP> sentinel = groups;
  const VHO_FHE_CKKS2C_COLLECT_RESULT sentinel_result = result;
  Initialize_Fixture(); events[1].step_ordinal = 0;
  assert(!Collect(&groups, &result) && groups.size() == sentinel.size() &&
         result.event_count == sentinel_result.event_count);
  Initialize_Fixture();
  VHO_FHE_CKKS2C_COLLECTION terminal;
  active_owner = 10;
  assert(VHO_FHE_CKKS2C_Collect_Active_PU(10, &terminal, stderr));
  assert(!VHO_FHE_CKKS2C_Collect_Active_PU(10, &terminal, stderr));
  active_owner = 11;
  assert(!VHO_FHE_CKKS2C_Collect_Active_PU(11, &terminal, stderr));
  Initialize_Fixture(); events[0].context_pu_identity_id = 2;
  assert(!Collect(&groups, &result));
  Initialize_Fixture(); events[0].flags = DSL_CKKS_EVENT_FINAL_RESULT;
  assert(!Collect(&groups, &result));
  Initialize_Fixture(); missing_state = TRUE;
  assert(!Collect(&groups, &result));
  Initialize_Fixture(); missing_key = TRUE;
  assert(!Collect(&groups, &result));
  Initialize_Fixture(); bad_checksum = TRUE;
  assert(!Collect(&groups, &result));

  printf("pus=2 groups=2 events=5 assets=1 keys=2 ");
  printf("direct=1 called=1 prior=1 bound=1 malformed=7 rejected\n");
  return 0;
}
