/*
 * Copyright (C) 2026 Open64 Project
 *
 * Link-test FHE state binding around the native CKKS expansion transaction.
 * The native WN/image transaction has its own mapped roundtrip tests.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_expand.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static UINT32 preflight_count;
static UINT32 expansion_count;
static UINT32 binding_count;
static UINT32 fail_binding_at;
static BOOL fail_native;
static BOOL wrong_key_set;
static BOOL wrong_bootstrap_profile;

/* Keep the template layout ID in range as the real string table does. */
STR_IDX STR_Table_Size()
{
  return 4;
}

/* Resolve the fixture's stable layout, key-set, and bootstrap profile. */
char *Index_To_Str(STR_IDX id)
{
  static char layout[] = "ckks.packed";
  static char key[] = "request_key";
  static char profile[] = "pre_relu_refresh_v1";
  return id == 1 ? layout : id == 2 ? key : id == 3 ? profile : NULL;
}

/* Observe that every bad FHE state rejects before the native transaction. */
BOOL DSL_IR_Can_Expand_Native_Value_To_CKKS_Events(
    PU_Info *, const DSL_CKKS_EXPANSION_REQUEST *, FILE *)
{
  ++preflight_count;
  return TRUE;
}

/* Stand in for the separately certified atomic common/com producer. */
BOOL DSL_IR_Expand_Native_Value_To_CKKS_Events(
    PU_Info *, const DSL_CKKS_EXPANSION_REQUEST *request, FILE *,
    DSL_CKKS_EXPANSION_STEP_RESULT *results)
{
  ++expansion_count;
  if (fail_native)
    return FALSE;
  for (UINT32 i = 0; i < request->step_count; ++i) {
    results[i].node_id = i + 1;
    results[i].value_id = i + 11;
    results[i].result_st = i + 21;
  }
  return TRUE;
}

/* A ciphertext and an encoded plaintext use distinct stable descriptors. */
BOOL DSL_FHE_Get_Encryption_Descriptor(
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID id,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{
  if (id < 1 || id > 2 || record == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->scheme = DSL_FHE_SCHEME_CKKS;
  record->config_id = 1;
  record->key_set_name = wrong_key_set ? 3 : 2;
  record->value_class = id == 1 ?
      DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT :
      DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  record->slot_count = 8;
  return TRUE;
}

/* Require the exact key class and signed rotation from the FHE image. */
UINT32 DSL_FHE_Key_Requirement_Count()
{
  return 3;
}

BOOL DSL_FHE_Get_Key_Requirement(
    DSL_FHE_KEY_REQUIREMENT_ID id, DSL_FHE_KEY_REQUIREMENT_RECORD *record)
{
  if (id < 1 || id > 3 || record == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->id = id;
  record->config_id = 1;
  record->key_set_name = 2;
  record->key_class = id == 1 ? DSL_FHE_KEY_BOOTSTRAP :
                      id == 2 ? DSL_FHE_KEY_ROTATION :
                                DSL_FHE_KEY_RELINEARIZATION;
  record->rotation_offset = id == 2 ? -2 : 0;
  record->bootstrap_profile = id == 1 ?
      (wrong_bootstrap_profile ? 1 : 3) : 0;
  return TRUE;
}

/* Require the exact canonical TY and descriptor chosen for this result. */
BOOL DSL_FHE_Find_Tensor_Binding(
    TY_IDX ty, DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
    DSL_FHE_TENSOR_BINDING_RECORD *record)
{
  if (ty != 17 || descriptor_id < 1 || descriptor_id > 2 ||
      record == NULL)
    return FALSE;
  memset(record, 0, sizeof(*record));
  record->tensor_ty = ty;
  record->encryption_descriptor_id = descriptor_id;
  return TRUE;
}

/* Simulate an append-only state-table failure after native publication. */
DSL_FHE_CKKS_VALUE_STATE_ID DSL_FHE_Plan_Add_CKKS_Value_State(
    const DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{
  ++binding_count;
  assert(record != NULL && record->value_id == binding_count + 10);
  return binding_count == fail_binding_at ? 0 : binding_count;
}

/* Exercise preflight, ordered success, native failure, and terminal failure. */
int main()
{
  DSL_CKKS_EXPANSION_STEP steps[2];
  memset(steps, 0, sizeof(steps));
  steps[0].dsl_operator = OPR_DSLCKKSENCODE;
  steps[1].dsl_operator = OPR_DSLCKKSBOOTSTRAP;
  steps[0].result_ty = steps[1].result_ty = 17;
  DSL_CKKS_EXPANSION_ATTRIBUTE bootstrap_attrs[3] = {
    { "attr.target_level", "15" },
    { "attr.reason", "PRE_RELU_REFRESH" },
    { "attr.key_id", "request_key" }
  };
  steps[1].attributes = bootstrap_attrs;
  steps[1].attribute_count = 3;
  DSL_CKKS_EXPANSION_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.steps = steps;
  request.step_count = 2;
  request.final_step_index = 1;
  VHO_FHE_CKKS_STEP_STATE states[2];
  memset(states, 0, sizeof(states));
  for (UINT32 i = 0; i < 2; ++i) {
    states[i].state.state_version = 1;
    states[i].state.encryption_descriptor_id = i + 1;
    states[i].state.scheme = DSL_FHE_SCHEME_CKKS;
    states[i].state.value_class = i == 0 ?
        DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT :
        DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    states[i].state.level = 15;
    states[i].state.scale_bits = 56;
    states[i].state.component_count = i == 0 ? 1 : 2;
    states[i].state.precision_bits = 30;
    states[i].state.slot_count = 8;
    states[i].state.encrypted_layout_name = 1;
  }
  PU_Info *pu = reinterpret_cast<PU_Info *>(&request);
  DSL_CKKS_EXPANSION_STEP_RESULT results[2];
  memset(results, 0, sizeof(results));

  states[1].state.pending_actions = DSL_FHE_CKKS_PENDING_BOOTSTRAP;
  assert(!VHO_FHE_CKKS_Expand_And_Bind_States(
      pu, &request, states, 2, NULL, results));
  assert(preflight_count == 0 && expansion_count == 0);
  states[1].state.pending_actions = 0;
  states[1].state.encryption_descriptor_id = 1;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  states[1].state.encryption_descriptor_id = 2;
  steps[1].result_ty = 18;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  steps[1].result_ty = 17;
  states[1].state.slot_count = 4;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  states[1].state.slot_count = 8;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 1, NULL));
  states[1].state.encrypted_layout_name = 4;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  states[1].state.encrypted_layout_name = 1;
  steps[0].dsl_operator = OPR_DSLCKKSMUL;
  states[0].state.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  states[0].state.encryption_descriptor_id = 2;
  states[0].state.component_count = 3;
  states[0].state.pending_actions = DSL_FHE_CKKS_PENDING_RESCALE |
                                    DSL_FHE_CKKS_PENDING_RELINEARIZE;
  assert(VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  steps[0].dsl_operator = OPR_DSLCKKSENCODE;
  states[0].state.value_class = DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT;
  states[0].state.encryption_descriptor_id = 1;
  states[0].state.component_count = 1;
  states[0].state.pending_actions = 0;
  states[1].state.pending_actions = DSL_FHE_CKKS_PENDING_RESCALE;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  states[1].state.pending_actions = 0;
  bootstrap_attrs[0].value = "14";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  bootstrap_attrs[0].value = "15";
  bootstrap_attrs[2].value = "wrong_key";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  bootstrap_attrs[2].value = "request_key";
  wrong_key_set = TRUE;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  wrong_key_set = FALSE;
  wrong_bootstrap_profile = TRUE;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  wrong_bootstrap_profile = FALSE;
  steps[1].attributes = NULL;
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  steps[1].attributes = bootstrap_attrs;
  DSL_CKKS_EXPANSION_ATTRIBUTE rotate_attrs[2] = {
    { "attr.signed_steps", "-2" },
    { "attr.key_id", "request_key" }
  };
  steps[1].dsl_operator = OPR_DSLCKKSROTATE;
  steps[1].attributes = rotate_attrs;
  steps[1].attribute_count = 2;
  assert(VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  rotate_attrs[0].value = "2";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  rotate_attrs[0].value = "-2";
  DSL_CKKS_EXPANSION_ATTRIBUTE rescale_attrs[2] = {
    { "attr.levels", "1" },
    { "attr.target_scale_bits", "56" }
  };
  steps[1].dsl_operator = OPR_DSLCKKSRESCALE;
  steps[1].attributes = rescale_attrs;
  steps[1].attribute_count = 2;
  assert(VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  rescale_attrs[1].value = "55";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  rescale_attrs[1].value = " 56";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  rescale_attrs[1].value = "56";
  DSL_CKKS_EXPANSION_ATTRIBUTE modswitch_attr = {
    "attr.target_level", "15"
  };
  steps[1].dsl_operator = OPR_DSLCKKSMODSWITCH;
  steps[1].attributes = &modswitch_attr;
  steps[1].attribute_count = 1;
  assert(VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  modswitch_attr.value = "16";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  DSL_CKKS_EXPANSION_ATTRIBUTE relin_attr = {
    "attr.key_id", "request_key"
  };
  steps[1].dsl_operator = OPR_DSLCKKSRELIN;
  steps[1].attributes = &relin_attr;
  steps[1].attribute_count = 1;
  assert(VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  relin_attr.value = "wrong_key";
  assert(!VHO_FHE_CKKS_Can_Expand_And_Bind_States(
      pu, &request, states, 2, NULL));
  steps[1].dsl_operator = OPR_DSLCKKSBOOTSTRAP;
  steps[1].attributes = bootstrap_attrs;
  steps[1].attribute_count = 3;
  assert(preflight_count == 5 && expansion_count == 0);

  fail_native = TRUE;
  assert(!VHO_FHE_CKKS_Expand_And_Bind_States(
      pu, &request, states, 2, NULL, results));
  assert(binding_count == 0 && results[0].value_id == 0);
  fail_native = FALSE;
  fail_binding_at = 2;
  binding_count = 0;
  assert(!VHO_FHE_CKKS_Expand_And_Bind_States(
      pu, &request, states, 2, NULL, results));
  assert(binding_count == 2 && results[0].value_id == 0);
  fail_binding_at = 0;
  binding_count = 0;
  assert(VHO_FHE_CKKS_Expand_And_Bind_States(
      pu, &request, states, 2, stderr, results));
  assert(binding_count == 2 && results[0].value_id == 11 &&
         results[1].value_id == 12);
  puts("FHE CKKS expansion state adapter passed");
  return 0;
}
