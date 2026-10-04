/*
 * Copyright (C) 2026 Open64 Project
 *
 * Bind value-specific CKKS state after common/com atomically creates native
 * executable steps. See doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include "fhe_ckks_expand.h"

#include <string.h>

#include <vector>

#include "fhe_image.h"
#include "fhe_ckks_transfer.h"
#include "strtab.h"

/* Keep pre-mutation errors distinct from terminal post-expansion failures. */
static BOOL
VHO_FHE_CKKS_Expand_Report(FILE *diagnostic, const char *message,
                           UINT32 step_index, BOOL terminal)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "%s: step=%u %s\n",
            terminal ? "CFHEIR-STATE-002" : "CFHEIR-STATE-001",
            step_index, message);
  return FALSE;
}

/* Read an exact registered attribute without depending on WN encoding. */
static const char *
VHO_FHE_CKKS_Step_Attribute(const DSL_CKKS_EXPANSION_STEP &step,
                            const char *name)
{
  if (step.attribute_count != 0 && step.attributes == NULL)
    return NULL;
  for (UINT32 i = 0; i < step.attribute_count; ++i) {
    if (step.attributes[i].name != NULL &&
        strcmp(step.attributes[i].name, name) == 0)
      return step.attributes[i].value;
  }
  return NULL;
}

/* Match the requested key use to an existing exact config/class/rotation row. */
static BOOL
VHO_FHE_CKKS_Key_Available(const DSL_CKKS_EXPANSION_STEP &step,
                            const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &desc,
                            UINT32 key_class, INT32 rotation)
{
  const char *name = VHO_FHE_CKKS_Step_Attribute(step, "attr.key_id");
  if (name == NULL || name[0] == '\0')
    return FALSE;
  for (UINT32 id = 1; id <= DSL_FHE_Key_Requirement_Count(); ++id) {
    DSL_FHE_KEY_REQUIREMENT_RECORD key;
    if (!DSL_FHE_Get_Key_Requirement(id, &key) || key.id != id ||
        key.key_set_name == STR_IDX_ZERO ||
        key.key_set_name >= STR_Table_Size())
      return FALSE;
    if (key.config_id == desc.config_id &&
        key.key_set_name == desc.key_set_name &&
        key.key_class == key_class &&
        key.rotation_offset == rotation &&
        strcmp(Index_To_Str(key.key_set_name), name) == 0 &&
        (key_class != DSL_FHE_KEY_BOOTSTRAP ||
         (key.bootstrap_profile != STR_IDX_ZERO &&
          key.bootstrap_profile < STR_Table_Size() &&
          strcmp(Index_To_Str(key.bootstrap_profile),
                 "pre_relu_refresh_v1") == 0)))
      return TRUE;
  }
  return FALSE;
}

/* Tie executable attributes to the concrete result state and key ledger.
 * Operand transfer and whole-program depth remain separate FHE gates. */
static BOOL
VHO_FHE_CKKS_Step_Attributes_Valid(
    const DSL_CKKS_EXPANSION_STEP &step,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &state,
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD &descriptor)
{
  INT32 number = 0;
  switch (step.dsl_operator) {
  case OPR_DSLCKKSBOOTSTRAP:
    return VHO_FHE_CKKS_Step_Integer(
               step, "attr.target_level", &number) &&
           number == state.level &&
           VHO_FHE_CKKS_Step_Attribute(step, "attr.reason") != NULL &&
           strcmp(VHO_FHE_CKKS_Step_Attribute(step, "attr.reason"),
                  "PRE_RELU_REFRESH") == 0 &&
           VHO_FHE_CKKS_Key_Available(
               step, descriptor, DSL_FHE_KEY_BOOTSTRAP, 0);
  case OPR_DSLCKKSROTATE:
    return VHO_FHE_CKKS_Step_Integer(
               step, "attr.signed_steps", &number) &&
           number != 0 && (INT64)number < (INT64)state.slot_count &&
           (INT64)number > -(INT64)state.slot_count &&
           VHO_FHE_CKKS_Key_Available(
               step, descriptor, DSL_FHE_KEY_ROTATION, number);
  case OPR_DSLCKKSRELIN:
    return VHO_FHE_CKKS_Key_Available(
        step, descriptor, DSL_FHE_KEY_RELINEARIZATION, 0);
  case OPR_DSLCKKSRESCALE:
    return VHO_FHE_CKKS_Step_Integer(step, "attr.levels", &number) &&
           number > 0 &&
           VHO_FHE_CKKS_Step_Integer(
               step, "attr.target_scale_bits", &number) &&
           number == state.scale_bits;
  case OPR_DSLCKKSMODSWITCH:
    return VHO_FHE_CKKS_Step_Integer(
               step, "attr.target_level", &number) &&
           number == state.level;
  default:
    return TRUE;
  }
}

/* Resolve a unary operand to a concrete state, preserving owner-qualified
 * native value checks for the common transaction that follows. */
static BOOL
VHO_FHE_CKKS_Unary_Input_State(
    const DSL_CKKS_EXPANSION_STEP &step, UINT32 step_index,
    const VHO_FHE_CKKS_STEP_STATE *states,
    DSL_FHE_CKKS_VALUE_STATE_RECORD *input)
{
  if (step.operand_count != 1 || step.operands == NULL || input == NULL)
    return FALSE;
  const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[0];
  if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP) {
    if (operand.step_index >= step_index)
      return FALSE;
    *input = states[operand.step_index].state;
    return TRUE;
  }
  return operand.kind == DSL_CKKS_EXPANSION_EXISTING_VALUE &&
         operand.value_id != DSL_IR_VALUE_INVALID_ID &&
         DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
             operand.value_id, input);
}

/* Match each proposed result to a canonical tensor/encryption association.
 * The common transaction separately checks operator schema and operands. */
BOOL
VHO_FHE_CKKS_Can_Expand_And_Bind_States(
    PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
    const VHO_FHE_CKKS_STEP_STATE *states, UINT32 state_count,
    FILE *diagnostic)
{
  if (pu_info == NULL || request == NULL || states == NULL ||
      state_count == 0 || state_count != request->step_count ||
      request->steps == NULL)
    return VHO_FHE_CKKS_Expand_Report(
        diagnostic, "state and native step counts disagree", 0, FALSE);

  for (UINT32 i = 0; i < state_count; ++i) {
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &state = states[i].state;
    const DSL_CKKS_EXPANSION_STEP &step = request->steps[i];
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
    DSL_FHE_TENSOR_BINDING_RECORD binding;
    UINT32 expected_class = step.dsl_operator == OPR_DSLCKKSENCODE ?
        DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT :
        DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    const UINT32 known_actions = DSL_FHE_CKKS_PENDING_RESCALE |
                                 DSL_FHE_CKKS_PENDING_RELINEARIZE |
                                 DSL_FHE_CKKS_PENDING_BOOTSTRAP;
    BOOL terminal_step = i == request->final_step_index;
    BOOL no_pending = state.pending_actions == 0 &&
                      state.pending_bootstrap_reason ==
                          DSL_FHE_BOOTSTRAP_REASON_NONE;
    if (state.id != DSL_FHE_CKKS_VALUE_STATE_INVALID_ID ||
        state.value_id != DSL_IR_VALUE_INVALID_ID ||
        state.state_version != 1 ||
        state.scheme != DSL_FHE_SCHEME_CKKS ||
        state.value_class != expected_class ||
        state.level < 0 || state.scale_bits <= 0 ||
        state.precision_bits <= 0 || state.slot_count == 0 ||
        (expected_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT ?
             state.component_count < 2 : state.component_count != 1) ||
        state.encrypted_layout_name == STR_IDX_ZERO ||
        state.encrypted_layout_name >= STR_Table_Size() ||
        (state.pending_actions & ~known_actions) != 0 ||
        ((state.pending_actions & DSL_FHE_CKKS_PENDING_BOOTSTRAP) == 0 &&
         state.pending_bootstrap_reason !=
             DSL_FHE_BOOTSTRAP_REASON_NONE) ||
        ((state.pending_actions & DSL_FHE_CKKS_PENDING_BOOTSTRAP) != 0 &&
         state.pending_bootstrap_reason ==
             DSL_FHE_BOOTSTRAP_REASON_NONE) ||
        (terminal_step && !no_pending) ||
        (step.dsl_operator == OPR_DSLCKKSBOOTSTRAP &&
         (!no_pending || state.component_count != 2)) ||
        (step.dsl_operator == OPR_DSLCKKSRELIN &&
         (state.component_count != 2 ||
          (state.pending_actions & DSL_FHE_CKKS_PENDING_RELINEARIZE) != 0)) ||
        (step.dsl_operator == OPR_DSLCKKSRESCALE &&
         (state.pending_actions & DSL_FHE_CKKS_PENDING_RESCALE) != 0) ||
        step.result_ty == TY_IDX_ZERO ||
        !DSL_FHE_Get_Encryption_Descriptor(
            state.encryption_descriptor_id, &descriptor) ||
        descriptor.scheme != DSL_FHE_SCHEME_CKKS ||
        descriptor.value_class != expected_class ||
        descriptor.config_id == DSL_FHE_CONFIG_INVALID_ID ||
        (descriptor.slot_count != 0 &&
         descriptor.slot_count != state.slot_count) ||
        !DSL_FHE_Find_Tensor_Binding(
            step.result_ty, state.encryption_descriptor_id, &binding) ||
        !VHO_FHE_CKKS_Step_Attributes_Valid(step, state, descriptor))
      return VHO_FHE_CKKS_Expand_Report(
          diagnostic, "result lacks a concrete compatible CKKS state",
          i, FALSE);
    if (step.dsl_operator == OPR_DSLCKKSBOOTSTRAP ||
        step.dsl_operator == OPR_DSLCKKSROTATE ||
        step.dsl_operator == OPR_DSLCKKSRELIN ||
        step.dsl_operator == OPR_DSLCKKSRESCALE ||
        step.dsl_operator == OPR_DSLCKKSMODSWITCH) {
      DSL_FHE_CKKS_VALUE_STATE_RECORD input;
      if (!VHO_FHE_CKKS_Unary_Input_State(
              step, i, states, &input) ||
          !VHO_FHE_CKKS_Verify_Unary_State_Transfer(
              step, input, state, diagnostic))
        return VHO_FHE_CKKS_Expand_Report(
            diagnostic, "unary CKKS state transfer failed", i, FALSE);
    }
  }
  return DSL_IR_Can_Expand_Native_Value_To_CKKS_Events(
      pu_info, request, diagnostic);
}

/* Native expansion owns its rollback. FHE state insertion occurs afterward;
 * any failure in that second phase aborts the dedicated checkpoint process. */
BOOL
VHO_FHE_CKKS_Expand_And_Bind_States(
    PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
    const VHO_FHE_CKKS_STEP_STATE *states, UINT32 state_count,
    FILE *diagnostic, DSL_CKKS_EXPANSION_STEP_RESULT *results)
{
  if (results == NULL || !VHO_FHE_CKKS_Can_Expand_And_Bind_States(
          pu_info, request, states, state_count, diagnostic))
    return FALSE;
  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> produced(state_count);
  if (!DSL_IR_Expand_Native_Value_To_CKKS_Events(
          pu_info, request, diagnostic, &produced[0]))
    return FALSE;

  for (UINT32 i = 0; i < state_count; ++i) {
    DSL_FHE_CKKS_VALUE_STATE_RECORD state = states[i].state;
    if (produced[i].node_id == DSL_IR_NODE_INVALID_ID ||
        produced[i].value_id == DSL_IR_VALUE_INVALID_ID ||
        produced[i].result_st == ST_IDX_ZERO)
      return VHO_FHE_CKKS_Expand_Report(
          diagnostic, "native expansion returned an invalid result",
          i, TRUE);
    state.value_id = produced[i].value_id;
    if (DSL_FHE_Plan_Add_CKKS_Value_State(&state) ==
        DSL_FHE_CKKS_VALUE_STATE_INVALID_ID)
      return VHO_FHE_CKKS_Expand_Report(
          diagnostic, "value-specific CKKS state binding failed; discard image",
          i, TRUE);
  }
  for (UINT32 i = 0; i < state_count; ++i)
    results[i] = produced[i];
  return TRUE;
}
