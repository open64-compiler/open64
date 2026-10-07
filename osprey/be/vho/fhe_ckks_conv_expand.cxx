/*
 * Copyright (C) 2026 Open64 Project
 *
 * Bridge the FHE-owned Conv schedule to common/com's only supported native
 * CKKS expansion transaction and then bind FHE value-specific state.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include "fhe_ckks_conv_expand.h"

#include <string.h>

#include <string>
#include <vector>

#include "fhe_ckks_expand.h"
#include "fhe_plan.h"
#include "strtab.h"
#include "wn.h"

namespace {

/* Report adapter rejection without changing the caller's result vector. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-CONV-EXPAND-001: %s\n", message);
  return FALSE;
}

/* Generate a deterministic, source-ordinal-qualified native result name. */
std::string Result_Name(UINT32 source_ordinal, UINT32 step)
{
  char name[80];
  snprintf(name, sizeof(name), "fhe_ckks_conv_s%u_step%u",
           source_ordinal, step);
  return name;
}

/* Copy one canonical plan state into the append-only FHE state template. */
VHO_FHE_CKKS_STEP_STATE State(
    const VHO_FHE_CKKS_PLAN_STATE &input, STR_IDX layout)
{
  VHO_FHE_CKKS_STEP_STATE output;
  DSL_FHE_CKKS_Value_State_Record_Init(&output.state);
  output.state.state_version = 1;
  output.state.encryption_descriptor_id =
      input.encryption_descriptor_id;
  output.state.scheme = input.scheme;
  output.state.value_class = input.value_class;
  output.state.level = input.level;
  output.state.scale_bits = input.scale_bits;
  output.state.component_count = input.component_count;
  output.state.precision_bits = input.precision_bits;
  output.state.slot_count = input.slot_count;
  output.state.alignment_group = input.alignment_group;
  output.state.encrypted_layout_name = layout;
  output.state.pending_actions = input.pending_actions;
  output.state.pending_bootstrap_reason =
      input.pending_bootstrap_reason;
  return output;
}

}  // namespace

/* Keep all pointer-backed native rows local until the complete request is
 * assembled. The shared transaction performs both physical and logical IR
 * preflight; FHE then appends one concrete state row per produced value. */
BOOL VHO_FHE_CKKS_Expand_Conv_Event(
    PU_Info *pu_info, const VHO_FHE_CKKS_EVENT_PLAN &plan,
    const VHO_FHE_CKKS_CONV_EXPANSION_SOURCE &source,
    FILE *diagnostic,
    std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> *results)
{
  std::vector<unsigned char> canonical;
  if (pu_info == NULL || results == NULL || plan.steps.empty() ||
      plan.group_output_steps.size() != 1 ||
      plan.group_output_steps[0] != plan.source_final_step_index ||
      source.source_definition == NULL || source.source_value_id == 0 ||
      source.expected_source_operator == OPR_DSLUNKNOWN ||
      source.expected_source_version == 0 ||
      source.context_pu_identity_id == 0 ||
      source.origin_owner_pu_st == ST_IDX_ZERO ||
      source.origin_source_value_id == 0 ||
      source.encrypted_layout_name == STR_IDX_ZERO ||
      source.encrypted_layout_name >= STR_Table_Size() ||
      !VHO_FHE_CKKS_Serialize_Event_Plan(
          plan, &canonical, diagnostic))
    return Report(diagnostic, "source or canonical Conv plan is incomplete");

  const char *layout = Index_To_Str(source.encrypted_layout_name);
  if (layout == NULL || plan.steps[0].result_state.encrypted_layout != layout)
    return Report(diagnostic, "plan and managed layout identity disagree");

  const size_t count = plan.steps.size();
  std::vector<DSL_CKKS_EXPANSION_STEP> steps(count);
  std::vector<std::vector<DSL_CKKS_EXPANSION_OPERAND> > operands(count);
  std::vector<std::vector<DSL_CKKS_EXPANSION_ATTRIBUTE> > attributes(count);
  std::vector<std::string> names(count);
  std::vector<VHO_FHE_CKKS_STEP_STATE> states(count);
  for (size_t i = 0; i < count; ++i) {
    const VHO_FHE_CKKS_PLAN_STEP &planned = plan.steps[i];
    if (planned.logical_operator < OPR_DSLCKKSADD ||
        planned.logical_operator > OPR_DSLCKKSBOOTSTRAP ||
        planned.operator_version > UINT16_MAX ||
        planned.result_state.encrypted_layout != layout)
      return Report(diagnostic, "plan step cannot enter native CKKS IR");
    operands[i].resize(planned.operands.size());
    for (size_t kid = 0; kid < planned.operands.size(); ++kid) {
      const VHO_FHE_CKKS_PLAN_OPERAND &planned_operand =
          planned.operands[kid];
      DSL_CKKS_EXPANSION_OPERAND &operand = operands[i][kid];
      memset(&operand, 0, sizeof(operand));
      if (planned_operand.kind == VHO_FHE_CKKS_PLAN_SOURCE_VALUE) {
        operand.kind = DSL_CKKS_EXPANSION_EXISTING_VALUE;
        operand.value_id = planned_operand.reference;
      } else if (planned_operand.kind == VHO_FHE_CKKS_PLAN_PRIOR_STEP) {
        operand.kind = DSL_CKKS_EXPANSION_PRIOR_STEP;
        operand.step_index = planned_operand.reference;
      } else {
        return Report(diagnostic,
                      "Conv plan contains a nonresident bound formal");
      }
    }
    attributes[i].resize(planned.attributes.size());
    for (size_t attribute = 0; attribute < planned.attributes.size();
         ++attribute) {
      attributes[i][attribute].name =
          planned.attributes[attribute].name.c_str();
      attributes[i][attribute].value =
          planned.attributes[attribute].value.c_str();
    }
    names[i] = Result_Name(plan.source_static_ordinal,
                           static_cast<UINT32>(i));
    memset(&steps[i], 0, sizeof(steps[i]));
    steps[i].dsl_operator =
        static_cast<DSL_OPERATOR>(planned.logical_operator);
    steps[i].version = static_cast<UINT16>(planned.operator_version);
    steps[i].operands = operands[i].empty() ? NULL : &operands[i][0];
    steps[i].operand_count = static_cast<UINT32>(operands[i].size());
    steps[i].attributes = attributes[i].empty() ? NULL : &attributes[i][0];
    steps[i].attribute_count = static_cast<UINT32>(attributes[i].size());
    steps[i].result_name = names[i].c_str();
    steps[i].result_ty = planned.result_ty;
    steps[i].source_position = WN_Get_Linenum(source.source_definition);
    states[i] = State(planned.result_state, source.encrypted_layout_name);
  }

  DSL_CKKS_EXPANSION_GROUP group;
  memset(&group, 0, sizeof(group));
  group.source_static_ordinal = plan.source_static_ordinal;
  group.origin_static_ordinal = plan.source_static_ordinal;
  group.step_count = static_cast<UINT32>(count);
  DSL_CKKS_EXPANSION_CONTEXT context;
  memset(&context, 0, sizeof(context));
  context.context_pu_identity_id = source.context_pu_identity_id;
  context.context_callsite_id = source.context_callsite_id;
  context.origin_owner_pu_st = source.origin_owner_pu_st;
  context.origin_source_value_id = source.origin_source_value_id;
  DSL_CKKS_EXPANSION_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.source_definition = source.source_definition;
  request.source_value_id = source.source_value_id;
  request.expected_source_operator = source.expected_source_operator;
  request.expected_source_version = source.expected_source_version;
  request.groups = &group;
  request.group_count = 1;
  request.steps = &steps[0];
  request.step_count = static_cast<UINT32>(count);
  request.contexts = &context;
  request.context_count = 1;
  request.final_step_index = plan.source_final_step_index;

  std::vector<DSL_CKKS_EXPANSION_STEP_RESULT> produced(count);
  if (!VHO_FHE_CKKS_Expand_And_Bind_States(
          pu_info, &request, &states[0], static_cast<UINT32>(states.size()),
          diagnostic, &produced[0])) {
    if (diagnostic != NULL)
      fprintf(diagnostic,
              "CFHEIR-CONV-EXPAND-001: owner=%u source=value%u "
              "ordinal=%u first_operator=%u state binding failed\n",
              PU_Info_proc_sym(pu_info), source.source_value_id,
              plan.source_static_ordinal,
              plan.steps[0].logical_operator);
    return FALSE;
  }
  results->swap(produced);
  return TRUE;
}
