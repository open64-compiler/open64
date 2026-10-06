/*
 * Copyright (C) 2026 Open64 Project
 *
 * Construct the complete process-local CKKS plan for one admitted stride-one
 * Conv context. Native WN/image mutation remains in dsl_ckks_expand.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_conv_plan.h"

#include <limits>
#include <utility>

#include "dsl_opcode.h"

namespace {

/* Emit one stable producer diagnostic without modifying the caller's plan. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-CONV-PLAN-001: %s\n", message);
  return false;
}

/* Convert a signed decimal attribute without locale or stream state. */
std::string Decimal(int32_t value)
{
  char text[32];
  snprintf(text, sizeof(text), "%d", value);
  return text;
}

/* Name one source value operand in the canonical event-plan vocabulary. */
VHO_FHE_CKKS_PLAN_OPERAND Source(uint32_t value_id)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand = {
    VHO_FHE_CKKS_PLAN_SOURCE_VALUE, value_id, ""
  };
  return operand;
}

/* Name one already emitted step in the canonical event-plan vocabulary. */
VHO_FHE_CKKS_PLAN_OPERAND Prior(uint32_t step_index)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand = {
    VHO_FHE_CKKS_PLAN_PRIOR_STEP, step_index, ""
  };
  return operand;
}

/* Materialize a complete value-state tuple for one proposed result. */
VHO_FHE_CKKS_PLAN_STATE State(
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy, bool plaintext,
    int32_t level, int32_t scale_bits, int32_t precision_bits)
{
  VHO_FHE_CKKS_PLAN_STATE state;
  state.encryption_descriptor_id = plaintext ?
      policy.plaintext_encryption_descriptor_id :
      policy.cipher_encryption_descriptor_id;
  state.scheme = policy.scheme;
  state.value_class = plaintext ? policy.plaintext_value_class :
                                  policy.ciphertext_value_class;
  state.level = level;
  state.scale_bits = scale_bits;
  state.component_count = plaintext ? 1 : policy.component_count;
  state.precision_bits = precision_bits;
  state.slot_count = policy.slot_count;
  state.alignment_group = policy.alignment_group;
  state.encrypted_layout = policy.encrypted_layout;
  state.pending_actions = 0;
  state.pending_bootstrap_reason = 0;
  return state;
}

/* Append one rotation with its exact signed key requirement. */
uint32_t Append_Rotate(
    VHO_FHE_CKKS_EVENT_PLAN *plan,
    const VHO_FHE_CKKS_PLAN_OPERAND &input, int32_t rotation,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSROTATE;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(input);
  step.attributes.push_back(
      VHO_FHE_CKKS_PLAN_ATTRIBUTE{
          "attr.signed_steps", Decimal(rotation)});
  step.attributes.push_back(
      VHO_FHE_CKKS_PLAN_ATTRIBUTE{
          "attr.key_id", policy.rotation_key_id});
  step.result_state = state;
  step.required_keys.push_back(policy.rotation_key_id);
  step.signed_rotations.push_back(rotation);
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append one aligned ciphertext addition. */
uint32_t Append_Add(
    VHO_FHE_CKKS_EVENT_PLAN *plan,
    const VHO_FHE_CKKS_PLAN_OPERAND &left,
    const VHO_FHE_CKKS_PLAN_OPERAND &right,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSADD;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(left);
  step.operands.push_back(right);
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append one authenticated external plaintext encode. */
uint32_t Append_Encode(
    VHO_FHE_CKKS_EVENT_PLAN *plan, uint32_t value_id,
    const std::string &sha256,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSENCODE;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(Source(value_id));
  step.result_state = state;
  step.plaintext_asset_sha256 = sha256;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append ciphertext-by-plaintext multiply with its pending rescale state. */
uint32_t Append_Multiply(
    VHO_FHE_CKKS_EVENT_PLAN *plan,
    const VHO_FHE_CKKS_PLAN_OPERAND &ciphertext, uint32_t encoded_step,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSMUL;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(ciphertext);
  step.operands.push_back(Prior(encoded_step));
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append the explicit one-level repair required after plain multiplication. */
uint32_t Append_Rescale(
    VHO_FHE_CKKS_EVENT_PLAN *plan, uint32_t multiplied_step,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSRESCALE;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(Prior(multiplied_step));
  step.attributes.push_back(
      VHO_FHE_CKKS_PLAN_ATTRIBUTE{"attr.levels", "1"});
  step.attributes.push_back(
      VHO_FHE_CKKS_PLAN_ATTRIBUTE{
          "attr.target_scale_bits", Decimal(policy.scale_bits)});
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append an explicit depth-exhaustion refresh before a deep O0 network. */
uint32_t Append_Bootstrap(
    VHO_FHE_CKKS_EVENT_PLAN *plan,
    const VHO_FHE_CKKS_PLAN_OPERAND &input,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSBOOTSTRAP;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands.push_back(input);
  step.attributes.push_back(VHO_FHE_CKKS_PLAN_ATTRIBUTE{
      "attr.target_level", Decimal(policy.capacity_refresh_target_level)});
  step.attributes.push_back(VHO_FHE_CKKS_PLAN_ATTRIBUTE{
      "attr.reason", policy.capacity_refresh_reason});
  step.attributes.push_back(VHO_FHE_CKKS_PLAN_ATTRIBUTE{
      "attr.key_id", policy.bootstrap_key_id});
  step.result_state = state;
  step.required_keys.push_back(policy.bootstrap_key_id);
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Prove the state and external-asset inputs are complete before planning. */
bool Policy_Valid(const VHO_FHE_CKKS_CONV_RECIPE &recipe,
                  const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy)
{
  const size_t rows = size_t(recipe.shape.input_channels) *
                      recipe.shape.kernel_height *
                      recipe.shape.kernel_width;
  if (policy.source_static_ordinal == 0 || policy.input_value_id == 0 ||
      policy.result_ty == 0 ||
      policy.cipher_encryption_descriptor_id == 0 ||
      policy.plaintext_encryption_descriptor_id == 0 ||
      policy.scheme == 0 || policy.ciphertext_value_class == 0 ||
      policy.plaintext_value_class == 0 ||
      policy.ciphertext_value_class == policy.plaintext_value_class ||
      policy.input_level <= 0 || policy.scale_bits <= 0 ||
      policy.component_count < 2 || policy.precision_bits < 5 ||
      policy.slot_count != recipe.shape.slot_count ||
      policy.rescale_pending_action == 0 ||
      policy.encrypted_layout.empty() || policy.rotation_key_id.empty() ||
      policy.row_value_ids.size() != rows ||
      policy.row_sha256.size() != rows || policy.bias_value_id == 0 ||
      policy.bias_sha256.empty())
    return false;
  if ((policy.capacity_refresh_target_level == 0 &&
       (!policy.capacity_refresh_reason.empty() ||
        !policy.bootstrap_key_id.empty())) ||
      (policy.capacity_refresh_target_level != 0 &&
       (policy.capacity_refresh_target_level <= policy.input_level ||
        policy.capacity_refresh_reason != "DEPTH_EXHAUSTION" ||
        policy.bootstrap_key_id.empty())))
    return false;
  for (size_t i = 0; i < rows; ++i)
    if (policy.row_value_ids[i] == 0 || policy.row_sha256[i].empty())
      return false;
  return true;
}

}  // namespace

/* Construct the complete fixed O0 Conv DAG in local storage, then use the
 * canonical serializer as the final structural guard before publication. */
bool VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  VHO_FHE_CKKS_CONV_ROW_SCHEDULE schedule;
  if (plan == NULL ||
      !VHO_FHE_CKKS_Validate_Column_Conv_Recipe(recipe, diagnostic) ||
      !VHO_FHE_CKKS_Build_Column_Conv_Row_Schedule(
          recipe, &schedule, diagnostic) ||
      !Policy_Valid(recipe, policy))
    return Report(diagnostic, "recipe, state, or asset policy is incomplete");

  VHO_FHE_CKKS_EVENT_PLAN built;
  built.source_static_ordinal = policy.source_static_ordinal;
  built.source_final_step_index = std::numeric_limits<uint32_t>::max();
  int32_t effective_input_level = policy.input_level;
  VHO_FHE_CKKS_PLAN_STATE input_state =
      State(policy, false, effective_input_level, policy.scale_bits,
            policy.precision_bits);
  VHO_FHE_CKKS_PLAN_OPERAND duplicated = Source(policy.input_value_id);
  if (policy.capacity_refresh_target_level != 0) {
    effective_input_level = policy.capacity_refresh_target_level;
    input_state = State(policy, false, effective_input_level,
                        policy.scale_bits, policy.precision_bits);
    const uint32_t refreshed = Append_Bootstrap(
        &built, duplicated, policy, input_state);
    duplicated = Prior(refreshed);
  }
  for (size_t i = 0; i < schedule.duplication_rotations.size(); ++i) {
    const uint32_t rotated = Append_Rotate(
        &built, Source(policy.input_value_id),
        schedule.duplication_rotations[i], policy, input_state);
    const uint32_t added = Append_Add(
        &built, duplicated, Prior(rotated), policy, input_state);
    duplicated = Prior(added);
  }

  const int32_t multiplied_precision = policy.precision_bits;
  const int32_t output_precision = policy.precision_bits;
  VHO_FHE_CKKS_PLAN_STATE encoded_state =
      State(policy, true, effective_input_level, policy.scale_bits,
            policy.precision_bits);
  VHO_FHE_CKKS_PLAN_STATE multiplied_state =
      State(policy, false, effective_input_level, policy.scale_bits * 2,
            multiplied_precision);
  multiplied_state.pending_actions = policy.rescale_pending_action;
  VHO_FHE_CKKS_PLAN_STATE output_state =
      State(policy, false, policy.input_level - 1, policy.scale_bits,
            output_precision);
  output_state.level = effective_input_level - 1;
  bool have_accumulator = false;
  uint32_t accumulator = 0;
  for (size_t row = 0; row < schedule.row_rotations.size(); ++row) {
    VHO_FHE_CKKS_PLAN_OPERAND row_input = duplicated;
    if (schedule.row_rotations[row] != 0) {
      const uint32_t rotated = Append_Rotate(
          &built, duplicated, schedule.row_rotations[row], policy,
          input_state);
      row_input = Prior(rotated);
    }
    const uint32_t encoded = Append_Encode(
        &built, policy.row_value_ids[row], policy.row_sha256[row], policy,
        encoded_state);
    const uint32_t multiplied = Append_Multiply(
        &built, row_input, encoded, policy, multiplied_state);
    const uint32_t rescaled = Append_Rescale(
        &built, multiplied, policy, output_state);
    if (!have_accumulator) {
      accumulator = rescaled;
      have_accumulator = true;
    } else {
      accumulator = Append_Add(
          &built, Prior(accumulator), Prior(rescaled), policy, output_state);
    }
  }
  if (!have_accumulator)
    return Report(diagnostic, "Conv produced no plaintext feature rows");

  VHO_FHE_CKKS_PLAN_STATE bias_state =
      State(policy, true, effective_input_level - 1, policy.scale_bits,
            output_precision);
  const uint32_t encoded_bias = Append_Encode(
      &built, policy.bias_value_id, policy.bias_sha256, policy, bias_state);
  const uint32_t final = Append_Add(
      &built, Prior(accumulator), Prior(encoded_bias), policy, output_state);
  built.group_output_steps.push_back(final);
  built.source_final_step_index = final;

  std::vector<unsigned char> canonical;
  if (!VHO_FHE_CKKS_Serialize_Event_Plan(
          built, &canonical, diagnostic))
    return Report(diagnostic, "constructed Conv plan is not canonical");
  plan->source_static_ordinal = built.source_static_ordinal;
  plan->steps.swap(built.steps);
  plan->group_output_steps.swap(built.group_output_steps);
  plan->source_final_step_index = built.source_final_step_index;
  return true;
}

/* Return the ordered source/target bit moves used by the accepted sequential
 * stride-two packing proof. Width and channels must both be powers of two. */
static bool Build_Bit_Moves(
    uint32_t width, uint32_t channels,
    std::vector<std::pair<uint32_t, uint32_t> > *moves)
{
  if (moves == NULL || width < 4 || channels == 0 ||
      (width & (width - 1)) != 0 || (channels & (channels - 1)) != 0)
    return false;
  uint32_t spatial_bits = 0;
  uint32_t channel_bits = 0;
  for (uint32_t value = width; value > 1; value >>= 1)
    ++spatial_bits;
  for (uint32_t value = channels; value > 1; value >>= 1)
    ++channel_bits;
  std::vector<std::pair<uint32_t, uint32_t> > built;
  for (uint32_t bit = 0; bit + 1 < spatial_bits; ++bit)
    built.push_back(std::make_pair(1 + bit, bit));
  for (uint32_t bit = 0; bit + 1 < spatial_bits; ++bit)
    built.push_back(std::make_pair(
        spatial_bits + 1 + bit, spatial_bits - 1 + bit));
  for (uint32_t bit = 0; bit < channel_bits; ++bit)
    built.push_back(std::make_pair(
        2 * spatial_bits + bit, 2 * spatial_bits - 2 + bit));
  moves->swap(built);
  return true;
}

/* Append one plaintext-mask multiply and rescale branch. */
static uint32_t Append_Masked_Branch(
    VHO_FHE_CKKS_EVENT_PLAN *plan, uint32_t input_step,
    uint32_t mask_value_id, const std::string &mask_sha256,
    int32_t input_level, uint32_t result_ty,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &policy)
{
  VHO_FHE_CKKS_CONV_PLAN_POLICY typed_policy = policy;
  typed_policy.result_ty = result_ty;
  VHO_FHE_CKKS_PLAN_STATE encoded =
      State(policy, true, input_level, policy.scale_bits,
            policy.precision_bits);
  VHO_FHE_CKKS_PLAN_STATE multiplied =
      State(policy, false, input_level, policy.scale_bits * 2,
            policy.precision_bits);
  multiplied.pending_actions = policy.rescale_pending_action;
  VHO_FHE_CKKS_PLAN_STATE rescaled =
      State(policy, false, input_level - 1, policy.scale_bits,
            policy.precision_bits);
  const uint32_t encoded_step = Append_Encode(
      plan, mask_value_id, mask_sha256, typed_policy, encoded);
  const uint32_t multiplied_step = Append_Multiply(
      plan, Prior(input_step), encoded_step, typed_policy, multiplied);
  return Append_Rescale(
      plan, multiplied_step, typed_policy, rescaled);
}

/* Extend the high-resolution plan with exact selection and bit-move stages;
 * the canonical serializer verifies the complete combined event afterward. */
bool VHO_FHE_CKKS_Build_Stride_Two_Conv_Event_Plan(
    const VHO_FHE_CKKS_CONV_RECIPE &high_resolution_recipe,
    const VHO_FHE_CKKS_CONV_PLAN_POLICY &conv_policy,
    const VHO_FHE_CKKS_STRIDE_COMPACTION_POLICY &compaction_policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  std::vector<std::pair<uint32_t, uint32_t> > moves;
  if (plan == NULL || conv_policy.capacity_refresh_target_level == 0 ||
      compaction_policy.high_resolution_ty == 0 ||
      compaction_policy.input_width != high_resolution_recipe.shape.width ||
      compaction_policy.output_channels !=
          high_resolution_recipe.shape.output_channels ||
      !Build_Bit_Moves(compaction_policy.input_width,
                       compaction_policy.output_channels, &moves) ||
      compaction_policy.mask_value_ids.size() != 1 + 2 * moves.size() ||
      compaction_policy.mask_sha256.size() != 1 + 2 * moves.size())
    return Report(diagnostic, "stride compaction policy is incomplete");
  for (size_t i = 0; i < compaction_policy.mask_value_ids.size(); ++i)
    if (compaction_policy.mask_value_ids[i] == 0 ||
        compaction_policy.mask_sha256[i].empty())
      return Report(diagnostic, "stride compaction mask is unauthenticated");

  VHO_FHE_CKKS_CONV_PLAN_POLICY high_policy = conv_policy;
  high_policy.result_ty = compaction_policy.high_resolution_ty;
  VHO_FHE_CKKS_EVENT_PLAN built;
  if (!VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
          high_resolution_recipe, high_policy, &built, diagnostic))
    return false;
  uint32_t current = built.source_final_step_index;
  int32_t level = built.steps[current].result_state.level;
  built.group_output_steps.clear();
  built.source_final_step_index = std::numeric_limits<uint32_t>::max();
  current = Append_Masked_Branch(
      &built, current, compaction_policy.mask_value_ids[0],
      compaction_policy.mask_sha256[0], level,
      compaction_policy.high_resolution_ty, high_policy);
  --level;
  for (size_t stage = 0; stage < moves.size(); ++stage) {
    const uint32_t selected = Append_Masked_Branch(
        &built, current, compaction_policy.mask_value_ids[1 + 2 * stage],
        compaction_policy.mask_sha256[1 + 2 * stage], level,
        compaction_policy.high_resolution_ty, high_policy);
    const int32_t rotation =
        int32_t(uint32_t(1) << moves[stage].first) -
        int32_t(uint32_t(1) << moves[stage].second);
    VHO_FHE_CKKS_PLAN_STATE branch_state =
        State(high_policy, false, level - 1, high_policy.scale_bits,
              high_policy.precision_bits);
    const uint32_t rotated = Append_Rotate(
        &built, Prior(selected), rotation, high_policy, branch_state);
    const uint32_t complement = Append_Masked_Branch(
        &built, current, compaction_policy.mask_value_ids[2 + 2 * stage],
        compaction_policy.mask_sha256[2 + 2 * stage], level,
        compaction_policy.high_resolution_ty, high_policy);
    VHO_FHE_CKKS_CONV_PLAN_POLICY add_policy = high_policy;
    if (stage + 1 == moves.size())
      add_policy.result_ty = conv_policy.result_ty;
    current = Append_Add(
        &built, Prior(complement), Prior(rotated), add_policy, branch_state);
    --level;
  }
  built.group_output_steps.push_back(current);
  built.source_final_step_index = current;
  std::vector<unsigned char> canonical;
  if (level < 0 || !VHO_FHE_CKKS_Serialize_Event_Plan(
          built, &canonical, diagnostic))
    return Report(diagnostic, "stride-two Conv plan is not canonical");
  plan->source_static_ordinal = built.source_static_ordinal;
  plan->steps.swap(built.steps);
  plan->group_output_steps.swap(built.group_output_steps);
  plan->source_final_step_index = built.source_final_step_index;
  return true;
}
