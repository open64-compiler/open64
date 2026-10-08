/*
 * Copyright (C) 2026 Open64 Project
 *
 * Construct explicit O0 CKKS circuits for the ResNet-20 pooling/classifier
 * tail. The circuit deliberately exposes every rotation, encode, multiply,
 * rescale, and add. Later O1 MetaKernel/IMRA replacement requires a separate
 * equivalence proof and does not change this correctness baseline.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C4.
 */

#include "fhe_ckks_tail_plan.h"

#include <limits>
#include <sstream>

#include "dsl_opcode.h"

namespace {

/* Emit one stable planner diagnostic without changing the output plan. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-TAIL-PLAN-001: %s\n", message);
  return false;
}

/* Render one integer attribute in canonical decimal spelling. */
std::string Decimal(int32_t value)
{
  std::ostringstream text;
  text << value;
  return text.str();
}

/* Construct one canonical attribute pair. */
VHO_FHE_CKKS_PLAN_ATTRIBUTE Attribute(const char *name,
                                      const std::string &value)
{
  VHO_FHE_CKKS_PLAN_ATTRIBUTE attribute;
  attribute.name = name;
  attribute.value = value;
  return attribute;
}

/* Refer to a pre-existing logical value. */
VHO_FHE_CKKS_PLAN_OPERAND Source(uint32_t value_id)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand;
  operand.kind = VHO_FHE_CKKS_PLAN_SOURCE_VALUE;
  operand.reference = value_id;
  return operand;
}

/* Refer to a prior step in the same event. */
VHO_FHE_CKKS_PLAN_OPERAND Prior(uint32_t step)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand;
  operand.kind = VHO_FHE_CKKS_PLAN_PRIOR_STEP;
  operand.reference = step;
  return operand;
}

/* Materialize one complete state from the reviewed tail policy. */
VHO_FHE_CKKS_PLAN_STATE State(
    const VHO_FHE_CKKS_TAIL_STATE_POLICY &policy, bool plaintext,
    int32_t level, int32_t scale_bits, uint32_t pending)
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
  state.precision_bits = policy.precision_bits;
  state.slot_count = policy.slot_count;
  state.alignment_group = policy.alignment_group;
  state.encrypted_layout = policy.encrypted_layout;
  state.pending_actions = pending;
  state.pending_bootstrap_reason = 0;
  return state;
}

/* Require the complete bounded CKKS state used by both tail operations. */
bool State_Policy_Valid(const VHO_FHE_CKKS_TAIL_STATE_POLICY &policy)
{
  return policy.source_static_ordinal != 0 &&
         policy.input_value_id != 0 && policy.input_ty != 0 &&
         policy.result_ty != 0 &&
         policy.cipher_encryption_descriptor_id != 0 &&
         policy.plaintext_encryption_descriptor_id != 0 &&
         policy.cipher_encryption_descriptor_id !=
             policy.plaintext_encryption_descriptor_id &&
         policy.scheme != 0 && policy.ciphertext_value_class != 0 &&
         policy.plaintext_value_class != 0 &&
         policy.ciphertext_value_class != policy.plaintext_value_class &&
         policy.input_level > 0 && policy.scale_bits > 0 &&
         policy.component_count == 2 && policy.precision_bits > 0 &&
         policy.slot_count == 32768 && policy.rescale_pending_action != 0 &&
         !policy.encrypted_layout.empty() &&
         !policy.rotation_key_id.empty();
}

/* Append one signed rotation with an explicit key requirement. */
uint32_t Append_Rotate(VHO_FHE_CKKS_EVENT_PLAN *plan,
                       const VHO_FHE_CKKS_PLAN_OPERAND &input,
                       int32_t rotation, uint32_t result_ty,
                       const VHO_FHE_CKKS_TAIL_STATE_POLICY &policy,
                       const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSROTATE;
  step.operator_version = 1;
  step.result_ty = result_ty;
  step.operands.push_back(input);
  step.attributes.push_back(
      Attribute("attr.signed_steps", Decimal(rotation)));
  step.attributes.push_back(
      Attribute("attr.key_id", policy.rotation_key_id));
  step.required_keys.push_back(policy.rotation_key_id);
  step.signed_rotations.push_back(rotation);
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append one state-aligned ciphertext addition. */
uint32_t Append_Add(VHO_FHE_CKKS_EVENT_PLAN *plan,
                    const VHO_FHE_CKKS_PLAN_OPERAND &left,
                    const VHO_FHE_CKKS_PLAN_OPERAND &right,
                    uint32_t result_ty,
                    const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSADD;
  step.operator_version = 1;
  step.result_ty = result_ty;
  step.operands.push_back(left);
  step.operands.push_back(right);
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append one authenticated external plaintext encode. */
uint32_t Append_Encode(VHO_FHE_CKKS_EVENT_PLAN *plan, uint32_t value_id,
                       const std::string &sha256, uint32_t result_ty,
                       const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSENCODE;
  step.operator_version = 1;
  step.result_ty = result_ty;
  step.operands.push_back(Source(value_id));
  step.plaintext_asset_sha256 = sha256;
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append one ciphertext/plaintext multiplication with pending rescale. */
uint32_t Append_Multiply(VHO_FHE_CKKS_EVENT_PLAN *plan,
                         const VHO_FHE_CKKS_PLAN_OPERAND &ciphertext,
                         uint32_t encoded_step, uint32_t result_ty,
                         const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSMUL;
  step.operator_version = 1;
  step.result_ty = result_ty;
  step.operands.push_back(ciphertext);
  step.operands.push_back(Prior(encoded_step));
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Append the explicit one-level repair after plaintext multiplication. */
uint32_t Append_Rescale(VHO_FHE_CKKS_EVENT_PLAN *plan,
                        uint32_t multiplied_step, uint32_t result_ty,
                        const VHO_FHE_CKKS_TAIL_STATE_POLICY &policy,
                        const VHO_FHE_CKKS_PLAN_STATE &state)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = OPR_DSLCKKSRESCALE;
  step.operator_version = 1;
  step.result_ty = result_ty;
  step.operands.push_back(Prior(multiplied_step));
  step.attributes.push_back(Attribute("attr.levels", "1"));
  step.attributes.push_back(
      Attribute("attr.target_scale_bits", Decimal(policy.scale_bits)));
  step.result_state = state;
  plan->steps.push_back(step);
  return static_cast<uint32_t>(plan->steps.size() - 1);
}

/* Complete and canonically validate one single-group source event. */
bool Finish(const VHO_FHE_CKKS_TAIL_STATE_POLICY &policy,
            VHO_FHE_CKKS_EVENT_PLAN *built,
            VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  if (built->steps.empty())
    return Report(diagnostic, "tail plan contains no steps");
  built->source_final_step_index =
      static_cast<uint32_t>(built->steps.size() - 1);
  built->group_output_steps.push_back(built->source_final_step_index);
  std::vector<unsigned char> canonical;
  if (!VHO_FHE_CKKS_Serialize_Event_Plan(
          *built, &canonical, diagnostic) || canonical.empty())
    return Report(diagnostic, "tail plan is not canonically serializable");
  (void)policy;
  *plan = *built;
  return true;
}

}  // namespace

bool VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
    const VHO_FHE_CKKS_POOL_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  if (plan == NULL || !State_Policy_Valid(policy.state) ||
      policy.state.input_level < 1 || policy.scale_mask_value_id == 0 ||
      policy.scale_mask_sha256.empty())
    return Report(diagnostic, "pool state or scale-mask policy is incomplete");

  VHO_FHE_CKKS_EVENT_PLAN built;
  built.source_static_ordinal = policy.state.source_static_ordinal;
  built.source_final_step_index = std::numeric_limits<uint32_t>::max();
  const VHO_FHE_CKKS_PLAN_STATE input = State(
      policy.state, false, policy.state.input_level,
      policy.state.scale_bits, 0);
  VHO_FHE_CKKS_PLAN_OPERAND sum = Source(policy.state.input_value_id);
  static const int32_t rotations[6] = {1, 2, 4, 8, 16, 32};
  for (uint32_t i = 0; i < 6; ++i) {
    const uint32_t rotated = Append_Rotate(
        &built, sum, rotations[i], policy.state.input_ty,
        policy.state, input);
    const uint32_t added = Append_Add(
        &built, sum, Prior(rotated), policy.state.input_ty, input);
    sum = Prior(added);
  }
  const VHO_FHE_CKKS_PLAN_STATE encoded = State(
      policy.state, true, policy.state.input_level,
      policy.state.scale_bits, 0);
  const uint32_t mask = Append_Encode(
      &built, policy.scale_mask_value_id, policy.scale_mask_sha256,
      policy.state.input_ty, encoded);
  const VHO_FHE_CKKS_PLAN_STATE multiplied = State(
      policy.state, false, policy.state.input_level,
      policy.state.scale_bits * 2,
      policy.state.rescale_pending_action);
  const uint32_t product = Append_Multiply(
      &built, sum, mask, policy.state.input_ty, multiplied);
  const VHO_FHE_CKKS_PLAN_STATE result = State(
      policy.state, false, policy.state.input_level - 1,
      policy.state.scale_bits, 0);
  Append_Rescale(&built, product, policy.state.result_ty,
                 policy.state, result);
  return Finish(policy.state, &built, plan, diagnostic);
}

bool VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
    const VHO_FHE_CKKS_LINEAR_PLAN_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  if (plan == NULL || !State_Policy_Valid(policy.state) ||
      policy.state.input_level < 1 ||
      policy.weight_mask_value_ids.size() != 10 ||
      policy.weight_mask_sha256.size() != 10 ||
      policy.bias_value_id == 0 || policy.bias_sha256.empty())
    return Report(diagnostic, "linear state or asset policy is incomplete");
  for (uint32_t i = 0; i < 10; ++i)
    if (policy.weight_mask_value_ids[i] == 0 ||
        policy.weight_mask_sha256[i].empty())
      return Report(diagnostic, "linear weight-mask policy is incomplete");

  VHO_FHE_CKKS_EVENT_PLAN built;
  built.source_static_ordinal = policy.state.source_static_ordinal;
  built.source_final_step_index = std::numeric_limits<uint32_t>::max();
  const VHO_FHE_CKKS_PLAN_STATE encoded = State(
      policy.state, true, policy.state.input_level,
      policy.state.scale_bits, 0);
  const VHO_FHE_CKKS_PLAN_STATE multiplied = State(
      policy.state, false, policy.state.input_level,
      policy.state.scale_bits * 2,
      policy.state.rescale_pending_action);
  const VHO_FHE_CKKS_PLAN_STATE result = State(
      policy.state, false, policy.state.input_level - 1,
      policy.state.scale_bits, 0);
  static const int32_t reductions[6] = {64, 128, 256, 512, 1024, 2048};
  bool have_logits = false;
  uint32_t logits = 0;
  for (uint32_t output = 0; output < 10; ++output) {
    const uint32_t weight = Append_Encode(
        &built, policy.weight_mask_value_ids[output],
        policy.weight_mask_sha256[output], policy.state.result_ty, encoded);
    const uint32_t product = Append_Multiply(
        &built, Source(policy.state.input_value_id), weight,
        policy.state.result_ty, multiplied);
    uint32_t branch = Append_Rescale(
        &built, product, policy.state.result_ty, policy.state, result);
    for (uint32_t rotation = 0; rotation < 6; ++rotation) {
      const uint32_t shifted = Append_Rotate(
          &built, Prior(branch), reductions[rotation],
          policy.state.result_ty, policy.state, result);
      branch = Append_Add(&built, Prior(branch), Prior(shifted),
                          policy.state.result_ty, result);
    }
    if (output != 0)
      branch = Append_Rotate(
          &built, Prior(branch), -static_cast<int32_t>(output),
          policy.state.result_ty, policy.state, result);
    if (!have_logits) {
      logits = branch;
      have_logits = true;
    } else {
      logits = Append_Add(&built, Prior(logits), Prior(branch),
                          policy.state.result_ty, result);
    }
  }
  const uint32_t bias = Append_Encode(
      &built, policy.bias_value_id, policy.bias_sha256,
      policy.state.result_ty, State(
          policy.state, true, policy.state.input_level - 1,
          policy.state.scale_bits, 0));
  Append_Add(&built, Prior(logits), Prior(bias),
             policy.state.result_ty, result);
  return Finish(policy.state, &built, plan, diagnostic);
}
