/*
 * Copyright (C) 2026 Open64 Project
 *
 * Explicit CKKS scheduling for the ANT ACE composite-ReLU recipe. The plan
 * makes every scale/level/component repair visible before WHIRL mutation.
 * Design: doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#include "fhe_ckks_relu_event_plan.h"

#include <limits.h>

#include "dsl_opcode.h"
#include "fhe_plan.h"

namespace {

/* One planned value and its concrete result state. */
struct PLANNED_VALUE {
  VHO_FHE_CKKS_PLAN_OPERAND operand;
  VHO_FHE_CKKS_PLAN_STATE state;
};

/* Emit one stable planning diagnostic without changing the caller's plan. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-RELU-EVENT-001: %s\n", message);
  return FALSE;
}

/* Format a signed attribute value without locale-dependent streams. */
std::string Decimal(INT32 value)
{
  char text[32];
  snprintf(text, sizeof(text), "%d", value);
  return text;
}

/* Name a stable source operand. */
VHO_FHE_CKKS_PLAN_OPERAND Source(UINT32 value_id)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand;
  operand.kind = VHO_FHE_CKKS_PLAN_SOURCE_VALUE;
  operand.reference = value_id;
  return operand;
}

/* Name an already emitted plan result. */
VHO_FHE_CKKS_PLAN_OPERAND Prior(UINT32 step)
{
  VHO_FHE_CKKS_PLAN_OPERAND operand;
  operand.kind = VHO_FHE_CKKS_PLAN_PRIOR_STEP;
  operand.reference = step;
  return operand;
}

/* Construct one stable textual attribute. */
VHO_FHE_CKKS_PLAN_ATTRIBUTE Attribute(const char *name,
                                      const std::string &value)
{
  VHO_FHE_CKKS_PLAN_ATTRIBUTE attribute;
  attribute.name = name;
  attribute.value = value;
  return attribute;
}

/* Construct one complete result-state tuple. */
VHO_FHE_CKKS_PLAN_STATE State(
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy, BOOL plaintext,
    INT32 level, INT32 scale_bits, INT32 components,
    UINT32 pending_actions)
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
  state.component_count = plaintext ? 1 : components;
  state.precision_bits = policy.precision_bits;
  state.slot_count = policy.slot_count;
  state.alignment_group = policy.alignment_group;
  state.encrypted_layout = policy.encrypted_layout;
  state.pending_actions = pending_actions;
  state.pending_bootstrap_reason = 0;
  return state;
}

/* Append one plan step and return its owner-qualified local result. */
PLANNED_VALUE Append(
    VHO_FHE_CKKS_EVENT_PLAN *plan, UINT32 logical_operator,
    const std::vector<VHO_FHE_CKKS_PLAN_OPERAND> &operands,
    const VHO_FHE_CKKS_PLAN_STATE &state,
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy)
{
  VHO_FHE_CKKS_PLAN_STEP step;
  step.logical_operator = logical_operator;
  step.operator_version = 1;
  step.result_ty = policy.result_ty;
  step.operands = operands;
  step.result_state = state;
  plan->steps.push_back(step);
  PLANNED_VALUE value;
  value.operand = Prior((UINT32)plan->steps.size() - 1);
  value.state = state;
  return value;
}

/* Encode one authenticated scalar tensor at an exact consumer level. */
PLANNED_VALUE Encode(
    VHO_FHE_CKKS_EVENT_PLAN *plan, UINT32 value_id,
    const std::string &sha256, INT32 level,
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy)
{
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> operands(1, Source(value_id));
  PLANNED_VALUE value = Append(
      plan, OPR_DSLCKKSENCODE, operands,
      State(policy, TRUE, level, policy.scale_bits, 1, 0), policy);
  plan->steps.back().plaintext_asset_sha256 = sha256;
  return value;
}

/* Lower a higher-level ciphertext to an exact consumer level. */
PLANNED_VALUE Modswitch(
    VHO_FHE_CKKS_EVENT_PLAN *plan, const PLANNED_VALUE &input,
    INT32 level, const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy)
{
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> operands(1, input.operand);
  VHO_FHE_CKKS_PLAN_STATE output = input.state;
  output.level = level;
  PLANNED_VALUE value = Append(
      plan, OPR_DSLCKKSMODSWITCH, operands, output, policy);
  plan->steps.back().attributes.push_back(
      Attribute("attr.target_level", Decimal(level)));
  return value;
}

/* Align two ciphertext values at the lower available modulus level. */
BOOL Align(
    VHO_FHE_CKKS_EVENT_PLAN *plan, PLANNED_VALUE *left,
    PLANNED_VALUE *right, const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy)
{
  if (left->state.value_class != policy.ciphertext_value_class ||
      right->state.value_class != policy.ciphertext_value_class ||
      left->state.scale_bits != right->state.scale_bits)
    return FALSE;
  const INT32 target = left->state.level < right->state.level ?
      left->state.level : right->state.level;
  if (left->state.level > target)
    *left = Modswitch(plan, *left, target, policy);
  if (right->state.level > target)
    *right = Modswitch(plan, *right, target, policy);
  return TRUE;
}

/* Append aligned addition; encoded scalar constants are already level-matched. */
BOOL Add(
    VHO_FHE_CKKS_EVENT_PLAN *plan, PLANNED_VALUE left,
    PLANNED_VALUE right, const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy,
    PLANNED_VALUE *result)
{
  const BOOL left_plain = left.state.value_class ==
                          policy.plaintext_value_class;
  const BOOL right_plain = right.state.value_class ==
                           policy.plaintext_value_class;
  if (left_plain && right_plain)
    return FALSE;
  if (!left_plain && !right_plain) {
    if (!Align(plan, &left, &right, policy))
      return FALSE;
  } else if (left.state.level != right.state.level ||
             left.state.scale_bits != right.state.scale_bits) {
    return FALSE;
  }
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> operands;
  operands.push_back(left.operand);
  operands.push_back(right.operand);
  const PLANNED_VALUE &cipher = left_plain ? right : left;
  *result = Append(plan, OPR_DSLCKKSADD, operands,
                   State(policy, FALSE, cipher.state.level,
                         cipher.state.scale_bits,
                         cipher.state.component_count, 0), policy);
  return TRUE;
}

/* Append multiply plus the explicit relinearize/rescale repair sequence. */
BOOL Multiply(
    VHO_FHE_CKKS_EVENT_PLAN *plan, PLANNED_VALUE left,
    PLANNED_VALUE right, const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy,
    PLANNED_VALUE *result)
{
  const BOOL left_plain = left.state.value_class ==
                          policy.plaintext_value_class;
  const BOOL right_plain = right.state.value_class ==
                           policy.plaintext_value_class;
  if (left_plain && right_plain)
    return FALSE;
  if (!left_plain && !right_plain) {
    if (!Align(plan, &left, &right, policy))
      return FALSE;
  } else if (left.state.level != right.state.level ||
             left.state.scale_bits != right.state.scale_bits) {
    return FALSE;
  }
  const BOOL two_ciphertexts = !left_plain && !right_plain;
  const PLANNED_VALUE &cipher = left_plain ? right : left;
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> operands;
  operands.push_back(left.operand);
  operands.push_back(right.operand);
  UINT32 pending = DSL_FHE_CKKS_PENDING_RESCALE |
      (two_ciphertexts ? DSL_FHE_CKKS_PENDING_RELINEARIZE : 0);
  PLANNED_VALUE product = Append(
      plan, OPR_DSLCKKSMUL, operands,
      State(policy, FALSE, cipher.state.level,
            left.state.scale_bits + right.state.scale_bits,
            two_ciphertexts ? left.state.component_count +
                                  right.state.component_count - 1 :
                              cipher.state.component_count,
            pending), policy);
  if (two_ciphertexts) {
    std::vector<VHO_FHE_CKKS_PLAN_OPERAND> relin_operands(
        1, product.operand);
    VHO_FHE_CKKS_PLAN_STATE relin_state = product.state;
    relin_state.component_count = 2;
    relin_state.pending_actions &=
        ~DSL_FHE_CKKS_PENDING_RELINEARIZE;
    product = Append(plan, OPR_DSLCKKSRELIN, relin_operands,
                     relin_state, policy);
    plan->steps.back().attributes.push_back(
        Attribute("attr.key_id", policy.relinearization_key_id));
    plan->steps.back().required_keys.push_back(
        policy.relinearization_key_id);
  }
  if (product.state.level <= 0)
    return FALSE;
  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> rescale_operands(
      1, product.operand);
  VHO_FHE_CKKS_PLAN_STATE rescale_state = product.state;
  --rescale_state.level;
  rescale_state.scale_bits = policy.scale_bits;
  rescale_state.pending_actions &= ~DSL_FHE_CKKS_PENDING_RESCALE;
  *result = Append(plan, OPR_DSLCKKSRESCALE, rescale_operands,
                   rescale_state, policy);
  plan->steps.back().attributes.push_back(Attribute("attr.levels", "1"));
  plan->steps.back().attributes.push_back(
      Attribute("attr.target_scale_bits", Decimal(policy.scale_bits)));
  return TRUE;
}

/* Resolve one algebraic root or prior result into the current event plan. */
BOOL Resolve(
    const VHO_FHE_RELU_VALUE_REF &reference,
    const PLANNED_VALUE &original, const PLANNED_VALUE &normalized,
    const std::vector<PLANNED_VALUE> &results, PLANNED_VALUE *value)
{
  if (value == NULL)
    return FALSE;
  if (reference.kind == VHO_FHE_RELU_VALUE_ORIGINAL_INPUT) {
    *value = original;
    return TRUE;
  }
  if (reference.kind == VHO_FHE_RELU_VALUE_NORMALIZED_INPUT) {
    *value = normalized;
    return TRUE;
  }
  if (reference.kind != VHO_FHE_RELU_VALUE_PRIOR_STEP ||
      reference.step_index >= results.size())
    return FALSE;
  *value = results[reference.step_index];
  return TRUE;
}

/* Reject incomplete policy and any recipe other than the pinned depth-11 DAG. */
BOOL Inputs_Valid(
    const VHO_FHE_RELU_RECIPE &recipe,
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy)
{
  return recipe.steps.size() == 94 && recipe.stages.size() == 3 &&
         recipe.stages[0].degree == 7 && recipe.stages[1].degree == 15 &&
         recipe.stages[2].degree == 13 &&
         recipe.stages[0].output_depth - recipe.stages[0].input_depth == 3 &&
         recipe.stages[1].output_depth - recipe.stages[1].input_depth == 4 &&
         recipe.stages[2].output_depth - recipe.stages[2].input_depth == 4 &&
         recipe.result.kind == VHO_FHE_RELU_VALUE_PRIOR_STEP &&
         recipe.result.step_index < recipe.steps.size() &&
         recipe.steps[recipe.result.step_index].algebraic_depth == 11 &&
         policy.source_static_ordinal != 0 && policy.source_value_id != 0 &&
         policy.result_ty != 0 && policy.cipher_encryption_descriptor_id != 0 &&
         policy.plaintext_encryption_descriptor_id != 0 &&
         policy.scheme != 0 && policy.ciphertext_value_class != 0 &&
         policy.plaintext_value_class != 0 &&
         policy.ciphertext_value_class != policy.plaintext_value_class &&
         policy.post_refresh_level >= 13 && policy.scale_bits > 0 &&
         policy.scale_bits <= INT_MAX / 2 && policy.component_count == 2 &&
         policy.precision_bits >= 30 && policy.slot_count != 0 &&
         policy.pending_bootstrap_action ==
             DSL_FHE_CKKS_PENDING_BOOTSTRAP &&
         policy.pre_relu_bootstrap_reason ==
             DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH &&
         !policy.encrypted_layout.empty() &&
         !policy.bootstrap_key_id.empty() &&
         !policy.relinearization_key_id.empty() &&
         policy.reciprocal_bound_value_id != 0 &&
         !policy.reciprocal_bound_sha256.empty() &&
         policy.scalar_value_ids.size() == recipe.steps.size() &&
         policy.scalar_sha256.size() == recipe.steps.size();
}

}  // namespace

/* Build the complete post-refresh depth-12 executable event plan. */
BOOL VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
    const VHO_FHE_RELU_RECIPE &recipe,
    const VHO_FHE_CKKS_RELU_EVENT_POLICY &policy,
    VHO_FHE_CKKS_EVENT_PLAN *plan, FILE *diagnostic)
{
  if (plan == NULL || !Inputs_Valid(recipe, policy))
    return Report(diagnostic, "recipe or executable policy is incomplete");

  VHO_FHE_CKKS_EVENT_PLAN built;
  built.source_static_ordinal = policy.source_static_ordinal;
  built.source_final_step_index = UINT_MAX;

  std::vector<VHO_FHE_CKKS_PLAN_OPERAND> bootstrap_operands(
      1, Source(policy.source_value_id));
  PLANNED_VALUE original = Append(
      &built, OPR_DSLCKKSBOOTSTRAP, bootstrap_operands,
      State(policy, FALSE, policy.post_refresh_level,
            policy.scale_bits, 2, 0), policy);
  built.steps.back().attributes.push_back(Attribute(
      "attr.target_level", Decimal(policy.post_refresh_level)));
  built.steps.back().attributes.push_back(
      Attribute("attr.reason", "PRE_RELU_REFRESH"));
  built.steps.back().attributes.push_back(
      Attribute("attr.key_id", policy.bootstrap_key_id));
  built.steps.back().required_keys.push_back(policy.bootstrap_key_id);

  PLANNED_VALUE reciprocal = Encode(
      &built, policy.reciprocal_bound_value_id,
      policy.reciprocal_bound_sha256, policy.post_refresh_level, policy);
  PLANNED_VALUE normalized;
  if (!Multiply(&built, original, reciprocal, policy, &normalized) ||
      normalized.state.level != policy.post_refresh_level - 1)
    return Report(diagnostic, "normalization did not consume one level");

  std::vector<PLANNED_VALUE> results;
  results.reserve(recipe.steps.size());
  for (UINT32 i = 0; i < recipe.steps.size(); ++i) {
    const VHO_FHE_RELU_RECIPE_STEP &step = recipe.steps[i];
    PLANNED_VALUE left;
    PLANNED_VALUE right;
    PLANNED_VALUE output;
    if (step.operation == VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT) {
      if (!Resolve(step.left, original, normalized, results, &left) ||
          policy.scalar_value_ids[i] == 0 ||
          policy.scalar_sha256[i].empty())
        return Report(diagnostic, "scalar constant lacks a level anchor");
      output = Encode(&built, policy.scalar_value_ids[i],
                      policy.scalar_sha256[i], left.state.level, policy);
    } else if (step.operation == VHO_FHE_RELU_RECIPE_MUL_SCALAR) {
      if (!Resolve(step.left, original, normalized, results, &left) ||
          policy.scalar_value_ids[i] == 0 ||
          policy.scalar_sha256[i].empty())
        return Report(diagnostic, "scalar multiply input is incomplete");
      right = Encode(&built, policy.scalar_value_ids[i],
                     policy.scalar_sha256[i], left.state.level, policy);
      if (!Multiply(&built, left, right, policy, &output))
        return Report(diagnostic, "scalar multiply could not be repaired");
    } else {
      if (!Resolve(step.left, original, normalized, results, &left) ||
          !Resolve(step.right, original, normalized, results, &right))
        return Report(diagnostic, "algebraic operand is unavailable");
      if (step.operation == VHO_FHE_RELU_RECIPE_ADD) {
        if (!Add(&built, left, right, policy, &output))
          return Report(diagnostic, "addition operands cannot be aligned");
      } else if (step.operation == VHO_FHE_RELU_RECIPE_MUL) {
        if (!Multiply(&built, left, right, policy, &output))
          return Report(diagnostic, "ciphertext multiply could not be repaired");
      } else {
        return Report(diagnostic, "unknown algebraic recipe operation");
      }
    }
    results.push_back(output);
  }

  PLANNED_VALUE final_value;
  if (!Resolve(recipe.result, original, normalized, results, &final_value) ||
      final_value.state.value_class != policy.ciphertext_value_class ||
      final_value.state.level != policy.post_refresh_level - 12 ||
      final_value.state.scale_bits != policy.scale_bits ||
      final_value.state.component_count != 2 ||
      final_value.state.pending_actions != 0 ||
      final_value.operand.kind != VHO_FHE_CKKS_PLAN_PRIOR_STEP)
    return Report(diagnostic, "final CKKS state does not match depth-12 path");
  built.source_final_step_index = final_value.operand.reference;
  built.group_output_steps.push_back(built.source_final_step_index);
  std::vector<unsigned char> canonical;
  if (!VHO_FHE_CKKS_Serialize_Event_Plan(
          built, &canonical, diagnostic))
    return Report(diagnostic, "completed event plan is not canonical");
  *plan = built;
  return TRUE;
}
