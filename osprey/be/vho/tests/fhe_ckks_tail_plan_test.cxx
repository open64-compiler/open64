/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused structural tests for the explicit ResNet-20 pooling/classifier CKKS
 * plans. Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C2-C4.
 */

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <map>
#include <string>
#include <vector>

#include "defs.h"
#include "dsl_opcode.h"
#include "fhe_ckks_tail_plan.h"

typedef std::vector<double> SLOT_VECTOR;
typedef std::map<UINT32, SLOT_VECTOR> SLOT_SOURCE_TABLE;

/* Resolve one clear operand from either an authenticated source asset or a
 * prior planned result. This evaluator is intentionally independent of the
 * planner's construction helpers. */
static const SLOT_VECTOR &Resolve(
    const VHO_FHE_CKKS_PLAN_OPERAND &operand,
    const SLOT_SOURCE_TABLE &sources,
    const std::vector<SLOT_VECTOR> &results)
{
  if (operand.kind == VHO_FHE_CKKS_PLAN_PRIOR_STEP) {
    assert(operand.reference < results.size());
    return results[operand.reference];
  }
  assert(operand.kind == VHO_FHE_CKKS_PLAN_SOURCE_VALUE);
  SLOT_SOURCE_TABLE::const_iterator found = sources.find(operand.reference);
  assert(found != sources.end());
  return found->second;
}

/* Read one required signed integer attribute from a planned operation. */
static INT32 Integer_Attribute(const VHO_FHE_CKKS_PLAN_STEP &step,
                               const char *name)
{
  for (size_t i = 0; i < step.attributes.size(); ++i)
    if (step.attributes[i].name == name) {
      char *end = NULL;
      const long value = strtol(step.attributes[i].value.c_str(), &end, 10);
      assert(end != NULL && *end == 0);
      return static_cast<INT32>(value);
    }
  assert(false);
  return 0;
}

/* Execute the provider-independent tail plan as exact clear slot algebra.
 * Rescale is an identity in this oracle because level/scale are certified by
 * the separate state-transfer contract. */
static SLOT_VECTOR Evaluate(const VHO_FHE_CKKS_EVENT_PLAN &plan,
                            const SLOT_SOURCE_TABLE &sources)
{
  std::vector<SLOT_VECTOR> results;
  for (size_t step_index = 0; step_index < plan.steps.size(); ++step_index) {
    const VHO_FHE_CKKS_PLAN_STEP &step = plan.steps[step_index];
    SLOT_VECTOR result(32768, 0.0);
    if (step.logical_operator == OPR_DSLCKKSENCODE) {
      assert(step.operands.size() == 1);
      result = Resolve(step.operands[0], sources, results);
    } else if (step.logical_operator == OPR_DSLCKKSROTATE) {
      assert(step.operands.size() == 1);
      const SLOT_VECTOR &input = Resolve(step.operands[0], sources, results);
      const INT32 rotation = Integer_Attribute(step, "attr.signed_steps");
      for (UINT32 slot = 0; slot < result.size(); ++slot) {
        const INT64 source =
            (INT64(slot) + rotation + result.size()) % result.size();
        result[slot] = input[source];
      }
    } else if (step.logical_operator == OPR_DSLCKKSADD ||
               step.logical_operator == OPR_DSLCKKSMUL) {
      assert(step.operands.size() == 2);
      const SLOT_VECTOR &left = Resolve(step.operands[0], sources, results);
      const SLOT_VECTOR &right = Resolve(step.operands[1], sources, results);
      for (UINT32 slot = 0; slot < result.size(); ++slot)
        result[slot] = step.logical_operator == OPR_DSLCKKSADD ?
            left[slot] + right[slot] : left[slot] * right[slot];
    } else if (step.logical_operator == OPR_DSLCKKSRESCALE) {
      assert(step.operands.size() == 1);
      result = Resolve(step.operands[0], sources, results);
    } else {
      assert(false);
    }
    results.push_back(result);
  }
  assert(!results.empty() &&
         plan.source_final_step_index == results.size() - 1);
  return results.back();
}

/* Compare deterministic clear values without relying on provider rounding. */
static void Near(double actual, double expected)
{
  assert(fabs(actual - expected) <= 1.0e-10 * (1.0 + fabs(expected)));
}

static VHO_FHE_CKKS_TAIL_STATE_POLICY State_Policy()
{
  VHO_FHE_CKKS_TAIL_STATE_POLICY policy;
  policy.source_static_ordinal = 145;
  policy.input_value_id = 260;
  policy.input_ty = 49154;
  policy.result_ty = 49666;
  policy.cipher_encryption_descriptor_id = 1;
  policy.plaintext_encryption_descriptor_id = 2;
  policy.scheme = 1;
  policy.ciphertext_value_class = 1;
  policy.plaintext_value_class = 2;
  policy.input_level = 5;
  policy.scale_bits = 56;
  policy.component_count = 2;
  policy.precision_bits = 30;
  policy.slot_count = 32768;
  policy.alignment_group = 0;
  policy.rescale_pending_action = 1;
  policy.encrypted_layout = "ckks.packed";
  policy.rotation_key_id = "secure_resnet20_keys";
  return policy;
}

static void Count(const VHO_FHE_CKKS_EVENT_PLAN &plan,
                  UINT32 *rotate, UINT32 *add, UINT32 *encode,
                  UINT32 *multiply, UINT32 *rescale)
{
  *rotate = *add = *encode = *multiply = *rescale = 0;
  for (size_t i = 0; i < plan.steps.size(); ++i) {
    switch (plan.steps[i].logical_operator) {
    case OPR_DSLCKKSROTATE: ++*rotate; break;
    case OPR_DSLCKKSADD: ++*add; break;
    case OPR_DSLCKKSENCODE: ++*encode; break;
    case OPR_DSLCKKSMUL: ++*multiply; break;
    case OPR_DSLCKKSRESCALE: ++*rescale; break;
    default: assert(false); break;
    }
  }
}

static void Check_Pool()
{
  VHO_FHE_CKKS_POOL_PLAN_POLICY policy;
  policy.state = State_Policy();
  policy.scale_mask_value_id = 400;
  policy.scale_mask_sha256 =
      "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
  VHO_FHE_CKKS_EVENT_PLAN plan;
  assert(VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
      policy, &plan, stderr));
  assert(plan.steps.size() == 15 && plan.group_output_steps.size() == 1 &&
         plan.source_final_step_index == 14 &&
         plan.steps.back().result_ty == policy.state.result_ty &&
         plan.steps.back().result_state.level == 4);
  UINT32 rotate, add, encode, multiply, rescale;
  Count(plan, &rotate, &add, &encode, &multiply, &rescale);
  assert(rotate == 6 && add == 6 && encode == 1 && multiply == 1 &&
         rescale == 1);
  static const INT32 expected[6] = {1, 2, 4, 8, 16, 32};
  UINT32 rotation_index = 0;
  for (size_t i = 0; i < plan.steps.size(); ++i)
    if (plan.steps[i].logical_operator == OPR_DSLCKKSROTATE) {
      assert(plan.steps[i].signed_rotations.size() == 1 &&
             plan.steps[i].signed_rotations[0] == expected[rotation_index]);
      ++rotation_index;
    }
  std::vector<unsigned char> first;
  std::vector<unsigned char> second;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &first, stderr));
  VHO_FHE_CKKS_EVENT_PLAN repeated;
  assert(VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
             policy, &repeated, stderr) &&
         VHO_FHE_CKKS_Serialize_Event_Plan(repeated, &second, stderr) &&
         first == second);
  policy.scale_mask_value_id = 0;
  VHO_FHE_CKKS_EVENT_PLAN rejected = plan;
  assert(!VHO_FHE_CKKS_Build_Global_Average_Pool_Event_Plan(
      policy, &rejected, NULL));
  assert(rejected.steps.size() == plan.steps.size());
  policy.scale_mask_value_id = 400;

  SLOT_SOURCE_TABLE sources;
  SLOT_VECTOR input(32768, 0.0);
  SLOT_VECTOR mask(32768, 0.0);
  for (UINT32 channel = 0; channel < 64; ++channel) {
    for (UINT32 spatial = 0; spatial < 64; ++spatial)
      input[channel * 64 + spatial] =
          double(channel) * 0.25 + double(spatial) / 16.0;
    mask[channel * 64] = 1.0 / 64.0;
  }
  sources[policy.state.input_value_id] = input;
  sources[policy.scale_mask_value_id] = mask;
  const SLOT_VECTOR pooled = Evaluate(plan, sources);
  for (UINT32 channel = 0; channel < 64; ++channel) {
    double expected = 0.0;
    for (UINT32 spatial = 0; spatial < 64; ++spatial)
      expected += input[channel * 64 + spatial];
    Near(pooled[channel * 64], expected / 64.0);
  }
  for (UINT32 slot = 0; slot < pooled.size(); ++slot)
    if (slot % 64 != 0)
      Near(pooled[slot], 0.0);
}

static void Check_Linear()
{
  VHO_FHE_CKKS_LINEAR_PLAN_POLICY policy;
  policy.state = State_Policy();
  policy.state.source_static_ordinal = 147;
  policy.state.input_value_id = 261;
  policy.state.input_ty = 49666;
  policy.state.result_ty = 50690;
  policy.state.input_level = 4;
  for (UINT32 i = 0; i < 10; ++i) {
    policy.weight_mask_value_ids.push_back(500 + i);
    policy.weight_mask_sha256.push_back(
        "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789");
  }
  policy.bias_value_id = 510;
  policy.bias_sha256 =
      "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210";
  VHO_FHE_CKKS_EVENT_PLAN plan;
  assert(VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
      policy, &plan, stderr));
  assert(plan.steps.size() == 170 && plan.group_output_steps.size() == 1 &&
         plan.source_final_step_index == 169 &&
         plan.steps.back().result_ty == policy.state.result_ty &&
         plan.steps.back().result_state.level == 3);
  UINT32 rotate, add, encode, multiply, rescale;
  Count(plan, &rotate, &add, &encode, &multiply, &rescale);
  assert(rotate == 69 && add == 70 && encode == 11 && multiply == 10 &&
         rescale == 10);
  std::vector<unsigned char> baseline;
  std::vector<unsigned char> changed;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &baseline, stderr));
  policy.weight_mask_value_ids[4] = 900;
  VHO_FHE_CKKS_EVENT_PLAN changed_plan;
  assert(VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
             policy, &changed_plan, stderr) &&
         VHO_FHE_CKKS_Serialize_Event_Plan(
             changed_plan, &changed, stderr) &&
         baseline != changed);
  policy.weight_mask_value_ids.pop_back();
  VHO_FHE_CKKS_EVENT_PLAN rejected = plan;
  assert(!VHO_FHE_CKKS_Build_ResNet20_Linear_Event_Plan(
      policy, &rejected, NULL));
  assert(rejected.steps.size() == plan.steps.size());
  policy.weight_mask_value_ids[4] = 504;
  policy.weight_mask_value_ids.push_back(509);

  SLOT_SOURCE_TABLE sources;
  SLOT_VECTOR input(32768, 0.0);
  double channel_values[64];
  for (UINT32 channel = 0; channel < 64; ++channel) {
    channel_values[channel] = double(channel + 1) / 32.0;
    input[channel * 64] = channel_values[channel];
  }
  sources[policy.state.input_value_id] = input;
  double expected[10];
  for (UINT32 output = 0; output < 10; ++output) {
    SLOT_VECTOR mask(32768, 0.0);
    expected[output] = 0.125 * double(output + 1);
    for (UINT32 channel = 0; channel < 64; ++channel) {
      const double weight =
          (double(output + 1) * double(channel + 3)) / 4096.0;
      mask[channel * 64] = weight;
      expected[output] += channel_values[channel] * weight;
    }
    sources[policy.weight_mask_value_ids[output]] = mask;
  }
  SLOT_VECTOR bias(32768, 0.0);
  for (UINT32 output = 0; output < 10; ++output)
    bias[output] = 0.125 * double(output + 1);
  sources[policy.bias_value_id] = bias;
  const SLOT_VECTOR logits = Evaluate(plan, sources);
  for (UINT32 output = 0; output < 10; ++output)
    Near(logits[output], expected[output]);
}

int main()
{
  Check_Pool();
  Check_Linear();
  printf("FHE CKKS ResNet tail plans passed.\n");
  return 0;
}
