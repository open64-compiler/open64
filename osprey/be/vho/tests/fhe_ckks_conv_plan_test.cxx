/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify the explicit stride-one Conv CKKS operation and state plan.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include <assert.h>
#include <stdio.h>

#include <vector>

#include "dsl_opcode.h"
#include "fhe_ckks_conv_plan.h"

/* Build one nonuniform bounded Conv whose clear recipe is already certified. */
static VHO_FHE_CKKS_CONV_RECIPE Recipe()
{
  VHO_FHE_CKKS_CONV_SHAPE shape = {
    1, 1, 2, 4, 4, 3, 3, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 32
  };
  float weights[18];
  float bias[2] = {0.25f, -0.5f};
  for (uint32_t i = 0; i < 18; ++i)
    weights[i] = float(i + 1) / 32.0f;
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, weights, 18, bias, 2, &recipe, stderr));
  return recipe;
}

/* Supply exact input state plus one authenticated value per row and bias. */
static VHO_FHE_CKKS_CONV_PLAN_POLICY Policy()
{
  VHO_FHE_CKKS_CONV_PLAN_POLICY policy = {};
  policy.source_static_ordinal = 17;
  policy.input_value_id = 3;
  policy.result_ty = 101;
  policy.cipher_encryption_descriptor_id = 1;
  policy.plaintext_encryption_descriptor_id = 2;
  policy.scheme = 1;
  policy.ciphertext_value_class = 1;
  policy.plaintext_value_class = 2;
  policy.input_level = 15;
  policy.scale_bits = 56;
  policy.component_count = 2;
  policy.precision_bits = 40;
  policy.slot_count = 32;
  policy.alignment_group = 7;
  policy.rescale_pending_action = 1;
  policy.encrypted_layout = "ckks.packed";
  policy.rotation_key_id = "resnet20-keyset";
  const char digest_symbols[] = "0123456789abcdef";
  for (uint32_t i = 0; i < 9; ++i) {
    policy.row_value_ids.push_back(100 + i);
    policy.row_sha256.push_back(std::string(64, digest_symbols[i]));
  }
  policy.bias_value_id = 200;
  policy.bias_sha256 = std::string(64, 'f');
  return policy;
}

/* Count exact operation classes and verify every state transition. */
static void Check_Complete_Plan()
{
  VHO_FHE_CKKS_EVENT_PLAN plan;
  assert(VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
      Recipe(), Policy(), &plan, stderr));
  assert(plan.source_static_ordinal == 17 && plan.steps.size() == 47 &&
         plan.group_output_steps.size() == 1 &&
         plan.source_final_step_index == 46 &&
         plan.group_output_steps[0] == 46);
  uint32_t rotate = 0, encode = 0, multiply = 0, rescale = 0, add = 0;
  for (size_t i = 0; i < plan.steps.size(); ++i) {
    const VHO_FHE_CKKS_PLAN_STEP &step = plan.steps[i];
    assert(step.result_ty == 101 && step.operator_version == 1 &&
           step.result_state.scheme == 1 &&
           step.result_state.slot_count == 32 &&
           step.result_state.alignment_group == 7 &&
           step.result_state.encrypted_layout == "ckks.packed");
    switch (step.logical_operator) {
    case OPR_DSLCKKSROTATE:
      ++rotate;
      assert(step.required_keys.size() == 1 &&
             step.signed_rotations.size() == 1 &&
             step.result_state.level == 15 &&
             step.result_state.scale_bits == 56);
      break;
    case OPR_DSLCKKSENCODE:
      ++encode;
      assert(step.plaintext_asset_sha256.size() == 64 &&
             step.result_state.value_class == 2 &&
             step.result_state.component_count == 1);
      break;
    case OPR_DSLCKKSMUL:
      ++multiply;
      assert(step.result_state.level == 15 &&
             step.result_state.scale_bits == 112 &&
             step.result_state.pending_actions == 1);
      break;
    case OPR_DSLCKKSRESCALE:
      ++rescale;
      assert(step.result_state.level == 14 &&
             step.result_state.scale_bits == 56 &&
             step.result_state.pending_actions == 0);
      break;
    case OPR_DSLCKKSADD:
      ++add;
      break;
    default:
      assert(false);
    }
  }
  assert(rotate == 9 && encode == 10 && multiply == 9 &&
         rescale == 9 && add == 10);
  std::vector<unsigned char> bytes;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, stderr));
  printf("stride-one Conv plan: steps=%lu bytes=%lu rotations=%u\n",
         static_cast<unsigned long>(plan.steps.size()),
         static_cast<unsigned long>(bytes.size()), rotate);
}

/* Exercise every stride-one channel/width family in the captured ResNet.
 * Zero coefficients are still represented by authenticated external rows at
 * O0, so the operation count depends only on shape and input duplication. */
static void Check_ResNet_Stride_One_Families()
{
  struct Family {
    uint32_t input_channels;
    uint32_t output_channels;
    uint32_t width;
    uint32_t expected_steps;
    uint32_t expected_rotations;
  } families[] = {
    {3, 16, 32, 147, 32},
    {16, 16, 32, 722, 144},
    {32, 32, 16, 1442, 288},
    {64, 64, 8, 2882, 576}
  };
  for (size_t family = 0; family < sizeof(families) / sizeof(families[0]);
       ++family) {
    const Family &item = families[family];
    VHO_FHE_CKKS_CONV_SHAPE shape = {
      1, item.input_channels, item.output_channels,
      item.width, item.width, 3, 3, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 32768
    };
    const size_t weight_count = size_t(item.input_channels) *
                                item.output_channels * 9;
    std::vector<float> weights(weight_count, 0.125f);
    std::vector<float> bias(item.output_channels, -0.25f);
    VHO_FHE_CKKS_CONV_RECIPE recipe;
    assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
        shape, &weights[0], weights.size(), &bias[0], bias.size(),
        &recipe, stderr));
    VHO_FHE_CKKS_CONV_PLAN_POLICY policy = Policy();
    policy.slot_count = 32768;
    policy.row_value_ids.clear();
    policy.row_sha256.clear();
    for (uint32_t row = 0; row < item.input_channels * 9; ++row) {
      policy.row_value_ids.push_back(1000 + row);
      policy.row_sha256.push_back(std::string(64, "0123456789abcdef"[
          row % 16]));
    }
    VHO_FHE_CKKS_EVENT_PLAN plan;
    assert(VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
        recipe, policy, &plan, stderr));
    uint32_t rotations = 0;
    for (size_t step = 0; step < plan.steps.size(); ++step)
      rotations += plan.steps[step].logical_operator == OPR_DSLCKKSROTATE;
    assert(plan.steps.size() == item.expected_steps &&
           rotations == item.expected_rotations &&
           plan.steps.back().result_state.level == 14);
    std::vector<unsigned char> bytes;
    assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, stderr));
    printf("ResNet stride-one family %ux%ux%u: steps=%lu bytes=%lu\n",
           item.input_channels, item.output_channels, item.width,
           static_cast<unsigned long>(plan.steps.size()),
           static_cast<unsigned long>(bytes.size()));
  }
}

/* Cover both captured downsample widths and both 1x1/3x3 branches. The
 * sequential O0 network must finish at level three after its explicit
 * depth-exhaustion refresh, high-resolution Conv, selection, and bit moves. */
static void Check_ResNet_Stride_Two_Families()
{
  struct Family {
    uint32_t input_channels;
    uint32_t output_channels;
    uint32_t width;
    uint32_t kernel;
    int32_t refresh_level;
    uint32_t expected_steps;
    uint32_t expected_masks;
  } families[] = {
    {16, 32, 32, 1, 18, 190, 27},
    {16, 32, 32, 3, 18, 830, 27},
    {32, 64, 16, 1, 17, 264, 25},
    {32, 64, 16, 3, 17, 1544, 25}
  };
  for (size_t family = 0; family < sizeof(families) / sizeof(families[0]);
       ++family) {
    const Family &item = families[family];
    const uint32_t pad = item.kernel / 2;
    VHO_FHE_CKKS_CONV_SHAPE shape = {
      1, item.input_channels, item.output_channels,
      item.width, item.width, item.kernel, item.kernel, 1, 1,
      pad, pad, pad, pad, 1, 1, 1, 32768
    };
    const size_t weight_count = size_t(item.input_channels) *
                                item.output_channels * item.kernel *
                                item.kernel;
    std::vector<float> weights(weight_count, 0.125f);
    std::vector<float> bias(item.output_channels, -0.25f);
    VHO_FHE_CKKS_CONV_RECIPE recipe;
    assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
        shape, &weights[0], weights.size(), &bias[0], bias.size(),
        &recipe, stderr));
    VHO_FHE_CKKS_CONV_PLAN_POLICY policy = Policy();
    policy.input_level = 7;
    policy.slot_count = 32768;
    policy.capacity_refresh_target_level = item.refresh_level;
    policy.capacity_refresh_reason = "DEPTH_EXHAUSTION";
    policy.bootstrap_key_id = "resnet20-keyset";
    policy.row_value_ids.clear();
    policy.row_sha256.clear();
    const uint32_t rows = item.input_channels * item.kernel * item.kernel;
    for (uint32_t row = 0; row < rows; ++row) {
      policy.row_value_ids.push_back(1000 + row);
      policy.row_sha256.push_back(std::string(64, "0123456789abcdef"[
          row % 16]));
    }
    VHO_FHE_CKKS_STRIDE_COMPACTION_POLICY compaction;
    compaction.input_width = item.width;
    compaction.output_channels = item.output_channels;
    compaction.high_resolution_ty = 102;
    for (uint32_t mask = 0; mask < item.expected_masks; ++mask) {
      compaction.mask_value_ids.push_back(5000 + mask);
      compaction.mask_sha256.push_back(std::string(
          64, "fedcba9876543210"[mask % 16]));
    }
    VHO_FHE_CKKS_EVENT_PLAN plan;
    assert(VHO_FHE_CKKS_Build_Stride_Two_Conv_Event_Plan(
        recipe, policy, compaction, &plan, stderr));
    assert(plan.steps.size() == item.expected_steps &&
           plan.steps[0].logical_operator == OPR_DSLCKKSBOOTSTRAP &&
           plan.steps[0].result_state.level == item.refresh_level &&
           plan.steps.back().logical_operator == OPR_DSLCKKSADD &&
           plan.steps.back().result_ty == policy.result_ty &&
           plan.steps.back().result_state.level == 3);
    for (size_t step = 0; step + 1 < plan.steps.size(); ++step)
      assert(plan.steps[step].result_ty == compaction.high_resolution_ty);
    std::vector<unsigned char> bytes;
    assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, stderr));
    printf("ResNet stride-two family %ux%ux%u k%u: steps=%lu bytes=%lu\n",
           item.input_channels, item.output_channels, item.width,
           item.kernel, static_cast<unsigned long>(plan.steps.size()),
           static_cast<unsigned long>(bytes.size()));
  }
}

/* Rejected state or asset arrays must preserve the caller's old plan. */
static void Check_Fail_Closed()
{
  VHO_FHE_CKKS_EVENT_PLAN sentinel;
  sentinel.source_static_ordinal = 99;
  sentinel.source_final_step_index = 88;
  sentinel.group_output_steps.push_back(77);
  VHO_FHE_CKKS_CONV_PLAN_POLICY policy = Policy();
  policy.row_sha256.pop_back();
  assert(!VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
      Recipe(), policy, &sentinel, NULL));
  assert(sentinel.source_static_ordinal == 99 &&
         sentinel.source_final_step_index == 88 &&
         sentinel.group_output_steps.size() == 1 &&
         sentinel.group_output_steps[0] == 77 && sentinel.steps.empty());
  policy = Policy();
  policy.input_level = 0;
  assert(!VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
      Recipe(), policy, &sentinel, NULL));
  policy = Policy();
  policy.rotation_key_id.clear();
  assert(!VHO_FHE_CKKS_Build_Stride_One_Conv_Event_Plan(
      Recipe(), policy, &sentinel, NULL));
}

/* Run exact sequence and atomic rejection checks as one focused lane. */
int main()
{
  Check_Complete_Plan();
  Check_ResNet_Stride_One_Families();
  Check_ResNet_Stride_Two_Families();
  Check_Fail_Closed();
  puts("FHE CKKS stride-one Conv executable plan passed");
  return 0;
}
