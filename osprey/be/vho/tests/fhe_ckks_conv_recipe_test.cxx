/*
 * Copyright (C) 2026 Open64 Project
 *
 * Clear tensor/slot oracle for the bounded S6-0c C2 Conv recipe.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_ckks_conv_recipe.h"

#include <algorithm>
#include <assert.h>
#include <math.h>
#include <limits>
#include <set>
#include <stdio.h>
#include <stdlib.h>
#include <vector>

/* Build the stem-like 3x16, 32x32, 3x3 same-Conv domain without relying on
 * any runtime provider or Python model process. */
static VHO_FHE_CKKS_CONV_SHAPE Stem_Shape()
{
  VHO_FHE_CKKS_CONV_SHAPE shape = {
    1, 3, 16, 32, 32, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 32768
  };
  return shape;
}

/* Use nontrivial signed, nonuniform test bytes so row, column, channel,
 * boundary, and bias errors cannot hide behind an all-ones fixture. */
static void Fixture_Bytes(std::vector<float> *weights,
                          std::vector<float> *bias,
                          std::vector<float> *input)
{
  weights->resize(16 * 3 * 3 * 3);
  bias->resize(16);
  input->assign(32768, 13.0f);
  for (size_t i = 0; i < weights->size(); ++i)
    (*weights)[i] = float(int(i % 13) - 6) / 32.0f;
  for (size_t i = 0; i < bias->size(); ++i)
    (*bias)[i] = float(int(i) - 7) / 8.0f;
  for (size_t i = 0; i < 3 * 32 * 32; ++i)
    (*input)[i] = float(int(i % 31) - 15) / 16.0f;
}

/* Independently compute NCHW/OIHW Conv without rotations, packing masks, or
 * recipe terms. This is the reference for every active output slot. */
static double Tensor_Oracle(uint32_t oc, uint32_t y, uint32_t x,
                            const std::vector<float> &weights,
                            const std::vector<float> &bias,
                            const std::vector<float> &input)
{
  double sum = bias[oc];
  for (uint32_t ci = 0; ci < 3; ++ci)
    for (uint32_t ky = 0; ky < 3; ++ky)
      for (uint32_t kx = 0; kx < 3; ++kx) {
        const int64_t iy = int64_t(y) + ky - 1;
        const int64_t ix = int64_t(x) + kx - 1;
        if (iy < 0 || ix < 0 || iy >= 32 || ix >= 32)
          continue;
        const size_t source = ci * 1024 + iy * 32 + ix;
        const size_t weight = ((oc * 3 + ci) * 3 + ky) * 3 + kx;
        sum += double(input[source]) * weights[weight];
      }
  return sum;
}

/* Check the complete 16-channel output and its inactive slot tail against
 * the independent tensor oracle, plus exact rotation/key summaries. */
static void Check_Stem_Like_Conv()
{
  const VHO_FHE_CKKS_CONV_SHAPE shape = Stem_Shape();
  std::vector<float> weights, bias, input;
  Fixture_Bytes(&weights, &bias, &input);
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, stderr));
  assert(recipe.terms.size() == 432);
  assert(recipe.active_input_slots == 3072 &&
         recipe.active_output_slots == 16384 &&
         recipe.plaintext_multiply_depth == 1);
  assert(recipe.terms[0].signed_rotation == -33);
  assert(recipe.terms[0].active_output_slots == 31 * 31);
  assert(recipe.terms[4].signed_rotation == 0);
  assert(recipe.terms[4].active_output_slots == 32 * 32);

  std::set<int32_t> keys;
  uint32_t live = 0;
  for (size_t i = 0; i < recipe.terms.size(); ++i)
    if (weights[i] != 0) {
      ++live;
      if (recipe.terms[i].signed_rotation != 0)
        keys.insert(recipe.terms[i].signed_rotation);
    }
  assert(recipe.live_term_count == live);
  assert(recipe.required_signed_rotations.size() == keys.size());
  assert(std::equal(recipe.required_signed_rotations.begin(),
                    recipe.required_signed_rotations.end(), keys.begin()));

  std::vector<double> actual;
  assert(VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &actual, stderr));
  assert(actual.size() == shape.slot_count);
  for (uint32_t oc = 0; oc < 16; ++oc)
    for (uint32_t y = 0; y < 32; ++y)
      for (uint32_t x = 0; x < 32; ++x) {
        const size_t index = oc * 1024 + y * 32 + x;
        assert(fabs(actual[index] - Tensor_Oracle(
            oc, y, x, weights, bias, input)) < 1e-9);
      }
  for (size_t i = 16384; i < actual.size(); ++i)
    assert(actual[i] == 0.0);
  printf("synthetic stem: terms=%zu live=%u signed_rotation_keys=%zu "
         "input_slots=%u output_slots=%u plaintext_depth=%u\n",
         recipe.terms.size(), recipe.live_term_count,
         recipe.required_signed_rotations.size(),
         recipe.active_input_slots, recipe.active_output_slots,
         recipe.plaintext_multiply_depth);
}

/* Unsupported strides, insufficient slots, malformed folded bytes, and
 * missing input slots must leave the caller's previous outputs unchanged. */
static void Check_Fail_Closed()
{
  VHO_FHE_CKKS_CONV_SHAPE shape = Stem_Shape();
  std::vector<float> weights, bias, input;
  Fixture_Bytes(&weights, &bias, &input);
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  recipe.live_term_count = 77;
  shape.stride_width = 2;
  assert(!VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, NULL));
  assert(recipe.live_term_count == 77 && recipe.terms.empty());
  shape = Stem_Shape();
  shape.slot_count = 8192;
  assert(!VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, NULL));
  shape = Stem_Shape();
  weights[0] = std::numeric_limits<float>::infinity();
  assert(!VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, NULL));
  weights[0] = 1.0f;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, stderr));
  std::vector<double> previous(1, 77.0);
  input.pop_back();
  assert(!VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &previous, NULL));
  assert(previous.size() == 1 && previous[0] == 77.0);
  input.push_back(13.0f);
  ++recipe.terms[0].signed_rotation;
  assert(!VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &previous, NULL));
  assert(previous.size() == 1 && previous[0] == 77.0);
}

/* Exercise the identical recipe/oracle on authenticated, extracted folded
 * stem bytes without making model data part of the source repository. */
static void Check_Captured_Folded_Conv(const char *fixture_path)
{
  FILE *fixture = fopen(fixture_path, "rb");
  assert(fixture != NULL);
  std::vector<float> weights(432), bias(16), input;
  assert(fread(&weights[0], sizeof(float), weights.size(), fixture) ==
         weights.size());
  assert(fread(&bias[0], sizeof(float), bias.size(), fixture) == bias.size());
  assert(fgetc(fixture) == EOF);
  assert(fclose(fixture) == 0);
  std::vector<float> unused_weights, unused_bias;
  Fixture_Bytes(&unused_weights, &unused_bias, &input);
  const VHO_FHE_CKKS_CONV_SHAPE shape = Stem_Shape();
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, stderr));
  std::vector<double> actual;
  assert(VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &actual, stderr));
  for (uint32_t oc = 0; oc < 16; ++oc)
    for (uint32_t y = 0; y < 32; ++y)
      for (uint32_t x = 0; x < 32; ++x)
        assert(fabs(actual[oc * 1024 + y * 32 + x] - Tensor_Oracle(
            oc, y, x, weights, bias, input)) < 1e-8);
  printf("captured folded stem: terms=%zu live=%u "
         "signed_rotation_keys=%zu output_slots=%u\n",
         recipe.terms.size(), recipe.live_term_count,
         recipe.required_signed_rotations.size(),
         recipe.active_output_slots);
}

/* Keep this focused fixture below native expansion and mapped-image claims. */
int main(int argc, char **argv)
{
  assert(argc == 1 || argc == 2);
  Check_Stem_Like_Conv();
  Check_Fail_Closed();
  if (argc == 2)
    Check_Captured_Folded_Conv(argv[1]);
  puts("FHE CKKS column-first Conv recipe passed (no WHIRL emitted)");
  return 0;
}
