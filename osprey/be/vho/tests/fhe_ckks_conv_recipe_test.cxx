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
static double Tensor_Oracle(const VHO_FHE_CKKS_CONV_SHAPE &shape,
                            uint32_t oc, uint32_t y, uint32_t x,
                            const std::vector<float> &weights,
                            const std::vector<float> &bias,
                            const std::vector<float> &input)
{
  double sum = bias[oc];
  for (uint32_t ci = 0; ci < shape.input_channels; ++ci)
    for (uint32_t ky = 0; ky < 3; ++ky)
      for (uint32_t kx = 0; kx < 3; ++kx) {
        const int64_t iy = int64_t(y) + ky - 1;
        const int64_t ix = int64_t(x) + kx - 1;
        if (iy < 0 || ix < 0 ||
            iy >= int64_t(shape.height) || ix >= int64_t(shape.width))
          continue;
        const size_t source =
            ci * shape.height * shape.width + iy * shape.width + ix;
        const size_t weight =
            ((oc * shape.input_channels + ci) * 3 + ky) * 3 + kx;
        sum += double(input[source]) * weights[weight];
      }
  return sum;
}

/* Certify ACE's row-indexed rotation schedule independently against OIHW
 * convolution; the model input tail must be zero before duplication. */
static VHO_FHE_CKKS_CONV_ROW_SCHEDULE Check_Row_Schedule(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const std::vector<float> &weights,
    const std::vector<float> &bias,
    const std::vector<float> &input)
{
  VHO_FHE_CKKS_CONV_ROW_SCHEDULE schedule;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Row_Schedule(
      recipe, &schedule, stderr));
  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe.shape;
  assert(schedule.row_rotations.size() == shape.input_channels * 9);
  assert(schedule.duplication_rotations.size() + 1 ==
         schedule.input_copy_count);
  assert(schedule.row_rotations[0] == -int32_t(shape.width) - 1);
  assert(schedule.row_rotations[4] == 0);
  std::vector<float> packed = input;
  std::fill(packed.begin() + recipe.active_input_slots,
            packed.end(), 0.0f);
  std::vector<double> actual;
  assert(VHO_FHE_CKKS_Evaluate_Column_Conv_Rows_Clear(
      recipe, schedule, packed, &actual, stderr));
  for (uint32_t oc = 0; oc < shape.output_channels; ++oc)
    for (uint32_t y = 0; y < shape.height; ++y)
      for (uint32_t x = 0; x < shape.width; ++x) {
        const size_t index = size_t(oc) * shape.height * shape.width +
                             y * shape.width + x;
        const double expected = Tensor_Oracle(
            shape, oc, y, x, weights, bias, packed);
        assert(fabs(actual[index] - expected) <
               1e-7 * (1.0 + fabs(expected)));
      }
  for (size_t i = recipe.active_output_slots; i < actual.size(); ++i)
    assert(actual[i] == 0.0);
  return schedule;
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
  std::vector<double> center_mask, corner_mask;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Rotation_Mask(
      recipe, 0, &center_mask, stderr));
  assert(VHO_FHE_CKKS_Build_Column_Conv_Rotation_Mask(
      recipe, -33, &corner_mask, stderr));
  assert(center_mask.size() == shape.slot_count &&
         center_mask[0] == weights[4] && center_mask[16384] == 0);
  assert(corner_mask[0] == 0 && corner_mask[33] == weights[0]);
  std::vector<unsigned char> corner_row, center_row;
  assert(VHO_FHE_CKKS_Build_Column_Conv_F32_Row_Bytes(
      recipe, 0, &corner_row, stderr));
  assert(VHO_FHE_CKKS_Build_Column_Conv_F32_Row_Bytes(
      recipe, 4, &center_row, stderr));
  assert(corner_row.size() == 16384 * 4 &&
         center_row.size() == 16384 * 4);
  assert(corner_row[0] == 0x00 && corner_row[1] == 0x00 &&
         corner_row[2] == 0x00 && corner_row[3] == 0x00);
  assert(corner_row[33 * 4] == 0x00 &&
         corner_row[33 * 4 + 1] == 0x00 &&
         corner_row[33 * 4 + 2] == 0x40 &&
         corner_row[33 * 4 + 3] == 0xbe);
  assert(corner_row[(1024 + 33) * 4 + 2] == 0x00 &&
         corner_row[(1024 + 33) * 4 + 3] == 0x3e);
  assert(center_row[0] == 0x00 && center_row[1] == 0x00 &&
         center_row[2] == 0x80 && center_row[3] == 0xbd);

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
  std::vector<double> grouped;
  assert(VHO_FHE_CKKS_Evaluate_Grouped_Column_Conv_Clear(
      recipe, input, &grouped, stderr));
  assert(actual.size() == shape.slot_count);
  for (uint32_t oc = 0; oc < 16; ++oc)
    for (uint32_t y = 0; y < 32; ++y)
      for (uint32_t x = 0; x < 32; ++x) {
        const size_t index = oc * 1024 + y * 32 + x;
        assert(fabs(actual[index] - Tensor_Oracle(
            shape, oc, y, x, weights, bias, input)) < 1e-9);
        assert(fabs(grouped[index] - actual[index]) < 1e-9);
      }
  for (size_t i = 16384; i < actual.size(); ++i)
    assert(actual[i] == 0.0 && grouped[i] == 0.0);
  const VHO_FHE_CKKS_CONV_ROW_SCHEDULE schedule =
      Check_Row_Schedule(recipe, weights, bias, input);
  assert(schedule.input_copy_count == 7 &&
         schedule.duplication_rotations[0] == -3072 &&
         schedule.row_rotations[9] == 991 &&
         schedule.required_signed_rotations.size() == 32);
  printf("synthetic stem: terms=%zu live=%u signed_rotation_keys=%zu "
         "ACE_row_keys=%zu input_copies=%u input_slots=%u "
         "output_slots=%u plaintext_depth=%u\n",
         recipe.terms.size(), recipe.live_term_count,
         recipe.required_signed_rotations.size(),
         schedule.required_signed_rotations.size(),
         schedule.input_copy_count,
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
  std::vector<double> prior_mask(1, 77.0);
  assert(!VHO_FHE_CKKS_Build_Column_Conv_Rotation_Mask(
      recipe, std::numeric_limits<int32_t>::max(), &prior_mask, NULL));
  assert(prior_mask.size() == 1 && prior_mask[0] == 77.0);
  std::vector<unsigned char> prior_f32(1, 77);
  assert(!VHO_FHE_CKKS_Build_Column_Conv_F32_Row_Bytes(
      recipe, shape.input_channels * 9, &prior_f32, NULL));
  assert(prior_f32.size() == 1 && prior_f32[0] == 77);
  VHO_FHE_CKKS_CONV_ROW_SCHEDULE schedule;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Row_Schedule(
      recipe, &schedule, stderr));
  std::vector<double> prior_rows(1, 77.0);
  assert(!VHO_FHE_CKKS_Evaluate_Column_Conv_Rows_Clear(
      recipe, schedule, input, &prior_rows, NULL));
  assert(prior_rows.size() == 1 && prior_rows[0] == 77.0);
  std::fill(input.begin() + recipe.active_input_slots,
            input.end(), 0.0f);
  ++schedule.row_rotations[0];
  assert(!VHO_FHE_CKKS_Evaluate_Column_Conv_Rows_Clear(
      recipe, schedule, input, &prior_rows, NULL));
  assert(prior_rows.size() == 1 && prior_rows[0] == 77.0);
  VHO_FHE_CKKS_CONV_SHAPE crowded = Stem_Shape();
  crowded.input_channels = 1;
  crowded.output_channels = 32;
  std::vector<float> crowded_weights(32 * 9, 1.0f);
  std::vector<float> crowded_bias(32, 0.0f);
  VHO_FHE_CKKS_CONV_RECIPE crowded_recipe;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      crowded, &crowded_weights[0], crowded_weights.size(),
      &crowded_bias[0], crowded_bias.size(), &crowded_recipe, stderr));
  VHO_FHE_CKKS_CONV_ROW_SCHEDULE capped_schedule;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Row_Schedule(
      crowded_recipe, &capped_schedule, stderr));
  assert(capped_schedule.input_copy_count == 32 &&
         capped_schedule.duplication_rotations.size() == 31);
  ++recipe.terms[0].signed_rotation;
  assert(!VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &previous, NULL));
  assert(previous.size() == 1 && previous[0] == 77.0);
}

/* Exercise the same recipe/oracle on authenticated folded bytes from three
 * spatial/channel contexts without putting model data in the repository. */
static void Check_Captured_Folded_Conv(
    const char *fixture_path, const char *label, uint32_t input_channels,
    uint32_t output_channels, uint32_t width)
{
  VHO_FHE_CKKS_CONV_SHAPE shape = Stem_Shape();
  shape.input_channels = input_channels;
  shape.output_channels = output_channels;
  shape.height = width;
  shape.width = width;
  FILE *fixture = fopen(fixture_path, "rb");
  assert(fixture != NULL);
  std::vector<float> weights(size_t(output_channels) *
                             input_channels * 9);
  std::vector<float> bias(output_channels), input(shape.slot_count, 13.0f);
  assert(fread(&weights[0], sizeof(float), weights.size(), fixture) ==
         weights.size());
  assert(fread(&bias[0], sizeof(float), bias.size(), fixture) == bias.size());
  assert(fgetc(fixture) == EOF);
  assert(fclose(fixture) == 0);
  for (size_t i = 0; i < size_t(input_channels) * width * width; ++i)
    input[i] = float(int(i % 31) - 15) / 16.0f;
  VHO_FHE_CKKS_CONV_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Column_Conv_Recipe(
      shape, &weights[0], weights.size(), &bias[0], bias.size(),
      &recipe, stderr));
  std::vector<double> actual;
  assert(VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
      recipe, input, &actual, stderr));
  std::vector<double> grouped;
  assert(VHO_FHE_CKKS_Evaluate_Grouped_Column_Conv_Clear(
      recipe, input, &grouped, stderr));
  for (uint32_t oc = 0; oc < output_channels; ++oc)
    for (uint32_t y = 0; y < width; ++y)
      for (uint32_t x = 0; x < width; ++x)
        {
          const size_t index = oc * width * width + y * width + x;
          const double expected = Tensor_Oracle(
              shape, oc, y, x, weights, bias, input);
          assert(fabs(actual[index] - expected) < 1e-8);
          assert(fabs(grouped[index] - expected) < 1e-8);
        }
  for (size_t i = recipe.active_output_slots; i < actual.size(); ++i)
    assert(actual[i] == 0.0 && grouped[i] == 0.0);
  const VHO_FHE_CKKS_CONV_ROW_SCHEDULE schedule =
      Check_Row_Schedule(recipe, weights, bias, input);
  printf("captured folded %s: terms=%zu live=%u "
         "signed_rotation_keys=%zu ACE_row_keys=%zu input_copies=%u "
         "diagonals=%zu output_slots=%u\n",
         label, recipe.terms.size(), recipe.live_term_count,
         recipe.required_signed_rotations.size(),
         schedule.required_signed_rotations.size(),
         schedule.input_copy_count,
         recipe.required_signed_rotations.size() + 1,
         recipe.active_output_slots);
}

/* Keep this focused fixture below native expansion and mapped-image claims. */
int main(int argc, char **argv)
{
  assert(argc == 1 || argc == 2 || argc == 6);
  Check_Stem_Like_Conv();
  Check_Fail_Closed();
  if (argc == 2)
    Check_Captured_Folded_Conv(argv[1], "stem", 3, 16, 32);
  if (argc == 6)
    Check_Captured_Folded_Conv(argv[1], argv[2],
                               uint32_t(strtoul(argv[3], NULL, 10)),
                               uint32_t(strtoul(argv[4], NULL, 10)),
                               uint32_t(strtoul(argv[5], NULL, 10)));
  puts("FHE CKKS column-first Conv recipe passed (no WHIRL emitted)");
  return 0;
}
