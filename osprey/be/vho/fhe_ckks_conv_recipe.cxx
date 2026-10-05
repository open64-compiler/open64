/*
 * Copyright (C) 2026 Open64 Project
 *
 * Bounded column-first packed-Conv recipe and clear slot oracle for C2.
 * No graph-wide layout selection, CKKS state repair, or WHIRL mutation.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_ckks_conv_recipe.h"

#include <algorithm>
#include <cfloat>
#include <limits>
#include <set>

namespace {

/* Report the first unsupported fixed-recipe condition without a partial
 * output. A later planner may handle it only after separate review. */
bool Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-CONV-001: %s\n", message);
  return false;
}

/* Reject NaN and infinities without requiring a newer backend C++ dialect. */
bool Finite(float value)
{
  return value == value && value <= FLT_MAX && value >= -FLT_MAX;
}

/* Keep the first recipe deliberately smaller than graph-wide layout work:
 * batch one, square spatial plane, 3x3 same Conv, groups one, and one CKKS
 * ciphertext for the complete input and output. This is not a model claim. */
bool Supported(const VHO_FHE_CKKS_CONV_SHAPE &shape)
{
  if (shape.batch != 1 || shape.input_channels == 0 ||
      shape.output_channels == 0 || shape.height < 3 ||
      shape.height != shape.width || shape.kernel_height != 3 ||
      shape.kernel_width != 3 || shape.stride_height != 1 ||
      shape.stride_width != 1 || shape.pad_top != 1 ||
      shape.pad_bottom != 1 || shape.pad_left != 1 ||
      shape.pad_right != 1 || shape.dilation_height != 1 ||
      shape.dilation_width != 1 || shape.groups != 1 ||
      shape.slot_count == 0 ||
      (shape.slot_count & (shape.slot_count - 1)) != 0 ||
      shape.slot_count > 32768)
    return false;
  const uint64_t plane = uint64_t(shape.height) * shape.width;
  if (plane > shape.slot_count ||
      shape.input_channels > shape.slot_count / plane ||
      shape.output_channels > shape.slot_count / plane)
    return false;
  const uint64_t input = plane * shape.input_channels;
  const uint64_t output = plane * shape.output_channels;
  const uint64_t terms = uint64_t(shape.input_channels) *
                         shape.output_channels * 9;
  return input <= shape.slot_count && output <= shape.slot_count &&
         terms <= 65536 &&
         input <= std::numeric_limits<uint32_t>::max() &&
         output <= std::numeric_limits<uint32_t>::max();
}

/* OIHW folded-weight row identity stays stable across slot traversal. */
size_t Term_Index(const VHO_FHE_CKKS_CONV_SHAPE &shape,
                  uint32_t oc, uint32_t ci, uint32_t ky, uint32_t kx)
{
  return (((size_t(oc) * shape.input_channels + ci) * 3 + ky) * 3 + kx);
}

/* A rectangular mask excludes spatial wrap; the input channel is selected
 * by the term's rotation, so invalid row/column slots never contribute. */
bool Valid_Output_Position(uint32_t y, uint32_t x,
                           uint32_t ky, uint32_t kx,
                           uint32_t height, uint32_t width)
{
  const int64_t input_y = int64_t(y) + ky - 1;
  const int64_t input_x = int64_t(x) + kx - 1;
  return input_y >= 0 && input_y < height &&
         input_x >= 0 && input_x < width;
}

/* A recipe is a process-local plan, but reject stale or modified terms before
 * its clear execution is used as evidence for lowering. */
bool Valid_Recipe(const VHO_FHE_CKKS_CONV_RECIPE &recipe)
{
  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe.shape;
  if (!Supported(shape) ||
      recipe.active_input_slots !=
          shape.input_channels * shape.height * shape.width ||
      recipe.active_output_slots !=
          shape.output_channels * shape.height * shape.width ||
      recipe.terms.size() != size_t(shape.output_channels) *
                             shape.input_channels * 9 ||
      recipe.folded_bias.size() != shape.output_channels ||
      recipe.plaintext_multiply_depth != 1)
    return false;
  for (size_t i = 0; i < recipe.folded_bias.size(); ++i)
    if (!Finite(recipe.folded_bias[i]))
      return false;
  std::set<int32_t> rotations;
  uint32_t live = 0;
  const int64_t plane = int64_t(shape.height) * shape.width;
  for (uint32_t oc = 0; oc < shape.output_channels; ++oc)
    for (uint32_t ci = 0; ci < shape.input_channels; ++ci)
      for (uint32_t ky = 0; ky < 3; ++ky)
        for (uint32_t kx = 0; kx < 3; ++kx) {
          const VHO_FHE_CKKS_CONV_TERM &term =
              recipe.terms[Term_Index(shape, oc, ci, ky, kx)];
          const uint32_t valid_rows = shape.height - (ky != 1);
          const uint32_t valid_columns = shape.width - (kx != 1);
          const int32_t rotation = static_cast<int32_t>(
              (int64_t(ci) - oc) * plane +
              (int64_t(ky) - 1) * shape.width + (int64_t(kx) - 1));
          if (term.output_channel != oc || term.input_channel != ci ||
              term.kernel_y != ky || term.kernel_x != kx ||
              term.signed_rotation != rotation ||
              term.active_output_slots != valid_rows * valid_columns ||
              !Finite(term.folded_weight))
            return false;
          if (term.folded_weight != 0) {
            ++live;
            if (rotation != 0)
              rotations.insert(rotation);
          }
        }
  return live == recipe.live_term_count &&
         recipe.required_signed_rotations.size() == rotations.size() &&
         std::equal(recipe.required_signed_rotations.begin(),
                    recipe.required_signed_rotations.end(), rotations.begin());
}

/* Fill one plaintext diagonal in output-slot order. Distinct terms with
 * the same rotation may share this mask only when their output slots do
 * not collide; a collision would require a separately reviewed sum rule. */
bool Build_Rotation_Mask(const VHO_FHE_CKKS_CONV_RECIPE &recipe,
                         int32_t rotation, std::vector<double> *mask)
{
  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe.shape;
  std::vector<double> built(shape.slot_count, 0.0);
  bool found = false;
  const uint32_t plane = shape.height * shape.width;
  for (size_t i = 0; i < recipe.terms.size(); ++i) {
    const VHO_FHE_CKKS_CONV_TERM &term = recipe.terms[i];
    if (term.signed_rotation != rotation || term.folded_weight == 0)
      continue;
    found = true;
    for (uint32_t y = 0; y < shape.height; ++y)
      for (uint32_t x = 0; x < shape.width; ++x) {
        if (!Valid_Output_Position(y, x, term.kernel_y, term.kernel_x,
                                   shape.height, shape.width))
          continue;
        const size_t column = size_t(term.output_channel) * plane +
                              y * shape.width + x;
        if (built[column] != 0)
          return false;
        built[column] = term.folded_weight;
      }
  }
  if (!found)
    return false;
  mask->swap(built);
  return true;
}

}  // namespace

/* Preflight the bounded shape and finite folded bytes, then construct a
 * column-first mask/rotation schedule into local storage before publish. */
bool VHO_FHE_CKKS_Build_Column_Conv_Recipe(
    const VHO_FHE_CKKS_CONV_SHAPE &shape,
    const float *folded_weights, size_t weight_count,
    const float *folded_bias, size_t bias_count,
    VHO_FHE_CKKS_CONV_RECIPE *recipe, FILE *diagnostic)
{
  if (recipe == NULL || !Supported(shape))
    return Report(diagnostic, "shape or packing exceeds fixed O0 domain");
  const size_t count = size_t(shape.output_channels) *
                       shape.input_channels * 9;
  if (folded_weights == NULL || folded_bias == NULL ||
      weight_count != count || bias_count != shape.output_channels)
    return Report(diagnostic, "folded weight/bias shape disagrees");
  for (size_t i = 0; i < weight_count; ++i)
    if (!Finite(folded_weights[i]))
      return Report(diagnostic, "folded weight is not finite");
  for (size_t i = 0; i < bias_count; ++i)
    if (!Finite(folded_bias[i]))
      return Report(diagnostic, "folded bias is not finite");

  VHO_FHE_CKKS_CONV_RECIPE built;
  built.shape = shape;
  built.active_input_slots = shape.input_channels * shape.height * shape.width;
  built.active_output_slots = shape.output_channels * shape.height * shape.width;
  built.live_term_count = 0;
  built.plaintext_multiply_depth = 1;
  built.folded_bias.assign(folded_bias, folded_bias + bias_count);
  built.terms.resize(count);
  const int64_t plane = int64_t(shape.height) * shape.width;
  for (uint32_t oc = 0; oc < shape.output_channels; ++oc)
    for (uint32_t ci = 0; ci < shape.input_channels; ++ci)
      for (uint32_t ky = 0; ky < 3; ++ky)
        for (uint32_t kx = 0; kx < 3; ++kx) {
          const size_t index = Term_Index(shape, oc, ci, ky, kx);
          VHO_FHE_CKKS_CONV_TERM &term = built.terms[index];
          term.output_channel = oc;
          term.input_channel = ci;
          term.kernel_y = ky;
          term.kernel_x = kx;
          term.signed_rotation = static_cast<int32_t>(
              (int64_t(ci) - oc) * plane +
              (int64_t(ky) - 1) * shape.width + (int64_t(kx) - 1));
          term.folded_weight = folded_weights[index];
          term.active_output_slots = 0;
        }

  /* Enumerate each output column first, then its kernel feature rows. This
   * constructs the exact spatial validity census without dense im2col data. */
  for (uint32_t column = 0; column < built.active_output_slots; ++column) {
    const uint32_t oc = column / (shape.height * shape.width);
    const uint32_t y = (column / shape.width) % shape.height;
    const uint32_t x = column % shape.width;
    for (uint32_t ci = 0; ci < shape.input_channels; ++ci)
      for (uint32_t ky = 0; ky < 3; ++ky)
        for (uint32_t kx = 0; kx < 3; ++kx)
          if (Valid_Output_Position(y, x, ky, kx,
                                    shape.height, shape.width))
            ++built.terms[Term_Index(shape, oc, ci, ky, kx)]
                  .active_output_slots;
  }

  std::set<int32_t> rotations;
  for (size_t i = 0; i < built.terms.size(); ++i) {
    const VHO_FHE_CKKS_CONV_TERM &term = built.terms[i];
    if (term.active_output_slots != 0 && term.folded_weight != 0) {
      ++built.live_term_count;
      if (term.signed_rotation != 0)
        rotations.insert(term.signed_rotation);
    }
  }
  built.required_signed_rotations.assign(rotations.begin(), rotations.end());
  recipe->shape = built.shape;
  recipe->terms.swap(built.terms);
  recipe->folded_bias.swap(built.folded_bias);
  recipe->required_signed_rotations.swap(built.required_signed_rotations);
  recipe->active_input_slots = built.active_input_slots;
  recipe->active_output_slots = built.active_output_slots;
  recipe->live_term_count = built.live_term_count;
  recipe->plaintext_multiply_depth = built.plaintext_multiply_depth;
  return true;
}

/* Simulate explicit rotate-mask-multiply/add behavior using a distinct slot
 * coordinate calculation from the tensor oracle in the focused test. */
bool VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const std::vector<float> &input_slots,
    std::vector<double> *output_slots, FILE *diagnostic)
{
  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe.shape;
  if (output_slots == NULL || !Valid_Recipe(recipe) ||
      input_slots.size() != shape.slot_count)
    return Report(diagnostic, "clear-slot recipe or input is incomplete");
  for (size_t i = 0; i < input_slots.size(); ++i)
    if (!Finite(input_slots[i]))
      return Report(diagnostic, "clear-slot input is not finite");

  std::vector<double> result(shape.slot_count, 0.0);
  const uint32_t plane = shape.height * shape.width;
  for (uint32_t column = 0; column < recipe.active_output_slots; ++column) {
    const uint32_t oc = column / plane;
    const uint32_t y = (column / shape.width) % shape.height;
    const uint32_t x = column % shape.width;
    double sum = recipe.folded_bias[oc];
    for (uint32_t ci = 0; ci < shape.input_channels; ++ci)
      for (uint32_t ky = 0; ky < 3; ++ky)
        for (uint32_t kx = 0; kx < 3; ++kx) {
          const VHO_FHE_CKKS_CONV_TERM &term =
              recipe.terms[Term_Index(shape, oc, ci, ky, kx)];
          if (term.folded_weight == 0 ||
              !Valid_Output_Position(y, x, ky, kx,
                                     shape.height, shape.width))
            continue;
          const int64_t rotated =
              (int64_t(column) + term.signed_rotation +
               shape.slot_count) % shape.slot_count;
          sum += double(input_slots[rotated]) * term.folded_weight;
        }
    result[column] = sum;
  }
  output_slots->swap(result);
  return true;
}

/* Public mask construction validates the complete recipe before exposing a
 * plaintext diagonal. Failure never replaces the caller's prior mask. */
bool VHO_FHE_CKKS_Build_Column_Conv_Rotation_Mask(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe, int32_t signed_rotation,
    std::vector<double> *mask, FILE *diagnostic)
{
  if (mask == NULL || !Valid_Recipe(recipe) ||
      !Build_Rotation_Mask(recipe, signed_rotation, mask))
    return Report(diagnostic, "rotation mask is absent or ambiguous");
  return true;
}

/* Keep grouped masks process-local and prove each diagonal contributes
 * exactly the same clear tensor value as the source OIHW Conv. */
bool VHO_FHE_CKKS_Evaluate_Grouped_Column_Conv_Clear(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const std::vector<float> &input_slots,
    std::vector<double> *output_slots, FILE *diagnostic)
{
  if (output_slots == NULL || !Valid_Recipe(recipe) ||
      input_slots.size() != recipe.shape.slot_count)
    return Report(diagnostic, "grouped clear-slot recipe is incomplete");
  for (size_t i = 0; i < input_slots.size(); ++i)
    if (!Finite(input_slots[i]))
      return Report(diagnostic, "grouped clear-slot input is not finite");

  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe.shape;
  const uint32_t plane = shape.height * shape.width;
  std::vector<double> result(shape.slot_count, 0.0);
  for (uint32_t oc = 0; oc < shape.output_channels; ++oc)
    for (uint32_t position = 0; position < plane; ++position)
      result[size_t(oc) * plane + position] = recipe.folded_bias[oc];

  std::set<int32_t> rotations(recipe.required_signed_rotations.begin(),
                              recipe.required_signed_rotations.end());
  for (size_t i = 0; i < recipe.terms.size(); ++i)
    if (recipe.terms[i].signed_rotation == 0 &&
        recipe.terms[i].folded_weight != 0)
      rotations.insert(0);
  for (std::set<int32_t>::const_iterator it = rotations.begin();
       it != rotations.end(); ++it) {
    std::vector<double> mask;
    if (!Build_Rotation_Mask(recipe, *it, &mask))
      return Report(diagnostic, "rotation mask collides");
    for (uint32_t column = 0; column < recipe.active_output_slots; ++column) {
      if (mask[column] == 0)
        continue;
      const int64_t source =
          (int64_t(column) + *it + shape.slot_count) % shape.slot_count;
      result[column] += double(input_slots[source]) * mask[column];
    }
  }
  output_slots->swap(result);
  return true;
}
