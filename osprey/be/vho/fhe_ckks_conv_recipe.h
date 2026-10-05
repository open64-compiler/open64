/*
 * Copyright (C) 2026 Open64 Project
 *
 * Provider-independent fixed packed-Conv correctness recipe for S6-0c C2.
 * It describes masks and rotations without allocating WN, TY, or mapped rows.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_ckks_conv_recipe_INCLUDED
#define fhe_ckks_conv_recipe_INCLUDED

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <vector>

struct VHO_FHE_CKKS_CONV_SHAPE {
  uint32_t batch;
  uint32_t input_channels;
  uint32_t output_channels;
  uint32_t height;
  uint32_t width;
  uint32_t kernel_height;
  uint32_t kernel_width;
  uint32_t stride_height;
  uint32_t stride_width;
  uint32_t pad_top;
  uint32_t pad_bottom;
  uint32_t pad_left;
  uint32_t pad_right;
  uint32_t dilation_height;
  uint32_t dilation_width;
  uint32_t groups;
  uint32_t slot_count;
};

struct VHO_FHE_CKKS_CONV_TERM {
  uint32_t output_channel;
  uint32_t input_channel;
  uint32_t kernel_y;
  uint32_t kernel_x;
  int32_t signed_rotation;
  float folded_weight;
  uint32_t active_output_slots;
};

struct VHO_FHE_CKKS_CONV_RECIPE {
  VHO_FHE_CKKS_CONV_SHAPE shape;
  std::vector<VHO_FHE_CKKS_CONV_TERM> terms;
  std::vector<float> folded_bias;
  std::vector<int32_t> required_signed_rotations;
  uint32_t active_input_slots;
  uint32_t active_output_slots;
  uint32_t live_term_count;
  uint32_t plaintext_multiply_depth;
};

/* Build the intentionally narrow O0 correctness recipe from already verified
 * OIHW float32 folded weights and float32 bias. Input and output slots use
 * NCHW channel-major order. The outer construction dimension is each output
 * column/slot; kernel feature rows are visited inside it. Every term records
 * the exact signed left rotation and sparse rectangular mask geometry; masks
 * are not materialized in this API. Failure leaves recipe unchanged. */
bool VHO_FHE_CKKS_Build_Column_Conv_Recipe(
    const VHO_FHE_CKKS_CONV_SHAPE &shape,
    const float *folded_weights, size_t weight_count,
    const float *folded_bias, size_t bias_count,
    VHO_FHE_CKKS_CONV_RECIPE *recipe, FILE *diagnostic);

/* Independent clear-slot execution of the fixed recipe. A rotation by r
 * reads input[(output_slot + r) mod slot_count]; the term mask keeps only
 * outputs whose source row/column is valid. This is an oracle, not runtime
 * lowering. Reject malformed slot vectors without changing output. */
bool VHO_FHE_CKKS_Evaluate_Column_Conv_Clear(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const std::vector<float> &input_slots,
    std::vector<double> *output_slots, FILE *diagnostic);

/* Build one plaintext coefficient mask for all live terms sharing an exact
 * signed rotation. This bounds later step planning by distinct rotations,
 * not OIHW term count, and keeps only one full mask in memory at a time.
 * No tensor TCON, key, state, or CKKS node is created here. */
bool VHO_FHE_CKKS_Build_Column_Conv_Rotation_Mask(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe, int32_t signed_rotation,
    std::vector<double> *mask, FILE *diagnostic);

/* Simulate the rotate-then-multiply-by-group-mask accumulation plus bias.
 * Compare this independently with the direct tensor and per-term oracles;
 * it remains clear-slot evidence, not CKKS IR or runtime execution. */
bool VHO_FHE_CKKS_Evaluate_Grouped_Column_Conv_Clear(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    const std::vector<float> &input_slots,
    std::vector<double> *output_slots, FILE *diagnostic);

#endif /* fhe_ckks_conv_recipe_INCLUDED */
