/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned semantic admission for materializing CKKS Conv plaintext assets.
 * Native value creation remains exclusively in common/com. This file checks
 * the ACE-aligned C2 recipe facts that the generic transactions deliberately
 * do not understand.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-GENERATED-MASK-GEOMETRY-MANIFEST.md.
 */

#include "fhe_ckks_conv_assets.h"

#include <string.h>

#include <map>
#include <set>
#include <utility>
#include <vector>

#include "mtypes.h"
#include "symtab.h"
#include "dsl_shape.h"
#include "dsl_tensor_fold.h"

namespace {

/* Emit one concise admission diagnostic without changing native state. */
BOOL
Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHE-CKKS-CONV-ASSET-001: %s\n", message);
  return FALSE;
}

/* Parse a canonical tensor descriptor and require an exact F32 rank. */
BOOL
Tensor_Dimensions(TY_IDX ty, UINT32 expected_rank, UINT64 *dimensions)
{
  UINT32 rank = 0;
  const char *dtype = TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE);
  const char *shape = TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);
  return TY_is_tensor_extension(ty) && TY_tensor_is_canonical(ty) &&
         TY_tensor_element_ty(ty) == MTYPE_To_TY(MTYPE_F4) &&
         dtype != NULL && strcmp(dtype, "float32") == 0 && shape != NULL &&
         DSL_Shape_Parse_Static_Dimensions(
             shape, dimensions, expected_rank, &rank) &&
         rank == expected_rank;
}

/* Accept only canonical lowercase SHA-256 text at the FHE boundary. */
BOOL
Canonical_SHA256(const char *text)
{
  if (text == NULL || strlen(text) != 64)
    return FALSE;
  for (UINT32 i = 0; i < 64; ++i) {
    const char c = text[i];
    if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')))
      return FALSE;
  }
  return TRUE;
}

}  // namespace

/* Validate recipe-specific source/result identities before delegating the
 * all-or-nothing batch to common/com. */
BOOL
VHO_FHE_CKKS_Materialize_Conv_Rows(
    PU_Info *pu_info, const VHO_FHE_CKKS_CONV_RECIPE *recipe,
    const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *requests,
    UINT32 request_count,
    DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *results,
    FILE *diagnostic)
{
  if (results != NULL && request_count != 0)
    memset(results, 0, request_count * sizeof(*results));
  if (pu_info == NULL || recipe == NULL || requests == NULL ||
      results == NULL || !VHO_FHE_CKKS_Validate_Column_Conv_Recipe(
                             *recipe, diagnostic))
    return Report(diagnostic, "row materialization input is incomplete");

  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe->shape;
  const UINT64 expected_rows = UINT64(shape.input_channels) *
                               shape.kernel_height * shape.kernel_width;
  const UINT64 expected_length = recipe->active_output_slots;
  if (expected_rows > UINT_MAX || request_count != expected_rows)
    return Report(diagnostic, "feature-row count does not match Conv shape");

  const ST_IDX source_owner = requests[0].source_owner_pu_st;
  const DSL_IR_VALUE_ID source_value = requests[0].source_value_id;
  const DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE source_handle =
      requests[0].source_handle;
  DSL_IR_VALUE_RECORD source;
  UINT64 source_shape[4] = { 0, 0, 0, 0 };
  if (source_owner == ST_IDX_ZERO ||
      source_value == DSL_IR_VALUE_INVALID_ID || source_handle == 0 ||
      !DSL_IR_Image_Get_Value(source_value, &source) ||
      !Tensor_Dimensions(source.ty, 4, source_shape) ||
      source_shape[0] != shape.output_channels ||
      source_shape[1] != shape.input_channels ||
      source_shape[2] != shape.kernel_height ||
      source_shape[3] != shape.kernel_width)
    return Report(diagnostic, "source is not the exact folded OIHW tensor");

  for (UINT32 i = 0; i < request_count; ++i) {
    const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST &request = requests[i];
    UINT64 result_shape[1] = { 0 };
    if (request.source_owner_pu_st != source_owner ||
        request.source_value_id != source_value ||
        request.source_handle != source_handle ||
        request.transformation_name == NULL ||
        strcmp(request.transformation_name, "fhe.conv_feature_row") != 0 ||
        request.transformation_version != 1 ||
        request.transformation_ordinal != i ||
        !Tensor_Dimensions(request.descriptor_ty, 1, result_shape) ||
        result_shape[0] != expected_length ||
        request.byte_length != expected_length * sizeof(float))
      return Report(diagnostic,
                    "feature-row type, order, or provenance is invalid");
  }

  if (!DSL_IR_Materialize_Typed_External_Tensor_Values(
          pu_info, requests, request_count, results))
    return Report(diagnostic, "typed-row transaction rejected the batch");
  return TRUE;
}

/* Store one F32 value in the canonical little-endian side-asset encoding. */
static void
Store_F32(std::vector<unsigned char> *bytes, UINT32 index, float value)
{
  const size_t offset = size_t(index) * sizeof(float);
  UINT32 bits = 0;
  memcpy(&bits, &value, sizeof(bits));
  (*bytes)[offset] = static_cast<unsigned char>(bits & 0xff);
  (*bytes)[offset + 1] = static_cast<unsigned char>((bits >> 8) & 0xff);
  (*bytes)[offset + 2] = static_cast<unsigned char>((bits >> 16) & 0xff);
  (*bytes)[offset + 3] = static_cast<unsigned char>((bits >> 24) & 0xff);
}

/* Serialize folded bias by repeating each channel value over its plane. */
BOOL
VHO_FHE_CKKS_Build_Conv_Expanded_Bias_F32(
    const VHO_FHE_CKKS_CONV_RECIPE &recipe,
    std::vector<unsigned char> *bytes, FILE *diagnostic)
{
  if (bytes == NULL ||
      !VHO_FHE_CKKS_Validate_Column_Conv_Recipe(recipe, diagnostic) ||
      recipe.folded_bias.size() != recipe.shape.output_channels)
    return Report(diagnostic, "expanded-bias recipe is invalid");
  const UINT32 plane = recipe.shape.height * recipe.shape.width;
  std::vector<unsigned char> built(
      size_t(recipe.active_output_slots) * sizeof(float), 0);
  for (UINT32 channel = 0; channel < recipe.shape.output_channels; ++channel) {
    const float value = recipe.folded_bias[channel];
    for (UINT32 spatial = 0; spatial < plane; ++spatial)
      Store_F32(&built, channel * plane + spatial, value);
  }
  bytes->swap(built);
  return TRUE;
}

/* Construct the exact sequential bit-deletion masks used by the O0 plan. */
BOOL
VHO_FHE_CKKS_Build_Stride_Compaction_F32_Masks(
    UINT32 width, UINT32 channels, UINT32 slot_count,
    std::vector<std::vector<unsigned char> > *masks, FILE *diagnostic)
{
  if (masks == NULL || width < 4 || channels == 0 || slot_count == 0 ||
      (width & (width - 1)) != 0 || (channels & (channels - 1)) != 0 ||
      UINT64(channels) * width * width > slot_count)
    return Report(diagnostic, "stride-compaction geometry is invalid");
  UINT32 spatial_bits = 0;
  UINT32 channel_bits = 0;
  for (UINT32 value = width; value > 1; value >>= 1)
    ++spatial_bits;
  for (UINT32 value = channels; value > 1; value >>= 1)
    ++channel_bits;
  std::vector<std::pair<UINT32, UINT32> > moves;
  for (UINT32 bit = 0; bit + 1 < spatial_bits; ++bit)
    moves.push_back(std::make_pair(1 + bit, bit));
  for (UINT32 bit = 0; bit + 1 < spatial_bits; ++bit)
    moves.push_back(std::make_pair(
        spatial_bits + 1 + bit, spatial_bits - 1 + bit));
  for (UINT32 bit = 0; bit < channel_bits; ++bit)
    moves.push_back(std::make_pair(
        2 * spatial_bits + bit, 2 * spatial_bits - 2 + bit));

  std::map<UINT32, UINT32> mapping;
  const UINT32 half = width / 2;
  for (UINT32 channel = 0; channel < channels; ++channel)
    for (UINT32 y = 0; y < half; ++y)
      for (UINT32 x = 0; x < half; ++x) {
        const UINT32 index = channel * width * width +
                             2 * y * width + 2 * x;
        mapping[index] = index;
      }

  std::vector<std::vector<unsigned char> > built;
  built.push_back(std::vector<unsigned char>(
      size_t(slot_count) * sizeof(float), 0));
  for (std::map<UINT32, UINT32>::const_iterator item = mapping.begin();
       item != mapping.end(); ++item)
    Store_F32(&built.back(), item->second, 1.0f);

  for (size_t move = 0; move < moves.size(); ++move) {
    const UINT32 source_bit = moves[move].first;
    const UINT32 target_bit = moves[move].second;
    const UINT32 rotation = (1U << source_bit) - (1U << target_bit);
    std::set<UINT32> selected;
    for (std::map<UINT32, UINT32>::const_iterator item = mapping.begin();
         item != mapping.end(); ++item)
      if ((item->first & (1U << source_bit)) != 0)
        selected.insert(item->second);
    std::vector<unsigned char> selected_mask(
        size_t(slot_count) * sizeof(float), 0);
    std::vector<unsigned char> complement_mask(
        size_t(slot_count) * sizeof(float), 0);
    for (UINT32 slot = 0; slot < slot_count; ++slot) {
      if (selected.find(slot) != selected.end())
        Store_F32(&selected_mask, slot, 1.0f);
      else
        Store_F32(&complement_mask, slot, 1.0f);
    }
    built.push_back(selected_mask);
    built.push_back(complement_mask);
    for (std::map<UINT32, UINT32>::iterator item = mapping.begin();
         item != mapping.end(); ++item)
      if (selected.find(item->second) != selected.end())
        item->second -= rotation;
  }
  std::set<UINT32> dense;
  for (std::map<UINT32, UINT32>::const_iterator item = mapping.begin();
       item != mapping.end(); ++item)
    dense.insert(item->second);
  UINT32 expected = 0;
  for (std::set<UINT32>::const_iterator item = dense.begin();
       item != dense.end(); ++item, ++expected)
    if (*item != expected)
      return Report(diagnostic,
                    "stride-compaction masks do not produce dense slots");
  if (dense.size() != size_t(channels) * half * half)
    return Report(diagnostic, "stride-compaction dense extent is invalid");
  masks->swap(built);
  return TRUE;
}

/* Validate the folded-bias source and exact ACE slot expansion before the
 * generic one-value typed-external transaction mutates the active PU. */
BOOL
VHO_FHE_CKKS_Materialize_Conv_Bias(
    PU_Info *pu_info, const VHO_FHE_CKKS_CONV_RECIPE *recipe,
    const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *request,
    DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *result,
    FILE *diagnostic)
{
  if (result != NULL)
    memset(result, 0, sizeof(*result));
  if (pu_info == NULL || recipe == NULL || request == NULL || result == NULL ||
      !VHO_FHE_CKKS_Validate_Column_Conv_Recipe(*recipe, diagnostic))
    return Report(diagnostic, "bias materialization input is incomplete");

  const VHO_FHE_CKKS_CONV_SHAPE &shape = recipe->shape;
  DSL_IR_VALUE_RECORD source;
  UINT64 source_shape[1] = { 0 };
  UINT64 result_shape[1] = { 0 };
  const UINT64 expected_length = recipe->active_output_slots;
  if (request->source_owner_pu_st == ST_IDX_ZERO ||
      request->source_value_id == DSL_IR_VALUE_INVALID_ID ||
      request->source_handle == 0 ||
      !DSL_IR_Image_Get_Value(request->source_value_id, &source) ||
      !Tensor_Dimensions(source.ty, 1, source_shape) ||
      source_shape[0] != shape.output_channels ||
      request->transformation_name == NULL ||
      strcmp(request->transformation_name, "fhe.conv_expanded_bias") != 0 ||
      request->transformation_version != 1 ||
      request->transformation_ordinal != 0 ||
      !Tensor_Dimensions(request->descriptor_ty, 1, result_shape) ||
      result_shape[0] != expected_length ||
      request->byte_length != expected_length * sizeof(float))
    return Report(diagnostic,
                  "expanded-bias type, shape, or provenance is invalid");

  if (!DSL_IR_Materialize_Typed_External_Tensor_Values(
          pu_info, request, 1, result))
    return Report(diagnostic, "typed-bias transaction rejected the value");
  return TRUE;
}

/* Validate FHE geometry/variant binding before delegating source-free values
 * to the generic generated-external transaction. */
BOOL
VHO_FHE_CKKS_Materialize_Conv_Masks(
    PU_Info *pu_info,
    const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST *requests,
    UINT32 request_count, const char *expected_geometry_sha256,
    const char *expected_variant_sha256,
    DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT *results,
    FILE *diagnostic)
{
  if (results != NULL && request_count != 0)
    memset(results, 0, request_count * sizeof(*results));
  if (pu_info == NULL || requests == NULL || request_count == 0 ||
      results == NULL || !Canonical_SHA256(expected_geometry_sha256) ||
      !Canonical_SHA256(expected_variant_sha256))
    return Report(diagnostic, "mask materialization input is incomplete");

  for (UINT32 i = 0; i < request_count; ++i) {
    const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &request = requests[i];
    UINT64 dimensions[1] = { 0 };
    if (!Tensor_Dimensions(request.descriptor_ty, 1, dimensions) ||
        dimensions[0] == 0 ||
        request.byte_length != dimensions[0] * sizeof(float) ||
        request.generation_name == NULL ||
        strcmp(request.generation_name,
               "fhe.ckks.stride_compaction.mask") != 0 ||
        request.generation_version != 1 ||
        request.geometry_manifest_sha256 == NULL ||
        strcmp(request.geometry_manifest_sha256,
               expected_geometry_sha256) != 0 ||
        request.variant_signature_sha256 == NULL ||
        strcmp(request.variant_signature_sha256,
               expected_variant_sha256) != 0)
      return Report(diagnostic,
                    "mask type, generator, or authenticated identity is invalid");
  }

  if (pu_info != Current_PU_Info)
    return Report(diagnostic, "mask owner is not the active PU");
  if (!DSL_IR_Generated_External_Tensor_Validate_PU(pu_info, diagnostic))
    return Report(diagnostic,
                  "existing generated external values are invalid");

  if (!DSL_IR_Materialize_Generated_External_Tensor_Values(
          pu_info, requests, request_count, results)) {
    if (diagnostic != NULL) {
      for (UINT32 i = 0; i < request_count; ++i) {
        DSL_TENSOR_TCON_RECORD tcon;
        const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &request = requests[i];
        if (!DSL_Tensor_TCON_Get(request.tensor_tcon, &tcon)) {
          fprintf(diagnostic,
                  "CFHE-CKKS-CONV-ASSET-001: mask[%u] has invalid "
                  "tensor_tcon=%u\n", i, (UINT32)request.tensor_tcon);
          continue;
        }
        if (tcon.descriptor_ty != request.descriptor_ty ||
            tcon.byte_offset != request.byte_offset ||
            tcon.byte_length != request.byte_length) {
          fprintf(diagnostic,
                  "CFHE-CKKS-CONV-ASSET-001: mask[%u] TCON mismatch "
                  "ty=%u/%u offset=%llu/%llu length=%llu/%llu\n",
                  i, tcon.descriptor_ty, request.descriptor_ty,
                  (unsigned long long)tcon.byte_offset,
                  (unsigned long long)request.byte_offset,
                  (unsigned long long)tcon.byte_length,
                  (unsigned long long)request.byte_length);
        }
      }
    }
    return Report(diagnostic,
                  "generated-external transaction rejected the batch");
  }
  return TRUE;
}
