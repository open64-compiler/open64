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

#include "mtypes.h"
#include "symtab.h"
#include "dsl_shape.h"

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

  if (!DSL_IR_Materialize_Generated_External_Tensor_Values(
          pu_info, requests, request_count, results))
    return Report(diagnostic,
                  "generated-external transaction rejected the batch");
  return TRUE;
}
