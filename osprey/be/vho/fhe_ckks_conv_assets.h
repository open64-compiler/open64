/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned semantic admission for materializing CKKS Conv plaintext assets.
 * The implementation validates the selected ACE-aligned row/mask contracts
 * and delegates native mutation to common/com transactions. It never creates
 * WN, ST, TY, TCON, or mapped-image records directly.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CONV-MASK-ASSET-OPTIONS.md.
 */

#ifndef fhe_ckks_conv_assets_INCLUDED
#define fhe_ckks_conv_assets_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "pu_info.h"
#include "dsl_ir_image.h"
#include "fhe_ckks_conv_recipe.h"

/* Validate one complete ACE feature-row batch against its source OIHW tensor
 * and bounded Conv recipe, then atomically create the external row values by
 * calling the reviewed common/com typed-row transaction. */
BOOL VHO_FHE_CKKS_Materialize_Conv_Rows(
    PU_Info *pu_info, const VHO_FHE_CKKS_CONV_RECIPE *recipe,
    const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *requests,
    UINT32 request_count,
    DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *results,
    FILE *diagnostic);

/* Validate one folded Conv bias and its slot-expanded rank-1 result, then
 * atomically create the external value through the reviewed typed-external
 * transaction. */
BOOL VHO_FHE_CKKS_Materialize_Conv_Bias(
    PU_Info *pu_info, const VHO_FHE_CKKS_CONV_RECIPE *recipe,
    const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *request,
    DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *result,
    FILE *diagnostic);

/* Validate one complete source-free generated-mask batch against the exact
 * authenticated geometry and variant digests, then atomically create values
 * through the reviewed common/com generated-external transaction. */
BOOL VHO_FHE_CKKS_Materialize_Conv_Masks(
    PU_Info *pu_info,
    const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST *requests,
    UINT32 request_count, const char *expected_geometry_sha256,
    const char *expected_variant_sha256,
    DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT *results,
    FILE *diagnostic);

#endif /* fhe_ckks_conv_assets_INCLUDED */
