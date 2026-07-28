/*
 * Copyright (C) 2026 Open64 Project
 */

#include <stdio.h>
#include <string.h>

#include "opt_dsl.h"

int
main(void)
{
  WOPT_DSL_SEMANTIC_INFO add;
  WOPT_DSL_SEMANTIC_INFO other;
  WOPT_DSL_SEMANTIC_INFO fetched;

  WOPT_DSL_Semantic_Info_Reset();
  memset(&add, 0, sizeof(add));
  add.logical_operator = OPR_DSLADD;
  add.version = 1;
  add.flags = WOPT_DSL_SEMANTIC_PURE |
              WOPT_DSL_SEMANTIC_PROJECTABLE;
  add.result_ty = 17;
  add.canonical_attribute_hash = 0x1234;
  add.operand_descriptor_hash = 0x5678;

  WOPT_DSL_SEMANTIC_INFO_ID add_id =
      WOPT_DSL_Semantic_Info_Intern(&add);
  if (add_id == WOPT_DSL_SEMANTIC_INFO_INVALID_ID ||
      WOPT_DSL_Semantic_Info_Intern(&add) != add_id ||
      WOPT_DSL_Semantic_Info_Count() != 1 ||
      WOPT_DSL_Semantic_Info_Hash(add_id) == 0 ||
      !WOPT_DSL_Semantic_Info_Get(add_id, &fetched) ||
      fetched.logical_operator != OPR_DSLADD ||
      fetched.result_ty != add.result_ty) {
    fprintf(stderr, "WOPT DSL semantic interning contract failed\n");
    return 1;
  }

  other = add;
  other.logical_operator = OPR_DSLMUL;
  WOPT_DSL_SEMANTIC_INFO_ID other_id =
      WOPT_DSL_Semantic_Info_Intern(&other);
  if (other_id == add_id || WOPT_DSL_Semantic_Info_Count() != 2 ||
      WOPT_DSL_Semantic_Info_Hash(other_id) ==
          WOPT_DSL_Semantic_Info_Hash(add_id)) {
    fprintf(stderr, "logical DSL operators collapsed in WOPT identity\n");
    return 1;
  }

  other = add;
  ++other.version;
  if (WOPT_DSL_Semantic_Info_Intern(&other) == add_id) {
    fprintf(stderr, "DSL operator versions collapsed in WOPT identity\n");
    return 1;
  }

  other = add;
  ++other.canonical_attribute_hash;
  if (WOPT_DSL_Semantic_Info_Intern(&other) == add_id) {
    fprintf(stderr, "DSL opcode attributes collapsed in WOPT identity\n");
    return 1;
  }

  other = add;
  ++other.result_ty;
  if (WOPT_DSL_Semantic_Info_Intern(&other) == add_id) {
    fprintf(stderr, "tensor descriptor identities collapsed in WOPT identity\n");
    return 1;
  }

  other = add;
  other.tensor_tcon_idx = 41;
  if (WOPT_DSL_Semantic_Info_Intern(&other) == add_id) {
    fprintf(stderr, "tensor constant identities collapsed in WOPT identity\n");
    return 1;
  }

  other = add;
  other.origin_node_id = 73;
  other.origin_result_value_id = 91;
  if (WOPT_DSL_Semantic_Info_Intern(&other) != add_id) {
    fprintf(stderr, "DSL reconstruction provenance inhibited value numbering\n");
    return 1;
  }

  if (!WOPT_DSL_Algebraic_Safety_Allows
          (DSL_ALGEBRAIC_SAFETY_INTEGER, FALSE, FALSE) ||
      WOPT_DSL_Algebraic_Safety_Allows
          (DSL_ALGEBRAIC_SAFETY_INTEGER, TRUE, TRUE) ||
      WOPT_DSL_Algebraic_Safety_Allows
          (DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
           TRUE, FALSE) ||
      !WOPT_DSL_Algebraic_Safety_Allows
          (DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
           TRUE, TRUE)) {
    fprintf(stderr, "DSL strict-FP algebraic safety contract failed\n");
    return 1;
  }

  WOPT_DSL_DIVREM_TARGET_POLICY policy;
  WOPT_DSL_Reset_DIVREM_Target_Policy();
  if (!WOPT_DSL_Get_DIVREM_Target_Policy(&policy) ||
      policy.lowering_capability || policy.profitable ||
      WOPT_DSL_DIVREM_Combination_Enabled(TRUE)) {
    fprintf(stderr, "DSL DIVREM target policy did not default off\n");
    return 1;
  }
  policy.lowering_capability = TRUE;
  policy.profitable = FALSE;
  if (!WOPT_DSL_Set_DIVREM_Target_Policy(&policy) ||
      WOPT_DSL_DIVREM_Combination_Enabled(TRUE)) {
    fprintf(stderr, "DSL DIVREM ignored profitability gate\n");
    return 1;
  }
  policy.profitable = TRUE;
  if (!WOPT_DSL_Set_DIVREM_Target_Policy(&policy) ||
      WOPT_DSL_DIVREM_Combination_Enabled(FALSE) ||
      !WOPT_DSL_DIVREM_Combination_Enabled(TRUE)) {
    fprintf(stderr, "DSL DIVREM option/capability gates changed\n");
    return 1;
  }

  WOPT_DSL_SEMANTIC_INFO standalone = add;
  WOPT_DSL_SEMANTIC_INFO combined;
  WOPT_DSL_SEMANTIC_INFO projection;
  WOPT_DSL_SEMANTIC_INFO recovered;
  standalone.logical_operator = OPR_DSLDIV;
  standalone.flags = WOPT_DSL_SEMANTIC_PURE;
  if (!WOPT_DSL_Create_DIVREM_Semantics
          (&standalone, &combined, &projection) ||
      !WOPT_DSL_Projectable_Info(&combined) ||
      !WOPT_DSL_Projection_Info(&projection) ||
      combined.logical_operator != OPR_DSLDIVREM ||
      combined.second_result_ty != standalone.result_ty ||
      projection.logical_operator != OPR_DSLDIVPART ||
      projection.projection_kind !=
          WOPT_DSL_PROJECTION_QUOTIENT ||
      !WOPT_DSL_Uncombine_Projection_Semantics
          (&projection, &recovered) ||
      recovered.logical_operator != OPR_DSLDIV ||
      recovered.second_result_ty != TY_IDX_ZERO ||
      recovered.projection_kind != WOPT_DSL_PROJECTION_NONE) {
    fprintf(stderr, "DSL quotient projectable semantics changed\n");
    return 1;
  }

  standalone.logical_operator = OPR_DSLREM;
  WOPT_DSL_SEMANTIC_INFO quotient_combined = combined;
  if (!WOPT_DSL_Create_DIVREM_Semantics
          (&standalone, &combined, &projection) ||
      projection.logical_operator != OPR_DSLREMPART ||
      projection.projection_kind !=
          WOPT_DSL_PROJECTION_REMAINDER ||
      !WOPT_DSL_Uncombine_Projection_Semantics
          (&projection, &recovered) ||
      recovered.logical_operator != OPR_DSLREM) {
    fprintf(stderr, "DSL remainder projectable semantics changed\n");
    return 1;
  }
  quotient_combined.origin_node_id = 101;
  combined.origin_node_id = 202;
  WOPT_DSL_SEMANTIC_INFO_ID quotient_combined_id =
      WOPT_DSL_Semantic_Info_Intern(&quotient_combined);
  WOPT_DSL_SEMANTIC_INFO_ID remainder_combined_id =
      WOPT_DSL_Semantic_Info_Intern(&combined);
  if (quotient_combined_id == WOPT_DSL_SEMANTIC_INFO_INVALID_ID ||
      quotient_combined_id != remainder_combined_id) {
    fprintf(stderr, "matching DSL DIV/REM did not share DIVREM identity\n");
    return 1;
  }
  ++combined.second_result_ty;
  if (WOPT_DSL_Semantic_Info_Intern(&combined) ==
          quotient_combined_id) {
    fprintf(stderr, "DSL DIVREM second result descriptor was ignored\n");
    return 1;
  }

  printf("WOPT DSL semantic-info contract passed\n");
  return 0;
}
