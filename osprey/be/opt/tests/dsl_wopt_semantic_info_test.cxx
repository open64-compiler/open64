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

  printf("WOPT DSL semantic-info contract passed\n");
  return 0;
}
