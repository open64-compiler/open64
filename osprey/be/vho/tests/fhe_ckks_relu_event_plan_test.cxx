/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify the explicit CKKS schedule generated from the pinned ANT ACE ReLU
 * algebra before any WHIRL mutation. Design:
 * doc/FHE-SYNC6-ACE-RELU-EXECUTION-RECIPE.md.
 */

#include <assert.h>

#include <map>
#include <vector>

#include "dsl_opcode.h"
#include "fhe_ckks_relu_event_plan.h"
#include "fhe_plan.h"

/* Return the exact pinned degree-7, degree-15, degree-13 coefficients. */
static std::vector<std::vector<double> > Coefficients()
{
  static const double stage0[] = {
    0.0, 1.277209679957775013e+00, 0.0,
    -4.369818210105346212e-01, 0.0, 2.781705762612975419e-01,
    0.0, -9.522998581241576277e-01
  };
  static const double stage1[] = {
    0.0, 1.336811809725395372e+00, 0.0,
    -3.314086854871873267e-01, 0.0, 2.739009935511804161e-01,
    0.0, -2.096678512577555831e-01, 0.0,
    6.827141455300124451e-02, 0.0, -1.036056317926726048e-02,
    0.0, 7.381161118162535544e-04, 0.0,
    -2.000350671563594715e-05
  };
  static const double stage2[] = {
    0.0, 1.229917329338358289e+00, 0.0,
    -3.099894039867301943e-01, 0.0, 1.047929208484282559e-01,
    0.0, -3.040264421328875422e-02, 0.0,
    6.507995190210730772e-03, 0.0, -8.815509689332230855e-04,
    0.0, 5.555595810150389487e-05
  };
  std::vector<std::vector<double> > result;
  result.push_back(std::vector<double>(
      stage0, stage0 + sizeof(stage0) / sizeof(stage0[0])));
  result.push_back(std::vector<double>(
      stage1, stage1 + sizeof(stage1) / sizeof(stage1[0])));
  result.push_back(std::vector<double>(
      stage2, stage2 + sizeof(stage2) / sizeof(stage2[0])));
  return result;
}

/* Build one complete policy with deterministic stand-in external value IDs. */
static VHO_FHE_CKKS_RELU_EVENT_POLICY Policy(
    const VHO_FHE_RELU_RECIPE &recipe, INT32 target)
{
  VHO_FHE_CKKS_RELU_EVENT_POLICY policy;
  policy.source_static_ordinal = 42;
  policy.source_value_id = 77;
  policy.result_ty = 101;
  policy.cipher_encryption_descriptor_id = 1;
  policy.plaintext_encryption_descriptor_id = 2;
  policy.scheme = DSL_FHE_SCHEME_CKKS;
  policy.ciphertext_value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
  policy.plaintext_value_class =
      DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT;
  policy.post_refresh_level = target;
  policy.scale_bits = 56;
  policy.component_count = 2;
  policy.precision_bits = 30;
  policy.slot_count = 32768;
  policy.alignment_group = 0;
  policy.pending_bootstrap_action = DSL_FHE_CKKS_PENDING_BOOTSTRAP;
  policy.pre_relu_bootstrap_reason =
      DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH;
  policy.encrypted_layout = "ckks.packed";
  policy.bootstrap_key_id = "model-key";
  policy.relinearization_key_id = "model-key";
  policy.reciprocal_bound_value_id = 900;
  policy.reciprocal_bound_sha256 = std::string(64, 'a');
  policy.scalar_value_ids.resize(recipe.steps.size());
  policy.scalar_sha256.resize(recipe.steps.size());
  for (UINT32 i = 0; i < recipe.steps.size(); ++i) {
    if (recipe.steps[i].operation == VHO_FHE_RELU_RECIPE_MUL_SCALAR ||
        recipe.steps[i].operation ==
            VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT) {
      policy.scalar_value_ids[i] = 1000 + i;
      policy.scalar_sha256[i] = std::string(64, "0123456789abcdef"[i % 16]);
    }
  }
  return policy;
}

/* Prove all three approved refresh levels share one exact executable shape. */
int main()
{
  VHO_FHE_RELU_RECIPE recipe;
  assert(VHO_FHE_CKKS_Build_Ace_Relu_Recipe(
      Coefficients(), &recipe, stderr));
  const INT32 targets[3] = {15, 17, 18};
  std::vector<unsigned char> reference;
  for (UINT32 target = 0; target < 3; ++target) {
    VHO_FHE_CKKS_RELU_EVENT_POLICY policy = Policy(recipe, targets[target]);
    VHO_FHE_CKKS_EVENT_PLAN plan;
    assert(VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
        recipe, policy, &plan, stderr));
    assert(!plan.steps.empty() && plan.group_output_steps.size() == 6);
    assert(plan.steps[0].logical_operator == OPR_DSLCKKSBOOTSTRAP);
    assert(plan.group_output_steps[0] == 0);
    assert(plan.group_output_steps[1] == 3);
    assert(plan.source_final_step_index == plan.group_output_steps.back());
    assert(plan.steps[plan.source_final_step_index].result_state.level ==
           targets[target] - 12);
    assert(plan.steps[plan.source_final_step_index].result_state.scale_bits ==
           56);
    assert(plan.steps[plan.source_final_step_index].result_state.component_count ==
           2);
    assert(plan.steps[plan.source_final_step_index].result_state.pending_actions ==
           0);

    std::map<UINT32, UINT32> census;
    for (UINT32 i = 0; i < plan.steps.size(); ++i)
      ++census[plan.steps[i].logical_operator];
    assert(census[OPR_DSLCKKSBOOTSTRAP] == 1);
    assert(plan.steps.size() == 223);
    assert(census[OPR_DSLCKKSENCODE] == 31);
    assert(census[OPR_DSLCKKSMUL] == 50);
    assert(census[OPR_DSLCKKSRESCALE] == census[OPR_DSLCKKSMUL]);
    assert(census[OPR_DSLCKKSRELIN] == 27);
    assert(census[OPR_DSLCKKSMODSWITCH] == 27);
    assert(census[OPR_DSLCKKSADD] == 37);

    std::vector<unsigned char> canonical;
    assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &canonical, stderr));
    if (target == 0)
      reference = canonical;
    else
      assert(canonical != reference);
    printf("target=%d final=%d steps=%u encode=%u mul=%u relin=%u "
           "rescale=%u modswitch=%u add=%u\n",
           targets[target], targets[target] - 12,
           (UINT32)plan.steps.size(), census[OPR_DSLCKKSENCODE],
           census[OPR_DSLCKKSMUL], census[OPR_DSLCKKSRELIN],
           census[OPR_DSLCKKSRESCALE], census[OPR_DSLCKKSMODSWITCH],
           census[OPR_DSLCKKSADD]);
  }

  VHO_FHE_CKKS_RELU_EVENT_POLICY malformed = Policy(recipe, 15);
  VHO_FHE_CKKS_EVENT_PLAN sentinel;
  sentinel.source_static_ordinal = 999;
  UINT32 required_scalar = 0;
  while (required_scalar < recipe.steps.size() &&
         recipe.steps[required_scalar].operation !=
             VHO_FHE_RELU_RECIPE_MUL_SCALAR &&
         recipe.steps[required_scalar].operation !=
             VHO_FHE_RELU_RECIPE_SCALAR_CONSTANT)
    ++required_scalar;
  assert(required_scalar < recipe.steps.size());
  malformed.scalar_value_ids[required_scalar] = 0;
  assert(!VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
      recipe, malformed, &sentinel, NULL));
  assert(sentinel.source_static_ordinal == 999 && sentinel.steps.empty());
  malformed = Policy(recipe, 12);
  assert(!VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan(
      recipe, malformed, &sentinel, NULL));
  assert(sentinel.source_static_ordinal == 999 && sentinel.steps.empty());
  return 0;
}
