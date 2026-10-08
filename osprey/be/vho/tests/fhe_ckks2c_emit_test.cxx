/*
 * Copyright (C) 2026 Open64 Project
 *
 * Focused CKKS2C source-generation and fail-closed tests.  The resulting C is
 * compiled and linked separately against facade declarations; no provider or
 * ciphertext execution is claimed.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D1.
 */

#include <assert.h>
#include <stdio.h>

#include <string>
#include <vector>

#include "defs.h"
#include "dsl_opcode.h"
#include "fhe_ckks2c_emit.h"

/* Construct one fixture containing every v1 CKKS facade primitive. */
static VHO_FHE_CKKS2C_GROUP Build_Group()
{
  VHO_FHE_CKKS_PLAN_STATE state = {
    1, 1, 2, 15, 56, 2, 40, 32768, 1, "ckks_slots_v1", 0, 0
  };
  VHO_FHE_CKKS_EVENT_PLAN plan;
  plan.source_static_ordinal = 7;
  plan.steps.push_back({
    OPR_DSLCKKSENCODE, 1, 1002,
    {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 20, ""}}, {}, state, {}, {},
    std::string(64, 'a')
  });
  plan.steps.push_back({
    OPR_DSLCKKSADD, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 10, ""},
     {VHO_FHE_CKKS_PLAN_PRIOR_STEP, 0, ""}}, {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSSUB, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 1, ""},
     {VHO_FHE_CKKS_PLAN_BOUND_FORMAL, 0, "relu_bound_0"}},
    {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSMUL, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 2, ""},
     {VHO_FHE_CKKS_PLAN_PRIOR_STEP, 0, ""}}, {}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSROTATE, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 3, ""}},
    {{"attr.signed_steps", "-3"}}, state,
    {"rotation-key--3"}, {-3}, ""
  });
  state.level = 14;
  state.scale_bits = 50;
  plan.steps.push_back({
    OPR_DSLCKKSRESCALE, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 4, ""}},
    {{"attr.levels", "1"}, {"attr.target_scale_bits", "50"}},
    state, {}, {}, ""
  });
  state.level = 13;
  plan.steps.push_back({
    OPR_DSLCKKSMODSWITCH, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 5, ""}},
    {{"attr.target_level", "13"}}, state, {}, {}, ""
  });
  plan.steps.push_back({
    OPR_DSLCKKSRELIN, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 6, ""}}, {}, state,
    {"relin-key"}, {}, ""
  });
  state.level = 18;
  state.scale_bits = 56;
  plan.steps.push_back({
    OPR_DSLCKKSBOOTSTRAP, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 7, ""}},
    {{"attr.reason", "pre_relu_refresh"},
     {"attr.target_level", "18"}},
    state, {"bootstrap-key"}, {}, ""
  });
  plan.group_output_steps.push_back(8);
  plan.source_final_step_index = 8;

  VHO_FHE_CKKS2C_GROUP group;
  group.function_name = "open64_fhe_eval_fixture_group";
  group.identity_sha256 = std::string(64, 'b');
  group.owner_pu_st = 101;
  group.source_value_id = 202;
  group.context_pu_identity_id = 303;
  group.context_callsite_id = 404;
  group.plan = plan;
  return group;
}

/* Check deterministic text, all primitive calls, explicit state, and cleanup. */
static std::string Check_Positive()
{
  const std::vector<VHO_FHE_CKKS2C_GROUP> groups(1, Build_Group());
  std::string first;
  std::string second;
  VHO_FHE_CKKS2C_EMIT_RESULT result = {0, 0, 0, 0};
  VHO_FHE_CKKS2C_EMIT_RESULT repeat = {0, 0, 0, 0};
  assert(VHO_FHE_CKKS2C_Emit_Module(
      "fixture", groups, &first, &result, stderr));
  assert(VHO_FHE_CKKS2C_Emit_Module(
      "fixture", groups, &second, &repeat, stderr));
  assert(first == second && result.group_count == 1 &&
         result.operation_count == 9 && result.source_value_count == 1 &&
         result.bound_value_count == 1);
  const char *calls[] = {
    "open64_fhe_ckks2c_encode_asset_v1",
    "open64_fhe_ckks2c_add_v1",
    "open64_fhe_ckks2c_sub_v1",
    "open64_fhe_ckks2c_mul_v1",
    "open64_fhe_ckks2c_rotate_v1",
    "open64_fhe_ckks2c_rescale_v1",
    "open64_fhe_ckks2c_modswitch_v1",
    "open64_fhe_ckks2c_relin_v1",
    "open64_fhe_ckks2c_bootstrap_v1",
    "open64_fhe_ckks2c_value_release_v1"
  };
  size_t prior = 0;
  for (size_t i = 0; i < sizeof(calls) / sizeof(calls[0]); ++i) {
    const size_t found = first.find(calls[i], prior);
    assert(found != std::string::npos);
    if (i + 1 != sizeof(calls) / sizeof(calls[0]))
      prior = found + 1;
  }
  assert(first.find("group_sha256=") != std::string::npos &&
         first.find("pre_relu_refresh") != std::string::npos &&
         first.find("ckks_slots_v1") != std::string::npos &&
         first.find("source_value_v1(execution, 20u") == std::string::npos);
  return first;
}

/* Unsupported operators and inconsistent state leave previous output intact. */
static void Check_Negative()
{
  VHO_FHE_CKKS2C_GROUP group = Build_Group();
  std::vector<VHO_FHE_CKKS2C_GROUP> groups(1, group);
  std::string output("retained");
  VHO_FHE_CKKS2C_EMIT_RESULT result = {9, 9, 9, 9};
  groups[0].plan.steps[3].logical_operator = OPR_DSLRELU;
  assert(!VHO_FHE_CKKS2C_Emit_Module(
      "fixture", groups, &output, &result, NULL));
  assert(output == "retained" && result.group_count == 9);

  groups[0] = group;
  groups[0].plan.steps[5].result_state.scale_bits = 49;
  assert(!VHO_FHE_CKKS2C_Emit_Module(
      "fixture", groups, &output, &result, NULL));
  assert(output == "retained" && result.group_count == 9);
}

/* Write the checked candidate for the script's independent C compiler lane. */
int main(int argc, char **argv)
{
  assert(argc == 2);
  const std::string output = Check_Positive();
  Check_Negative();
  FILE *file = fopen(argv[1], "wb");
  assert(file != NULL && fwrite(output.data(), 1, output.size(), file) ==
                             output.size());
  assert(fclose(file) == 0);
  puts("FHE CKKS2C primitive emission passed");
  return 0;
}
