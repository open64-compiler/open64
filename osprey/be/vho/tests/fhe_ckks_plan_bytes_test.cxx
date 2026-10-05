/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify canonical CKKS event-plan bytes before whole-PU grouping.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_ckks_plan_bytes.h"

#include <assert.h>
#include <limits>
#include <stdio.h>
#include <vector>

/* Model one executable refresh followed by a bound-dependent step. */
static VHO_FHE_CKKS_EVENT_PLAN Build_Plan()
{
  VHO_FHE_CKKS_PLAN_STATE state = {
    1, 1, 2, 15, 56, 2, 40, 32768, 1, "ckks_slots_v1", 0, 0
  };
  VHO_FHE_CKKS_PLAN_STEP bootstrap = {
    601, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 37, ""}},
    {{"attr.target_level", "15"}, {"attr.reason", "pre_relu_refresh"}},
    state, {"bootstrap-key"}, {}, ""
  };
  VHO_FHE_CKKS_PLAN_STEP normalize = {
    602, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, 0, ""},
     {VHO_FHE_CKKS_PLAN_BOUND_FORMAL, 0, "relu_bound_0"}},
    {{"attr.role", "normalize"}}, state,
    {"rotation-key-1", "rotation-key-2"}, {-1, 2},
    std::string(64, 'a')
  };
  VHO_FHE_CKKS_EVENT_PLAN plan = {
    27, {bootstrap, normalize}, {0, 1}, 1
  };
  return plan;
}

/* Semantic sets have one byte encoding regardless of producer order. */
static void Check_Deterministic_Encoding()
{
  VHO_FHE_CKKS_EVENT_PLAN plan = Build_Plan();
  std::vector<unsigned char> baseline;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &baseline, stderr));
  assert(baseline.size() > 100 && baseline[0] == 'F' && baseline[7] == '1');

  VHO_FHE_CKKS_EVENT_PLAN reordered = plan;
  std::swap(reordered.steps[0].attributes[0],
            reordered.steps[0].attributes[1]);
  std::swap(reordered.steps[1].required_keys[0],
            reordered.steps[1].required_keys[1]);
  std::swap(reordered.steps[1].signed_rotations[0],
            reordered.steps[1].signed_rotations[1]);
  std::vector<unsigned char> candidate;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate == baseline);

  reordered.steps[1].result_state.level = 14;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate != baseline);
  reordered = plan;
  reordered.steps[1].signed_rotations[1] = 3;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate != baseline);
  reordered = plan;
  reordered.steps[1].result_ty = 1002;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate != baseline);
  reordered = plan;
  reordered.steps[1].operands[1].bound_role = "relu_bound_1";
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate != baseline);
  reordered = plan;
  reordered.steps[1].plaintext_asset_sha256 = std::string(64, 'b');
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(reordered, &candidate, stderr));
  assert(candidate != baseline);
}

/* Every rejection must preserve the caller's previous complete bytes. */
static void Check_Fail_Closed()
{
  VHO_FHE_CKKS_EVENT_PLAN plan = Build_Plan();
  std::vector<unsigned char> bytes(1, 0x7f);
  plan.steps[1].operands[0].reference = 2;
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  assert(bytes.size() == 1 && bytes[0] == 0x7f);
  plan = Build_Plan();
  plan.steps[0].attributes.push_back(plan.steps[0].attributes[0]);
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  plan = Build_Plan();
  plan.steps[1].required_keys.push_back("rotation-key-1");
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  plan = Build_Plan();
  plan.steps[1].signed_rotations.push_back(-1);
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  plan = Build_Plan();
  plan.steps[1].plaintext_asset_sha256 = "not-a-digest";
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  plan = Build_Plan();
  plan.group_output_steps = {1, 0};
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  plan = Build_Plan();
  plan.source_final_step_index = 0;
  assert(!VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, NULL));
  assert(bytes.size() == 1 && bytes[0] == 0x7f);
}

/* Probe C1's actual byte and step guards for the largest grouped Conv shape.
 * These fictitious operator IDs prove serialization capacity only; no CKKS
 * legality, asset binding, or executable Conv plan is inferred from this. */
static void Check_Grouped_Conv_Structural_Budget()
{
  const uint32_t masks = 1143;
  VHO_FHE_CKKS_PLAN_STATE state = {
    1, 1, 2, 15, 56, 2, 40, 32768, 1, "ckks_slots_v1", 0, 0
  };
  VHO_FHE_CKKS_EVENT_PLAN plan;
  plan.source_static_ordinal = 1;
  plan.source_final_step_index = 0;
  uint32_t accumulated = 0;
  for (uint32_t i = 0; i < masks; ++i) {
    uint32_t input = 0;
    if (i != 0) {
      VHO_FHE_CKKS_PLAN_STEP rotate = {
        701, 1, 1001,
        {{VHO_FHE_CKKS_PLAN_SOURCE_VALUE, 37, ""}}, {}, state,
        {"rotation-key-12345"}, {static_cast<int32_t>(i)}, ""
      };
      input = static_cast<uint32_t>(plan.steps.size());
      plan.steps.push_back(rotate);
    }
    VHO_FHE_CKKS_PLAN_STEP encode = {
      702, 1, 1002, {}, {}, state, {}, {}, std::string(64, 'a')
    };
    const uint32_t encoded = static_cast<uint32_t>(plan.steps.size());
    plan.steps.push_back(encode);
    VHO_FHE_CKKS_PLAN_OPERAND source = {
      i == 0 ? VHO_FHE_CKKS_PLAN_SOURCE_VALUE :
               VHO_FHE_CKKS_PLAN_PRIOR_STEP,
      i == 0 ? 37 : input, ""
    };
    VHO_FHE_CKKS_PLAN_STEP multiply = {
      703, 1, 1001,
      {source, {VHO_FHE_CKKS_PLAN_PRIOR_STEP, encoded, ""}},
      {}, state, {}, {}, ""
    };
    plan.steps.push_back(multiply);
    VHO_FHE_CKKS_PLAN_STEP rescale = {
      704, 1, 1001,
      {{VHO_FHE_CKKS_PLAN_PRIOR_STEP,
        static_cast<uint32_t>(plan.steps.size() - 1), ""}},
      {}, state, {}, {}, ""
    };
    plan.steps.push_back(rescale);
    const uint32_t term = static_cast<uint32_t>(plan.steps.size() - 1);
    if (i == 0) {
      accumulated = term;
    } else {
      VHO_FHE_CKKS_PLAN_STEP add = {
        705, 1, 1001,
        {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, accumulated, ""},
         {VHO_FHE_CKKS_PLAN_PRIOR_STEP, term, ""}},
        {}, state, {}, {}, ""
      };
      accumulated = static_cast<uint32_t>(plan.steps.size());
      plan.steps.push_back(add);
    }
  }
  VHO_FHE_CKKS_PLAN_STEP bias = {
    702, 1, 1002, {}, {}, state, {}, {}, std::string(64, 'b')
  };
  const uint32_t bias_index = static_cast<uint32_t>(plan.steps.size());
  plan.steps.push_back(bias);
  VHO_FHE_CKKS_PLAN_STEP final_add = {
    705, 1, 1001,
    {{VHO_FHE_CKKS_PLAN_PRIOR_STEP, accumulated, ""},
     {VHO_FHE_CKKS_PLAN_PRIOR_STEP, bias_index, ""}},
    {}, state, {}, {}, ""
  };
  plan.steps.push_back(final_add);
  plan.source_final_step_index = static_cast<uint32_t>(plan.steps.size() - 1);
  plan.group_output_steps.push_back(plan.source_final_step_index);
  assert(plan.steps.size() == 5 * masks);
  std::vector<unsigned char> bytes;
  assert(VHO_FHE_CKKS_Serialize_Event_Plan(plan, &bytes, stderr));
  assert(bytes.size() < 4 * 1024 * 1024);
  printf("grouped Conv structural budget: steps=%lu bytes=%lu\n",
         static_cast<unsigned long>(plan.steps.size()),
         static_cast<unsigned long>(bytes.size()));
}

/* Run canonicalization and no-partial-output negatives as one focused lane. */
int main()
{
  Check_Deterministic_Encoding();
  Check_Fail_Closed();
  Check_Grouped_Conv_Structural_Budget();
  puts("FHE CKKS canonical event-plan bytes passed (no WHIRL emitted)");
  return 0;
}
