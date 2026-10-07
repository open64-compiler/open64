/*
 * Copyright (C) 2026 Open64 Project
 *
 * Link-test the FHE CKKS collector against read-only schedule/call-image
 * substitutes. No WHIRL image or common/com implementation is modified.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_source_events.h"
#include "fhe_ckks_relu_plan.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_plan.h"
#include "fhe_semantic_runtime_lower.h"

static DSL_PU_SOURCE_IDENTITY_RECORD identities[2];
static DSL_CALLSITE_METADATA_RECORD callsites[2];
static VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD schedules[2];
static DSL_FHE_MATERIALIZATION_OPERATION_RECORD operations[12];
static DSL_FHE_CONTEXT_RANGE_RECORD ranges[2];
static DSL_FHE_CONTEXT_CKKS_STATE_RECORD states[12];
static DSL_FHE_COMPOSITE_PROFILE_RECORD profile;
static DSL_FHE_APPROX_STAGE_RECORD stages[3];
static UINT32 dynamic_count;

/* Supply a valid two-PU root/callee identity table to the collector. */
UINT32 DSL_Call_Image_PU_Identity_Count(void)
{
  return 2;
}

/* Reject an out-of-range identity as the managed table would. */
BOOL DSL_Call_Image_Get_PU_Identity(
    DSL_PU_SOURCE_IDENTITY_ID id,
    DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
  if (id == 0 || id > 2 || record == NULL)
    return FALSE;
  *record = identities[id - 1];
  return TRUE;
}

/* Supply both direct callsites into the shared callee PU. */
UINT32 DSL_Call_Image_Callsite_Count(void)
{
  return 2;
}

/* Reject an out-of-range callsite without changing the caller's record. */
BOOL DSL_Call_Image_Get_Callsite(
    DSL_CALLSITE_METADATA_ID id,
    DSL_CALLSITE_METADATA_RECORD *record)
{
  if (id == 0 || id > 2 || record == NULL)
    return FALSE;
  *record = callsites[id - 1];
  return TRUE;
}

/* Model the existing schedule's read-only preparation boundary. */
BOOL VHO_FHE_Runtime_Static_Schedule_Prepare(FILE *)
{
  return TRUE;
}

/* Expose the two physical source definitions in this linked fixture. */
UINT32 VHO_FHE_Runtime_Static_Schedule_Record_Count(void)
{
  return 2;
}

/* Keep the ordinal range and multiplicity exactly as stored by SYNC-5. */
BOOL VHO_FHE_Runtime_Static_Schedule_Get(
    UINT32 index,
    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD *record)
{
  if (index >= 2 || record == NULL)
    return FALSE;
  *record = schedules[index];
  return TRUE;
}

/* Report the independently counted dynamic event total for comparison. */
UINT32 VHO_FHE_Runtime_Dynamic_Evaluation_Count(void)
{
  return dynamic_count;
}

/* Resolve the exact context/ordinal row from a six-part plan. */
BOOL DSL_FHE_Materialization_Find(
    ST_IDX owner, DSL_IR_VALUE_ID source,
    DSL_PU_SOURCE_IDENTITY_ID identity, DSL_CALLSITE_METADATA_ID callsite,
    UINT32 ordinal, DSL_FHE_MATERIALIZATION_OPERATION_RECORD *record)
{
  if (owner != 11 || source != 200 || identity != 2 ||
      callsite == 0 || callsite > 2 || ordinal >= 6 || record == NULL)
    return FALSE;
  *record = operations[(callsite - 1) * 6 + ordinal];
  return TRUE;
}

/* Match the first-path table size independently of the event array. */
UINT32 DSL_FHE_Materialization_Operation_Count(void)
{
  return 12;
}

/* Expose context-specific positive bounds without constructing TCONs. */
BOOL DSL_FHE_Context_Range_Get(
    DSL_FHE_CONTEXT_RANGE_ID id, DSL_FHE_CONTEXT_RANGE_RECORD *record)
{
  if (id == 0 || id > 2 || record == NULL)
    return FALSE;
  *record = ranges[id - 1];
  return TRUE;
}

/* Expose the state role/identity chain; payload legality is image-owned. */
BOOL DSL_FHE_Context_State_Get(
    DSL_FHE_CONTEXT_CKKS_STATE_ID id,
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD *record)
{
  if (id == 0 || id > 12 || record == NULL)
    return FALSE;
  *record = states[id - 1];
  return TRUE;
}

/* Give the consumer one complete depth-11 composite profile. */
BOOL DSL_FHE_Approx_Profile_Get(
    DSL_FHE_COMPOSITE_PROFILE_ID id,
    DSL_FHE_COMPOSITE_PROFILE_RECORD *record)
{
  if (id != profile.id || record == NULL)
    return FALSE;
  *record = profile;
  return TRUE;
}

/* Resolve ordered stage depth contracts independently of state rows. */
BOOL DSL_FHE_Approx_Stage_Get(
    DSL_FHE_APPROX_STAGE_ID id, DSL_FHE_APPROX_STAGE_RECORD *record)
{
  if (id < 4 || id > 6 || record == NULL)
    return FALSE;
  *record = stages[id - 4];
  return TRUE;
}

/* Certify exact source identities and leave prior output on every rejection. */
int main()
{
  memset(identities, 0, sizeof(identities));
  memset(callsites, 0, sizeof(callsites));
  memset(schedules, 0, sizeof(schedules));
  memset(operations, 0, sizeof(operations));
  memset(ranges, 0, sizeof(ranges));
  memset(states, 0, sizeof(states));
  memset(&profile, 0, sizeof(profile));
  memset(stages, 0, sizeof(stages));
  profile.id = 1;
  profile.first_stage_id = 4;
  profile.stage_count = 3;
  profile.total_multiplicative_depth = 11;
  profile.reconstruction =
      DSL_FHE_RECONSTRUCTION_RELU_FROM_NORMALIZED_SIGN;
  profile.normalization_policy =
      DSL_FHE_NORMALIZATION_POSITIVE_CONTEXT_BOUND;
  profile.pre_refresh_policy = DSL_FHE_PRE_REFRESH_REQUIRED;
  const INT32 stage_depths[3] = {3, 4, 4};
  const UINT32 stage_degrees[3] = {7, 15, 13};
  for (UINT32 stage = 0; stage < 3; ++stage) {
    stages[stage].id = stage + 4;
    stages[stage].profile_id = 1;
    stages[stage].stage_ordinal = stage;
    stages[stage].degree = stage_degrees[stage];
    stages[stage].basis = DSL_FHE_APPROX_BASIS_CHEBYSHEV;
    stages[stage].evaluation_scheme =
        DSL_FHE_APPROX_EVAL_ADDITION_CHAIN;
    stages[stage].required_input_value_class =
        DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    stages[stage].output_scale_policy =
        DSL_FHE_APPROX_OUTPUT_SCALE_PRESERVE_INPUT;
    stages[stage].output_component_policy =
        DSL_FHE_APPROX_COMPONENT_RELINEARIZED_TWO;
    stages[stage].level_consumption = stage_depths[stage];
    stages[stage].minimum_precision_bits = 30;
  }
  identities[0].id = 1;
  identities[0].owner_pu_st = 10;
  identities[1].id = 2;
  identities[1].owner_pu_st = 11;
  for (UINT32 i = 0; i < 2; ++i) {
    callsites[i].id = i + 1;
    callsites[i].owner_pu_st = 10;
    callsites[i].callee_pu_st = 11;
  }
  schedules[0].owner_pu_st = 10;
  schedules[0].result_value_id = 100;
  schedules[0].first_static_ordinal = 1;
  schedules[0].static_evaluation_count = 1;
  schedules[0].execution_multiplicity = 1;
  schedules[0].logical_operator = OPR_DSLCONV2D;
  schedules[1].owner_pu_st = 11;
  schedules[1].result_value_id = 200;
  schedules[1].first_static_ordinal = 2;
  schedules[1].static_evaluation_count = 6;
  schedules[1].execution_multiplicity = 2;
  schedules[1].logical_operator = OPR_DSLRELU;
  dynamic_count = 13;

  const UINT32 kinds[6] = {
    DSL_FHE_MATERIALIZATION_OPERATION_REFRESH,
    DSL_FHE_MATERIALIZATION_OPERATION_NORMALIZE,
    DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
    DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
    DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
    DSL_FHE_MATERIALIZATION_OPERATION_RECONSTRUCT_RELU
  };
  for (UINT32 context = 0; context < 2; ++context) {
    ranges[context].id = context + 1;
    ranges[context].owner_pu_st = 11;
    ranges[context].source_relu_value_id = 200;
    ranges[context].context_pu_identity_id = 2;
    ranges[context].context_callsite_id = context + 1;
    ranges[context].profile_id = 1;
    ranges[context].positive_bound_tcon = context == 0 ? 2 : 8;
    for (UINT32 ordinal = 0; ordinal < 6; ++ordinal) {
      UINT32 index = context * 6 + ordinal;
      operations[index].id = index + 1;
      operations[index].owner_pu_st = 11;
      operations[index].source_relu_value_id = 200;
      operations[index].context_pu_identity_id = 2;
      operations[index].context_callsite_id = context + 1;
      operations[index].operation_kind = kinds[ordinal];
      operations[index].operation_ordinal = ordinal;
      operations[index].profile_id = 1;
      operations[index].range_id = context + 1;
      operations[index].input_state_id = ordinal == 0 ? 0 : index;
      operations[index].output_state_id = index + 1;
      operations[index].stage_id =
          ordinal >= 2 && ordinal <= 4 ?
              profile.first_stage_id + ordinal - 2 : 0;
      operations[index].parameter_tcon =
          ordinal >= 1 && ordinal <= 4 ? index + 1 : 0;
      states[index].id = index + 1;
      states[index].owner_pu_st = 11;
      states[index].source_value_id = 200;
      states[index].context_pu_identity_id = 2;
      states[index].context_callsite_id = context + 1;
      states[index].scheme = DSL_FHE_SCHEME_CKKS;
      states[index].value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
      states[index].encryption_descriptor_id = 1;
      states[index].level =
          (context == 0 ? 15 : 18) -
          (ordinal >= 4 ? 11 : ordinal == 3 ? 7 :
           ordinal == 2 ? 3 : 0);
      states[index].scale_bits = 56;
      states[index].component_count = 2;
      states[index].precision_bits = 30;
      states[index].slot_count = 32768;
      states[index].encrypted_layout_name = 1;
      states[index].pending_actions =
          ordinal == 0 ? DSL_FHE_CKKS_PENDING_BOOTSTRAP : 0;
      states[index].pending_bootstrap_reason =
          ordinal == 0 ? DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH : 0;
      states[index].state_role =
          ordinal == 0 ? DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH :
          ordinal == 5 ? DSL_FHE_CONTEXT_STATE_ROLE_RESULT :
                         DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION;
    }
  }

  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  assert(VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13 && events[0].context_callsite_id == 0);
  assert(events[1].source_value_id == 200 &&
         events[1].source_static_ordinal == 2 &&
         events[6].source_static_ordinal == 7 &&
         events[7].context_callsite_id == 2 &&
         events[12].source_static_ordinal == 7);
  std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> plans;
  assert(VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12 && plans[0].operation_id == 1 &&
         plans[0].range_id == 1 && plans[0].output_state_id == 1 &&
         plans[11].operation_id == 12 && plans[11].range_id == 2 &&
         plans[11].output_state_id == 12);
  std::vector<VHO_FHE_CKKS_RELU_BOUND_BINDING> bindings;
  assert(VHO_FHE_CKKS_Collect_Relu_Bound_Bindings(
      plans, &bindings, NULL));
  assert(bindings.size() == 2 &&
         bindings[0].positive_bound_tcon == 2 &&
         bindings[1].positive_bound_tcon == 8 &&
         bindings[0].context_callsite_id == 1 &&
         bindings[1].context_callsite_id == 2);
  operations[7].parameter_tcon = 2;
  assert(!VHO_FHE_CKKS_Collect_Relu_Bound_Bindings(
      plans, &bindings, NULL));
  assert(bindings.size() == 2 && bindings[1].positive_bound_tcon == 8);
  operations[7].parameter_tcon = 8;
  ranges[1].positive_bound_tcon = 0;
  assert(!VHO_FHE_CKKS_Collect_Relu_Bound_Bindings(
      plans, &bindings, NULL));
  assert(bindings.size() == 2);
  ranges[1].positive_bound_tcon = 8;
  std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> incomplete(plans);
  incomplete.pop_back();
  assert(!VHO_FHE_CKKS_Collect_Relu_Bound_Bindings(
      incomplete, &bindings, NULL));
  assert(bindings.size() == 2);

  operations[8].input_state_id = 1;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  operations[8].input_state_id = 8;
  ranges[1].context_callsite_id = 1;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  ranges[1].context_callsite_id = 2;
  operations[3].parameter_tcon = 0;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  operations[3].parameter_tcon = 4;
  operations[2].stage_id = 5;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  operations[2].stage_id = 4;
  states[6].pending_bootstrap_reason = 0;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[6].pending_bootstrap_reason =
      DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH;
  operations[7].owner_pu_st = 10;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  operations[7].owner_pu_st = 11;

  states[9].level += 1;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[9].level -= 1;
  stages[1].level_consumption = 3;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  stages[1].level_consumption = 4;
  stages[1].degree = 14;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  stages[1].degree = 15;
  stages[0].output_scale_policy =
      DSL_FHE_APPROX_OUTPUT_SCALE_DEFAULT_RESCALE;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  stages[0].output_scale_policy =
      DSL_FHE_APPROX_OUTPUT_SCALE_PRESERVE_INPUT;
  profile.total_multiplicative_depth = 10;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  profile.total_multiplicative_depth = 11;
  states[7].level -= 1;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[7].level += 1;
  states[11].scale_bits = 55;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[11].scale_bits = 56;
  states[6].encryption_descriptor_id = 2;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[6].encryption_descriptor_id = 1;
  states[11].precision_bits = 29;
  assert(!VHO_FHE_CKKS_Collect_Relu_Plan_Steps(events, &plans, NULL));
  assert(plans.size() == 12);
  states[11].precision_bits = 30;

  callsites[1].owner_pu_st = 99;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13 && events[12].source_static_ordinal == 7);
  callsites[1].owner_pu_st = 10;

  dynamic_count = 12;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13);
  dynamic_count = 13;
  schedules[1].execution_multiplicity = 3;
  assert(!VHO_FHE_CKKS_Collect_Source_Events(&events, NULL));
  assert(events.size() == 13);

  printf("linked_schedule_rows=2 source_events=13 relu_plan_steps=12 ");
  printf("relu_static_ordinals=6 stage_depth=3+4+4 bound_contexts=2 ");
  printf("partial_output=none\n");
  return 0;
}
