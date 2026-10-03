/*
 * Copyright (C) 2026 Open64 Project
 *
 * Resolve exact per-context ReLU source events to the existing six-operation
 * FHE materialization plan. No WN, TY, ST, or mapped table is mutated.
 * Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_relu_plan.h"

#include <map>
#include <set>
#include <tuple>

#include "dsl_opcode.h"
#include "fhe_plan.h"
#include "fhe_semantic_runtime_lower.h"

namespace {

typedef std::tuple<ST_IDX, DSL_IR_VALUE_ID, DSL_PU_SOURCE_IDENTITY_ID,
                   DSL_CALLSITE_METADATA_ID> CONTEXT_KEY;
typedef std::tuple<ST_IDX, DSL_IR_VALUE_ID,
                   DSL_PU_SOURCE_IDENTITY_ID, DSL_CALLSITE_METADATA_ID,
                   UINT32> EVENT_KEY;
typedef std::pair<ST_IDX, DSL_IR_VALUE_ID> SOURCE_KEY;

/* Keep the six source-static events distinct from CKKS step ordinals. */
const UINT32 expected_kind[6] = {
  DSL_FHE_MATERIALIZATION_OPERATION_REFRESH,
  DSL_FHE_MATERIALIZATION_OPERATION_NORMALIZE,
  DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
  DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
  DSL_FHE_MATERIALIZATION_OPERATION_APPROX_STAGE,
  DSL_FHE_MATERIALIZATION_OPERATION_RECONSTRUCT_RELU
};

/* Report a failed read-only table join without publishing partial plans. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-RELU-PLAN-001: %s\n", message);
  return FALSE;
}

}  // namespace

/*
 * Join schedule, event, materialization, range, and value-state identities.
 * The existing FHE image validators own full row legality; this preflight
 * proves the exact source-static-event chain that the CKKS producer consumes.
 */
BOOL VHO_FHE_CKKS_Collect_Relu_Plan_Steps(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> *plans,
    FILE *diagnostic)
{
  if (plans == NULL || events.empty() ||
      events.size() != VHO_FHE_Runtime_Dynamic_Evaluation_Count())
    return Report(diagnostic, "source event set is incomplete");

  std::map<SOURCE_KEY, VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD> schedule;
  for (UINT32 i = 0; i < VHO_FHE_Runtime_Static_Schedule_Record_Count();
       ++i) {
    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD record;
    if (!VHO_FHE_Runtime_Static_Schedule_Get(i, &record) ||
        record.first_static_ordinal == 0 ||
        !schedule.insert(std::make_pair(
            SOURCE_KEY(record.owner_pu_st, record.result_value_id),
            record)).second)
      return Report(diagnostic, "static source schedule is ambiguous");
  }
  if (schedule.empty())
    return Report(diagnostic, "static source schedule is absent");

  std::vector<VHO_FHE_CKKS_RELU_PLAN_STEP> collected;
  std::set<EVENT_KEY> seen;
  std::map<CONTEXT_KEY, UINT32> context_count;
  std::map<CONTEXT_KEY, DSL_FHE_CONTEXT_CKKS_STATE_ID> previous_state;
  std::map<CONTEXT_KEY, DSL_FHE_CONTEXT_RANGE_ID> context_range;
  for (size_t i = 0; i < events.size(); ++i) {
    const VHO_FHE_CKKS_EVENT_IDENTITY &event = events[i];
    std::map<SOURCE_KEY, VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD>
        ::const_iterator source = schedule.find(
            SOURCE_KEY(event.owner_pu_st, event.source_value_id));
    if (source == schedule.end() ||
        event.source_static_ordinal < source->second.first_static_ordinal ||
        event.source_static_ordinal - source->second.first_static_ordinal >=
            source->second.static_evaluation_count ||
        !seen.insert(EVENT_KEY(
            event.owner_pu_st, event.source_value_id,
            event.context_pu_identity_id, event.context_callsite_id,
            event.source_static_ordinal)).second)
      return Report(diagnostic, "event is not an exact scheduled source use");
    if (source->second.logical_operator != OPR_DSLRELU)
      continue;

    UINT32 ordinal = event.source_static_ordinal -
                     source->second.first_static_ordinal;
    CONTEXT_KEY key(event.owner_pu_st, event.source_value_id,
                    event.context_pu_identity_id,
                    event.context_callsite_id);
    if (source->second.static_evaluation_count != 6 ||
        ordinal != context_count[key]++)
      return Report(diagnostic, "ReLU plan ordinal is missing or reordered");

    DSL_FHE_MATERIALIZATION_OPERATION_RECORD operation;
    DSL_FHE_CONTEXT_RANGE_RECORD range;
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD output_state;
    if (!DSL_FHE_Materialization_Find(
            event.owner_pu_st, event.source_value_id,
            event.context_pu_identity_id, event.context_callsite_id,
            ordinal, &operation) ||
        operation.id == 0 ||
        operation.owner_pu_st != event.owner_pu_st ||
        operation.source_relu_value_id != event.source_value_id ||
        operation.context_pu_identity_id !=
            event.context_pu_identity_id ||
        operation.context_callsite_id != event.context_callsite_id ||
        operation.operation_ordinal != ordinal ||
        operation.operation_kind != expected_kind[ordinal] ||
        operation.output_state_id == 0 ||
        operation.range_id == 0 ||
        !DSL_FHE_Context_Range_Get(
            operation.range_id, &range) ||
        range.owner_pu_st != event.owner_pu_st ||
        range.source_relu_value_id != event.source_value_id ||
        range.context_pu_identity_id !=
            event.context_pu_identity_id ||
        range.context_callsite_id != event.context_callsite_id ||
        range.profile_id != operation.profile_id ||
        !DSL_FHE_Context_State_Get(
            operation.output_state_id, &output_state) ||
        output_state.owner_pu_st != event.owner_pu_st ||
        output_state.source_value_id != event.source_value_id ||
        output_state.context_pu_identity_id !=
            event.context_pu_identity_id ||
        output_state.context_callsite_id !=
            event.context_callsite_id ||
        output_state.scheme != DSL_FHE_SCHEME_CKKS ||
        output_state.value_class !=
            DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
        (ordinal == 0 &&
         (operation.input_state_id != 0 ||
          output_state.state_role !=
              DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH ||
          (output_state.pending_actions &
               DSL_FHE_CKKS_PENDING_BOOTSTRAP) == 0 ||
          output_state.pending_bootstrap_reason !=
              DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH)) ||
        (ordinal > 0 &&
         (operation.input_state_id != previous_state[key] ||
          output_state.state_role !=
              (ordinal == 5 ? DSL_FHE_CONTEXT_STATE_ROLE_RESULT :
                              DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION) ||
          output_state.pending_actions != 0)) ||
        (ordinal > 0 && operation.range_id != context_range[key]) ||
        (ordinal >= 2 && ordinal <= 4 &&
         (operation.stage_id != ordinal - 1 ||
          operation.parameter_tcon == 0)) ||
        (ordinal == 1 && (operation.stage_id != 0 ||
                          operation.parameter_tcon == 0)) ||
        ((ordinal == 0 || ordinal == 5) &&
         (operation.stage_id != 0 ||
          operation.parameter_tcon != 0)))
      return Report(diagnostic, "ReLU range or CKKS state chain disagrees");

    previous_state[key] = operation.output_state_id;
    context_range[key] = operation.range_id;
    VHO_FHE_CKKS_RELU_PLAN_STEP step = {
      event, operation.id, range.id, output_state.id
    };
    collected.push_back(step);
  }
  for (std::map<CONTEXT_KEY, UINT32>::const_iterator it =
           context_count.begin(); it != context_count.end(); ++it) {
    if (it->second != 6)
      return Report(diagnostic, "ReLU context lacks six operations");
  }
  if (collected.size() != DSL_FHE_Materialization_Operation_Count())
    return Report(diagnostic, "materialization table has extra or missing rows");
  plans->swap(collected);
  return TRUE;
}
