/*
 * Copyright (C) 2026 Open64 Project
 *
 * Convert the specialized ResNet residual joins into explicit CKKS level
 * alignment and addition operations. The source operator remains inspectable
 * as lowered provenance while common/com atomically redirects all uses to the
 * final CKKS value. No provider call or POLY lowering is performed here.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include "fhe_ckks_residual_materialize.h"

#include <algorithm>
#include <set>
#include <string>
#include <vector>

#include "config_fhe.h"
#include "dsl_ckks_expand.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_ckks_event_coverage.h"
#include "fhe_ckks_expand.h"
#include "fhe_ckks_source_events.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "strtab.h"

namespace {

struct Residual_Job {
  VHO_FHE_CKKS_EVENT_IDENTITY event;
  DSL_IR_NODE_ID source_node_id;
  DSL_IR_VALUE_ID source_value_id;
  BOOL processed;
};

struct Residual_State {
  BOOL initialized;
  BOOL active;
  std::vector<Residual_Job> jobs;
  std::set<ST_IDX> processed_owners;
  UINT32 add_count;
  UINT32 modswitch_count;
  std::string report_final;
  std::string report_temp;

  /* Start each compiler/checkpoint lifetime with no borrowed program state. */
  Residual_State()
      : initialized(FALSE), active(FALSE), add_count(0),
        modswitch_count(0) {}
};

Residual_State Residual_state;

/* Emit one stable diagnostic for this bounded ResNet policy. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-RESIDUAL-001: %s\n", message);
  return FALSE;
}

/* Sort jobs by resident owner and source node for deterministic expansion. */
struct Job_Less {
  bool operator()(const Residual_Job &left,
                  const Residual_Job &right) const
  {
    if (left.event.owner_pu_st != right.event.owner_pu_st)
      return left.event.owner_pu_st < right.event.owner_pu_st;
    return left.source_node_id < right.source_node_id;
  }
};

/* Read one exact logical operand reference from a live source node. */
BOOL Operand(const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
             DSL_IR_VALUE_ID *value_id)
{
  DSL_IR_VALUE_REFERENCE_RECORD reference;
  if (value_id == NULL || ordinal >= node.operand_count ||
      !DSL_IR_Image_Get_Value_Reference(
          node.first_operand_reference_id + ordinal, &reference) ||
      reference.owner_node_id != node.id ||
      reference.ordinal != ordinal ||
      reference.value_id == DSL_IR_VALUE_INVALID_ID)
    return FALSE;
  *value_id = reference.value_id;
  return TRUE;
}

/* Locate a native definition by stable value identity in the active PU. */
WN *Find_Definition(PU_Info *pu_info, WN *tree,
                    DSL_IR_VALUE_ID value_id)
{
  if (tree == NULL)
    return NULL;
  if (WN_operator(tree) == OPR_STID) {
    DSL_IR_VALUE_RECORD value;
    if (DSL_IR_Image_Find_Definition_Value(pu_info, tree, &value) &&
        value.id == value_id)
      return tree;
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL;
         statement = WN_next(statement)) {
      WN *found = Find_Definition(pu_info, statement, value_id);
      if (found != NULL)
        return found;
    }
    return NULL;
  }
  for (INT kid = 0; kid < WN_kid_count(tree); ++kid) {
    WN *found = Find_Definition(pu_info, WN_kid(tree, kid), value_id);
    if (found != NULL)
      return found;
  }
  return NULL;
}

/* Collect either the five shared definitions or the nine resident variants.
 * Only the latter activates this production transformation. */
BOOL Initialize(FILE *diagnostic)
{
  if (Residual_state.initialized)
    return TRUE;
  std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> events;
  if (!VHO_FHE_CKKS_Collect_Source_Events(&events, diagnostic))
    return Report(diagnostic, "source event census cannot be collected");

  std::set<std::pair<ST_IDX, DSL_IR_NODE_ID> > definitions;
  std::vector<Residual_Job> jobs;
  for (size_t i = 0; i < events.size(); ++i) {
    DSL_IR_VALUE_RECORD result;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Value(events[i].source_value_id, &result) ||
        !DSL_IR_Image_Get_Node(result.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "source event cannot be resolved");
    if (opcode.logical_operator != OPR_DSLRESIDUALADD)
      continue;
    if (opcode.version != 2 || node.result_value_id != result.id ||
        node.operand_count != 2 || node.flags != DSL_IR_NODE_FLAG_NONE)
      return Report(diagnostic, "residual source contract is unsupported");
    definitions.insert(std::make_pair(events[i].owner_pu_st, node.id));
    Residual_Job job;
    job.event = events[i];
    job.source_node_id = node.id;
    job.source_value_id = result.id;
    job.processed = FALSE;
    jobs.push_back(job);
  }
  if (jobs.size() != 9 ||
      (definitions.size() != 5 && definitions.size() != 9))
    return Report(diagnostic,
                  "residual census is not nine contexts over five or nine definitions");
  std::sort(jobs.begin(), jobs.end(), Job_Less());
  Residual_state.jobs.swap(jobs);
  Residual_state.active = definitions.size() == 9;
  Residual_state.initialized = TRUE;
  return TRUE;
}

/* Initialize one append-only state template from an existing concrete state. */
VHO_FHE_CKKS_STEP_STATE Step_State(
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &input)
{
  VHO_FHE_CKKS_STEP_STATE output;
  output.state = input;
  output.state.id = DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
  output.state.value_id = DSL_IR_VALUE_INVALID_ID;
  output.state.state_version = 1;
  return output;
}

/* Prove the facts that must match before an explicit level repair is legal. */
BOOL Compatible_Operands(
    const DSL_IR_VALUE_RECORD &left_value,
    const DSL_IR_VALUE_RECORD &right_value,
    const DSL_IR_VALUE_RECORD &result_value,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &left,
    const DSL_FHE_CKKS_VALUE_STATE_RECORD &right)
{
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD left_descriptor;
  DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD right_descriptor;
  return left_value.ty == right_value.ty &&
         left_value.ty == result_value.ty &&
         left.scheme == DSL_FHE_SCHEME_CKKS &&
         right.scheme == DSL_FHE_SCHEME_CKKS &&
         left.value_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT &&
         right.value_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT &&
         left.encryption_descriptor_id == right.encryption_descriptor_id &&
         DSL_FHE_Get_Encryption_Descriptor(
             left.encryption_descriptor_id, &left_descriptor) &&
         DSL_FHE_Get_Encryption_Descriptor(
             right.encryption_descriptor_id, &right_descriptor) &&
         left_descriptor.config_id == right_descriptor.config_id &&
         left_descriptor.key_set_name == right_descriptor.key_set_name &&
         left.level >= 0 && right.level >= 0 &&
         left.scale_bits > 0 && left.scale_bits == right.scale_bits &&
         left.component_count == 2 && right.component_count == 2 &&
         left.precision_bits > 0 && right.precision_bits > 0 &&
         left.slot_count != 0 && left.slot_count == right.slot_count &&
         left.encrypted_layout_name != STR_IDX_ZERO &&
         left.encrypted_layout_name == right.encrypted_layout_name &&
         left.alignment_group == right.alignment_group &&
         left.pending_actions == 0 && right.pending_actions == 0 &&
         left.pending_bootstrap_reason == DSL_FHE_BOOTSTRAP_REASON_NONE &&
         right.pending_bootstrap_reason == DSL_FHE_BOOTSTRAP_REASON_NONE;
}

/* Expand one source residual into an optional modswitch and one ckks.add. */
BOOL Expand_Job(PU_Info *pu_info, WN *definition, Residual_Job *job,
                FILE *diagnostic)
{
  DSL_IR_NODE_RECORD node;
  DSL_IR_VALUE_RECORD source_value;
  DSL_IR_VALUE_RECORD operand_values[2];
  DSL_IR_VALUE_ID operand_ids[2];
  DSL_FHE_CKKS_VALUE_STATE_RECORD input_states[2];
  if (pu_info == NULL || definition == NULL || job == NULL ||
      !DSL_IR_Image_Get_Node(job->source_node_id, &node) ||
      node.flags != DSL_IR_NODE_FLAG_NONE || node.operand_count != 2 ||
      !DSL_IR_Image_Get_Value(job->source_value_id, &source_value) ||
      source_value.producer_node_id != node.id)
    return Report(diagnostic, "live residual definition disappeared");
  for (UINT32 i = 0; i < 2; ++i) {
    if (!Operand(node, i, &operand_ids[i]) ||
        !DSL_IR_Image_Get_Value(operand_ids[i], &operand_values[i]) ||
        !DSL_FHE_Plan_Find_Latest_CKKS_Value_State(
            operand_ids[i], &input_states[i]))
      return Report(diagnostic, "residual operand state is unavailable");
  }
  if (!Compatible_Operands(operand_values[0], operand_values[1],
                           source_value, input_states[0], input_states[1]))
    return Report(diagnostic, "residual operands are not CKKS-compatible");

  const INT32 target_level = input_states[0].level < input_states[1].level ?
      input_states[0].level : input_states[1].level;
  const INT32 aligned = input_states[0].level == input_states[1].level ?
      -1 : input_states[0].level > target_level ? 0 : 1;
  const UINT32 step_count = aligned < 0 ? 1 : 2;
  DSL_CKKS_EXPANSION_STEP steps[2];
  DSL_CKKS_EXPANSION_OPERAND operands[2][2];
  VHO_FHE_CKKS_STEP_STATE states[2];
  std::string names[2];
  char level_text[32];
  snprintf(level_text, sizeof(level_text), "%d", target_level);
  DSL_CKKS_EXPANSION_ATTRIBUTE modswitch_attribute = {
    "attr.target_level", level_text
  };
  memset(steps, 0, sizeof(steps));
  memset(operands, 0, sizeof(operands));

  UINT32 add_index = 0;
  if (aligned >= 0) {
    char name[96];
    snprintf(name, sizeof(name), "fhe_ckks_residual_s%u_align_%s",
             job->event.source_static_ordinal,
             aligned == 0 ? "left" : "right");
    names[0] = name;
    operands[0][0].kind = DSL_CKKS_EXPANSION_EXISTING_VALUE;
    operands[0][0].value_id = operand_ids[aligned];
    steps[0].dsl_operator = OPR_DSLCKKSMODSWITCH;
    steps[0].version = 1;
    steps[0].operands = operands[0];
    steps[0].operand_count = 1;
    steps[0].attributes = &modswitch_attribute;
    steps[0].attribute_count = 1;
    steps[0].result_name = names[0].c_str();
    steps[0].result_ty = operand_values[aligned].ty;
    steps[0].source_position = WN_Get_Linenum(definition);
    states[0] = Step_State(input_states[aligned]);
    states[0].state.level = target_level;
    add_index = 1;
  }

  char add_name[96];
  snprintf(add_name, sizeof(add_name), "fhe_ckks_residual_s%u_add",
           job->event.source_static_ordinal);
  names[add_index] = add_name;
  for (UINT32 i = 0; i < 2; ++i) {
    if ((INT32)i == aligned) {
      operands[add_index][i].kind = DSL_CKKS_EXPANSION_PRIOR_STEP;
      operands[add_index][i].step_index = 0;
    } else {
      operands[add_index][i].kind = DSL_CKKS_EXPANSION_EXISTING_VALUE;
      operands[add_index][i].value_id = operand_ids[i];
    }
  }
  steps[add_index].dsl_operator = OPR_DSLCKKSADD;
  steps[add_index].version = 1;
  steps[add_index].operands = operands[add_index];
  steps[add_index].operand_count = 2;
  steps[add_index].result_name = names[add_index].c_str();
  steps[add_index].result_ty = source_value.ty;
  steps[add_index].source_position = WN_Get_Linenum(definition);
  states[add_index] = Step_State(input_states[0]);
  states[add_index].state.level = target_level;
  states[add_index].state.component_count =
      input_states[0].component_count > input_states[1].component_count ?
          input_states[0].component_count : input_states[1].component_count;
  states[add_index].state.precision_bits =
      input_states[0].precision_bits < input_states[1].precision_bits ?
          input_states[0].precision_bits : input_states[1].precision_bits;

  DSL_CKKS_EXPANSION_GROUP group;
  memset(&group, 0, sizeof(group));
  group.source_static_ordinal = job->event.source_static_ordinal;
  group.origin_static_ordinal = job->event.source_static_ordinal;
  group.step_count = step_count;
  DSL_CKKS_EXPANSION_CONTEXT context;
  memset(&context, 0, sizeof(context));
  context.context_pu_identity_id = job->event.context_pu_identity_id;
  context.context_callsite_id = job->event.context_callsite_id;
  context.origin_owner_pu_st = job->event.owner_pu_st;
  context.origin_source_value_id = job->event.source_value_id;
  DSL_CKKS_EXPANSION_REQUEST request;
  memset(&request, 0, sizeof(request));
  request.source_definition = definition;
  request.source_value_id = job->source_value_id;
  request.expected_source_operator = OPR_DSLRESIDUALADD;
  request.expected_source_version = 2;
  request.groups = &group;
  request.group_count = 1;
  request.steps = steps;
  request.step_count = step_count;
  request.contexts = &context;
  request.context_count = 1;
  request.final_step_index = add_index;
  DSL_CKKS_EXPANSION_STEP_RESULT results[2];
  memset(results, 0, sizeof(results));
  if (!VHO_FHE_CKKS_Expand_And_Bind_States(
          pu_info, &request, states, step_count, diagnostic, results))
    return Report(diagnostic, "native residual expansion was rejected");
  ++Residual_state.add_count;
  if (aligned >= 0)
    ++Residual_state.modswitch_count;
  job->processed = TRUE;
  return TRUE;
}

/* Expand every residual definition owned by the active resident PU. */
BOOL Process_Owner(PU_Info *pu_info, WN *tree, FILE *diagnostic)
{
  const ST_IDX owner = PU_Info_proc_sym(pu_info);
  if (Residual_state.processed_owners.find(owner) !=
      Residual_state.processed_owners.end())
    return Report(diagnostic, "residual owner callback was repeated");
  for (size_t i = 0; i < Residual_state.jobs.size(); ++i) {
    Residual_Job &job = Residual_state.jobs[i];
    if (job.event.owner_pu_st != owner)
      continue;
    WN *definition = Find_Definition(
        pu_info, tree, job.source_value_id);
    if (definition == NULL ||
        !Expand_Job(pu_info, definition, &job, diagnostic))
      return FALSE;
  }
  Residual_state.processed_owners.insert(owner);
  return TRUE;
}

}  // namespace

/* Initialize the census while all source definitions are still live. */
BOOL
VHO_FHE_CKKS_Residual_Materialization_Gatekeeper(
    PU_Info *pu_info, WN *tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  return pu_info != NULL && tree != NULL && options != NULL &&
         Initialize(diagnostic);
}

/* Consume the concrete Conv outputs produced earlier in this callback. */
BOOL
VHO_FHE_CKKS_Residual_Materialization_Pass(
    PU_Info *pu_info, WN **tree,
    const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
  if (pu_info == NULL || tree == NULL || *tree == NULL || options == NULL ||
      !Initialize(diagnostic))
    return FALSE;
  return !Residual_state.active || Process_Owner(pu_info, *tree, diagnostic);
}

/* Verify exact real-model coverage and register a deterministic report. */
BOOL
VHO_FHE_CKKS_Residual_Materialization_Finalize(FILE *diagnostic)
{
  if (!Residual_state.active)
    return TRUE;
  UINT32 processed = 0;
  UINT32 live = 0;
  UINT32 lowered = 0;
  for (size_t i = 0; i < Residual_state.jobs.size(); ++i)
    if (Residual_state.jobs[i].processed)
      ++processed;
  for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Node(id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(
            node.opcode_descriptor_id, &opcode))
      return Report(diagnostic, "logical node census cannot be read");
    if (opcode.logical_operator != OPR_DSLRESIDUALADD)
      continue;
    if ((node.flags & DSL_IR_NODE_FLAG_LOWERED) != 0)
      ++lowered;
    else if ((node.flags & (DSL_IR_NODE_FLAG_RETIRED |
                            DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0)
      ++live;
  }
  const BOOL event_image_valid =
      DSL_CKKS_Event_Image_Validate(diagnostic);
  const BOOL plan_image_valid =
      DSL_FHE_Plan_Image_Validate(diagnostic);
  if (processed != 9 || Residual_state.add_count != 9 ||
      Residual_state.modswitch_count != 9 || live != 0 || lowered != 9 ||
      !event_image_valid || !plan_image_valid) {
    if (diagnostic != NULL)
      fprintf(diagnostic,
              "CFHEIR-RESIDUAL-001: measured processed=%u adds=%u "
              "modswitches=%u live=%u lowered=%u event_image=%u "
              "plan_image=%u\n",
              processed, Residual_state.add_count,
              Residual_state.modswitch_count, live, lowered,
              event_image_valid, plan_image_valid);
    return Report(diagnostic,
                  "coverage is not 9 adds, 9 alignments, and 9 lowered sources");
  }

  const char *output = VHO_FHE_Materialization_Checkpoint_Output;
  if (output == NULL || output[0] == '\0')
    return Report(diagnostic, "checkpoint output path is unavailable");
  Residual_state.report_final = std::string(output) +
                                ".residual-report.txt";
  Residual_state.report_temp = Residual_state.report_final + ".tmp";
  FILE *report = fopen(Residual_state.report_temp.c_str(), "w");
  if (report == NULL)
    return Report(diagnostic, "residual report temp cannot be created");
  fprintf(report, "FHE SYNC-6 residual materialization report\n");
  fprintf(report, "source_contexts=9\n");
  fprintf(report, "ckks_adds=9\n");
  fprintf(report, "ckks_modswitches=9\n");
  fprintf(report, "projection_joins_already_aligned=0\n");
  fprintf(report, "source_residual_nodes_live=0\n");
  BOOL valid = fclose(report) == 0;
  if (!valid || !VHO_FHE_Materialize_Checkpoint_Register_Artifact(
          Residual_state.report_temp.c_str(),
          Residual_state.report_final.c_str())) {
    remove(Residual_state.report_temp.c_str());
    return Report(diagnostic, "residual report cannot join checkpoint");
  }
  fprintf(diagnostic,
          "FHE-SYNC6-RESIDUAL: contexts=9 adds=9 modswitches=9 live=0\n");
  return TRUE;
}

/* Reset all process-local state; checkpoint rollback owns registered files. */
void
VHO_FHE_CKKS_Residual_Materialization_Complete(BOOL committed)
{
  (void)committed;
  Residual_state = Residual_State();
}
