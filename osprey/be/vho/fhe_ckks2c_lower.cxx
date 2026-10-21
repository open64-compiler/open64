/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: lower one active PU's provider-independent CKKS event groups to
 * standard-WHIRL calls for the frozen public FHE ABI. Provider-private CKKS C
 * emission remains a sibling output; this file never lowers each primitive to
 * a public call and never changes mapped-image layouts or runtime ABI types.
 *
 * A source context normally owns one private CKKS group. ReLU owns six
 * private groups but lowers them as one block so bootstrap/polynomial
 * intermediates remain local handles rather than public projections.
 *
 * Design references:
 *   doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D4
 *   doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md
 *   doc/FHE-SYNC6-D4-GROUPED-LOWERING-HANDOFF.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#include "fhe_ckks2c_lower.h"

#include <algorithm>
#include <cstring>
#include <map>
#include <set>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_semantic_runtime_lower.h"
#include "pu_info.h"
#include "stab.h"
#include "wn.h"

namespace {

struct Context_Key {
  uint32_t source_value_id;
  uint32_t context_pu_identity_id;
  uint32_t context_callsite_id;

  /* Keep one deterministic order without relying on pointer identity. */
  bool operator<(const Context_Key &other) const
  {
    if (source_value_id != other.source_value_id)
      return source_value_id < other.source_value_id;
    if (context_pu_identity_id != other.context_pu_identity_id)
      return context_pu_identity_id < other.context_pu_identity_id;
    return context_callsite_id < other.context_callsite_id;
  }
};

struct Prepared_Group {
  std::vector<uint32_t> group_indices;
  const VHO_FHE_CKKS2C_GROUP *first_group;
  const VHO_FHE_CKKS2C_GROUP *final_group;
  uint32_t operation_kind;
  std::vector<DSL_IR_VALUE_ID> operands;
  std::string bound_role;
  std::vector<DSL_IR_NATIVE_GROUP_MEMBER_REQUEST> members;
  DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
  SRCPOS source_position;
  VHO_FHE_RUNTIME_CALL_SEQUENCE sequence;

  Prepared_Group() : first_group(NULL), final_group(NULL), operation_kind(0),
                     source_position(0)
  {
    memset(&projection, 0, sizeof(projection));
    VHO_FHE_Runtime_Call_Sequence_Init(&sequence);
  }
};

/* Emit one stable materialization diagnostic without changing managed IR. */
bool Report(FILE *diagnostic, uint32_t id, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHECKKS2C-LOWER-001: id=%u %s\n", id, message);
  return false;
}

/* Apply the source anchor to every statement in a detached failure path. */
void Set_Source_Position(WN *tree, SRCPOS position)
{
  if (tree == NULL)
    return;
  OPERATOR opr = WN_operator(tree);
  if ((OPERATOR_is_stmt(opr) || OPERATOR_is_scf(opr)) && opr != OPR_BLOCK)
    WN_Set_Linenum(tree, position);
  if (opr == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL;
         statement = WN_next(statement))
      Set_Source_Position(statement, position);
    return;
  }
  for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid)
    Set_Source_Position(WN_kid(tree, kid), position);
}

/* Return from the active PU after evaluating a failed status and output. */
WN *Build_Failure(ST_IDX status_st, ST_IDX output_st,
                  void *context, FILE *diagnostic)
{
  (void)diagnostic;
  SRCPOS *source_position = static_cast<SRCPOS *>(context);
  if (source_position == NULL || *source_position == 0)
    return NULL;
  WN *block = WN_CreateBlock();
  TYPE_ID status_mtype = TY_mtype(ST_type(status_st));
  WN_INSERT_BlockLast(
      block, WN_CreateEval(WN_CreateLdid(
                 OPR_LDID, status_mtype, status_mtype, 0,
                 status_st, ST_type(status_st))));
  if (output_st != ST_IDX_ZERO) {
    TYPE_ID output_mtype = TY_mtype(ST_type(output_st));
    WN_INSERT_BlockLast(
        block, WN_CreateEval(WN_CreateLdid(
                   OPR_LDID, output_mtype, output_mtype, 0,
                   output_st, ST_type(output_st))));
  }
  WN_INSERT_BlockLast(block, WN_CreateReturn());
  Set_Source_Position(block, *source_position);
  return block;
}

/* Resolve one logical value's native STID from the authoritative PU tree. */
WN *Find_Definition(PU_Info *pu_info, WN *tree, DSL_IR_VALUE_ID value_id)
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
  for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
    WN *found = Find_Definition(pu_info, WN_kid(tree, kid), value_id);
    if (found != NULL)
      return found;
  }
  return NULL;
}

/* Copy one logical node's ordered direct operand values. */
bool Node_Operands(const DSL_IR_NODE_RECORD &node,
                   std::vector<DSL_IR_VALUE_ID> *operands)
{
  if (operands == NULL ||
      (node.operand_count != 0 && node.first_operand_reference_id == 0))
    return false;
  std::vector<DSL_IR_VALUE_ID> candidate;
  for (uint32_t ordinal = 0; ordinal < node.operand_count; ++ordinal) {
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    if (!DSL_IR_Image_Get_Value_Reference(
            node.first_operand_reference_id + ordinal, &reference) ||
        reference.owner_node_id != node.id ||
        reference.ordinal != ordinal || reference.value_id == 0 ||
        reference.flags != 0 || reference.reserved != 0)
      return false;
    candidate.push_back(reference.value_id);
  }
  operands->swap(candidate);
  return true;
}

/* Map one non-ReLU source semantic to its sole public ABI operation. */
bool Single_Operation_Kind(uint32_t logical_operator, uint32_t *kind)
{
  if (kind == NULL)
    return false;
  switch (logical_operator) {
  case OPR_DSLCONV2D:
    *kind = VHO_FHE_RUNTIME_OP_CONV2D_PLAIN;
    return true;
  case OPR_DSLRESIDUALADD:
    *kind = VHO_FHE_RUNTIME_OP_RESIDUAL_ADD;
    return true;
  case OPR_DSLGLOBALAVGPOOL2D:
    *kind = VHO_FHE_RUNTIME_OP_AVERAGE_POOL;
    return true;
  case OPR_DSLLINEAR:
    *kind = VHO_FHE_RUNTIME_OP_LINEAR_PLAIN;
    return true;
  default:
    return false;
  }
}

/* Resolve one private group to exactly one image-derived public descriptor. */
bool Find_Public_Route(
    const VHO_FHE_CKKS2C_GROUP &group,
    const std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> &routes,
    VHO_FHE_CKKS2C_PUBLIC_ROUTE *route)
{
  if (route == NULL)
    return false;
  uint32_t matches = 0;
  for (uint32_t index = 0; index < routes.size(); ++index) {
    const VHO_FHE_CKKS2C_PUBLIC_ROUTE &candidate = routes[index];
    if (candidate.specialized_owner_pu_st != group.owner_pu_st ||
        candidate.specialized_source_value_id != group.source_value_id ||
        candidate.context_pu_identity_id != group.context_pu_identity_id ||
        candidate.context_callsite_id != group.context_callsite_id ||
        candidate.origin_static_ordinal != group.origin_static_ordinal)
      continue;
    *route = candidate;
    ++matches;
  }
  return matches == 1;
}

/* Sort a small context's group indices by frozen public static ordinal. */
void Sort_Context_Indices(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    std::vector<uint32_t> *indices)
{
  for (uint32_t left = 0; left < indices->size(); ++left)
    for (uint32_t right = left + 1; right < indices->size(); ++right)
      if (groups[(*indices)[right]].origin_static_ordinal <
          groups[(*indices)[left]].origin_static_ordinal)
        std::swap((*indices)[left], (*indices)[right]);
}

/* Assign the frozen public operation kind to every context-owned group. */
bool Assign_Operation_Kinds(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    std::vector<uint32_t> *kinds, FILE *diagnostic)
{
  if (kinds == NULL)
    return false;
  std::map<Context_Key, std::vector<uint32_t> > by_context;
  for (uint32_t index = 0; index < groups.size(); ++index) {
    Context_Key key = {groups[index].source_value_id,
                       groups[index].context_pu_identity_id,
                       groups[index].context_callsite_id};
    by_context[key].push_back(index);
  }
  std::vector<uint32_t> candidate(groups.size(), 0);
  const uint32_t relu_kinds[6] = {
      VHO_FHE_RUNTIME_OP_BOOTSTRAP,
      VHO_FHE_RUNTIME_OP_RELU_NORMALIZE,
      VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE,
      VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE,
      VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE,
      VHO_FHE_RUNTIME_OP_RELU_RECONSTRUCT};
  for (std::map<Context_Key, std::vector<uint32_t> >::iterator context =
           by_context.begin(); context != by_context.end(); ++context) {
    std::vector<uint32_t> &indices = context->second;
    const VHO_FHE_CKKS2C_GROUP &first = groups[indices[0]];
    if (first.source_logical_operator == OPR_DSLRELU) {
      if (indices.size() != 6)
        return Report(diagnostic, first.source_value_id,
                      "ReLU context does not own six groups");
      Sort_Context_Indices(groups, &indices);
      for (uint32_t ordinal = 0; ordinal < 6; ++ordinal) {
        if (ordinal != 0 &&
            groups[indices[ordinal - 1]].origin_static_ordinal + 1 !=
                groups[indices[ordinal]].origin_static_ordinal)
          return Report(diagnostic, first.source_value_id,
                        "ReLU public ordinals are not contiguous");
        candidate[indices[ordinal]] = relu_kinds[ordinal];
      }
    } else {
      if (indices.size() != 1 ||
          !Single_Operation_Kind(first.source_logical_operator,
                                 &candidate[indices[0]]))
        return Report(diagnostic, first.source_value_id,
                      "source context does not own one supported group");
    }
  }
  kinds->swap(candidate);
  return true;
}

/* Prove that one private group really depends on a required public input. */
bool Group_References_Source(const VHO_FHE_CKKS2C_GROUP &group,
                             DSL_IR_VALUE_ID value_id)
{
  for (uint32_t step = 0; step < group.plan.steps.size(); ++step)
    for (uint32_t operand = 0;
         operand < group.plan.steps[step].operands.size(); ++operand)
      if (group.plan.steps[step].operands[operand].kind ==
              VHO_FHE_CKKS_PLAN_SOURCE_VALUE &&
          group.plan.steps[step].operands[operand].reference == value_id)
        return true;
  return false;
}

/* Derive public operands from source semantics and ordered ReLU group flow. */
bool Prepare_Operands(
    const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const std::vector<uint32_t> &kinds, uint32_t index,
    std::vector<DSL_IR_VALUE_ID> *operands, std::string *bound_role,
    FILE *diagnostic)
{
  const VHO_FHE_CKKS2C_GROUP &group = groups[index];
  DSL_IR_NODE_RECORD source_node;
  if (operands == NULL || bound_role == NULL ||
      !DSL_IR_Image_Get_Node(group.source_node_id, &source_node) ||
      source_node.result_value_id != group.source_value_id)
    return Report(diagnostic, group.source_value_id,
                  "source node is unavailable");
  if (group.source_logical_operator != OPR_DSLRELU)
    return Node_Operands(source_node, operands) ||
           Report(diagnostic, group.source_value_id,
                  "source operands are malformed");

  std::vector<uint32_t> context;
  for (uint32_t candidate = 0; candidate < groups.size(); ++candidate)
    if (groups[candidate].source_value_id == group.source_value_id &&
        groups[candidate].context_pu_identity_id ==
            group.context_pu_identity_id &&
        groups[candidate].context_callsite_id == group.context_callsite_id)
      context.push_back(candidate);
  Sort_Context_Indices(groups, &context);
  std::vector<DSL_IR_VALUE_ID> source_operands;
  if (context.size() != 6 || !Node_Operands(source_node, &source_operands) ||
      source_operands.size() != 1)
    return Report(diagnostic, group.source_value_id,
                  "ReLU source/context operands are malformed");
  uint32_t ordinal = 0;
  while (ordinal < context.size() && context[ordinal] != index)
    ++ordinal;
  if (ordinal == context.size())
    return Report(diagnostic, group.source_value_id,
                  "ReLU group is absent from its context");
  if (ordinal == 0)
    operands->push_back(source_operands[0]);
  else if (ordinal == 5) {
    operands->push_back(groups[context[0]].group_output_value_id);
    operands->push_back(groups[context[4]].group_output_value_id);
  } else {
    operands->push_back(groups[context[ordinal - 1]].group_output_value_id);
  }
  for (uint32_t operand = 0; operand < operands->size(); ++operand)
    if (!Group_References_Source(group, (*operands)[operand]))
      return Report(diagnostic, group.source_value_id,
                    "ReLU group does not consume its public input");
  if (kinds[index] == VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE) {
    static const char *roles[3] = {
        "fhe.relu.coefficient.stage0",
        "fhe.relu.coefficient.stage1",
        "fhe.relu.coefficient.stage2"};
    *bound_role = roles[ordinal - 2];
  }
  return true;
}

/* Release caller-owned detached blocks after any precommit rejection. */
void Delete_Detached_Blocks(std::vector<Prepared_Group> *prepared)
{
  if (prepared == NULL)
    return;
  for (uint32_t index = 0; index < prepared->size(); ++index)
    if ((*prepared)[index].sequence.block != NULL) {
      WN_DELETE_Tree((*prepared)[index].sequence.block);
      (*prepared)[index].sequence.block = NULL;
    }
}

}  // namespace

/* Build and atomically commit one standard-WHIRL block per source context. */
bool VHO_FHE_CKKS2C_Lower_Active_PU(
    PU_Info *pu_info, const std::vector<VHO_FHE_CKKS2C_GROUP> &groups,
    const std::vector<VHO_FHE_CKKS2C_PUBLIC_ROUTE> &routes,
    FILE *diagnostic, VHO_FHE_CKKS2C_LOWER_RESULT *result)
{
  if (pu_info == NULL || pu_info != Current_PU_Info || groups.empty() ||
      routes.empty() || result == NULL)
    return Report(diagnostic, 0, "active-PU lowering request is incomplete");
  const ST_IDX owner = PU_Info_proc_sym(pu_info);
  WN *tree = PU_Info_tree_ptr(pu_info);
  if (owner == ST_IDX_ZERO || tree == NULL)
    return Report(diagnostic, 0, "active PU tree is unavailable");

  std::vector<uint32_t> kinds;
  if (!Assign_Operation_Kinds(groups, &kinds, diagnostic))
    return false;
  std::vector<uint32_t> public_ordinals(groups.size(), 0);
  for (uint32_t index = 0; index < groups.size(); ++index) {
    VHO_FHE_CKKS2C_PUBLIC_ROUTE route;
    if (!Find_Public_Route(groups[index], routes, &route) ||
        route.operation_kind != kinds[index] ||
        route.public_static_ordinal == 0)
      return Report(diagnostic, groups[index].source_value_id,
                    "private group has no exact public descriptor route");
    public_ordinals[index] = route.public_static_ordinal;
  }

  /* A non-ReLU context owns one private group. A ReLU context owns six
   * private groups but one public block so its intermediate handles stay
   * local and never acquire fabricated runtime projections. */
  std::map<Context_Key, std::vector<uint32_t> > context_groups;
  for (uint32_t index = 0; index < groups.size(); ++index) {
    Context_Key key = {groups[index].source_value_id,
                       groups[index].context_pu_identity_id,
                       groups[index].context_callsite_id};
    context_groups[key].push_back(index);
  }
  std::vector<Prepared_Group> prepared(context_groups.size());
  uint32_t prepared_index = 0;
  for (std::map<Context_Key, std::vector<uint32_t> >::iterator context =
           context_groups.begin(); context != context_groups.end();
       ++context, ++prepared_index) {
    Prepared_Group &entry = prepared[prepared_index];
    entry.group_indices = context->second;
    Sort_Context_Indices(groups, &entry.group_indices);
    entry.first_group = &groups[entry.group_indices.front()];
    entry.final_group = &groups[entry.group_indices.back()];
    entry.operation_kind = kinds[entry.group_indices.front()];
    if (entry.first_group->source_logical_operator == OPR_DSLRELU) {
      if (entry.group_indices.size() != 6)
        return Report(diagnostic, entry.first_group->source_value_id,
                      "ReLU replacement does not own six private groups");
      for (uint32_t ordinal = 1; ordinal < entry.group_indices.size();
           ++ordinal)
        if (public_ordinals[entry.group_indices[ordinal - 1]] + 1 !=
            public_ordinals[entry.group_indices[ordinal]])
          return Report(diagnostic, entry.first_group->source_value_id,
                        "ReLU public descriptor routes are not contiguous");
    } else if (entry.group_indices.size() != 1) {
      return Report(diagnostic, entry.first_group->source_value_id,
                    "non-ReLU replacement owns multiple private groups");
    }
  }

  std::set<DSL_IR_VALUE_ID> member_values;
  for (uint32_t index = 0; index < prepared.size(); ++index) {
    Prepared_Group &entry = prepared[index];
    const VHO_FHE_CKKS2C_GROUP &source_group = *entry.first_group;
    if (source_group.owner_pu_st != owner ||
        source_group.source_operator_version != 2)
      return Report(diagnostic, index + 1,
                    "replacement owner or source version is invalid");
    DSL_IR_NODE_RECORD source_node;
    if (!DSL_IR_Image_Get_Node(source_group.source_node_id, &source_node) ||
        source_node.result_value_id != source_group.source_value_id ||
        !Node_Operands(source_node, &entry.operands))
      return Report(diagnostic, source_group.source_value_id,
                    "replacement source operands are malformed");
    if (source_group.source_logical_operator == OPR_DSLRELU &&
        entry.operands.size() != 1)
      return Report(diagnostic, source_group.source_value_id,
                    "ReLU replacement does not have one public input");
    if (!DSL_Runtime_Interface_Image_Find_Value(
            owner, entry.final_group->group_output_value_id,
            &entry.projection)) {
      Delete_Detached_Blocks(&prepared);
      return Report(diagnostic, entry.final_group->group_output_value_id,
                    "final source replacement projection is absent");
    }
    for (uint32_t selected = 0; selected < entry.group_indices.size();
         ++selected) {
      const uint32_t group_index = entry.group_indices[selected];
      const VHO_FHE_CKKS2C_GROUP &group = groups[group_index];
      std::vector<DSL_IR_VALUE_ID> checked_operands;
      std::string checked_bound_role;
      if (group.owner_pu_st != owner ||
          group.source_value_id != source_group.source_value_id ||
          group.source_node_id != source_group.source_node_id ||
          group.source_logical_operator !=
              source_group.source_logical_operator ||
          group.source_operator_version !=
              source_group.source_operator_version ||
          group.result_node_ids.empty() ||
          group.result_node_ids.size() != group.result_value_ids.size() ||
          group.result_value_ids.size() != group.plan.steps.size() ||
          group.group_output_value_id != group.result_value_ids.back() ||
          group.plan.group_output_steps.size() != 1 ||
          group.plan.group_output_steps[0] + 1 != group.plan.steps.size() ||
          !Prepare_Operands(groups, kinds, group_index, &checked_operands,
                            &checked_bound_role, diagnostic)) {
        Delete_Detached_Blocks(&prepared);
        return Report(diagnostic, group_index + 1,
                      "private group identity is inconsistent");
      }
      for (uint32_t member = 0; member < group.result_value_ids.size();
           ++member) {
        DSL_IR_VALUE_RECORD value;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
        WN *definition = Find_Definition(
            pu_info, tree, group.result_value_ids[member]);
        if (definition == NULL ||
            !member_values.insert(group.result_value_ids[member]).second ||
            !DSL_IR_Image_Get_Value(group.result_value_ids[member], &value) ||
            !DSL_IR_Image_Get_Node(group.result_node_ids[member], &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor(
                node.opcode_descriptor_id, &opcode) ||
            value.producer_node_id != node.id ||
            node.result_value_id != value.id ||
            value.id != group.result_value_ids[member] ||
            node.id != group.result_node_ids[member] ||
            opcode.logical_operator !=
                group.plan.steps[member].logical_operator ||
            opcode.version != group.plan.steps[member].operator_version) {
          Delete_Detached_Blocks(&prepared);
          return Report(diagnostic, group.result_value_ids[member],
                        "private member definition is inconsistent");
        }
        DSL_IR_NATIVE_GROUP_MEMBER_REQUEST request;
        memset(&request, 0, sizeof(request));
        request.native_definition = definition;
        request.node_id = node.id;
        request.value_id = value.id;
        request.expected_operator =
            static_cast<DSL_OPERATOR>(opcode.logical_operator);
        request.expected_version = opcode.version;
        entry.members.push_back(request);
      }
    }
    entry.source_position = WN_Get_Linenum(
        entry.members.back().native_definition);
    if (entry.source_position == 0)
      entry.source_position = WN_Get_Linenum(
          entry.members.front().native_definition);
    if (entry.source_position == 0) {
      Delete_Detached_Blocks(&prepared);
      return Report(diagnostic, index + 1,
                    "group has no source position");
    }
  }

  for (uint32_t index = 0; index < prepared.size(); ++index) {
    Prepared_Group &entry = prepared[index];
    VHO_FHE_RUNTIME_HANDLE_BINDING output;
    WN *result_definition = NULL;
    bool built = false;
    if (entry.first_group->source_logical_operator == OPR_DSLRELU) {
      built = VHO_FHE_Runtime_Build_Relu_Sequence(
          pu_info, entry.operands[0],
          public_ordinals[entry.group_indices.front()],
          entry.source_position, Build_Failure, &entry.source_position,
          diagnostic,
          &entry.sequence);
    } else {
      VHO_FHE_RUNTIME_OPERATION_REQUEST operation;
      memset(&operation, 0, sizeof(operation));
      operation.operation_kind = entry.operation_kind;
      operation.static_ordinal =
          public_ordinals[entry.group_indices.front()];
      operation.operand_value_ids = &entry.operands[0];
      operation.operand_count = entry.operands.size();
      built = VHO_FHE_Runtime_Build_Operation_Sequence(
          pu_info, &operation, entry.source_position, Build_Failure,
          &entry.source_position, diagnostic, &entry.sequence);
    }
    if (!built ||
        !VHO_FHE_Runtime_Resolve_Value_Handle(
            pu_info, entry.final_group->group_output_value_id, diagnostic,
            &output) ||
        output.handle_st != entry.projection.handle_st ||
        output.handle_ty != entry.projection.handle_ty ||
        !VHO_FHE_Runtime_Finalize_Projected_Output(
            &output, entry.source_position, &entry.sequence,
            &result_definition) || result_definition == NULL) {
      Delete_Detached_Blocks(&prepared);
      return Report(diagnostic, index + 1,
                    "standard runtime block construction failed");
    }
  }

  std::vector<DSL_IR_NATIVE_GROUP_LOWER_REQUEST> requests(prepared.size());
  std::vector<DSL_IR_NATIVE_GROUP_LOWER_RESULT> lowered(prepared.size());
  for (uint32_t index = 0; index < prepared.size(); ++index) {
    memset(&requests[index], 0, sizeof(requests[index]));
    requests[index].owner_pu_st = owner;
    requests[index].members = &prepared[index].members[0];
    requests[index].member_count = prepared[index].members.size();
    requests[index].group_output_value_id =
        prepared[index].final_group->group_output_value_id;
    requests[index].semantic_source_value_id =
        prepared[index].first_group->source_value_id;
    requests[index].standard_block = prepared[index].sequence.block;
    requests[index].relation.relation_kind =
        DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION;
    requests[index].relation.value_projection_id =
        prepared[index].projection.id;
  }
  if (!DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks(
          pu_info, &requests[0], requests.size(), diagnostic, &lowered[0])) {
    Delete_Detached_Blocks(&prepared);
    return Report(diagnostic, 0, "atomic grouped lowering rejected the PU");
  }

  VHO_FHE_CKKS2C_LOWER_RESULT counts = {0, 0, 0, 0, 0, 0, 0};
  counts.group_count = groups.size();
  counts.replacement_block_count = prepared.size();
  for (uint32_t index = 0; index < prepared.size(); ++index) {
    prepared[index].sequence.block = NULL;
    counts.retired_member_count += lowered[index].retired_member_count;
    counts.inserted_statement_count += lowered[index].inserted_statement_count;
    counts.standard_call_count += prepared[index].sequence.standard_call_count;
    counts.output_handle_count += prepared[index].sequence.output_handle_count;
    counts.status_check_count += prepared[index].sequence.status_check_count;
  }
  *result = counts;
  return true;
}
