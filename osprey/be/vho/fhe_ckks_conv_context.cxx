/*
 * Copyright (C) 2026 Open64 Project
 *
 * Read the existing DSL/FHE identity tables into exact CKKS Conv contexts.
 * Asset production and native expansion consume this preflighted inventory.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "fhe_ckks_conv_context.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <set>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "fhe_semantic_runtime_lower.h"
#include "strtab.h"
#include "symtab.h"

namespace {

struct Context_Key {
  ST_IDX owner;
  DSL_IR_NODE_ID node;
  DSL_PU_SOURCE_IDENTITY_ID identity;
  DSL_CALLSITE_METADATA_ID callsite;

  /* Establish one complete source/context ordering for duplicate checks. */
  bool operator<(const Context_Key &other) const
  {
    if (owner != other.owner) return owner < other.owner;
    if (node != other.node) return node < other.node;
    if (identity != other.identity) return identity < other.identity;
    return callsite < other.callsite;
  }
};

/* Emit one stable collector rejection without publishing partial output. */
BOOL Report(FILE *diagnostic, const char *message)
{
  if (diagnostic != NULL)
    fprintf(diagnostic, "CFHEIR-CONV-CONTEXT-001: %s\n", message);
  return FALSE;
}

/* Resolve one node attribute as a borrowed canonical string. */
BOOL Attribute(const DSL_IR_NODE_RECORD &node, const char *name,
               const char **value)
{
  if (name == NULL || value == NULL)
    return FALSE;
  *value = NULL;
  for (UINT32 i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute(node.first_attribute_id + i,
                                    &attribute) ||
        attribute.name == STR_IDX_ZERO)
      return FALSE;
    if (strcmp(Index_To_Str(attribute.name), name) == 0) {
      if (*value != NULL || attribute.value == STR_IDX_ZERO)
        return FALSE;
      *value = Index_To_Str(attribute.value);
    }
  }
  return *value != NULL;
}

/* Parse a fixed-rank canonical comma-separated unsigned integer tuple.
 * Zero is syntactically valid here because Conv padding may be zero; callers
 * impose nonzero constraints on dimensions, kernels, strides, and groups. */
BOOL Unsigned_Tuple(const char *text, UINT32 count, UINT32 *values)
{
  if (text == NULL || count == 0 || values == NULL)
    return FALSE;
  const char *cursor = text;
  if (*cursor == '[')
    ++cursor;
  for (UINT32 i = 0; i < count; ++i) {
    if (!isdigit((unsigned char)*cursor))
      return FALSE;
    char *end = NULL;
    unsigned long parsed = strtoul(cursor, &end, 10);
    if (end == cursor || parsed > UINT32_MAX)
      return FALSE;
    values[i] = static_cast<UINT32>(parsed);
    cursor = end;
    if (i + 1 != count) {
      if (*cursor != ',')
        return FALSE;
      ++cursor;
    }
  }
  if (*cursor == ']')
    ++cursor;
  return *cursor == '\0';
}

/* Read an exact-rank canonical TensorDescriptorIR shape. */
BOOL Tensor_Shape(TY_IDX ty, UINT32 rank, UINT32 *values)
{
  TENSOR_DESCRIPTOR_RECORD descriptor;
  if (!TY_get_tensor_descriptor_record(ty, &descriptor) ||
      descriptor.rank != static_cast<INT32>(rank) ||
      descriptor.logical_shape == STR_IDX_ZERO ||
      !Unsigned_Tuple(Index_To_Str(descriptor.logical_shape), rank, values))
    return FALSE;
  for (UINT32 i = 0; i < rank; ++i)
    if (values[i] == 0)
      return FALSE;
  return TRUE;
}

/* Resolve one direct logical operand value by ordinal. */
BOOL Operand(const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
             DSL_IR_VALUE_ID *value_id)
{
  DSL_IR_VALUE_REFERENCE_RECORD reference;
  if (value_id == NULL || ordinal >= node.operand_count ||
      !DSL_IR_Image_Get_Value_Reference(
          node.first_operand_reference_id + ordinal, &reference) ||
      reference.owner_node_id != node.id || reference.ordinal != ordinal)
    return FALSE;
  *value_id = reference.value_id;
  return *value_id != DSL_IR_VALUE_INVALID_ID;
}

/* Prove one Conv's tensor geometry and convert source stride to the fixed
 * high-resolution recipe consumed by the CKKS planner. */
BOOL Geometry(const DSL_IR_NODE_RECORD &node,
              const DSL_IR_VALUE_RECORD &input,
              const DSL_IR_VALUE_RECORD &weight,
              const DSL_IR_VALUE_RECORD &result,
              UINT32 slot_count,
              VHO_FHE_CKKS_CONV_CONTEXT *context)
{
  UINT32 input_shape[4] = {0, 0, 0, 0};
  UINT32 weight_shape[4] = {0, 0, 0, 0};
  UINT32 result_shape[4] = {0, 0, 0, 0};
  const char *kernel_text = NULL;
  const char *stride_text = NULL;
  const char *padding_text = NULL;
  const char *dilation_text = NULL;
  const char *groups_text = NULL;
  const char *input_layout = NULL;
  const char *weight_layout = NULL;
  const char *output_layout = NULL;
  UINT32 kernel[2] = {0, 0};
  UINT32 stride[2] = {0, 0};
  UINT32 padding[2] = {0, 0};
  UINT32 dilation[2] = {0, 0};
  UINT32 groups[1] = {0};
  if (!Tensor_Shape(input.ty, 4, input_shape) ||
      !Tensor_Shape(weight.ty, 4, weight_shape) ||
      !Tensor_Shape(result.ty, 4, result_shape) ||
      !Attribute(node, "attr.kernel_shape", &kernel_text) ||
      !Attribute(node, "attr.stride", &stride_text) ||
      !Attribute(node, "attr.padding", &padding_text) ||
      !Attribute(node, "attr.dilation", &dilation_text) ||
      !Attribute(node, "attr.groups", &groups_text) ||
      !Attribute(node, "attr.input_layout", &input_layout) ||
      !Attribute(node, "attr.weight_layout", &weight_layout) ||
      !Attribute(node, "attr.output_layout", &output_layout) ||
      !Unsigned_Tuple(kernel_text, 2, kernel) ||
      !Unsigned_Tuple(stride_text, 2, stride) ||
      !Unsigned_Tuple(padding_text, 2, padding) ||
      !Unsigned_Tuple(dilation_text, 2, dilation) ||
      !Unsigned_Tuple(groups_text, 1, groups))
    return FALSE;

  const UINT32 expected_output = stride[0] == 1 ? input_shape[2] :
      input_shape[2] / 2;
  if (input_shape[0] != 1 || result_shape[0] != 1 ||
      input_shape[2] != input_shape[3] ||
      result_shape[2] != result_shape[3] ||
      weight_shape[0] != result_shape[1] ||
      weight_shape[1] != input_shape[1] ||
      weight_shape[2] != kernel[0] || weight_shape[3] != kernel[1] ||
      (kernel[0] != 1 && kernel[0] != 3) || kernel[1] != kernel[0] ||
      (stride[0] != 1 && stride[0] != 2) || stride[1] != stride[0] ||
      (stride[0] == 2 && (input_shape[2] & 1) != 0) ||
      padding[0] != kernel[0] / 2 || padding[1] != padding[0] ||
      dilation[0] != 1 || dilation[1] != 1 || groups[0] != 1 ||
      result_shape[2] != expected_output ||
      strcmp(input_layout, "NCHW") != 0 ||
      strcmp(weight_layout, "OIHW") != 0 ||
      strcmp(output_layout, "NCHW") != 0)
    return FALSE;

  VHO_FHE_CKKS_CONV_SHAPE &shape = context->high_resolution_shape;
  shape.batch = 1;
  shape.input_channels = input_shape[1];
  shape.output_channels = result_shape[1];
  shape.height = input_shape[2];
  shape.width = input_shape[3];
  shape.kernel_height = kernel[0];
  shape.kernel_width = kernel[1];
  shape.stride_height = 1;
  shape.stride_width = 1;
  shape.pad_top = padding[0];
  shape.pad_bottom = padding[0];
  shape.pad_left = padding[1];
  shape.pad_right = padding[1];
  shape.dilation_height = dilation[0];
  shape.dilation_width = dilation[1];
  shape.groups = groups[0];
  shape.slot_count = slot_count;
  context->source_stride_height = stride[0];
  context->source_stride_width = stride[1];
  return TRUE;
}

}  // namespace

/* Build the complete context inventory in temporary storage. No managed row,
 * native tree, symbol, type, or caller-owned vector changes on rejection. */
BOOL
VHO_FHE_CKKS_Collect_Conv_Contexts(
    const std::vector<VHO_FHE_CKKS_EVENT_IDENTITY> &events,
    UINT32 slot_count,
    std::vector<VHO_FHE_CKKS_CONV_CONTEXT> *contexts,
    FILE *diagnostic)
{
  if (events.empty() || slot_count == 0 ||
      (slot_count & (slot_count - 1)) != 0 || contexts == NULL ||
      !VHO_FHE_Runtime_Static_Schedule_Prepare(diagnostic))
    return Report(diagnostic, "event, slot, or schedule input is invalid");

  std::set<Context_Key> seen;
  std::vector<VHO_FHE_CKKS_CONV_CONTEXT> collected;
  for (size_t i = 0; i < events.size(); ++i) {
    const VHO_FHE_CKKS_EVENT_IDENTITY &event = events[i];
    DSL_IR_VALUE_RECORD result;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD schedule;
    if (!DSL_IR_Image_Get_Value(event.source_value_id, &result) ||
        !DSL_IR_Image_Get_Node(result.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,
                                            &opcode) ||
        !VHO_FHE_Runtime_Static_Schedule_Find(node.id, &schedule))
      return Report(diagnostic, "event source cannot be resolved");
    if (opcode.logical_operator != OPR_DSLCONV2D)
      continue;
    if (opcode.version != 2 || node.result_value_id != result.id ||
        node.operand_count != 3 ||
        schedule.owner_pu_st != event.owner_pu_st ||
        schedule.result_value_id != event.source_value_id ||
        schedule.logical_operator != OPR_DSLCONV2D ||
        schedule.static_evaluation_count != 1 ||
        schedule.first_static_ordinal != event.source_static_ordinal)
      return Report(diagnostic, "Conv event disagrees with source schedule");

    Context_Key key = {event.owner_pu_st, node.id,
                       event.context_pu_identity_id,
                       event.context_callsite_id};
    if (!seen.insert(key).second)
      return Report(diagnostic, "Conv context identity is duplicated");

    DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
    DSL_FHE_BN_FOLD_PROVENANCE_RECORD fold;
    if (!DSL_FHE_Plan_Find_Conversion_Disposition(node.id, &disposition) ||
        disposition.owner_pu_st != event.owner_pu_st ||
        disposition.result_value_id != result.id ||
        disposition.first_bn_fold_id ==
            DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID ||
        disposition.bn_fold_count == 0 ||
        !DSL_FHE_Plan_Find_BN_Fold_Provenance(
            node.id, event.context_pu_identity_id,
            event.context_callsite_id, &fold) ||
        fold.id < disposition.first_bn_fold_id ||
        fold.id >= disposition.first_bn_fold_id + disposition.bn_fold_count ||
        fold.owner_pu_st != event.owner_pu_st ||
        fold.conv_node_id != node.id ||
        (fold.flags & DSL_FHE_BN_FOLD_CONTEXT_IDENTITY_IS_CALLEE) == 0 ||
        fold.folded_weight_tcon == TCON_IDX_ZERO ||
        fold.folded_bias_tcon == TCON_IDX_ZERO)
      return Report(diagnostic, "Conv has no exact live BN-fold context");

    VHO_FHE_CKKS_CONV_CONTEXT context;
    memset(&context, 0, sizeof(context));
    context.event = event;
    context.source_node_id = node.id;
    context.source_result_ty = result.ty;
    context.folded_weight_tcon = fold.folded_weight_tcon;
    context.folded_bias_tcon = fold.folded_bias_tcon;
    context.bn_fold_flags = fold.flags;
    if (!Operand(node, 0, &context.input_value_id) ||
        !Operand(node, 1, &context.source_weight_operand_value_id) ||
        !Operand(node, 2, &context.source_bias_operand_value_id))
      return Report(diagnostic, "Conv operand identity is incomplete");
    DSL_IR_VALUE_RECORD input;
    DSL_IR_VALUE_RECORD weight;
    if (!DSL_IR_Image_Get_Value(context.input_value_id, &input) ||
        !DSL_IR_Image_Get_Value(context.source_weight_operand_value_id,
                                &weight) ||
        !Geometry(node, input, weight, result, slot_count, &context))
      return Report(diagnostic, "Conv tensor geometry is unsupported");
    context.input_ty = input.ty;
    collected.push_back(context);
  }
  if (collected.empty())
    return Report(diagnostic, "source schedule contains no Conv contexts");
  contexts->swap(collected);
  return TRUE;
}
