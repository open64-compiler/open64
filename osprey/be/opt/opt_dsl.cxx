/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "opt_dsl.h"
#include "dsl_ir_image.h"
#include "dsl_tensor_fold.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"

static UINT64
WOPT_DSL_Hash_Combine(UINT64 hash, UINT64 value)
{
  hash ^= value;
  hash *= 1099511628211ULL;
  return hash;
}

static UINT64
WOPT_DSL_Hash_String(UINT64 hash, const char *text)
{
  if (text == NULL)
    return WOPT_DSL_Hash_Combine(hash, 0);
  while (*text != '\0') {
    hash ^= (unsigned char)*text++;
    hash *= 1099511628211ULL;
  }
  return WOPT_DSL_Hash_Combine(hash, 0xff);
}

static BOOL
WOPT_DSL_Result_Value(ST_IDX result_st, const char *owner_pu,
                      DSL_IR_VALUE_RECORD *value,
                      DSL_IR_NODE_RECORD *node)
{
  const char *name;

  if (ST_IDX_index(result_st) == 0 || owner_pu == NULL ||
      value == NULL || node == NULL)
    return FALSE;
  name = ST_name(St_Table[result_st]);
  return name != NULL &&
         DSL_IR_Image_Find_PU_Value
             (result_st, name, owner_pu, value) &&
         value->producer_node_id != DSL_IR_NODE_INVALID_ID &&
         DSL_IR_Image_Get_Node(value->producer_node_id, node);
}

static BOOL
WOPT_DSL_TCON_Index(ST_IDX st, TCON_IDX *tcon_idx)
{
  DSL_TENSOR_TCON_RECORD record;
  const char *text = ST_tensor_metadata(st, "tensor_tcon_idx");
  char *end;
  unsigned long value;

  if (tcon_idx != NULL)
    *tcon_idx = TCON_IDX_ZERO;
  if (text == NULL || text[0] == '\0' || tcon_idx == NULL)
    return FALSE;
  errno = 0;
  value = strtoul(text, &end, 10);
  if (errno == ERANGE || end == text || *end != '\0' ||
      value == 0 || value > UINT32_MAX)
    return FALSE;
  *tcon_idx = (TCON_IDX)value;
  return DSL_Tensor_TCON_Get(*tcon_idx, &record);
}

BOOL
WOPT_DSL_Import_Semantic_Info(const WN *wn, ST_IDX result_st,
                              const char *owner_pu,
                              WOPT_DSL_SEMANTIC_INFO *info,
                              FILE *diagnostic)
{
  DSL_LOGICAL_OPCODE logical;
  DSL_OPERATOR_INFO operator_info;
  DSL_IR_VALUE_RECORD result_value;
  DSL_IR_NODE_RECORD node;
  UINT64 attribute_hash = 1469598103934665603ULL;
  UINT64 operand_hash = 1469598103934665603ULL;

  if (info != NULL)
    memset(info, 0, sizeof(*info));
  if (wn == NULL || info == NULL ||
      !DSL_WN_Get_Logical_Opcode(wn, &logical, diagnostic) ||
      !DSL_Operator_Get_Info_Version
          (logical.dsl_operator, logical.effective_version, &operator_info) ||
      !WOPT_DSL_Result_Value
          (result_st, owner_pu, &result_value, &node) ||
      node.operand_count != (UINT32)WN_kid_count(wn)) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL import: incomplete semantic identity\n");
    return FALSE;
  }

  if (logical.dsl_operator != OPR_DSLTENSORCONST &&
      logical.dsl_operator != OPR_DSLADD &&
      logical.dsl_operator != OPR_DSLMUL) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL import: %s is outside M5 scope\n",
              DSL_OPERATOR_name(logical.dsl_operator));
    return FALSE;
  }
  if (operator_info.effect_model != DSL_EFFECT_MODEL_PURE) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL import: effectful operator %s rejected\n",
              operator_info.logical_name);
    return FALSE;
  }

  for (UINT32 i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute
            (node.first_attribute_id + i, &attribute))
      return FALSE;
    attribute_hash = WOPT_DSL_Hash_String
                         (attribute_hash, Index_To_Str(attribute.name));
    attribute_hash = WOPT_DSL_Hash_Combine
                         (attribute_hash, attribute.value_kind);
    attribute_hash = WOPT_DSL_Hash_String
                         (attribute_hash,
                          attribute.value == STR_IDX_ZERO ?
                              "" : Index_To_Str(attribute.value));
  }
  for (UINT32 i = 0; i < node.operand_count; ++i) {
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    DSL_IR_VALUE_RECORD operand;
    if (!DSL_IR_Image_Get_Value_Reference
            (node.first_operand_reference_id + i, &reference) ||
        !DSL_IR_Image_Get_Value(reference.value_id, &operand))
      return FALSE;
    operand_hash = WOPT_DSL_Hash_Combine(operand_hash, operand.ty);
  }

  info->logical_operator = logical.dsl_operator;
  info->version = logical.effective_version;
  info->flags = WOPT_DSL_SEMANTIC_PURE |
                WOPT_DSL_SEMANTIC_PROJECTABLE;
  info->result_ty = result_value.ty;
  info->origin_node_id = node.id;
  info->origin_result_value_id = result_value.id;
  info->canonical_attribute_hash = attribute_hash;
  info->operand_descriptor_hash = operand_hash;
  info->effect_identity = operator_info.effect_model;
  if (logical.dsl_operator == OPR_DSLTENSORCONST &&
      !WOPT_DSL_TCON_Index(result_st, &info->tensor_tcon_idx)) {
    if (diagnostic != NULL)
      fprintf(diagnostic,
              "WOPT DSL import: tensor constant has no compact TCON\n");
    return FALSE;
  }
  return TRUE;
}

BOOL
WOPT_DSL_Create_Folded_Tensor_Info
    (const WOPT_DSL_SEMANTIC_INFO *origin, TCON_IDX result_tcon_idx,
     WOPT_DSL_SEMANTIC_INFO *result)
{
  DSL_TENSOR_TCON_RECORD record;

  if (origin == NULL || result == NULL ||
      !DSL_Tensor_TCON_Get(result_tcon_idx, &record) ||
      record.descriptor_ty != origin->result_ty)
    return FALSE;
  *result = *origin;
  result->id = WOPT_DSL_SEMANTIC_INFO_INVALID_ID;
  result->logical_operator = OPR_DSLTENSORCONST;
  result->version = 1;
  result->tensor_tcon_idx = result_tcon_idx;
  result->canonical_attribute_hash =
      WOPT_DSL_Hash_Combine
          (WOPT_DSL_Hash_Combine(1469598103934665603ULL,
                                 DSL_TENSOR_TCON_STORAGE_SPLAT),
           (UINT64)record.scalar_integer_value);
  result->operand_descriptor_hash = 1469598103934665603ULL;
  return TRUE;
}

BOOL
WOPT_DSL_Fold_Compact_Tensors
    (const WOPT_DSL_SEMANTIC_INFO *origin,
     const TCON_IDX *operand_tcon_idx, UINT32 operand_count,
     WOPT_DSL_SEMANTIC_INFO *result, FILE *diagnostic)
{
  DSL_IR_NODE_RECORD node;
  DSL_TENSOR_FOLD_POLICY policy;
  DSL_TENSOR_FOLD_CANDIDATE candidate;
  DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT context;
  DSL_TENSOR_FOLD_REPLACEMENT replacement;
  TCON operands[2];
  TY_IDX operand_ty[2];
  TY_IDX result_ty[1];
  std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;

  if (origin == NULL || operand_tcon_idx == NULL || result == NULL ||
      operand_count != 2 ||
      (origin->logical_operator != OPR_DSLADD &&
       origin->logical_operator != OPR_DSLMUL) ||
      !DSL_IR_Image_Get_Node(origin->origin_node_id, &node) ||
      node.operand_count != operand_count)
    return FALSE;

  for (UINT32 i = 0; i < operand_count; ++i) {
    DSL_TENSOR_TCON_RECORD record;
    if (!DSL_Tensor_TCON_Get(operand_tcon_idx[i], &record) ||
        !DSL_Tensor_TCON_Get_Carrier(operand_tcon_idx[i], &operands[i]))
      return FALSE;
    operand_ty[i] = record.descriptor_ty;
  }

  for (UINT32 i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute
            (node.first_attribute_id + i, &attribute))
      return FALSE;
    attributes.push_back(attribute);
  }

  DSL_Tensor_Fold_Default_Policy(&policy);
  result_ty[0] = origin->result_ty;
  memset(&candidate, 0, sizeof(candidate));
  candidate.dsl_operator = origin->logical_operator;
  candidate.version = origin->version;
  candidate.result_count = 1;
  candidate.operand_count = operand_count;
  candidate.operands = operands;
  candidate.operand_ty = operand_ty;
  candidate.result_ty = result_ty;
  candidate.attributes =
      attributes.empty() ? NULL : &attributes[0];
  candidate.attribute_count = attributes.size();
  candidate.policy = &policy;

  memset(&context, 0, sizeof(context));
  context.result_ty = origin->result_ty;
  context.origin_node_id = origin->origin_node_id;
  context.origin_result_value_id = origin->origin_result_value_id;
  DSL_TENSOR_FOLD_STATUS status =
      DSL_Tensor_Fold_Describe_Replacement
          (&candidate, &context, &replacement);
  if (status != DSL_TENSOR_FOLD_SUCCESS) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL fold: %s rejected: %s\n",
              DSL_OPERATOR_name(origin->logical_operator),
              DSL_Tensor_Fold_Status_Name(status));
    return FALSE;
  }
  return WOPT_DSL_Create_Folded_Tensor_Info
             (origin, replacement.result_tcon_idx, result);
}

static std::string
WOPT_DSL_Tensor_Constant_Payload(const char *name, TY_IDX ty,
                                INT64 value)
{
  char rank[32];
  char scalar[32];
  const char *dtype = TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE);
  const char *shape = TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);

  snprintf(rank, sizeof(rank), "%d", TY_tensor_rank(ty));
  snprintf(scalar, sizeof(scalar), "%lld", (long long)value);
  std::string payload = "name=";
  payload += name == NULL ? "" : name;
  payload += ";dtype=";
  payload += dtype == NULL ? "" : dtype;
  payload += ";rank=";
  payload += rank;
  payload += ";shape=";
  payload += shape == NULL ? "" : shape;
  payload += ";value_kind=splat;value=";
  payload += scalar;
  return payload;
}

WN *
WOPT_DSL_Emit_WN(const WOPT_DSL_SEMANTIC_INFO *info,
                 const WN *original, ST_IDX result_st,
                 WN **kids, UINT32 kid_count, FILE *diagnostic)
{
  DSL_OPCODE_ANNOTATION annotation;
  DSL_LOGICAL_OPCODE original_logical;
  DSL_IR_VALUE_RECORD result_value;
  DSL_IR_NODE_RECORD node;
  const char *owner_pu =
      Current_PU_Info == NULL ? NULL :
          ST_name(PU_Info_proc_sym(Current_PU_Info));
  std::string generated_payload;
  const char *payload = NULL;

  if (info == NULL ||
      (kid_count != 0 && kids == NULL) ||
      !WOPT_DSL_Result_Value
          (result_st, owner_pu, &result_value, &node))
    return NULL;

  if (original != NULL &&
      DSL_WN_Get_Logical_Opcode(original, &original_logical, NULL) &&
      original_logical.dsl_operator == info->logical_operator &&
      original_logical.effective_version == info->version &&
      DSL_WN_Get_Opcode_Annotation(original, &annotation))
    payload = annotation.payload;

  if (info->logical_operator == OPR_DSLTENSORCONST) {
    DSL_TENSOR_TCON_RECORD tcon;
    if (!DSL_Tensor_TCON_Get(info->tensor_tcon_idx, &tcon))
      return NULL;
    generated_payload = WOPT_DSL_Tensor_Constant_Payload
                            (ST_name(St_Table[result_st]), info->result_ty,
                             tcon.scalar_integer_value);
    payload = generated_payload.c_str();

    DSL_IR_ATTRIBUTE_RECORD attributes[2];
    DSL_IR_Attribute_Record_Init(&attributes[0]);
    attributes[0].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attributes[0].name = Save_Str("value_kind");
    attributes[0].value = Save_Str("splat");
    DSL_IR_Attribute_Record_Init(&attributes[1]);
    attributes[1].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attributes[1].name = Save_Str("value");
    char scalar[32];
    snprintf(scalar, sizeof(scalar), "%lld",
             (long long)tcon.scalar_integer_value);
    attributes[1].value = Save_Str(scalar);

    DSL_IR_NODE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.node_id = node.id;
    request.opcode_descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor
            (OPR_DSLTENSORCONST, info->version);
    request.payload = Save_Str(payload);
    request.attributes = attributes;
    request.attribute_count = 2;
    request.result_value_kind = DSL_IR_VALUE_CONSTANT;
    if (request.opcode_descriptor_id ==
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        !DSL_IR_Image_Rewrite_Node(&request))
      return NULL;

    char tcon_text[32];
    snprintf(tcon_text, sizeof(tcon_text), "%u",
             (UINT32)info->tensor_tcon_idx);
    ST_tensor_bind_metadata(result_st, "tensor_tcon_idx", tcon_text);
    ST_tensor_bind_metadata(result_st, "tensor_fold.origin", "WOPT");
  }

  if (payload == NULL) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL emission: payload unavailable for %s\n",
              DSL_OPERATOR_name(info->logical_operator));
    return NULL;
  }
  return DSL_WN_Create_Native(info->logical_operator, info->version,
                              payload, kids, kid_count);
}
