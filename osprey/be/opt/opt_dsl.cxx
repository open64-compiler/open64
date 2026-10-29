/*
 * Copyright (C) 2026 Open64 Project
 */

/* PU-local WHIRL/CODEREP semantic bridge; common/com owns records, while
 * PREOPT verifies canonical forms before import and after emission. Reduction
 * contract: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md; Softmax:
 * doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md; normalization:
 * doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md; numerical binding:
 * doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md. No builder or
 * target-selection policy is linked into this backend component. */

#include <string.h>
#include "../../common/ir_image/dsl_image_service.h"
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
#include "dsl_gatekeeper.h"
#include "dsl_shape.h"

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
WOPT_DSL_Result_Value_For_WN(const DSL_LOGICAL_OPCODE *logical,
                             UINT32 operand_count, const char *owner_pu,
                             DSL_IR_VALUE_RECORD *value,
                             DSL_IR_NODE_RECORD *node)
{
  DSL_IR_VALUE_RECORD match_value;
  DSL_IR_NODE_RECORD match_node;
  BOOL found = FALSE;

  if (logical == NULL || owner_pu == NULL || value == NULL || node == NULL)
    return FALSE;

  for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
    DSL_IR_NODE_RECORD candidate;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_IR_VALUE_RECORD result;
    DSL_IR_VALUE_RECORD owned_result;
    if (!DSL_IR_Image_Get_Node(id, &candidate) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (candidate.opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != (UINT32)logical->dsl_operator ||
        descriptor.version != logical->effective_version ||
        candidate.operand_count != operand_count ||
        strcmp(candidate.payload == STR_IDX_ZERO ? "" :
                   Index_To_Str(candidate.payload),
               logical->payload == NULL ? "" : logical->payload) != 0 ||
        !DSL_IR_Image_Get_Value(candidate.result_value_id, &result) ||
        result.st == ST_IDX_ZERO || result.name == STR_IDX_ZERO ||
        !DSL_IR_Image_Find_PU_Value
            (result.st, Index_To_Str(result.name), owner_pu, &owned_result) ||
        owned_result.id != result.id)
      continue;
    if (found)
      return FALSE;
    match_value = result;
    match_node = candidate;
    found = TRUE;
  }

  if (!found)
    return FALSE;
  *value = match_value;
  *node = match_node;
  return TRUE;
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

/* Import the managed identity for a direct native expression in this PU.
 * Row recipes are opaque pure operations: no reassociation or scalar folding is
 * enabled by admission. Invalid/incomplete identities do not enter CODEREP. */
BOOL
WOPT_DSL_import_semantic_info(const WN *wn, ST_IDX result_st,
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
  ST_IDX semantic_result_st = result_st;

  if (info != NULL)
    memset(info, 0, sizeof(*info));
  if (wn == NULL || info == NULL ||
      !DSL_WN_Get_Logical_Opcode(wn, &logical, diagnostic) ||
      !DSL_Operator_Get_Info_Version
          (logical.dsl_operator, logical.effective_version, &operator_info) ||
      (!WOPT_DSL_Result_Value
           (result_st, owner_pu, &result_value, &node) &&
       !WOPT_DSL_Result_Value_For_WN
           (&logical, (UINT32)WN_kid_count(wn), owner_pu,
            &result_value, &node)) ||
      node.operand_count != (UINT32)WN_kid_count(wn)) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "WOPT DSL import: incomplete semantic identity\n");
    return FALSE;
  }
  semantic_result_st = result_value.st;

  if (logical.dsl_operator != OPR_DSLTENSORCONST &&
      logical.dsl_operator != OPR_DSLADD &&
      logical.dsl_operator != OPR_DSLMUL &&
      logical.dsl_operator != OPR_DSLDIV &&
      logical.dsl_operator != OPR_DSLREM &&
      logical.dsl_operator != OPR_DSLREDUCESUM &&
      logical.dsl_operator != OPR_DSLSTABLESOFTMAX &&
      logical.dsl_operator != OPR_DSLMEANVARNORM) {
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
      !WOPT_DSL_TCON_Index(semantic_result_st, &info->tensor_tcon_idx)) {
    if (diagnostic != NULL)
      fprintf(diagnostic,
              "WOPT DSL import: tensor constant has no compact TCON\n");
    return FALSE;
  }
  return TRUE;
}

BOOL
WOPT_DSL_create_folded_tensor_info
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
WOPT_DSL_fold_compact_tensors
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
       origin->logical_operator != OPR_DSLMUL &&
       origin->logical_operator != OPR_DSLDIV &&
       origin->logical_operator != OPR_DSLREM) ||
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
  return WOPT_DSL_create_folded_tensor_info
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

static BOOL
WOPT_DSL_Copy_Node_Attributes(
    const DSL_IR_NODE_RECORD *node,
    std::vector<DSL_IR_ATTRIBUTE_RECORD> *attributes)
{
  if (node == NULL || attributes == NULL)
    return FALSE;
  attributes->clear();
  for (UINT32 i = 0; i < node->attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute
            (node->first_attribute_id + i, &attribute))
      return FALSE;
    attribute.id = DSL_IR_ATTRIBUTE_INVALID_ID;
    attribute.owner_node_id = DSL_IR_NODE_INVALID_ID;
    attributes->push_back(attribute);
  }
  return TRUE;
}

static BOOL
WOPT_DSL_Operand_Value(const WN *operand, const char *owner_pu,
                       DSL_IR_VALUE_RECORD *value)
{
  return operand != NULL && WN_operator(operand) == OPR_LDID &&
         owner_pu != NULL && value != NULL &&
         DSL_IR_Image_Find_PU_Value
             (WN_st_idx(operand), ST_name(WN_st(operand)),
              owner_pu, value);
}

static std::string
WOPT_DSL_Binary_Payload(
    WN **kids, UINT32 kid_count,
    const std::vector<DSL_IR_ATTRIBUTE_RECORD> &attributes)
{
  std::string payload;
  for (UINT32 i = 0; i < kid_count; ++i) {
    char ordinal[32];
    snprintf(ordinal, sizeof(ordinal), "%u", i);
    if (!payload.empty())
      payload += ";";
    payload += "kid";
    payload += ordinal;
    payload += "=";
    payload += ST_name(WN_st(kids[i]));
  }
  for (UINT32 i = 0; i < attributes.size(); ++i) {
    if (!payload.empty())
      payload += ";";
    payload += Index_To_Str(attributes[i].name);
    payload += "=";
    if (attributes[i].value != STR_IDX_ZERO)
      payload += Index_To_Str(attributes[i].value);
  }
  return payload;
}

/* Rebuild the typed expression and managed links together, including all three
 * normalization operands. Existing common rewrite services own the mutation;
 * this bridge preserves semantics, never selecting a numerical algorithm. */
WN *
WOPT_DSL_emit_WN(const WOPT_DSL_SEMANTIC_INFO *info,
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
  std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;
  const char *payload = NULL;

  if (info == NULL ||
      (kid_count != 0 && kids == NULL))
    return NULL;
  if (!WOPT_DSL_Result_Value
          (result_st, owner_pu, &result_value, &node)) {
    DSL_IR_VALUE_RECORD owned_result;
    if (info->origin_result_value_id == DSL_IR_VALUE_INVALID_ID ||
        !DSL_IR_Image_Get_Value
            (info->origin_result_value_id, &result_value) ||
        result_value.st == ST_IDX_ZERO ||
        result_value.name == STR_IDX_ZERO ||
        !DSL_IR_Image_Find_PU_Value
            (result_value.st, Index_To_Str(result_value.name), owner_pu,
             &owned_result) ||
        owned_result.id != result_value.id ||
        result_value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        !DSL_IR_Image_Get_Node(result_value.producer_node_id, &node))
      return NULL;
  }
  result_st = result_value.st;

  if (original != NULL &&
      DSL_WN_Get_Logical_Opcode(original, &original_logical, NULL) &&
      original_logical.dsl_operator == info->logical_operator &&
      original_logical.effective_version == info->version &&
      DSL_WN_Get_Opcode_Annotation(original, &annotation))
    payload = annotation.payload;
  if (payload == NULL && node.payload != STR_IDX_ZERO)
    payload = Index_To_Str(node.payload);

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
  } else if (info->logical_operator == OPR_DSLADD ||
             info->logical_operator == OPR_DSLMUL ||
             info->logical_operator == OPR_DSLDIV ||
             info->logical_operator == OPR_DSLREM ||
             info->logical_operator == OPR_DSLREDUCESUM ||
             info->logical_operator == OPR_DSLSTABLESOFTMAX ||
             info->logical_operator == OPR_DSLMEANVARNORM) {
    DSL_IR_VALUE_ID operand_ids[3];
    UINT32 expected_kids =
        (info->logical_operator == OPR_DSLREDUCESUM ||
         info->logical_operator == OPR_DSLSTABLESOFTMAX) ? 1 :
        (info->logical_operator == OPR_DSLMEANVARNORM ? 3 : 2);
    if (kid_count != expected_kids ||
        !WOPT_DSL_Copy_Node_Attributes(&node, &attributes))
      return NULL;
    for (UINT32 i = 0; i < kid_count; ++i) {
      DSL_IR_VALUE_RECORD operand;
      if (!WOPT_DSL_Operand_Value(kids[i], owner_pu, &operand))
        return NULL;
      operand_ids[i] = operand.id;
    }

    generated_payload =
        WOPT_DSL_Binary_Payload(kids, kid_count, attributes);
    payload = generated_payload.c_str();
    DSL_IR_NODE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.node_id = node.id;
    request.opcode_descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor
            (info->logical_operator, info->version);
    request.payload = Save_Str(payload);
    request.operand_value_ids = operand_ids;
    request.operand_count = kid_count;
    request.attributes =
        attributes.empty() ? NULL : &attributes[0];
    request.attribute_count = (UINT32)attributes.size();
    request.result_value_kind = result_value.value_kind;
    if (request.opcode_descriptor_id ==
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        !DSL_IR_Image_Rewrite_Node(&request))
      return NULL;
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

/* Derive the v1 recipe from a gatekeeper-verified assignment and managed joins.
 * No reduction schedule, FP equivalence proof or physical access is inferred. */
static BOOL
WOPT_DSL_Derive_Reduction(WN *assignment,
                          WOPT_DSL_REDUCTION_CHARACTERIZATION *output)
{
  if (assignment == NULL || output == NULL || Current_PU_Info == NULL ||
      WN_operator(assignment) != OPR_STID)
    return FALSE;
  WN *expression = WN_kid0(assignment);
  DSL_LOGICAL_OPCODE logical;
  if (expression == NULL || WN_operator(expression) != OPR_DSL ||
      !DSL_WN_Get_Logical_Opcode(expression, &logical, NULL) ||
      logical.dsl_operator != OPR_DSLREDUCESUM ||
      (logical.effective_version != 1 && logical.effective_version != 2) ||
      WN_kid_count(expression) != 1)
    return FALSE;
  WOPT_DSL_REDUCTION_CHARACTERIZATION fact;
  memset(&fact, 0, sizeof(fact));
  DSL_IR_VALUE_RECORD result, input;
  DSL_IR_NODE_RECORD node;
  const char *owner = ST_name(PU_Info_proc_sym(Current_PU_Info));
  UINT64 dimensions[2];
  UINT32 rank = 0;
  fact.input_ty = WN_ty(WN_kid0(expression));
  fact.result_ty = WN_ty(assignment);
  if (!WOPT_DSL_Result_Value(WN_st_idx(assignment), owner, &result, &node) ||
      !WOPT_DSL_Operand_Value(WN_kid0(expression), owner, &input) ||
      !DSL_Shape_Reduce_Sum_Types_Valid
          (fact.input_ty, fact.result_ty, "1", "false", "float32", "false") ||
      !DSL_Shape_Parse_Static_Dimensions
          (TY_tensor_attribute(fact.input_ty, TY_TENSOR_SCHEMA_SHAPE),
           dimensions, 2, &rank) || rank != 2)
    return FALSE;
  fact.recipe = logical.effective_version == 1 ? WOPT_DSL_REDUCTION_SUM_F32_ROWS_V1 :
                                               WOPT_DSL_REDUCTION_SUM_F32_ORDERED_V2;
  fact.flags = WOPT_DSL_REDUCTION_INPUT_READ_ONLY |
      WOPT_DSL_REDUCTION_REASSOC_FORBIDDEN |
      WOPT_DSL_REDUCTION_REFORMULATION_NOT_GRANTED |
      WOPT_DSL_REDUCTION_ORDER_UNRESOLVED |
      WOPT_DSL_REDUCTION_FINITE_UNPROVEN |
      WOPT_DSL_REDUCTION_OVERFLOW_UNPROVEN |
      WOPT_DSL_REDUCTION_ACCURACY_UNPROVEN |
      WOPT_DSL_REDUCTION_ALIAS_UNPROVEN |
      WOPT_DSL_REDUCTION_STORAGE_UNPLANNED |
      WOPT_DSL_REDUCTION_OUTPUT_OWNER_PER_ROW;
  if (logical.effective_version == 2)
    fact.flags &= ~WOPT_DSL_REDUCTION_ORDER_UNRESOLVED;
  fact.owner_pu = PU_Info_proc_sym(Current_PU_Info);
  fact.node_id = node.id;
  fact.input_value_id = input.id;
  fact.result_value_id = result.id;
  fact.rows = dimensions[0];
  fact.width = dimensions[1];
  SRCPOS position = WN_Get_Linenum(assignment);
  fact.source_line = SRCPOS_linenum(position);
  *output = fact;
  return TRUE;
}

/* Prove membership without traversing another PU or switching symbol tables. */
static BOOL
WOPT_DSL_Contains_Node(WN *scope, WN *node)
{
  if (scope == NULL) return FALSE;
  if (scope == node) return TRUE;
  if (WN_operator(scope) == OPR_BLOCK) {
    for (WN *statement = WN_first(scope); statement != NULL;
         statement = WN_next(statement))
      if (WOPT_DSL_Contains_Node(statement, node)) return TRUE;
  } else {
    for (INT32 i = 0; i < WN_kid_count(scope); ++i)
      if (WOPT_DSL_Contains_Node(WN_kid(scope, i), node)) return TRUE;
  }
  return FALSE;
}

/* Public read-only characterization uses the same native verifier as PREOPT;
 * no output is published until scope, membership and canonical meaning pass. */
BOOL
WOPT_DSL_characterize_reduction
    (WN *scope, WN *assignment, WOPT_DSL_REDUCTION_CHARACTERIZATION *output,
     FILE *diagnostic)
{
  DSL_GATEKEEPER_RESULT gate;
  return output != NULL && Current_PU_Info != NULL && scope != NULL &&
      WN_operator(scope) == OPR_FUNC_ENTRY &&
      PU_Info_tree_ptr(Current_PU_Info) == scope && assignment != NULL &&
      WOPT_DSL_Contains_Node(scope, assignment) &&
      DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &gate) &&
      WOPT_DSL_Derive_Reduction(assignment, output);
}

/* Positional numerical schema is separate from legacy v1 FORM/FACT rows. */
static void
WOPT_DSL_reduction_numerical_header(FILE *trace)
{
  fprintf(trace, "AI_REDUCE_NUM 1 HEADER phase pu node input result version rows width "
                 "order identity_bits environment nan_policy binding source_line\n");
}

/* Report only source-bound meaning; target arithmetic support is not inferred. */
static void
WOPT_DSL_trace_reduction_numerical
    (const WOPT_DSL_REDUCTION_CHARACTERIZATION &fact, const char *phase, FILE *trace)
{
  BOOL bound = fact.recipe == WOPT_DSL_REDUCTION_SUM_F32_ORDERED_V2;
  fprintf(trace, "AI_REDUCE_NUM 1 BIND %s %u %u %u %u %u %llu %llu %s %s %s %s %s %u\n",
          phase, (UINT32)fact.owner_pu, fact.node_id, fact.input_value_id,
          fact.result_value_id, bound ? 2 : 1, (unsigned long long)fact.rows,
          (unsigned long long)fact.width, bound ? "ascending_left_fold" : "source_unresolved",
          bound ? "00000000" : "semantic_zero",
          bound ? "rne_gradual_no_traps" : "source_unresolved",
          bound ? "class_only" : "source_unresolved", bound ? "explicit" : "missing",
          fact.source_line);
}

/* R2 exposes alternative legality without selecting, rewriting or binding a
 * target. Each row has one ordered accumulator; staging copies do no arithmetic.
 * Recompute after any source/type/attribute/member change. Scope is active PU. */
BOOL
WOPT_DSL_query_reduction_alternatives
    (WN *scope, WN *assignment, UINT64 chunk_elements, UINT64 scratch_budget_bytes,
     WOPT_DSL_REDUCTION_ALTERNATIVES *output, FILE *diagnostic, FILE *trace)
{
  if (output == NULL || chunk_elements == 0) return FALSE;
  WOPT_DSL_REDUCTION_ALTERNATIVES plans;
  memset(&plans, 0, sizeof(plans));
  if (!WOPT_DSL_characterize_reduction(scope, assignment, &plans.fact, diagnostic))
    return FALSE;
  plans.chunk_elements = chunk_elements < plans.fact.width ? chunk_elements : plans.fact.width;
  if (plans.chunk_elements > (~(UINT64)0) / 4) return FALSE;
  plans.scratch_bytes = plans.chunk_elements * 4;
  plans.scratch_budget_bytes = scratch_budget_bytes;
  BOOL bound = plans.fact.recipe == WOPT_DSL_REDUCTION_SUM_F32_ORDERED_V2;
  plans.direct_status = bound ? WOPT_DSL_REDUCE_LEGAL_RECIPE : WOPT_DSL_REDUCE_SOURCE_UNBOUND;
  plans.staged_status = !bound ? WOPT_DSL_REDUCE_SOURCE_UNBOUND :
      (plans.scratch_bytes <= scratch_budget_bytes ? WOPT_DSL_REDUCE_LEGAL_RECIPE :
                                                   WOPT_DSL_REDUCE_STAGING_BUDGET);
  plans.tree_status = WOPT_DSL_REDUCE_REASSOC_FORBIDDEN;
  if (trace != NULL) {
    WOPT_DSL_reduction_numerical_header(trace);
    WOPT_DSL_trace_reduction_numerical(plans.fact, "r2_query", trace);
    fprintf(trace, "AI_REDUCE_ALT 1 HEADER phase node candidate chunk scratch_bytes "
                   "budget_bytes numerical storage target selected reason source_line\n");
    const char *names[3] = { "direct_ordered", "staged_ordered", "partition_tree" };
    WOPT_DSL_REDUCTION_ALTERNATIVE_STATUS status[3] =
        { plans.direct_status, plans.staged_status, plans.tree_status };
    const char *reason[4] = { "source_binding_missing", "ordered_recipe_preserved",
                             "scratch_budget_exceeded", "reassociation_forbidden" };
    for (UINT32 i = 0; i < 3; ++i)
      fprintf(trace, "AI_REDUCE_ALT 1 ALT r2_query %u %s %llu %llu %llu %s %s "
                     "unbound 0 %s %u\n", plans.fact.node_id, names[i],
              (unsigned long long)(i == 1 ? plans.chunk_elements : 0),
              (unsigned long long)(i == 1 ? plans.scratch_bytes : 0),
              (unsigned long long)scratch_budget_bytes,
              i == 2 ? "forbidden" : (bound ? "legal_recipe" : "unbound"),
              i == 2 ? "not_evaluated" :
              (status[i] == WOPT_DSL_REDUCE_SOURCE_UNBOUND ? "not_evaluated" :
               (status[i] == WOPT_DSL_REDUCE_STAGING_BUDGET ? "budget_rejected" : "budget_ok")),
              reason[status[i]], plans.fact.source_line);
  }
  *output = plans;
  return TRUE;
}

/* Walk only the driver-selected tree; never traverse PU_Info or change symbol
 * tables. Emission occurs only after the public entry verifies the whole PU. */
static UINT32
WOPT_DSL_Reduction_Forms(WN *tree, const char *phase, FILE *trace,
                          BOOL verified = FALSE)
{
  if (tree == NULL)
    return 0;
  UINT32 count = 0;
  if (WN_operator(tree) == OPR_DSL) {
    DSL_LOGICAL_OPCODE logical;
    if (DSL_WN_Get_Logical_Opcode(tree, &logical, NULL) &&
        logical.dsl_operator == OPR_DSLREDUCESUM)
      ++count;
  }
  if (WN_operator(tree) == OPR_STID && WN_kid0(tree) != NULL &&
      WN_operator(WN_kid0(tree)) == OPR_DSL) {
    DSL_LOGICAL_OPCODE logical;
    WN *expression = WN_kid0(tree);
    if (DSL_WN_Get_Logical_Opcode(expression, &logical, NULL) &&
        logical.dsl_operator == OPR_DSLREDUCESUM) {
      if (verified) {
        WOPT_DSL_REDUCTION_CHARACTERIZATION fact;
        FmtAssert(WOPT_DSL_Derive_Reduction(tree, &fact),
                  ("verified reduction identity unavailable"));
        if (trace != NULL)
          WOPT_DSL_trace_reduction_numerical(fact, phase, trace);
        if (trace != NULL && fact.recipe == WOPT_DSL_REDUCTION_SUM_F32_ROWS_V1) {
          fprintf(trace, "AI_REDUCE 1 FORM %s %u %u %llu %llu 1 0 "
                         "float32 0 verified %u\n", phase, fact.node_id,
                  fact.result_value_id, (unsigned long long)fact.rows,
                  (unsigned long long)fact.width, fact.source_line);
          fprintf(trace, "AI_REDUCE_FACT 1 FACT %s %u %u %u %u %u %u %llu %llu "
                       "sum_f32_per_row 0 fp32_add identity real_add prohibited "
                       "source_unresolved 0 not_granted unproved unproved unproved "
                       "X[r,k] Y[r] read_only may_alias per_row unplanned %u\n",
                  phase, (UINT32)fact.owner_pu, fact.node_id, fact.input_value_id,
                  fact.result_value_id, (UINT32)fact.input_ty, (UINT32)fact.result_ty,
                  (unsigned long long)fact.rows, (unsigned long long)fact.width,
                  fact.source_line);
        }
      }
    }
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL;
         statement = WN_next(statement))
      count += WOPT_DSL_Reduction_Forms(statement, phase, trace, verified);
  } else {
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
      count += WOPT_DSL_Reduction_Forms(WN_kid(tree, i), phase, trace, verified);
  }
  return count;
}

/* PREOPT owns this verification, including the LNO/IPA PREOPT variants. The
 * initial reduction scope is a whole active PU; REGION support is not inferred.
 * No mutation precedes validation, and validation failure leaves IR untouched. */
BOOL
WOPT_DSL_verify_reduction_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace)
{
  if (WOPT_DSL_Reduction_Forms(tree, phase, NULL) == 0)
    return TRUE;
  DSL_GATEKEEPER_RESULT result;
  if (phase == NULL || Current_PU_Info == NULL ||
      WN_operator(tree) != OPR_FUNC_ENTRY ||
      PU_Info_tree_ptr(Current_PU_Info) != tree ||
      !DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &result)) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "PREOPT reduction: invalid canonical form or "
                          "unsupported compilation scope\n");
    return FALSE;
  }
  if (trace != NULL) {
    WOPT_DSL_reduction_numerical_header(trace);
    fprintf(trace, "AI_REDUCE 1 HEADER phase node value rows width axis "
                   "keepdim accum_dtype reassociate status source_line\n");
    fprintf(trace, "AI_REDUCE_FACT 1 HEADER phase pu node input result input_ty "
                   "result_ty rows width state identity update finalize real_merge "
                   "merge_permission order reassociate reformulate finite overflow "
                   "accuracy input_access output_access input_effect alias "
                   "output_owner storage source_line\n");
  }
  WOPT_DSL_Reduction_Forms(tree, phase, trace, TRUE);
  return TRUE;
}

/* Derive only the published source recipe from verified native joins. The
 * row statistics are logical state, not an allocated or mergeable (m,l) pair. */
static BOOL
WOPT_DSL_Derive_Softmax(WN *assignment,
                       WOPT_DSL_SOFTMAX_CHARACTERIZATION *output)
{
  if (assignment == NULL || output == NULL || Current_PU_Info == NULL ||
      WN_operator(assignment) != OPR_STID)
    return FALSE;
  WN *expression = WN_kid0(assignment);
  DSL_LOGICAL_OPCODE logical;
  if (expression == NULL || WN_operator(expression) != OPR_DSL ||
      !DSL_WN_Get_Logical_Opcode(expression, &logical, NULL) ||
      logical.dsl_operator != OPR_DSLSTABLESOFTMAX ||
      logical.effective_version != 1 || WN_kid_count(expression) != 1)
    return FALSE;
  WOPT_DSL_SOFTMAX_CHARACTERIZATION fact;
  memset(&fact, 0, sizeof(fact));
  DSL_IR_VALUE_RECORD result, input;
  DSL_IR_NODE_RECORD node;
  const char *owner = ST_name(PU_Info_proc_sym(Current_PU_Info));
  UINT64 dimensions[2];
  UINT32 rank = 0;
  fact.input_ty = WN_ty(WN_kid0(expression));
  fact.result_ty = WN_ty(assignment);
  if (!WOPT_DSL_Result_Value(WN_st_idx(assignment), owner, &result, &node) ||
      !WOPT_DSL_Operand_Value(WN_kid0(expression), owner, &input) ||
      !DSL_Shape_Stable_Softmax_Types_Valid
          (fact.input_ty, fact.result_ty, "1", "float32", "false", "false",
           "source_unresolved") ||
      !DSL_Shape_Parse_Static_Dimensions
          (TY_tensor_attribute(fact.input_ty, TY_TENSOR_SCHEMA_SHAPE),
           dimensions, 2, &rank) || rank != 2)
    return FALSE;
  fact.recipe = WOPT_DSL_SOFTMAX_STABLE_F32_ROWS_V1;
  fact.flags = WOPT_DSL_SOFTMAX_INPUT_READ_ONLY |
      WOPT_DSL_SOFTMAX_REASSOC_FORBIDDEN |
      WOPT_DSL_SOFTMAX_REFORMULATION_FORBIDDEN |
      WOPT_DSL_SOFTMAX_ORDER_UNRESOLVED |
      WOPT_DSL_SOFTMAX_FINITE_UNPROVEN |
      WOPT_DSL_SOFTMAX_OVERFLOW_UNPROVEN |
      WOPT_DSL_SOFTMAX_ACCURACY_UNPROVEN |
      WOPT_DSL_SOFTMAX_ALIAS_UNPROVEN |
      WOPT_DSL_SOFTMAX_STORAGE_UNPLANNED |
      WOPT_DSL_SOFTMAX_OUTPUT_OWNER_PER_ROW |
      WOPT_DSL_SOFTMAX_EXP_UNRESOLVED |
      WOPT_DSL_SOFTMAX_EXCEPTION_UNRESOLVED;
  fact.owner_pu = PU_Info_proc_sym(Current_PU_Info);
  fact.node_id = node.id;
  fact.input_value_id = input.id;
  fact.result_value_id = result.id;
  fact.rows = dimensions[0];
  fact.width = dimensions[1];
  SRCPOS position = WN_Get_Linenum(assignment);
  fact.source_line = SRCPOS_linenum(position);
  *output = fact;
  return TRUE;
}

/* Public read-only query: whole active PU and member assignment only. */
BOOL
WOPT_DSL_characterize_softmax
    (WN *scope, WN *assignment, WOPT_DSL_SOFTMAX_CHARACTERIZATION *output,
     FILE *diagnostic)
{
  DSL_GATEKEEPER_RESULT gate;
  return output != NULL && Current_PU_Info != NULL && scope != NULL &&
      WN_operator(scope) == OPR_FUNC_ENTRY &&
      PU_Info_tree_ptr(Current_PU_Info) == scope && assignment != NULL &&
      WOPT_DSL_Contains_Node(scope, assignment) &&
      DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &gate) &&
      WOPT_DSL_Derive_Softmax(assignment, output);
}

/* Count only selected-tree native nodes; derive/trace only after verification. */
static UINT32
WOPT_DSL_Softmax_Forms(WN *tree, const char *phase, FILE *trace,
                      BOOL verified = FALSE)
{
  if (tree == NULL) return 0;
  UINT32 count = 0;
  DSL_LOGICAL_OPCODE logical;
  if (WN_operator(tree) == OPR_DSL &&
      DSL_WN_Get_Logical_Opcode(tree, &logical, NULL) &&
      logical.dsl_operator == OPR_DSLSTABLESOFTMAX)
    ++count;
  if (WN_operator(tree) == OPR_STID && WN_kid0(tree) != NULL &&
      WN_operator(WN_kid0(tree)) == OPR_DSL &&
      DSL_WN_Get_Logical_Opcode(WN_kid0(tree), &logical, NULL) &&
      logical.dsl_operator == OPR_DSLSTABLESOFTMAX && verified) {
    WOPT_DSL_SOFTMAX_CHARACTERIZATION fact;
    FmtAssert(WOPT_DSL_Derive_Softmax(tree, &fact),
              ("verified stable Softmax identity unavailable"));
    if (trace != NULL) {
      fprintf(trace, "AI_SOFTMAX 1 FORM %s %u %u %llu %llu 1 float32 0 0 "
                     "source_unresolved verified %u\n", phase, fact.node_id,
              fact.result_value_id, (unsigned long long)fact.rows,
              (unsigned long long)fact.width, fact.source_line);
      fprintf(trace, "AI_SOFTMAX_FACT 1 FACT %s %u %u %u %u %u %u %llu %llu "
                     "max_exp_sum_div max_f32,sum_f32 subtract_row_max "
                     "source_unresolved exp_over_sum source_unresolved 0 0 "
                     "unproved unproved unproved source_unresolved "
                     "X[r,k] Y[r,k] read_only may_alias per_row unplanned %u\n",
              phase, (UINT32)fact.owner_pu, fact.node_id, fact.input_value_id,
              fact.result_value_id, (UINT32)fact.input_ty, (UINT32)fact.result_ty,
              (unsigned long long)fact.rows, (unsigned long long)fact.width,
              fact.source_line);
    }
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL;
         statement = WN_next(statement))
      count += WOPT_DSL_Softmax_Forms(statement, phase, trace, verified);
  } else {
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
      count += WOPT_DSL_Softmax_Forms(WN_kid(tree, i), phase, trace, verified);
  }
  return count;
}

/* Verify native form at both PREOPT boundaries; tracing is not a legality gate.
 * Unlowered recipe is preserved, with no executable implementation promised. */
BOOL
WOPT_DSL_verify_softmax_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace)
{
  if (WOPT_DSL_Softmax_Forms(tree, phase, NULL) == 0) return TRUE;
  DSL_GATEKEEPER_RESULT result;
  if (phase == NULL || Current_PU_Info == NULL ||
      WN_operator(tree) != OPR_FUNC_ENTRY ||
      PU_Info_tree_ptr(Current_PU_Info) != tree ||
      !DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &result)) {
    if (diagnostic != NULL)
      fprintf(diagnostic, "PREOPT stable Softmax: invalid canonical form or "
                          "unsupported compilation scope\n");
    return FALSE;
  }
  if (trace != NULL) {
    fprintf(trace, "AI_SOFTMAX 1 HEADER phase node value rows width axis "
                   "accum_dtype reassociate reformulate exp status source_line\n");
    fprintf(trace, "AI_SOFTMAX_FACT 1 HEADER phase pu node input result input_ty "
                   "result_ty rows width recipe state stabilize exp finalize "
                   "order reassociate reformulate finite overflow accuracy "
                   "exception input_access output_access input_effect alias "
                   "output_owner storage source_line\n");
  }
  WOPT_DSL_Softmax_Forms(tree, phase, trace, TRUE);
  return TRUE;
}

/* Derive the fixed centered population recipe only from verified canonical
 * assignments, affine operand joins, TYs and the exact epsilon attribute. */
static BOOL
WOPT_DSL_derive_normalization
    (WN *assignment, WOPT_DSL_NORMALIZATION_CHARACTERIZATION *output)
{
  if (assignment == NULL || output == NULL || Current_PU_Info == NULL ||
      WN_operator(assignment) != OPR_STID) return FALSE;
  WN *expression = WN_kid0(assignment);
  DSL_LOGICAL_OPCODE logical;
  if (expression == NULL || WN_operator(expression) != OPR_DSL ||
      !DSL_WN_Get_Logical_Opcode(expression, &logical, NULL) ||
      logical.dsl_operator != OPR_DSLMEANVARNORM ||
      logical.effective_version != 1 || WN_kid_count(expression) != 3)
    return FALSE;
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION fact;
  memset(&fact, 0, sizeof(fact));
  DSL_IR_VALUE_RECORD result;
  DSL_IR_NODE_RECORD node;
  const char *owner = ST_name(PU_Info_proc_sym(Current_PU_Info));
  if (!WOPT_DSL_Result_Value(WN_st_idx(assignment), owner, &result, &node)) return FALSE;
  for (UINT32 i = 0; i < 3; ++i) {
    DSL_IR_VALUE_RECORD operand;
    if (!WOPT_DSL_Operand_Value(WN_kid(expression, i), owner, &operand)) return FALSE;
    fact.operand_types[i] = WN_ty(WN_kid(expression, i));
    fact.operand_value_ids[i] = operand.id;
  }
  const char *epsilon = NULL;
  for (UINT32 i = 0; i < node.attribute_count; ++i) {
    DSL_IR_ATTRIBUTE_RECORD attribute;
    if (!DSL_IR_Image_Get_Attribute(node.first_attribute_id + i, &attribute)) return FALSE;
    if (strcmp(Index_To_Str(attribute.name), "attr.epsilon_bits") == 0)
      epsilon = Index_To_Str(attribute.value);
  }
  UINT64 dimensions[2];
  UINT32 rank = 0;
  fact.result_ty = WN_ty(assignment);
  if (!DSL_mean_variance_norm_types_valid
          (fact.operand_types[0], fact.operand_types[1], fact.operand_types[2],
           fact.result_ty, "1", "float32", "population_centered", epsilon,
           "false", "false", "false", "source_unresolved") ||
      !DSL_positive_f32_bits_valid(epsilon, &fact.epsilon_bits) ||
      !DSL_Shape_Parse_Static_Dimensions
          (TY_tensor_attribute(fact.operand_types[0], TY_TENSOR_SCHEMA_SHAPE),
           dimensions, 2, &rank) || rank != 2)
    return FALSE;
  fact.recipe = WOPT_DSL_NORM_CENTERED_POPULATION_F32_ROWS_V1;
  fact.flags = WOPT_DSL_NORM_INPUTS_READ_ONLY | WOPT_DSL_NORM_REASSOC_FORBIDDEN |
      WOPT_DSL_NORM_REFORMULATION_FORBIDDEN | WOPT_DSL_NORM_ORDER_UNRESOLVED |
      WOPT_DSL_NORM_FINITE_UNPROVEN | WOPT_DSL_NORM_OVERFLOW_UNPROVEN |
      WOPT_DSL_NORM_ACCURACY_UNPROVEN | WOPT_DSL_NORM_ALIAS_UNPROVEN |
      WOPT_DSL_NORM_STORAGE_UNPLANNED | WOPT_DSL_NORM_OUTPUT_OWNER_PER_ROW |
      WOPT_DSL_NORM_RSQRT_UNRESOLVED | WOPT_DSL_NORM_EXCEPTION_UNRESOLVED |
      WOPT_DSL_NORM_CONTRACTION_FORBIDDEN;
  fact.owner_pu = PU_Info_proc_sym(Current_PU_Info);
  fact.node_id = node.id;
  fact.result_value_id = result.id;
  fact.rows = dimensions[0];
  fact.width = dimensions[1];
  SRCPOS position = WN_Get_Linenum(assignment);
  fact.source_line = SRCPOS_linenum(position);
  *output = fact;
  return TRUE;
}

/* Public PU-local read-only query; reject foreign/nonmember/REGION scopes and
 * malformed native contracts without publishing a partial snapshot. */
BOOL
WOPT_DSL_characterize_normalization
    (WN *scope, WN *assignment, WOPT_DSL_NORMALIZATION_CHARACTERIZATION *output,
     FILE *diagnostic)
{
  DSL_GATEKEEPER_RESULT gate;
  return output != NULL && Current_PU_Info != NULL && scope != NULL &&
      WN_operator(scope) == OPR_FUNC_ENTRY &&
      PU_Info_tree_ptr(Current_PU_Info) == scope && assignment != NULL &&
      WOPT_DSL_Contains_Node(scope, assignment) &&
      DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &gate) &&
      WOPT_DSL_derive_normalization(assignment, output);
}

/* Count selected-tree operators; derive and optionally trace only after the
 * whole active PU passed gatekeeping. No allocation or schedule is chosen. */
static UINT32
WOPT_DSL_normalization_forms(WN *tree, const char *phase, FILE *trace,
                             BOOL verified = FALSE)
{
  if (tree == NULL) return 0;
  UINT32 count = 0;
  DSL_LOGICAL_OPCODE logical;
  if (WN_operator(tree) == OPR_DSL &&
      DSL_WN_Get_Logical_Opcode(tree, &logical, NULL) &&
      logical.dsl_operator == OPR_DSLMEANVARNORM) ++count;
  if (verified && WN_operator(tree) == OPR_STID && WN_kid0(tree) != NULL &&
      WN_operator(WN_kid0(tree)) == OPR_DSL &&
      DSL_WN_Get_Logical_Opcode(WN_kid0(tree), &logical, NULL) &&
      logical.dsl_operator == OPR_DSLMEANVARNORM) {
    WOPT_DSL_NORMALIZATION_CHARACTERIZATION f;
    FmtAssert(WOPT_DSL_derive_normalization(tree, &f),
              ("verified normalization identity unavailable"));
    if (trace != NULL) {
      fprintf(trace, "AI_NORM 1 FORM %s %u %u %llu %llu 1 population_centered "
                     "%08x float32 0 0 0 source_unresolved verified %u\n", phase,
              f.node_id, f.result_value_id, (unsigned long long)f.rows,
              (unsigned long long)f.width, f.epsilon_bits, f.source_line);
      fprintf(trace, "AI_NORM_FACT 1 FACT %s %u %u %u %u %u %u %u %u %u %u "
                     "%llu %llu %08x centered_population mean_f32,variance_f32 "
                     "%llu inside_rsqrt normalize_then_scale_then_bias "
                     "source_unresolved source_unresolved 0 0 0 unproved "
                     "unproved unproved source_unresolved X[r,k] gamma[k] beta[k] "
                     "Y[r,k] read_only may_alias per_row unplanned %u\n", phase,
              (UINT32)f.owner_pu, f.node_id, f.operand_value_ids[0],
              f.operand_value_ids[1], f.operand_value_ids[2], f.result_value_id,
              (UINT32)f.operand_types[0], (UINT32)f.operand_types[1],
              (UINT32)f.operand_types[2], (UINT32)f.result_ty,
              (unsigned long long)f.rows, (unsigned long long)f.width,
              f.epsilon_bits, (unsigned long long)f.width, f.source_line);
    }
  }
  if (WN_operator(tree) == OPR_BLOCK) {
    for (WN *statement = WN_first(tree); statement != NULL; statement = WN_next(statement))
      count += WOPT_DSL_normalization_forms(statement, phase, trace, verified);
  } else {
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
      count += WOPT_DSL_normalization_forms(WN_kid(tree, i), phase, trace, verified);
  }
  return count;
}

/* Preserve canonical normalization at both PREOPT boundaries. Tracing never
 * gates legality, and a REGION cannot expand itself into a whole-PU analysis. */
BOOL
WOPT_DSL_verify_normalization_forms
    (WN *tree, const char *phase, FILE *diagnostic, FILE *trace)
{
  if (WOPT_DSL_normalization_forms(tree, phase, NULL) == 0) return TRUE;
  DSL_GATEKEEPER_RESULT result;
  if (phase == NULL || Current_PU_Info == NULL || WN_operator(tree) != OPR_FUNC_ENTRY ||
      PU_Info_tree_ptr(Current_PU_Info) != tree ||
      !DSL_Gatekeeper_Verify_PU(Current_PU_Info, diagnostic, &result)) return FALSE;
  if (trace != NULL) {
    fprintf(trace, "AI_NORM 1 HEADER phase node value rows width axis variance epsilon_bits "
                   "accum_dtype reassociate reformulate contract rsqrt status source_line\n");
    fprintf(trace, "AI_NORM_FACT 1 HEADER phase pu node input gamma beta result input_ty "
                   "gamma_ty beta_ty result_ty rows width epsilon_bits recipe state denominator "
                   "epsilon_position affine_order order rsqrt reassociate reformulate contract "
                   "finite overflow accuracy exception input_access gamma_access beta_access "
                   "output_access input_effect alias output_owner storage source_line\n");
  }
  WOPT_DSL_normalization_forms(tree, phase, trace, TRUE);
  return TRUE;
}
