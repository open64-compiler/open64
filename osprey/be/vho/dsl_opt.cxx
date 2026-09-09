/*
 * Copyright (C) 2026 Open64 Project
 */

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include "dsl_opt.h"
#include "config.h"
#include "config_dsl.h"
#include "dsl_ir_image.h"
#include "dsl_memory_behavior.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "pu_info.h"
#include "stab.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

static VHO_DSL_OPT_PASS VHO_DSL_opt_pass[VHO_DSL_OPT_STAGE_COUNT];

static const char *VHO_DSL_opt_stage_name[VHO_DSL_OPT_STAGE_COUNT] = {
    "canonicalization",
    "descriptor_propagation",
    "constant_propagation",
    "algebraic_simplification",
    "dead_result_elimination",
    "common_subexpression",
    "ssa_pre",
    "quantization",
    "fusion",
    "parallelization",
    "implementation_selection"
};

static BOOL
VHO_DSL_Is_Integer_Tensor (TY_IDX ty)
{
    return TY_is_tensor_extension(ty) &&
           MTYPE_is_integral(TY_mtype(TY_tensor_element_ty(ty)));
}

static BOOL
VHO_DSL_Image_Node_For_Result
        (ST_IDX result_st,
         const char *owner_pu,
         DSL_IR_VALUE_RECORD *value,
         DSL_IR_NODE_RECORD *node)
{
    const char *name = ST_name(St_Table[result_st]);

    return DSL_IR_Image_Find_PU_Value
               (result_st, name, owner_pu, value) &&
           value->producer_node_id != DSL_IR_NODE_INVALID_ID &&
           DSL_IR_Image_Get_Node(value->producer_node_id, node);
}

static BOOL
VHO_DSL_Image_Value_For_Operand
        (WN *operand,
         const char *owner_pu,
         DSL_IR_VALUE_RECORD *value)
{
    return operand != NULL && WN_operator(operand) == OPR_LDID &&
           DSL_IR_Image_Find_PU_Value
               (WN_st_idx(operand), ST_name(WN_st(operand)),
                owner_pu, value);
}

static BOOL
VHO_DSL_Copy_Node_Attributes
        (const DSL_IR_NODE_RECORD *node,
         std::vector<DSL_IR_ATTRIBUTE_RECORD> *attributes)
{
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

static std::string
VHO_DSL_Binary_Payload
        (WN *kid0,
         WN *kid1,
         const std::vector<DSL_IR_ATTRIBUTE_RECORD> &attributes)
{
    std::string payload = "kid0=";
    payload += ST_name(WN_st(kid0));
    payload += ";kid1=";
    payload += ST_name(WN_st(kid1));
    for (UINT32 i = 0; i < attributes.size(); ++i) {
        payload += ";";
        payload += Index_To_Str(attributes[i].name);
        payload += "=";
        if (attributes[i].value != STR_IDX_ZERO)
            payload += Index_To_Str(attributes[i].value);
    }
    return payload;
}

static void
VHO_DSL_Operand_Key (WN *operand, char *key, UINT32 key_size)
{
    ST_IDX st = WN_st_idx(operand);
    snprintf(key, key_size, "%s:%u:%u", ST_name(WN_st(operand)),
             ST_IDX_level(st), ST_IDX_index(st));
}

static std::string VHO_DSL_Tensor_Constant_Payload
        (const char *name, TY_IDX ty, const char *value);

static BOOL
VHO_DSL_Publish_Coefficient_Two
        (WN *block,
         WN *statement,
         const DSL_IR_VALUE_RECORD *owner_value,
         TY_IDX ty,
         DSL_IR_VALUE_RECORD *coefficient_value)
{
    TCON_IDX tcon_idx;
    char name[128];
    char tcon_text[32];
    snprintf(name, sizeof(name), "__dsl_coeff2_%u_%u",
             ST_IDX_level(WN_st_idx(statement)),
             ST_IDX_index(WN_st_idx(statement)));

    if (!DSL_Tensor_TCON_Create_Integer_Splat
             (ty, 2, &tcon_idx, NULL))
        return FALSE;

    ST_IDX st = DSL_Tensor_Create_Result_Symbol
                    (name, ty, SCLASS_AUTO, EXPORT_LOCAL);
    if (ST_IDX_index(st) == 0)
        return FALSE;
    Set_ST_Srcpos(St_Table[st], WN_Get_Linenum(statement));
    snprintf(tcon_text, sizeof(tcon_text), "%u", (UINT32)tcon_idx);
    ST_tensor_bind_metadata(st, "tensor_tcon_idx", tcon_text);
    ST_tensor_bind_metadata(st, "tensor_fold.origin",
                            "vho.coefficient_collection");

    std::string payload =
        VHO_DSL_Tensor_Constant_Payload(name, ty, "2");
    WN *constant = DSL_WN_Create_Native
                       (OPR_DSLTENSORCONST, 1, payload.c_str(), NULL, 0);
    if (constant == NULL)
        return FALSE;
    WN *definition =
        WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, st, ty, constant);
    WN_Set_Linenum(definition, WN_Get_Linenum(statement));

    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor(OPR_DSLTENSORCONST, 1);
    DSL_IR_NODE_RECORD node;
    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = descriptor_id;
    node.payload = Save_Str(payload.c_str());
    DSL_IR_NODE_ID node_id = DSL_IR_Image_Add_Node(&node);
    if (node_id == DSL_IR_NODE_INVALID_ID)
        return FALSE;

    DSL_IR_ATTRIBUTE_RECORD attrs[2];
    DSL_IR_Attribute_Record_Init(&attrs[0]);
    attrs[0].owner_node_id = node_id;
    attrs[0].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attrs[0].name = Save_Str("value_kind");
    attrs[0].value = Save_Str("splat");
    DSL_IR_ATTRIBUTE_ID first_attr = DSL_IR_Image_Add_Attribute(&attrs[0]);
    DSL_IR_Attribute_Record_Init(&attrs[1]);
    attrs[1].owner_node_id = node_id;
    attrs[1].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attrs[1].name = Save_Str("value");
    attrs[1].value = Save_Str("2");
    if (first_attr == DSL_IR_ATTRIBUTE_INVALID_ID ||
        DSL_IR_Image_Add_Attribute(&attrs[1]) ==
            DSL_IR_ATTRIBUTE_INVALID_ID)
        return FALSE;

    DSL_IR_Value_Record_Init(coefficient_value);
    coefficient_value->value_kind = DSL_IR_VALUE_CONSTANT;
    coefficient_value->producer_node_id = node_id;
    coefficient_value->ty = ty;
    coefficient_value->st = st;
    coefficient_value->name = Save_Str(name);
    coefficient_value->metadata = owner_value->metadata;
    coefficient_value->id = DSL_IR_Image_Add_Value(coefficient_value);
    if (coefficient_value->id == DSL_IR_VALUE_INVALID_ID ||
        !DSL_IR_Image_Set_Node_Links
             (node_id, DSL_IR_VALUE_REFERENCE_INVALID_ID, 0,
              first_attr, 2, coefficient_value->id))
        return FALSE;

    WN_INSERT_BlockBefore(block, statement, definition);
    return TRUE;
}

static BOOL
VHO_DSL_Collect_Duplicate_Term
        (WN *block,
         WN *statement,
         const DSL_LOGICAL_OPCODE *logical_opcode,
         const DSL_IR_NODE_RECORD *image_node,
         const DSL_IR_VALUE_RECORD *result_value,
         const DSL_IR_VALUE_RECORD operand_value[2],
         const std::vector<DSL_IR_ATTRIBUTE_RECORD> &attributes)
{
    WN *expression = WN_kid0(statement);
    DSL_ALGEBRAIC_INFO algebraic;
    if (logical_opcode->dsl_operator != OPR_DSLADD ||
        WN_st_idx(WN_kid0(expression)) != WN_st_idx(WN_kid1(expression)) ||
        !DSL_Operator_Get_Algebraic_Info
             (logical_opcode->dsl_operator,
              logical_opcode->effective_version, &algebraic) ||
        (algebraic.flags & DSL_ALGEBRAIC_ALLOW_REASSOCIATION) == 0 ||
        (algebraic.reassociation_safety != DSL_ALGEBRAIC_SAFETY_INTEGER &&
         algebraic.reassociation_safety !=
             DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE))
        return FALSE;

    DSL_IR_VALUE_RECORD coefficient_value;
    if (!VHO_DSL_Publish_Coefficient_Two
             (block, statement, result_value, WN_ty(statement),
              &coefficient_value))
        return FALSE;

    WN *operands[2] = {
        WN_COPY_Tree(WN_kid0(expression)),
        WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0,
                      coefficient_value.st, coefficient_value.ty)
    };
    std::string payload =
        VHO_DSL_Binary_Payload(operands[0], operands[1], attributes);
    WN *replacement = DSL_WN_Create_Native
                          (OPR_DSLMUL, 1, payload.c_str(), operands, 2);
    if (replacement == NULL)
        return FALSE;

    DSL_IR_VALUE_ID operand_ids[2] = {
        operand_value[0].id, coefficient_value.id
    };
    DSL_IR_NODE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.node_id = image_node->id;
    request.opcode_descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor(OPR_DSLMUL, 1);
    request.payload = Save_Str(payload.c_str());
    request.operand_value_ids = operand_ids;
    request.operand_count = 2;
    request.attributes = attributes.empty() ? NULL : &attributes[0];
    request.attribute_count = (UINT32)attributes.size();
    request.result_value_kind = result_value->value_kind;
    if (request.opcode_descriptor_id ==
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        !DSL_IR_Image_Rewrite_Node(&request)) {
        WN_DELETE_Tree(replacement);
        return FALSE;
    }

    WN_kid0(statement) = replacement;
    WN_DELETE_Tree(expression);
    return TRUE;
}

static BOOL
VHO_DSL_Canonicalize_Definition
        (WN *block,
         WN *statement,
         const char *owner_pu,
         FILE *diagnostic)
{
    WN *expression = WN_kid0(statement);
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_IR_VALUE_RECORD result_value;
    DSL_IR_VALUE_RECORD operand_value[2];
    DSL_IR_NODE_RECORD image_node;
    std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;
    char kid_key[2][256];
    BOOL kid_is_constant[2];

    if (!DSL_WN_Get_Logical_Opcode
             (expression, &logical_opcode, diagnostic) ||
        WN_kid_count(expression) != 2 ||
        !VHO_DSL_Is_Integer_Tensor(WN_ty(statement)) ||
        !VHO_DSL_Image_Node_For_Result
             (WN_st_idx(statement), owner_pu,
              &result_value, &image_node) ||
        !VHO_DSL_Image_Value_For_Operand
             (WN_kid0(expression), owner_pu, &operand_value[0]) ||
        !VHO_DSL_Image_Value_For_Operand
             (WN_kid1(expression), owner_pu, &operand_value[1]) ||
        operand_value[0].ty != operand_value[1].ty ||
        operand_value[0].ty != WN_ty(statement) ||
        !VHO_DSL_Copy_Node_Attributes(&image_node, &attributes))
        return TRUE;

    if (logical_opcode.dsl_operator == OPR_DSLADD &&
        WN_st_idx(WN_kid0(expression)) == WN_st_idx(WN_kid1(expression)))
        return VHO_DSL_Collect_Duplicate_Term
                   (block, statement, &logical_opcode, &image_node,
                    &result_value, operand_value, attributes);

    kid_is_constant[0] =
        operand_value[0].value_kind == DSL_IR_VALUE_CONSTANT;
    kid_is_constant[1] =
        operand_value[1].value_kind == DSL_IR_VALUE_CONSTANT;
    VHO_DSL_Operand_Key(WN_kid0(expression), kid_key[0],
                        sizeof(kid_key[0]));
    VHO_DSL_Operand_Key(WN_kid1(expression), kid_key[1],
                        sizeof(kid_key[1]));
    if (!DSL_Algebraic_Should_Swap_Binary_Operands
             (logical_opcode.dsl_operator,
              logical_opcode.effective_version, TRUE, TRUE,
              kid_is_constant[0], kid_is_constant[1],
              kid_key[0], kid_key[1]))
        return TRUE;

    WN *operands[2] = {
        WN_COPY_Tree(WN_kid1(expression)),
        WN_COPY_Tree(WN_kid0(expression))
    };
    std::string payload =
        VHO_DSL_Binary_Payload(operands[0], operands[1], attributes);
    WN *replacement = DSL_WN_Create_Native
                          (logical_opcode.dsl_operator,
                           logical_opcode.effective_version,
                           payload.c_str(), operands, 2);
    if (replacement == NULL) {
        WN_DELETE_Tree(operands[0]);
        WN_DELETE_Tree(operands[1]);
        return FALSE;
    }

    DSL_IR_VALUE_ID operand_value_id[2] = {
        operand_value[1].id, operand_value[0].id
    };
    DSL_IR_NODE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.node_id = image_node.id;
    request.opcode_descriptor_id = image_node.opcode_descriptor_id;
    request.payload = Save_Str(payload.c_str());
    request.operand_value_ids = operand_value_id;
    request.operand_count = 2;
    request.attributes = attributes.empty() ? NULL : &attributes[0];
    request.attribute_count = (UINT32)attributes.size();
    request.result_value_kind = result_value.value_kind;
    if (!DSL_IR_Image_Rewrite_Node(&request)) {
        WN_DELETE_Tree(replacement);
        return FALSE;
    }

    WN_kid0(statement) = replacement;
    WN_DELETE_Tree(expression);
    return TRUE;
}

static BOOL
VHO_DSL_Canonicalize_Tree
        (WN *tree,
         const char *owner_pu,
         FILE *diagnostic)
{
    if (tree == NULL)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (WN_operator(statement) == OPR_STID &&
                WN_kid_count(statement) == 1 &&
                DSL_WN_Is_Native(WN_kid0(statement))) {
                if (!VHO_DSL_Canonicalize_Definition
                         (tree, statement, owner_pu, diagnostic))
                    return FALSE;
            } else if (!VHO_DSL_Canonicalize_Tree
                            (statement, owner_pu, diagnostic)) {
                return FALSE;
            }
        }
        return TRUE;
    }
    for (INT kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (!VHO_DSL_Canonicalize_Tree
                 (WN_kid(tree, kid), owner_pu, diagnostic))
            return FALSE;
    }
    return TRUE;
}

static BOOL
VHO_DSL_Canonicalization_Pass
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic)
{
    const char *owner_pu =
        ST_name(St_Table[PU_Info_proc_sym(pu_info)]);
    return tree != NULL &&
           VHO_DSL_Canonicalize_Tree(*tree, owner_pu, diagnostic);
}

static BOOL
VHO_DSL_Tensor_TCON_Index (ST_IDX st, TCON_IDX *tcon_idx)
{
    const char *text = ST_tensor_metadata(st, "tensor_tcon_idx");
    char *end;

    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;
    if (text == NULL || tcon_idx == NULL)
        return FALSE;
    errno = 0;
    unsigned long value = strtoul(text, &end, 10);
    if (errno == ERANGE || end == text || *end != '\0' ||
        value == 0 || value > UINT_MAX)
        return FALSE;
    *tcon_idx = (TCON_IDX)value;
    return TRUE;
}

static BOOL
VHO_DSL_Compact_Operand
        (WN *operand,
         const char *owner_pu,
         DSL_IR_VALUE_RECORD *image_value,
         DSL_TENSOR_FOLD_VALUE *fold_value)
{
    TCON_IDX tcon_idx;
    DSL_TENSOR_FOLD_STATUS reason;

    return VHO_DSL_Image_Value_For_Operand
               (operand, owner_pu, image_value) &&
           VHO_DSL_Tensor_TCON_Index(WN_st_idx(operand), &tcon_idx) &&
           DSL_Tensor_Fold_Identify_Compact_TCON
               (tcon_idx, image_value->ty, image_value->st,
                image_value->id, fold_value, &reason);
}

static std::string
VHO_DSL_Tensor_Constant_Payload
        (const char *name,
         TY_IDX ty,
         const char *value)
{
    char rank[32];
    const char *dtype =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE);
    const char *shape =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);
    snprintf(rank, sizeof(rank), "%d", TY_tensor_rank(ty));

    std::string payload = "name=";
    payload += name;
    payload += ";dtype=";
    payload += dtype == NULL ? "" : dtype;
    payload += ";rank=";
    payload += rank;
    payload += ";shape=";
    payload += shape == NULL ? "" : shape;
    payload += ";value_kind=splat;value=";
    payload += value;
    return payload;
}

static BOOL
VHO_DSL_Fold_Definition
        (WN *statement,
         const char *owner_pu,
         FILE *diagnostic,
         BOOL *changed)
{
    WN *expression = WN_kid0(statement);
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_IR_VALUE_RECORD result_value;
    DSL_IR_VALUE_RECORD operand_value[2];
    DSL_IR_NODE_RECORD image_node;
    DSL_TENSOR_FOLD_VALUE fold_value[2];
    std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;

    if (!DSL_WN_Get_Logical_Opcode
             (expression, &logical_opcode, diagnostic) ||
        WN_kid_count(expression) != 2 ||
        !VHO_DSL_Is_Integer_Tensor(WN_ty(statement)) ||
        !VHO_DSL_Image_Node_For_Result
             (WN_st_idx(statement), owner_pu,
              &result_value, &image_node) ||
        !VHO_DSL_Compact_Operand
             (WN_kid0(expression), owner_pu,
              &operand_value[0], &fold_value[0]) ||
        !VHO_DSL_Compact_Operand
             (WN_kid1(expression), owner_pu,
              &operand_value[1], &fold_value[1]) ||
        !VHO_DSL_Copy_Node_Attributes(&image_node, &attributes))
        return TRUE;

    TCON operands[2] = {
        fold_value[0].carrier, fold_value[1].carrier
    };
    TY_IDX operand_ty[2] = {
        fold_value[0].ty, fold_value[1].ty
    };
    TY_IDX result_ty[1] = { WN_ty(statement) };
    DSL_TENSOR_FOLD_POLICY policy;
    DSL_Tensor_Fold_Default_Policy(&policy);

    DSL_TENSOR_FOLD_CANDIDATE candidate;
    memset(&candidate, 0, sizeof(candidate));
    candidate.dsl_operator = logical_opcode.dsl_operator;
    candidate.version = logical_opcode.effective_version;
    candidate.result_count = 1;
    candidate.operand_count = 2;
    candidate.operands = operands;
    candidate.operand_ty = operand_ty;
    candidate.result_ty = result_ty;
    candidate.attributes =
        attributes.empty() ? NULL : &attributes[0];
    candidate.attribute_count = (UINT32)attributes.size();
    candidate.policy = &policy;

    const char *lineage =
        TY_tensor_attribute(WN_ty(statement), TY_TENSOR_SCHEMA_LINEAGE);
    DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT context;
    memset(&context, 0, sizeof(context));
    context.result_ty = WN_ty(statement);
    context.result_st = WN_st_idx(statement);
    context.source_position = WN_Get_Linenum(statement);
    context.origin_node_id = image_node.id;
    context.origin_result_value_id = result_value.id;
    context.result_name = result_value.name;
    context.metadata = result_value.metadata;
    context.lineage = lineage == NULL ? STR_IDX_ZERO : Save_Str(lineage);

    DSL_TENSOR_FOLD_REPLACEMENT fold;
    DSL_TENSOR_FOLD_STATUS status =
        DSL_Tensor_Fold_Describe_Replacement(&candidate, &context, &fold);
    if (status != DSL_TENSOR_FOLD_SUCCESS)
        return TRUE;

    const char *result_name = result_value.name == STR_IDX_ZERO ?
                                  ST_name(WN_st(statement)) :
                                  Index_To_Str(result_value.name);
    std::string payload =
        VHO_DSL_Tensor_Constant_Payload
            (result_name, fold.result_ty, fold.compact_scalar_text);
    WN *replacement = DSL_WN_Create_Native
                          (fold.logical_operator, fold.version,
                           payload.c_str(), NULL, 0);
    if (replacement == NULL)
        return FALSE;

    DSL_IR_ATTRIBUTE_RECORD tensor_const_attributes[2];
    DSL_IR_Attribute_Record_Init(&tensor_const_attributes[0]);
    tensor_const_attributes[0].value_kind =
        DSL_IR_ATTRIBUTE_VALUE_STRING;
    tensor_const_attributes[0].name = Save_Str("value_kind");
    tensor_const_attributes[0].value = Save_Str("splat");
    DSL_IR_Attribute_Record_Init(&tensor_const_attributes[1]);
    tensor_const_attributes[1].value_kind =
        DSL_IR_ATTRIBUTE_VALUE_STRING;
    tensor_const_attributes[1].name = Save_Str("value");
    tensor_const_attributes[1].value =
        Save_Str(fold.compact_scalar_text);

    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor
            (fold.logical_operator, fold.version);
    DSL_IR_NODE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.node_id = fold.origin_node_id;
    request.opcode_descriptor_id = descriptor_id;
    request.payload = Save_Str(payload.c_str());
    request.attributes = tensor_const_attributes;
    request.attribute_count = 2;
    request.result_value_kind = DSL_IR_VALUE_CONSTANT;
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        !DSL_IR_Image_Rewrite_Node(&request)) {
        WN_DELETE_Tree(replacement);
        return FALSE;
    }

    char tcon_text[32];
    snprintf(tcon_text, sizeof(tcon_text), "%u",
             (UINT32)fold.result_tcon_idx);
    ST_tensor_bind_metadata
        (fold.result_st, "tensor_tcon_idx", tcon_text);
    ST_tensor_bind_metadata
        (fold.result_st, "tensor_fold.origin",
         DSL_OPERATOR_name(logical_opcode.dsl_operator));
    WN_kid0(statement) = replacement;
    WN_DELETE_Tree(expression);
    *changed = TRUE;
    return TRUE;
}

static BOOL
VHO_DSL_Fold_Tree
        (WN *tree,
         const char *owner_pu,
         FILE *diagnostic,
         BOOL *changed)
{
    if (tree == NULL)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (WN_operator(statement) == OPR_STID &&
                WN_kid_count(statement) == 1 &&
                DSL_WN_Is_Native(WN_kid0(statement))) {
                if (!VHO_DSL_Fold_Definition
                         (statement, owner_pu, diagnostic, changed))
                    return FALSE;
            } else if (!VHO_DSL_Fold_Tree
                            (statement, owner_pu,
                             diagnostic, changed)) {
                return FALSE;
            }
        }
        return TRUE;
    }
    for (INT kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (!VHO_DSL_Fold_Tree
                 (WN_kid(tree, kid), owner_pu, diagnostic, changed))
            return FALSE;
    }
    return TRUE;
}

static BOOL
VHO_DSL_Algebraic_Simplification_Pass
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic)
{
    if (tree == NULL)
        return FALSE;

    UINT32 remaining_sweeps = DSL_IR_Image_Node_Count() + 1;
    const char *owner_pu =
        ST_name(St_Table[PU_Info_proc_sym(pu_info)]);
    BOOL changed = TRUE;
    while (changed && remaining_sweeps-- != 0) {
        changed = FALSE;
        if (!VHO_DSL_Fold_Tree
                 (*tree, owner_pu, diagnostic, &changed))
            return FALSE;
    }
    return !changed;
}

const char *
VHO_DSL_Opt_Stage_Name (VHO_DSL_OPT_STAGE stage)
{
    return stage >= VHO_DSL_OPT_CANONICALIZATION &&
           stage < VHO_DSL_OPT_STAGE_COUNT ?
           VHO_DSL_opt_stage_name[stage] : "unknown";
}

BOOL
VHO_DSL_Opt_Stage_Enabled (VHO_DSL_OPT_STAGE stage)
{
    switch (stage) {
    case VHO_DSL_OPT_CANONICALIZATION:
        return VHO_DSL_Enable_Canonicalization;
    case VHO_DSL_OPT_DESCRIPTOR_PROPAGATION:
        return VHO_DSL_Enable_Descriptor_Propagation;
    case VHO_DSL_OPT_CONSTANT_PROPAGATION:
        return VHO_DSL_Enable_Constant_Propagation;
    case VHO_DSL_OPT_ALGEBRAIC_SIMPLIFICATION:
        return VHO_DSL_Enable_Algebraic_Simplification;
    case VHO_DSL_OPT_DEAD_RESULT_ELIMINATION:
        return VHO_DSL_Enable_Dead_Result_Elimination;
    case VHO_DSL_OPT_COMMON_SUBEXPRESSION:
        return VHO_DSL_Enable_Common_Subexpression;
    case VHO_DSL_OPT_SSA_PRE:
        return VHO_DSL_Enable_SSA_PRE;
    case VHO_DSL_OPT_QUANTIZATION:
        return VHO_DSL_Enable_Quantization;
    case VHO_DSL_OPT_FUSION:
        return VHO_DSL_Enable_Fusion;
    case VHO_DSL_OPT_PARALLELIZATION:
        return VHO_DSL_Enable_Parallelization;
    case VHO_DSL_OPT_IMPLEMENTATION_SELECTION:
        return VHO_DSL_Enable_Implementation_Selection;
    default:
        return FALSE;
    }
}

BOOL
VHO_DSL_Opt_Register_Pass
        (VHO_DSL_OPT_STAGE stage,
         VHO_DSL_OPT_PASS pass)
{
    if (stage < VHO_DSL_OPT_CANONICALIZATION ||
        stage >= VHO_DSL_OPT_STAGE_COUNT ||
        pass == NULL || VHO_DSL_opt_pass[stage] != NULL)
        return FALSE;
    VHO_DSL_opt_pass[stage] = pass;
    return TRUE;
}

BOOL
VHO_DSL_Opt_Register_Default_Passes (void)
{
    if (VHO_DSL_opt_pass[VHO_DSL_OPT_CANONICALIZATION] == NULL)
        VHO_DSL_opt_pass[VHO_DSL_OPT_CANONICALIZATION] =
            VHO_DSL_Canonicalization_Pass;
    if (VHO_DSL_opt_pass[VHO_DSL_OPT_ALGEBRAIC_SIMPLIFICATION] == NULL)
        VHO_DSL_opt_pass[VHO_DSL_OPT_ALGEBRAIC_SIMPLIFICATION] =
            VHO_DSL_Algebraic_Simplification_Pass;
    return TRUE;
}

void
VHO_DSL_Opt_Reset_Passes (void)
{
    memset(VHO_DSL_opt_pass, 0, sizeof(VHO_DSL_opt_pass));
}

BOOL
VHO_DSL_Optimize_Program_Unit
        (struct pu_info *pu_info,
         WN **tree,
         FILE *diagnostic,
         VHO_DSL_OPT_RESULT *result)
{
    VHO_DSL_OPT_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    local_result.failed_stage = VHO_DSL_OPT_STAGE_COUNT;

    if (pu_info == NULL || tree == NULL || *tree == NULL)
        return FALSE;

    for (UINT32 ordinal = 0; ordinal < VHO_DSL_OPT_STAGE_COUNT; ++ordinal) {
        VHO_DSL_OPT_STAGE stage = (VHO_DSL_OPT_STAGE)ordinal;
        if (!VHO_DSL_Opt_Stage_Enabled(stage))
            continue;

        ++local_result.enabled_stage_count;
        if (VHO_DSL_opt_pass[stage] == NULL) {
            ++local_result.missing_stage_count;
            local_result.failed_stage = stage;
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL VHO optimization stage %s is enabled but has "
                        "no implementation\n",
                        VHO_DSL_Opt_Stage_Name(stage));
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }

        if (!VHO_DSL_opt_pass[stage](pu_info, tree, diagnostic)) {
            local_result.failed_stage = stage;
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }
        ++local_result.executed_stage_count;
    }

    if (result != NULL)
        *result = local_result;
    return TRUE;
}
