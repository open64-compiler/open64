/*
 * Copyright (C) 2026 Open64 Project
 */

#include <stdarg.h>
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "dsl_memory_behavior.h"
#include "dsl_opcode.h"
#include "symtab.h"
#include "wn.h"

typedef struct {
    std::vector<ST_IDX> result_symbols;
    std::vector<UINT32> input_ordinals;
    FILE *diagnostic;
    DSL_GATEKEEPER_RESULT result;
} DSL_GATEKEEPER_CONTEXT;

static BOOL
DSL_Gatekeeper_Report
        (DSL_GATEKEEPER_CONTEXT *context,
         const char *format,
         ...)
{
    ++context->result.error_count;
    if (context->diagnostic != NULL) {
        fprintf (context->diagnostic, "DSL gatekeeper error: ");
        va_list ap;
        va_start(ap, format);
        vfprintf (context->diagnostic, format, ap);
        va_end(ap);
        fprintf (context->diagnostic, "\n");
    }
    return FALSE;
}

static BOOL
DSL_Gatekeeper_Is_Result_Symbol
        (const DSL_GATEKEEPER_CONTEXT *context,
         ST_IDX st)
{
    for (size_t i = 0; i < context->result_symbols.size(); ++i) {
        if (context->result_symbols[i] == st)
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Gatekeeper_Collect_Results
        (WN *wn,
         DSL_GATEKEEPER_CONTEXT *context)
{
    if (wn == NULL)
        return TRUE;

    BOOL valid = TRUE;
    if (WN_operator(wn) == OPR_FUNC_ENTRY) {
        for (INT i = 0; i < WN_num_formals(wn); ++i) {
            WN *formal = WN_formal(wn, i);
            if (formal != NULL && WN_operator(formal) == OPR_IDNAME &&
                !DSL_Gatekeeper_Is_Result_Symbol
                     (context, WN_st_idx(formal)))
                context->result_symbols.push_back(WN_st_idx(formal));
        }
    }
    if (WN_operator(wn) == OPR_CALL) {
        for (INT i = 0; i < WN_kid_count(wn); ++i) {
            WN *parm = WN_kid(wn, i);
            WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                          NULL : WN_kid0(parm);
            if (address != NULL && WN_Parm_Out(parm) &&
                WN_operator(address) == OPR_LDA &&
                !DSL_Gatekeeper_Is_Result_Symbol
                     (context, WN_st_idx(address)))
                context->result_symbols.push_back(WN_st_idx(address));
        }
    }
    if (WN_operator(wn) == OPR_STID && WN_kid0(wn) != NULL &&
        DSL_WN_Is_Native(WN_kid0(wn))) {
        ST_IDX st = WN_st_idx(wn);
        if (DSL_Gatekeeper_Is_Result_Symbol(context, st))
            valid = DSL_Gatekeeper_Report
                        (context, "result symbol <%u,%u> has multiple "
                         "native definitions", ST_IDX_level(st),
                         ST_IDX_index(st));
        else
            context->result_symbols.push_back(st);
    }

    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
            if (!DSL_Gatekeeper_Collect_Results(stmt, context))
                valid = FALSE;
        }
        return valid;
    }

    for (INT i = 0; i < WN_kid_count(wn); ++i) {
        if (!DSL_Gatekeeper_Collect_Results(WN_kid(wn, i), context))
            valid = FALSE;
    }
    return valid;
}

static BOOL
DSL_Gatekeeper_ST_Valid (ST_IDX st)
{
    SYMTAB_IDX level = ST_IDX_level(st);
    return ST_IDX_index(st) != 0 && level <= CURRENT_SYMTAB &&
           Scope_tab[level].st_tab != NULL &&
           ST_IDX_index(st) < ST_Table_Size(level);
}

static BOOL
DSL_Gatekeeper_TY_Valid (TY_IDX ty)
{
    return TY_IDX_index(ty) != 0 && TY_IDX_index(ty) < TY_Table_Size();
}

static BOOL
DSL_Gatekeeper_Tensor_Core_Complete (TY_IDX ty)
{
    return DSL_Gatekeeper_TY_Valid(ty) && TY_is_tensor_extension(ty) &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_KIND) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_RANK) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE) != NULL;
}

static BOOL
DSL_Gatekeeper_Has_Unique_Ownership (ST_IDX st)
{
    if (!DSL_Gatekeeper_ST_Valid(st))
        return FALSE;
    const char *value = ST_tensor_attribute
                            (st, TY_tensor_schema_key_name
                                     (TY_TENSOR_SCHEMA_NO_ALIAS));
    return value != NULL && strcmp(value, "true") == 0;
}

static BOOL
DSL_Gatekeeper_Tensor_Key_Equal
        (TY_IDX ty0,
         TY_IDX ty1,
         TY_TENSOR_SCHEMA_KEY key)
{
    const char *value0 = TY_tensor_attribute(ty0, key);
    const char *value1 = TY_tensor_attribute(ty1, key);
    return value0 == NULL ? value1 == NULL :
           value1 != NULL && strcmp(value0, value1) == 0;
}

static BOOL
DSL_Gatekeeper_Tensor_Element_Representation_Compatible
        (TY_IDX ty0,
         TY_IDX ty1)
{
    static const TY_TENSOR_SCHEMA_KEY representation_keys[] = {
        TY_TENSOR_SCHEMA_LAYOUT,
        TY_TENSOR_SCHEMA_SHARDING,
        TY_TENSOR_SCHEMA_PLACEMENT,
        TY_TENSOR_SCHEMA_MEMORY,
        TY_TENSOR_SCHEMA_QUANTIZATION,
        TY_TENSOR_SCHEMA_RUNTIME_STATE
    };

    if (!DSL_Gatekeeper_Tensor_Core_Complete(ty0) ||
        !DSL_Gatekeeper_Tensor_Core_Complete(ty1) ||
        TY_tensor_element_ty(ty0) != TY_tensor_element_ty(ty1) ||
        !DSL_Gatekeeper_Tensor_Key_Equal
             (ty0, ty1, TY_TENSOR_SCHEMA_DTYPE))
        return FALSE;

    for (UINT32 i = 0;
         i < sizeof(representation_keys) / sizeof(representation_keys[0]);
         ++i) {
        if (!DSL_Gatekeeper_Tensor_Key_Equal
                 (ty0, ty1, representation_keys[i]))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Tensor_Compatible
        (TY_IDX ty0,
         TY_IDX ty1,
         BOOL require_shape)
{
    return DSL_Gatekeeper_Tensor_Element_Representation_Compatible
               (ty0, ty1) &&
           DSL_Gatekeeper_Tensor_Key_Equal
               (ty0, ty1, TY_TENSOR_SCHEMA_RANK) &&
           (!require_shape || DSL_Gatekeeper_Tensor_Key_Equal
                                  (ty0, ty1, TY_TENSOR_SCHEMA_SHAPE));
}

static BOOL
DSL_Gatekeeper_Tensor_Element_Type_Compatible
        (TY_IDX ty0,
         TY_IDX ty1)
{
    return DSL_Gatekeeper_Tensor_Core_Complete(ty0) &&
           DSL_Gatekeeper_Tensor_Core_Complete(ty1) &&
           TY_tensor_element_ty(ty0) == TY_tensor_element_ty(ty1) &&
           DSL_Gatekeeper_Tensor_Key_Equal
               (ty0, ty1, TY_TENSOR_SCHEMA_DTYPE);
}

static BOOL
DSL_Gatekeeper_Parse_Matrix_Shape
        (const char *shape,
         char *dim0,
         size_t dim0_size,
         char *dim1,
         size_t dim1_size)
{
    if (shape == NULL || dim0 == NULL || dim1 == NULL ||
        dim0_size == 0 || dim1_size == 0)
        return FALSE;
    const char *begin = shape;
    while (isspace((unsigned char)*begin))
        ++begin;
    if (*begin++ != '[')
        return FALSE;
    const char *comma = strchr(begin, ',');
    const char *close = strrchr(begin, ']');
    if (comma == NULL || close == NULL || comma >= close ||
        strchr(comma + 1, ',') != NULL)
        return FALSE;

    const char *end0 = comma;
    while (end0 > begin && isspace((unsigned char)end0[-1]))
        --end0;
    while (begin < end0 && isspace((unsigned char)*begin))
        ++begin;
    const char *begin1 = comma + 1;
    while (begin1 < close && isspace((unsigned char)*begin1))
        ++begin1;
    const char *end1 = close;
    while (end1 > begin1 && isspace((unsigned char)end1[-1]))
        --end1;

    size_t length0 = end0 - begin;
    size_t length1 = end1 - begin1;
    if (length0 == 0 || length1 == 0 || length0 >= dim0_size ||
        length1 >= dim1_size)
        return FALSE;
    memcpy (dim0, begin, length0);
    dim0[length0] = '\0';
    memcpy (dim1, begin1, length1);
    dim1[length1] = '\0';
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Matmul_Shapes_Compatible
        (TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         TY_IDX result_ty)
{
    char kid0_dim0[128];
    char kid0_dim1[128];
    char kid1_dim0[128];
    char kid1_dim1[128];
    char result_dim0[128];
    char result_dim1[128];
    return DSL_Gatekeeper_Parse_Matrix_Shape
               (TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_SHAPE),
                kid0_dim0, sizeof(kid0_dim0), kid0_dim1,
                sizeof(kid0_dim1)) &&
           DSL_Gatekeeper_Parse_Matrix_Shape
               (TY_tensor_attribute(kid1_ty, TY_TENSOR_SCHEMA_SHAPE),
                kid1_dim0, sizeof(kid1_dim0), kid1_dim1,
                sizeof(kid1_dim1)) &&
           DSL_Gatekeeper_Parse_Matrix_Shape
               (TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE),
                result_dim0, sizeof(result_dim0), result_dim1,
                sizeof(result_dim1)) &&
           strcmp(kid0_dim1, kid1_dim0) == 0 &&
           strcmp(result_dim0, kid0_dim0) == 0 &&
           strcmp(result_dim1, kid1_dim1) == 0;
}

static BOOL
DSL_Gatekeeper_Find_Image_Node
        (ST_IDX result_st,
         const char *result_name,
         DSL_OPERATOR dsl_operator,
         UINT16 version,
         const char *payload,
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Count(); ++i) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(i, &value) || value.st != result_st ||
            value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
            result_name == NULL ||
            value.name == STR_IDX_ZERO ||
            strcmp(Index_To_Str(value.name), result_name) != 0)
            continue;
        if (!DSL_IR_Image_Get_Node(value.producer_node_id, node))
            return FALSE;
        if (!DSL_IR_Image_Get_Opcode_Descriptor
                 (node->opcode_descriptor_id, descriptor))
            return FALSE;
        if (descriptor->logical_operator != (UINT32)dsl_operator ||
            descriptor->version != version ||
            node->payload == STR_IDX_ZERO || payload == NULL ||
            strcmp(Index_To_Str(node->payload), payload) != 0)
            continue;
        return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Gatekeeper_Node_Attribute
        (const DSL_IR_NODE_RECORD *node,
         const char *name,
         const char **value)
{
    for (UINT32 i = 0; i < node->attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (!DSL_IR_Image_Get_Attribute
                 (node->first_attribute_id + i, &attribute))
            return FALSE;
        if (strcmp(Index_To_Str(attribute.name), name) == 0 &&
            attribute.value_kind != DSL_IR_ATTRIBUTE_VALUE_UNKNOWN) {
            if (value != NULL)
                *value = Index_To_Str(attribute.value);
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
DSL_Gatekeeper_Node_Has_Attribute
        (const DSL_IR_NODE_RECORD *node,
         const char *name)
{
    return DSL_Gatekeeper_Node_Attribute(node, name, NULL);
}

static BOOL
DSL_Gatekeeper_Attribute_Equals
        (const DSL_IR_NODE_RECORD *node,
         const char *name,
         const char *expected)
{
    const char *value = NULL;
    return DSL_Gatekeeper_Node_Attribute(node, name, &value) &&
           value != NULL && strcmp(value, expected) == 0;
}

static BOOL
DSL_Gatekeeper_Parse_Signed
        (const char *text,
         INT32 *value)
{
    if (text == NULL || text[0] == '\0')
        return FALSE;
    char *end = NULL;
    long parsed = strtol(text, &end, 10);
    if (end == text || *end != '\0' || parsed < INT32_MIN ||
        parsed > INT32_MAX)
        return FALSE;
    if (value != NULL)
        *value = (INT32)parsed;
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Parse_Static_Shape
        (const char *shape,
         std::vector<UINT64> *dimensions)
{
    if (shape == NULL || dimensions == NULL)
        return FALSE;
    dimensions->clear();
    const char *cursor = shape;
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor++ != '[')
        return FALSE;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 dimension = 0;
        while (isdigit((unsigned char)*cursor)) {
            UINT64 digit = (UINT64)(*cursor - '0');
            if (dimension > (~(UINT64)0 - digit) / 10)
                return FALSE;
            dimension = dimension * 10 + digit;
            ++cursor;
        }
        if (dimension == 0)
            return FALSE;
        dimensions->push_back(dimension);
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor != ']')
            return FALSE;
    }
    while (isspace((unsigned char)*cursor))
        ++cursor;
    return *cursor == '\0';
}

static BOOL
DSL_Gatekeeper_Parse_Unsigned_List
        (const char *text,
         BOOL allow_zero,
         std::vector<UINT64> *values)
{
    if (text == NULL || values == NULL || text[0] == '\0')
        return FALSE;

    values->clear();
    const char *cursor = text;
    while (*cursor != '\0') {
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 value = 0;
        while (isdigit((unsigned char)*cursor)) {
            UINT64 digit = (UINT64)(*cursor - '0');
            if (value > (~(UINT64)0 - digit) / 10)
                return FALSE;
            value = value * 10 + digit;
            ++cursor;
        }
        if (!allow_zero && value == 0)
            return FALSE;
        values->push_back(value);
        if (*cursor == '\0')
            break;
        if (*cursor++ != ',' || *cursor == '\0')
            return FALSE;
    }
    return !values->empty();
}

static BOOL DSL_Gatekeeper_Parse_Unsigned(const char *, UINT64 *);

static BOOL
DSL_Gatekeeper_Parse_Pair
        (const DSL_IR_NODE_RECORD *node,
         const char *name,
         BOOL allow_zero,
         UINT64 values[2])
{
    const char *text = NULL;
    if (!DSL_Gatekeeper_Node_Attribute(node, name, &text) || text == NULL ||
        !isdigit((unsigned char)text[0]))
        return FALSE;
    char *end = NULL;
    unsigned long long first = strtoull(text, &end, 10);
    if (end == text || *end != ',')
        return FALSE;
    const char *second_text = end + 1;
    if (!isdigit((unsigned char)second_text[0]))
        return FALSE;
    unsigned long long second = strtoull(second_text, &end, 10);
    if (end == second_text || *end != '\0' ||
        (!allow_zero && (first == 0 || second == 0)))
        return FALSE;
    values[0] = first;
    values[1] = second;
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Static_Dimensions
        (TY_IDX ty,
         std::vector<UINT64> *dimensions)
{
    return DSL_Gatekeeper_Parse_Static_Shape
               (TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE), dimensions);
}

static BOOL
DSL_Gatekeeper_Dimensions_Equal
        (const std::vector<UINT64> &left,
         const std::vector<UINT64> &right)
{
    if (left.size() != right.size())
        return FALSE;
    for (UINT32 i = 0; i < left.size(); ++i) {
        if (left[i] != right[i])
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Result_Dimensions
        (TY_IDX input_ty,
         TY_IDX result_ty,
         const std::vector<UINT64> &expected)
{
    std::vector<UINT64> result;
    return DSL_Gatekeeper_Static_Dimensions(result_ty, &result) &&
           DSL_Gatekeeper_Dimensions_Equal(result, expected) &&
           DSL_Gatekeeper_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Gatekeeper_Computed_Result_Dimensions
        (TY_IDX input_ty,
         TY_IDX result_ty,
         const std::vector<UINT64> &expected)
{
    std::vector<UINT64> result;
    const char *placement =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_PLACEMENT);
    const char *memory =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_MEMORY);
    return DSL_Gatekeeper_Static_Dimensions(result_ty, &result) &&
           DSL_Gatekeeper_Dimensions_Equal(result, expected) &&
           DSL_Gatekeeper_Tensor_Element_Type_Compatible
               (input_ty, result_ty) &&
           DSL_Gatekeeper_Tensor_Key_Equal
               (input_ty, result_ty, TY_TENSOR_SCHEMA_LAYOUT) &&
           DSL_Gatekeeper_Tensor_Key_Equal
               (input_ty, result_ty, TY_TENSOR_SCHEMA_QUANTIZATION) &&
           (placement == NULL || strcmp(placement, "side_file") != 0) &&
           (memory == NULL || strcmp(memory, "external_data") != 0);
}

static BOOL
DSL_Gatekeeper_Linear_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    UINT32 expected_operands = version == 2 ? 3 : 2;
    const char *expected_bias = version == 2 ? "true" : "false";
    if ((version != 2 && version != 3) ||
        operands.size() != expected_operands ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.has_bias", expected_bias) ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.transpose_input", "false") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.transpose_weight", "true") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.weight_layout", "OI"))
        return FALSE;

    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Gatekeeper_Static_Dimensions(operands[0], &input) ||
        !DSL_Gatekeeper_Static_Dimensions(operands[1], &weight) ||
        input.size() < 2 || weight.size() != 2 ||
        input[input.size() - 1] != weight[1] ||
        !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[1]))
        return FALSE;
    if (version == 2 &&
        (!DSL_Gatekeeper_Static_Dimensions(operands[2], &bias) ||
         bias.size() != 1 || bias[0] != weight[0] ||
         !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[2])))
        return FALSE;
    input[input.size() - 1] = weight[0];
    return DSL_Gatekeeper_Result_Dimensions
               (operands[0], result_ty, input);
}

static BOOL
DSL_Gatekeeper_Reshape_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    const char *target_shape = NULL;
    std::vector<UINT64> input;
    std::vector<UINT64> target;
    std::vector<UINT64> result;
    if (!DSL_Gatekeeper_Node_Attribute
             (node, "attr.target_shape", &target_shape) ||
        !DSL_Gatekeeper_Static_Dimensions(input_ty, &input) ||
        !DSL_Gatekeeper_Parse_Unsigned_List
             (target_shape, FALSE, &target) ||
        !DSL_Gatekeeper_Static_Dimensions(result_ty, &result) ||
        !DSL_Gatekeeper_Dimensions_Equal(target, result))
        return FALSE;

    UINT64 input_elements = 1;
    UINT64 target_elements = 1;
    for (UINT32 i = 0; i < input.size(); ++i) {
        if (input_elements > ~(UINT64)0 / input[i])
            return FALSE;
        input_elements *= input[i];
    }
    for (UINT32 i = 0; i < target.size(); ++i) {
        if (target_elements > ~(UINT64)0 / target[i])
            return FALSE;
        target_elements *= target[i];
    }
    return input_elements == target_elements &&
           DSL_Gatekeeper_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Gatekeeper_Transpose_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    const char *permutation_text = NULL;
    std::vector<UINT64> input;
    std::vector<UINT64> permutation;
    std::vector<UINT64> expected;
    if (!DSL_Gatekeeper_Node_Attribute
             (node, "attr.permutation", &permutation_text) ||
        !DSL_Gatekeeper_Static_Dimensions(input_ty, &input) ||
        !DSL_Gatekeeper_Parse_Unsigned_List
             (permutation_text, TRUE, &permutation) ||
        permutation.size() != input.size())
        return FALSE;

    std::vector<BOOL> seen(input.size(), FALSE);
    expected.resize(input.size());
    for (UINT32 i = 0; i < permutation.size(); ++i) {
        if (permutation[i] >= input.size() || seen[permutation[i]])
            return FALSE;
        seen[permutation[i]] = TRUE;
        expected[i] = input[permutation[i]];
    }
    return DSL_Gatekeeper_Result_Dimensions(input_ty, result_ty, expected);
}

static BOOL
DSL_Gatekeeper_Matmul_V2_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         TY_IDX result_ty)
{
    BOOL transpose_kid0;
    BOOL transpose_kid1;
    if (DSL_Gatekeeper_Attribute_Equals
            (node, "attr.transpose_kid0", "true"))
        transpose_kid0 = TRUE;
    else if (DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.transpose_kid0", "false"))
        transpose_kid0 = FALSE;
    else
        return FALSE;
    if (DSL_Gatekeeper_Attribute_Equals
            (node, "attr.transpose_kid1", "true"))
        transpose_kid1 = TRUE;
    else if (DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.transpose_kid1", "false"))
        transpose_kid1 = FALSE;
    else
        return FALSE;
    if (!DSL_Gatekeeper_Attribute_Equals
             (node, "attr.batch_rule", "exact") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.accum_dtype", "float32"))
        return FALSE;

    std::vector<UINT64> kid0;
    std::vector<UINT64> kid1;
    if (!DSL_Gatekeeper_Static_Dimensions(kid0_ty, &kid0) ||
        !DSL_Gatekeeper_Static_Dimensions(kid1_ty, &kid1) ||
        kid0.size() < 2 || kid0.size() != kid1.size() ||
        !DSL_Gatekeeper_Tensor_Element_Representation_Compatible
             (kid0_ty, kid1_ty))
        return FALSE;
    for (UINT32 i = 0; i + 2 < kid0.size(); ++i) {
        if (kid0[i] != kid1[i])
            return FALSE;
    }

    UINT32 rank = kid0.size();
    UINT64 left_m = kid0[rank - (transpose_kid0 ? 1 : 2)];
    UINT64 left_k = kid0[rank - (transpose_kid0 ? 2 : 1)];
    UINT64 right_k = kid1[rank - (transpose_kid1 ? 1 : 2)];
    UINT64 right_n = kid1[rank - (transpose_kid1 ? 2 : 1)];
    if (left_k != right_k)
        return FALSE;

    std::vector<UINT64> expected = kid0;
    expected[rank - 2] = left_m;
    expected[rank - 1] = right_n;
    return DSL_Gatekeeper_Result_Dimensions(kid0_ty, result_ty, expected);
}

static BOOL
DSL_Gatekeeper_Output_Logits_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty,
         UINT16 version)
{
    if (!DSL_Gatekeeper_Tensor_Compatible(input_ty, result_ty, TRUE))
        return FALSE;
    if (version == 2)
        return DSL_Gatekeeper_Attribute_Equals
                   (node, "attr.semantic", "logits");
    if (version != 3 ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.semantic", "token_logits") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.sequence_axis", "-2") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.vocabulary_axis", "-1"))
        return FALSE;

    std::vector<UINT64> dimensions;
    return DSL_Gatekeeper_Static_Dimensions(input_ty, &dimensions) &&
           dimensions.size() == 3;
}

static BOOL
DSL_Gatekeeper_Positive_Float_Attribute
        (const DSL_IR_NODE_RECORD *node,
         const char *name)
{
    const char *text = NULL;
    if (!DSL_Gatekeeper_Node_Attribute(node, name, &text) ||
        text == NULL || text[0] == '\0')
        return FALSE;
    char *end = NULL;
    double value = strtod(text, &end);
    return end != text && *end == '\0' && value > 0.0 && value <= DBL_MAX;
}

static BOOL
DSL_Gatekeeper_Transformer_Result_Valid
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty)
{
    UINT32 expected_operands = 2;
    if (dsl_operator == OPR_DSLATTENTION)
        expected_operands = 3;
    else if (dsl_operator == OPR_DSLROTARYEMBEDDING)
        expected_operands = version == 2 ? 4 : 3;
    if (operands.size() != expected_operands)
        return FALSE;

    std::vector<UINT64> kid0;
    std::vector<UINT64> kid1;
    std::vector<UINT64> kid2;
    if (!DSL_Gatekeeper_Static_Dimensions(operands[0], &kid0) ||
        !DSL_Gatekeeper_Static_Dimensions(operands[1], &kid1))
        return FALSE;

    if (dsl_operator == OPR_DSLTOKENEMBEDDING) {
        const char *token_dtype =
            TY_tensor_attribute(operands[0], TY_TENSOR_SCHEMA_DTYPE);
        const char *weight_dtype =
            TY_tensor_attribute(operands[1], TY_TENSOR_SCHEMA_DTYPE);
        if (kid0.size() != 2 || kid1.size() != 2 ||
            token_dtype == NULL || strcmp(token_dtype, "int64") != 0 ||
            weight_dtype == NULL || strcmp(weight_dtype, "float32") != 0 ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.padding_idx", "none") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.bounds_policy", "runtime_check"))
            return FALSE;
        std::vector<UINT64> expected;
        expected.push_back(kid0[0]);
        expected.push_back(kid0[1]);
        expected.push_back(kid1[1]);
        return DSL_Gatekeeper_Computed_Result_Dimensions
                   (operands[1], result_ty, expected);
    }

    const char *activation_dtype =
        TY_tensor_attribute(operands[0], TY_TENSOR_SCHEMA_DTYPE);
    if (activation_dtype == NULL || strcmp(activation_dtype, "float32") != 0)
        return FALSE;

    if (dsl_operator == OPR_DSLRMSNORM) {
        return kid0.size() >= 2 && kid1.size() == 1 &&
               kid1[0] == kid0[kid0.size() - 1] &&
               DSL_Gatekeeper_Tensor_Element_Type_Compatible
                   (operands[0], operands[1]) &&
               DSL_Gatekeeper_Attribute_Equals(node, "attr.axis", "-1") &&
               DSL_Gatekeeper_Positive_Float_Attribute
                   (node, "attr.epsilon") &&
               DSL_Gatekeeper_Attribute_Equals
                   (node, "attr.accum_dtype", "float32") &&
               DSL_Gatekeeper_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    if (dsl_operator == OPR_DSLROTARYEMBEDDING) {
        std::vector<UINT64> position;
        if (!DSL_Gatekeeper_Static_Dimensions(operands[2], &kid2) ||
            kid0.size() != 4 || kid1.size() != 4 ||
            !DSL_Gatekeeper_Dimensions_Equal(kid1, kid2) ||
            kid1[0] != 1 || kid1[1] != 1 ||
            (version == 1 && kid1[2] != kid0[2]) ||
            (version == 2 && kid1[2] < kid0[2]) ||
            kid1[3] != kid0[3] || kid0[3] % 2 != 0 ||
            !DSL_Gatekeeper_Tensor_Element_Type_Compatible
                 (operands[0], operands[1]) ||
            !DSL_Gatekeeper_Tensor_Element_Type_Compatible
                 (operands[0], operands[2]) ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.head_layout", "BHSD") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.sequence_axis", "2") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.feature_axis", "3") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.pairing", "half_split") ||
            (version == 1 &&
             (!DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.position_mode", "zero_based_static") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.position_offset", "0"))) ||
            (version == 2 &&
             (!DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.position_mode", "explicit_operand") ||
              !DSL_Gatekeeper_Static_Dimensions(operands[3], &position) ||
              position.size() != 1 || position[0] != kid0[2] ||
              strcmp(TY_tensor_attribute
                         (operands[3], TY_TENSOR_SCHEMA_DTYPE),
                     "int64") != 0)))
            return FALSE;
        return DSL_Gatekeeper_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    if (dsl_operator == OPR_DSLATTENTION) {
        const char *query_heads_text = NULL;
        const char *kv_heads_text = NULL;
        const char *head_dim_text = NULL;
        UINT64 query_heads;
        UINT64 kv_heads;
        UINT64 head_dim;
        if (!DSL_Gatekeeper_Static_Dimensions(operands[2], &kid2) ||
            kid0.size() != 4 || kid1.size() != 4 || kid2.size() != 4 ||
            (version == 1 &&
             (!DSL_Gatekeeper_Dimensions_Equal(kid0, kid1) ||
              !DSL_Gatekeeper_Dimensions_Equal(kid0, kid2))) ||
            (version == 2 &&
             (kid0[0] != kid1[0] || kid0[0] != kid2[0] ||
              kid0[2] != 1 || !DSL_Gatekeeper_Dimensions_Equal(kid1, kid2))) ||
            !DSL_Gatekeeper_Tensor_Element_Representation_Compatible
                 (operands[0], operands[1]) ||
            !DSL_Gatekeeper_Tensor_Element_Representation_Compatible
                 (operands[0], operands[2]) ||
            !DSL_Gatekeeper_Node_Attribute
                 (node, "attr.query_heads", &query_heads_text) ||
            !DSL_Gatekeeper_Node_Attribute
                 (node, "attr.kv_heads", &kv_heads_text) ||
            !DSL_Gatekeeper_Node_Attribute
                 (node, "attr.head_dim", &head_dim_text) ||
            !DSL_Gatekeeper_Parse_Unsigned(query_heads_text, &query_heads) ||
            !DSL_Gatekeeper_Parse_Unsigned(kv_heads_text, &kv_heads) ||
            !DSL_Gatekeeper_Parse_Unsigned(head_dim_text, &head_dim) ||
            query_heads == 0 || query_heads != kv_heads || head_dim == 0 ||
            query_heads != kid0[1] || head_dim != kid0[3] ||
            (version == 2 &&
             (kv_heads != kid1[1] || head_dim != kid1[3])) ||
            (version == 1 &&
             (!DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.execution_mode", "full_sequence") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.mask_mode", "causal") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.cache_mode", "none"))) ||
            (version == 2 &&
             (!DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.execution_mode", "single_token_decode") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.mask_mode", "implicit_prefix_causal") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.cache_mode", "functional_append") ||
              !DSL_Gatekeeper_Attribute_Equals
                  (node, "attr.cache_sequence_axis", "2"))) ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.head_layout", "BHSD") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.scale_mode", "inverse_sqrt_head_dim") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.softmax_axis", "-1") ||
            !DSL_Gatekeeper_Attribute_Equals
                 (node, "attr.softmax_accum_dtype", "float32"))
            return FALSE;
        return DSL_Gatekeeper_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    return dsl_operator == OPR_DSLSWIGLU && kid0.size() == 3 &&
           DSL_Gatekeeper_Dimensions_Equal(kid0, kid1) &&
           DSL_Gatekeeper_Tensor_Element_Representation_Compatible
               (operands[0], operands[1]) &&
           DSL_Gatekeeper_Attribute_Equals
               (node, "attr.activation", "silu") &&
           DSL_Gatekeeper_Result_Dimensions(operands[0], result_ty, kid0);
}

static BOOL
DSL_Gatekeeper_Conv2D_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty)
{
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    const char *groups_text = NULL;
    UINT64 groups;
    if (operands.size() != 3 ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.input_layout", "NCHW") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.weight_layout", "OIHW") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.output_layout", "NCHW") ||
        !DSL_Gatekeeper_Parse_Pair
             (node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Gatekeeper_Parse_Pair(node, "attr.stride", FALSE, stride) ||
        !DSL_Gatekeeper_Parse_Pair(node, "attr.padding", TRUE, padding) ||
        !DSL_Gatekeeper_Parse_Pair
             (node, "attr.dilation", FALSE, dilation) ||
        !DSL_Gatekeeper_Node_Attribute
             (node, "attr.groups", &groups_text) ||
        !DSL_Gatekeeper_Parse_Unsigned(groups_text, &groups) || groups == 0)
        return FALSE;

    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Gatekeeper_Static_Dimensions(operands[0], &input) ||
        !DSL_Gatekeeper_Static_Dimensions(operands[1], &weight) ||
        !DSL_Gatekeeper_Static_Dimensions(operands[2], &bias) ||
        input.size() != 4 || weight.size() != 4 || bias.size() != 1 ||
        kernel[0] != weight[2] || kernel[1] != weight[3] ||
        input[1] % groups != 0 || weight[0] % groups != 0 ||
        weight[1] > ~(UINT64)0 / groups ||
        weight[1] * groups != input[1] || bias[0] != weight[0] ||
        !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[1]) ||
        !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[2]))
        return FALSE;

    std::vector<UINT64> result(4);
    result[0] = input[0];
    result[1] = weight[0];
    for (UINT32 i = 0; i < 2; ++i) {
        if (kernel[i] - 1 > ~(UINT64)0 / dilation[i] ||
            padding[i] > (~(UINT64)0 - input[i + 2]) / 2)
            return FALSE;
        UINT64 effective_kernel = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = input[i + 2] + 2 * padding[i];
        if (padded < effective_kernel)
            return FALSE;
        result[i + 2] = (padded - effective_kernel) / stride[i] + 1;
    }
    return DSL_Gatekeeper_Result_Dimensions
               (operands[0], result_ty, result);
}

static BOOL
DSL_Gatekeeper_Batch_Norm_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty)
{
    const char *epsilon_text = NULL;
    if (operands.size() != 5 ||
        !DSL_Gatekeeper_Attribute_Equals(node, "attr.training", "false") ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.input_layout", "NCHW") ||
        !DSL_Gatekeeper_Attribute_Equals(node, "attr.channel_axis", "1") ||
        !DSL_Gatekeeper_Node_Attribute
             (node, "attr.epsilon", &epsilon_text))
        return FALSE;
    char *epsilon_end = NULL;
    double epsilon = strtod(epsilon_text, &epsilon_end);
    if (epsilon_end == epsilon_text || *epsilon_end != '\0' ||
        !(epsilon > 0.0 && epsilon < 1.0))
        return FALSE;

    std::vector<UINT64> input;
    if (!DSL_Gatekeeper_Static_Dimensions(operands[0], &input) ||
        input.size() != 4)
        return FALSE;
    for (UINT32 i = 1; i < operands.size(); ++i) {
        std::vector<UINT64> parameter;
        if (!DSL_Gatekeeper_Static_Dimensions(operands[i], &parameter) ||
            parameter.size() != 1 || parameter[0] != input[1] ||
            !DSL_Gatekeeper_Tensor_Element_Type_Compatible
                 (operands[0], operands[i]))
            return FALSE;
    }
    return DSL_Gatekeeper_Result_Dimensions
               (operands[0], result_ty, input);
}

static BOOL
DSL_Gatekeeper_Max_Pool_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    BOOL use_ceil;
    if (!DSL_Gatekeeper_Parse_Pair
             (node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Gatekeeper_Parse_Pair(node, "attr.stride", FALSE, stride) ||
        !DSL_Gatekeeper_Parse_Pair(node, "attr.padding", TRUE, padding) ||
        !DSL_Gatekeeper_Parse_Pair
             (node, "attr.dilation", FALSE, dilation) ||
        (!DSL_Gatekeeper_Attribute_Equals
              (node, "attr.ceil_mode", "false") &&
         !DSL_Gatekeeper_Attribute_Equals
              (node, "attr.ceil_mode", "true")))
        return FALSE;
    use_ceil = DSL_Gatekeeper_Attribute_Equals
                   (node, "attr.ceil_mode", "true");

    std::vector<UINT64> input;
    if (!DSL_Gatekeeper_Static_Dimensions(input_ty, &input) ||
        input.size() != 4)
        return FALSE;
    std::vector<UINT64> result = input;
    for (UINT32 i = 0; i < 2; ++i) {
        if (kernel[i] - 1 > ~(UINT64)0 / dilation[i] ||
            padding[i] > (~(UINT64)0 - input[i + 2]) / 2)
            return FALSE;
        UINT64 effective_kernel = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = input[i + 2] + 2 * padding[i];
        if (padded < effective_kernel)
            return FALSE;
        UINT64 numerator = padded - effective_kernel;
        result[i + 2] = numerator / stride[i] + 1;
        if (use_ceil && numerator % stride[i] != 0)
            ++result[i + 2];
    }
    return DSL_Gatekeeper_Result_Dimensions(input_ty, result_ty, result);
}

static BOOL
DSL_Gatekeeper_Global_Avg_Pool_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    UINT64 output_size[2];
    std::vector<UINT64> input;
    if (!DSL_Gatekeeper_Attribute_Equals
             (node, "attr.reduction_axes", "spatial") ||
        !DSL_Gatekeeper_Parse_Pair
             (node, "attr.output_size", FALSE, output_size) ||
        output_size[0] != 1 || output_size[1] != 1 ||
        !DSL_Gatekeeper_Static_Dimensions(input_ty, &input) ||
        input.size() != 4)
        return FALSE;
    input[2] = output_size[0];
    input[3] = output_size[1];
    return DSL_Gatekeeper_Result_Dimensions(input_ty, result_ty, input);
}

static BOOL
DSL_Gatekeeper_Flatten_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    const char *start_text = NULL;
    const char *end_text = NULL;
    INT32 start_dim;
    INT32 end_dim;
    INT32 rank = TY_tensor_rank(input_ty);
    if (rank <= 0 ||
        !DSL_Gatekeeper_Node_Attribute
             (node, "attr.start_dim", &start_text) ||
        !DSL_Gatekeeper_Node_Attribute
             (node, "attr.end_dim", &end_text) ||
        !DSL_Gatekeeper_Parse_Signed(start_text, &start_dim) ||
        !DSL_Gatekeeper_Parse_Signed(end_text, &end_dim))
        return FALSE;
    if (start_dim < 0)
        start_dim += rank;
    if (end_dim < 0)
        end_dim += rank;
    if (start_dim < 0 || end_dim < start_dim || end_dim >= rank)
        return FALSE;

    std::vector<UINT64> input_dimensions;
    std::vector<UINT64> result_dimensions;
    if (!DSL_Gatekeeper_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE),
              &input_dimensions) ||
        !DSL_Gatekeeper_Parse_Static_Shape
             (TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE),
              &result_dimensions) ||
        (INT32)input_dimensions.size() != rank ||
        (INT32)result_dimensions.size() !=
            rank - (end_dim - start_dim))
        return FALSE;

    UINT32 result_index = 0;
    for (INT32 i = 0; i < start_dim; ++i, ++result_index) {
        if (result_dimensions[result_index] != input_dimensions[i])
            return FALSE;
    }
    UINT64 flattened = 1;
    for (INT32 i = start_dim; i <= end_dim; ++i) {
        if (flattened > ~(UINT64)0 / input_dimensions[i])
            return FALSE;
        flattened *= input_dimensions[i];
    }
    if (result_dimensions[result_index++] != flattened)
        return FALSE;
    for (INT32 i = end_dim + 1; i < rank; ++i, ++result_index) {
        if (result_dimensions[result_index] != input_dimensions[i])
            return FALSE;
    }
    return DSL_Gatekeeper_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Gatekeeper_Parse_Unsigned
        (const char *text,
         UINT64 *value)
{
    if (text == NULL || text[0] == '\0')
        return FALSE;

    UINT64 parsed = 0;
    for (const char *cursor = text; *cursor != '\0'; ++cursor) {
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 digit = (UINT64)(*cursor - '0');
        if (parsed > (~(UINT64)0 - digit) / 10)
            return FALSE;
        parsed = parsed * 10 + digit;
    }
    if (value != NULL)
        *value = parsed;
    return TRUE;
}

static UINT64
DSL_Gatekeeper_Tensor_Element_Size (const char *dtype)
{
    if (dtype == NULL)
        return 0;
    if (strcmp(dtype, "bool") == 0 || strcmp(dtype, "int8") == 0 ||
        strcmp(dtype, "uint8") == 0)
        return 1;
    if (strcmp(dtype, "float16") == 0 || strcmp(dtype, "bfloat16") == 0 ||
        strcmp(dtype, "int16") == 0 || strcmp(dtype, "uint16") == 0)
        return 2;
    if (strcmp(dtype, "float32") == 0 || strcmp(dtype, "int32") == 0 ||
        strcmp(dtype, "uint32") == 0)
        return 4;
    if (strcmp(dtype, "float64") == 0 || strcmp(dtype, "int64") == 0 ||
        strcmp(dtype, "uint64") == 0)
        return 8;
    return 0;
}

static BOOL
DSL_Gatekeeper_Static_Tensor_Byte_Size
        (TY_IDX ty,
         UINT64 *element_size,
         UINT64 *byte_size)
{
    const char *dtype =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE);
    const char *shape =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);
    UINT64 item_size = DSL_Gatekeeper_Tensor_Element_Size(dtype);
    INT32 rank = TY_tensor_rank(ty);
    if (item_size == 0 || rank < 0 || shape == NULL || shape[0] != '[')
        return FALSE;

    const char *cursor = shape + 1;
    UINT64 elements = 1;
    INT32 dimension_count = 0;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 dimension = 0;
        while (isdigit((unsigned char)*cursor)) {
            UINT64 digit = (UINT64)(*cursor - '0');
            if (dimension > (~(UINT64)0 - digit) / 10)
                return FALSE;
            dimension = dimension * 10 + digit;
            ++cursor;
        }
        if (dimension == 0 || elements > ~(UINT64)0 / dimension)
            return FALSE;
        elements *= dimension;
        ++dimension_count;
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor != ']')
            return FALSE;
    }
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor != '\0' || dimension_count != rank ||
        elements > ~(UINT64)0 / item_size)
        return FALSE;
    if (element_size != NULL)
        *element_size = item_size;
    if (byte_size != NULL)
        *byte_size = elements * item_size;
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Verify_Model_Input
        (const DSL_IR_NODE_RECORD *node,
         DSL_GATEKEEPER_CONTEXT *context)
{
    const char *ordinal_text = NULL;
    UINT64 ordinal = 0;
    if (!DSL_Gatekeeper_Node_Attribute
             (node, "attr.input_ordinal", &ordinal_text) ||
        !DSL_Gatekeeper_Parse_Unsigned(ordinal_text, &ordinal) ||
        ordinal > ~(UINT32)0)
        return DSL_Gatekeeper_Report
                   (context, "common.model_input has an invalid input ordinal");

    for (size_t i = 0; i < context->input_ordinals.size(); ++i) {
        if (context->input_ordinals[i] == (UINT32)ordinal)
            return DSL_Gatekeeper_Report
                       (context, "common.model_input ordinal %u is duplicated",
                        (UINT32)ordinal);
    }
    context->input_ordinals.push_back((UINT32)ordinal);
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Checksum_Valid (const char *checksum)
{
    if (checksum == NULL || checksum[0] == '\0')
        return TRUE;
    if (strlen(checksum) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!isxdigit((unsigned char)checksum[i]))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Verify_External_Tensor
        (const DSL_IR_NODE_RECORD *node,
         ST_IDX result_st,
         TY_IDX result_ty,
         DSL_GATEKEEPER_CONTEXT *context)
{
    const char *value_kind = NULL;
    const char *value = NULL;
    if (!DSL_Gatekeeper_Node_Attribute
             (node, "value_kind", &value_kind) ||
        strcmp(value_kind, "external_data") != 0)
        return TRUE;
    if (!DSL_Gatekeeper_Node_Attribute(node, "value", &value))
        return DSL_Gatekeeper_Report
                   (context, "external tensor has no storage reference");

    const char *format = ST_tensor_metadata(result_st, "storage_format");
    const char *file = ST_tensor_metadata(result_st, "storage_file");
    const char *key = ST_tensor_metadata(result_st, "storage_tensor_key");
    const char *offset_text =
        ST_tensor_metadata(result_st, "storage_byte_offset");
    const char *length_text =
        ST_tensor_metadata(result_st, "storage_byte_length");
    const char *checksum =
        ST_tensor_metadata(result_st, "storage_checksum");
    UINT64 offset = 0;
    UINT64 length = 0;
    UINT64 element_size = 0;
    UINT64 tensor_byte_size = 0;
    if (format == NULL || format[0] == '\0' ||
        file == NULL || file[0] == '\0' || key == NULL || key[0] == '\0' ||
        !DSL_Gatekeeper_Parse_Unsigned(offset_text, &offset) ||
        !DSL_Gatekeeper_Parse_Unsigned(length_text, &length) || length == 0 ||
        offset + length < offset ||
        !DSL_Gatekeeper_Checksum_Valid(checksum) ||
        !DSL_Gatekeeper_Static_Tensor_Byte_Size
             (result_ty, &element_size, &tensor_byte_size) ||
        offset % element_size != 0 || length != tensor_byte_size)
        return DSL_Gatekeeper_Report
                   (context, "external tensor storage metadata is invalid");

    const char *layout =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_LAYOUT);
    const char *placement =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_PLACEMENT);
    const char *memory =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_MEMORY);
    if (layout == NULL || layout[0] == '\0' || placement == NULL ||
        strcmp(placement, "side_file") != 0 || memory == NULL ||
        strcmp(memory, "external_data") != 0)
        return DSL_Gatekeeper_Report
                   (context, "external tensor descriptor does not describe "
                    "side-file storage");

    size_t expected_size = strlen(format) + strlen(file) + strlen(key) +
                           strlen(checksum == NULL ? "" : checksum) + 96;
    char *expected = new char[expected_size];
    snprintf(expected, expected_size,
             "%s://%s#%s?offset=%llu&length=%llu&checksum=%s",
             format, file, key, (unsigned long long)offset,
             (unsigned long long)length, checksum == NULL ? "" : checksum);
    BOOL matches = strcmp(value, expected) == 0;
    delete [] expected;
    return matches ? TRUE : DSL_Gatekeeper_Report
                                (context, "external tensor storage reference "
                                 "does not match its metadata");
}

static BOOL
DSL_Gatekeeper_Required_Attributes
        (const DSL_IR_NODE_RECORD *node,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor,
         DSL_GATEKEEPER_CONTEXT *context)
{
    const char *schema = descriptor->attribute_schema == STR_IDX_ZERO ? "" :
                         Index_To_Str(descriptor->attribute_schema);
    const char *cursor = schema;
    BOOL valid = TRUE;
    UINT32 required_count = 0;

    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t length = end == NULL ? strlen(cursor) :
                                     (size_t)(end - cursor);
        char *name = new char[length + 1];
        memcpy (name, cursor, length);
        name[length] = '\0';
        if (length != 0) {
            ++required_count;
            if (!DSL_Gatekeeper_Node_Has_Attribute(node, name))
                valid = DSL_Gatekeeper_Report
                            (context, "%s.v%u is missing typed attribute %s",
                             Index_To_Str(descriptor->stable_name),
                             descriptor->version, name);
        }
        delete [] name;
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    DSL_OPERATOR dsl_operator =
        (DSL_OPERATOR)descriptor->logical_operator;
    BOOL exact_schema = dsl_operator == OPR_DSLRESHAPE ||
                        dsl_operator == OPR_DSLTRANSPOSE ||
                        dsl_operator == OPR_DSLTOKENEMBEDDING ||
                        dsl_operator == OPR_DSLRMSNORM ||
                        dsl_operator == OPR_DSLROTARYEMBEDDING ||
                        dsl_operator == OPR_DSLATTENTION ||
                        dsl_operator == OPR_DSLSWIGLU ||
                        dsl_operator == OPR_DSLSCATTER ||
                        (dsl_operator == OPR_DSLMATMUL &&
                         descriptor->version == 2) ||
                        (dsl_operator == OPR_DSLLINEAR &&
                         descriptor->version == 3) ||
                        (dsl_operator == OPR_DSLOUTPUTLOGITS &&
                         descriptor->version == 3);
    if (exact_schema && node->attribute_count != required_count)
        valid = DSL_Gatekeeper_Report
                    (context, "%s.v%u requires exactly %u typed attributes, "
                     "found %u", Index_To_Str(descriptor->stable_name),
                     descriptor->version, required_count,
                     node->attribute_count);
    return valid;
}

static BOOL
DSL_Gatekeeper_Verify_Memory_Behavior
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         const DSL_IR_NODE_RECORD *node,
         DSL_GATEKEEPER_CONTEXT *context)
{
    DSL_MEMORY_BEHAVIOR_CONTRACT contract;
    if (!DSL_Memory_Behavior_Get_Contract
             (dsl_operator, version, &contract))
        return DSL_Gatekeeper_Report
                   (context, "%s.v%u has no operand memory contract",
                    DSL_OPERATOR_name(dsl_operator), version);
    if (contract.operand_count != node->operand_count)
        return DSL_Gatekeeper_Report
                   (context, "%s.v%u memory contract has wrong operand count",
                    DSL_OPERATOR_name(dsl_operator), version);

    UINT32 effect_count = 0;
    UINT32 modify_count = 0;
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect))
            return FALSE;
        if (effect.owner_node_id != node->id)
            continue;
        ++effect_count;
        if (effect.effect_kind == DSL_STATE_EFFECT_MODIFY)
            ++modify_count;
    }
    if (dsl_operator == OPR_DSLSCATTER &&
        (effect_count == 0 || modify_count == 0))
        return DSL_Gatekeeper_Report
                   (context, "OPR_DSLSCATTER requires a modifying state edge");
    if (dsl_operator == OPR_DSLATTENTION && version == 2 &&
        (effect_count != 2 || modify_count != 2))
        return DSL_Gatekeeper_Report
                   (context, "OPR_DSLATTENTION.v2 requires distinct key and "
                    "value cache MODIFY edges");
    return TRUE;
}

static BOOL
DSL_Gatekeeper_Verify_Native_Node
        (WN *assignment,
         WN *expression,
         DSL_GATEKEEPER_CONTEXT *context)
{
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPCODE_ANNOTATION annotation;
    DSL_OPERATOR dsl_operator = OPR_DSLUNKNOWN;
    DSL_OPERATOR_INFO info;
    DSL_IR_NODE_RECORD image_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD image_descriptor;
    ST_IDX result_st = WN_st_idx(assignment);
    TY_IDX result_ty = WN_ty(assignment);
    BOOL valid = TRUE;
    BOOL image_valid;
    const char *result_name = DSL_Gatekeeper_ST_Valid(result_st) ?
                              ST_name(St_Table[result_st]) : NULL;

    ++context->result.native_node_count;
    if (!DSL_WN_Get_Logical_Opcode(expression, &logical_opcode,
                                   context->diagnostic) ||
        (dsl_operator = logical_opcode.dsl_operator) == OPR_DSLUNKNOWN ||
        !DSL_Operator_Get_Info_Version
             (dsl_operator, logical_opcode.effective_version, &info) ||
        (info.nkids >= 0 && (UINT32)info.nkids != WN_kid_count(expression)))
        valid = DSL_Gatekeeper_Report
                    (context, "unsupported logical operator or version");
    if (dsl_operator == OPR_DSLDIVREM ||
        dsl_operator == OPR_DSLDIVPART ||
        dsl_operator == OPR_DSLREMPART)
        valid = DSL_Gatekeeper_Report
                   (context, "%s is a WOPT-internal projectable operator "
                    "without a selected binary lowering contract",
                    DSL_OPERATOR_name(dsl_operator));

    if (WN_rtype(expression) != MTYPE_M || WN_desc(expression) != MTYPE_V ||
        WN_desc(assignment) != MTYPE_M ||
        !DSL_Gatekeeper_ST_Valid(result_st) ||
        ST_class(St_Table[result_st]) != CLASS_VAR ||
        !ST_is_temp_var(St_Table[result_st]) ||
        ST_type(St_Table[result_st]) != result_ty ||
        !DSL_Gatekeeper_Tensor_Core_Complete(result_ty) ||
        !DSL_Gatekeeper_Has_Unique_Ownership(result_st))
        valid = DSL_Gatekeeper_Report
                    (context, "%s result is not a complete no-alias tensor "
                     "temporary", DSL_OPERATOR_name(dsl_operator));

    image_valid = DSL_WN_Get_Opcode_Annotation(expression, &annotation) &&
                  annotation.payload != NULL &&
                  DSL_Gatekeeper_Find_Image_Node
                      (result_st, result_name, dsl_operator,
                       logical_opcode.effective_version, annotation.payload,
                       &image_node, &image_descriptor);
    if (!image_valid)
        valid = DSL_Gatekeeper_Report
                    (context, "%s result has no matching DSL image node",
                     DSL_OPERATOR_name(dsl_operator));
    else {
        if (!DSL_Gatekeeper_Required_Attributes
                 (&image_node, &image_descriptor, context))
            valid = FALSE;
        if (dsl_operator == OPR_DSLMODELINPUT &&
            !DSL_Gatekeeper_Verify_Model_Input(&image_node, context))
            valid = FALSE;
        if (dsl_operator == OPR_DSLTENSORCONST &&
            !DSL_Gatekeeper_Verify_External_Tensor
                 (&image_node, result_st, result_ty, context))
            valid = FALSE;
        if (info.effect_model == DSL_EFFECT_MODEL_RUNTIME_EFFECT &&
            !DSL_Gatekeeper_Verify_Memory_Behavior
                 (dsl_operator, logical_opcode.effective_version,
                  &image_node, context))
            valid = FALSE;
    }

    TY_IDX first_operand_ty = TY_IDX_ZERO;
    TY_IDX second_operand_ty = TY_IDX_ZERO;
    std::vector<TY_IDX> operand_types
                            (WN_kid_count(expression), TY_IDX_ZERO);
    for (UINT32 i = 0; i < WN_kid_count(expression); ++i) {
        WN *operand = WN_kid(expression, i);
        if (operand == NULL || WN_operator(operand) != OPR_LDID ||
            WN_rtype(operand) != MTYPE_M || WN_desc(operand) != MTYPE_M ||
            !DSL_Gatekeeper_ST_Valid(WN_st_idx(operand)) ||
            !DSL_Gatekeeper_Is_Result_Symbol
                 (context, WN_st_idx(operand)) ||
            ST_type(St_Table[WN_st_idx(operand)]) != WN_ty(operand) ||
            !DSL_Gatekeeper_Tensor_Core_Complete(WN_ty(operand))) {
            valid = DSL_Gatekeeper_Report
                        (context, "%s kid%u is not a direct tensor-result "
                         "LDID", DSL_OPERATOR_name(dsl_operator), i);
            continue;
        }
        operand_types[i] = WN_ty(operand);
        if (i == 0)
            first_operand_ty = WN_ty(operand);
        else {
            if (i == 1)
                second_operand_ty = WN_ty(operand);
            if ((dsl_operator == OPR_DSLADD ||
                 dsl_operator == OPR_DSLMUL ||
                 dsl_operator == OPR_DSLDIV ||
                 dsl_operator == OPR_DSLREM ||
                 dsl_operator == OPR_DSLMATMUL ||
                 dsl_operator == OPR_DSLRESIDUALADD) &&
                !DSL_Gatekeeper_Tensor_Compatible
                      (first_operand_ty, WN_ty(operand),
                       dsl_operator == OPR_DSLADD ||
                       dsl_operator == OPR_DSLMUL ||
                       dsl_operator == OPR_DSLDIV ||
                       dsl_operator == OPR_DSLREM ||
                       dsl_operator == OPR_DSLRESIDUALADD))
                valid = DSL_Gatekeeper_Report
                            (context, "%s kid%u tensor is incompatible "
                             "with kid0", DSL_OPERATOR_name(dsl_operator),
                             i);
        }
    }

    if ((dsl_operator == OPR_DSLADD ||
         dsl_operator == OPR_DSLMUL ||
         dsl_operator == OPR_DSLDIV ||
         dsl_operator == OPR_DSLREM) &&
        first_operand_ty != TY_IDX_ZERO &&
        !DSL_Gatekeeper_Tensor_Compatible
             (first_operand_ty, result_ty, TRUE))
        valid = DSL_Gatekeeper_Report
                    (context, "%s result tensor is incompatible "
                     "with its operands",
                     DSL_OPERATOR_name(dsl_operator));
    if (dsl_operator == OPR_DSLMATMUL && first_operand_ty != TY_IDX_ZERO) {
        BOOL matmul_valid = second_operand_ty != TY_IDX_ZERO && image_valid;
        if (matmul_valid && logical_opcode.effective_version == 1)
            matmul_valid = DSL_Gatekeeper_Tensor_Compatible
                               (first_operand_ty, result_ty, FALSE) &&
                           DSL_Gatekeeper_Matmul_Shapes_Compatible
                               (first_operand_ty, second_operand_ty, result_ty);
        else if (matmul_valid && logical_opcode.effective_version == 2)
            matmul_valid = DSL_Gatekeeper_Matmul_V2_Result_Valid
                               (&image_node, first_operand_ty,
                                second_operand_ty, result_ty);
        else
            matmul_valid = FALSE;
        if (!matmul_valid)
            valid = DSL_Gatekeeper_Report
                        (context, "OPR_DSLMATMUL.v%u tensor or attribute "
                         "contract is invalid",
                         logical_opcode.effective_version);
    }
    if ((dsl_operator == OPR_DSLRELU ||
         dsl_operator == OPR_DSLRESIDUALADD ||
         dsl_operator == OPR_DSLOUTPUTLOGITS ||
         dsl_operator == OPR_DSLSCATTER) &&
        first_operand_ty != TY_IDX_ZERO &&
        !DSL_Gatekeeper_Tensor_Compatible
             (first_operand_ty, result_ty, TRUE))
        valid = DSL_Gatekeeper_Report
                    (context, "%s result tensor is incompatible with kid0",
                     DSL_OPERATOR_name(dsl_operator));
    if (image_valid && dsl_operator == OPR_DSLFLATTEN &&
        first_operand_ty != TY_IDX_ZERO &&
        !DSL_Gatekeeper_Flatten_Result_Valid
             (&image_node, first_operand_ty, result_ty))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLFLATTEN result shape is invalid");
    if (image_valid && dsl_operator == OPR_DSLLINEAR &&
        !DSL_Gatekeeper_Linear_Result_Valid
             (&image_node, operand_types, result_ty,
              logical_opcode.effective_version))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLLINEAR tensor or attribute contract "
                     "is invalid");
    if (image_valid && dsl_operator == OPR_DSLCONV2D &&
        !DSL_Gatekeeper_Conv2D_Result_Valid
             (&image_node, operand_types, result_ty))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLCONV2D tensor or attribute contract "
                     "is invalid");
    if (image_valid && dsl_operator == OPR_DSLBATCHNORMINFER &&
        !DSL_Gatekeeper_Batch_Norm_Result_Valid
             (&image_node, operand_types, result_ty))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLBATCHNORMINFER tensor or attribute "
                     "contract is invalid");
    if (image_valid && dsl_operator == OPR_DSLMAXPOOL2D &&
        (first_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Max_Pool_Result_Valid
              (&image_node, first_operand_ty, result_ty)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLMAXPOOL2D result shape or attribute "
                     "contract is invalid");
    if (image_valid && dsl_operator == OPR_DSLGLOBALAVGPOOL2D &&
        (first_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Global_Avg_Pool_Result_Valid
              (&image_node, first_operand_ty, result_ty)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLGLOBALAVGPOOL2D result shape or "
                     "attribute contract is invalid");
    if (image_valid && dsl_operator == OPR_DSLRESHAPE &&
        (first_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Reshape_Result_Valid
              (&image_node, first_operand_ty, result_ty)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLRESHAPE result shape or attribute "
                     "contract is invalid");
    if (image_valid && dsl_operator == OPR_DSLTRANSPOSE &&
        (first_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Transpose_Result_Valid
              (&image_node, first_operand_ty, result_ty)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLTRANSPOSE result shape or permutation "
                     "contract is invalid");
    if (image_valid &&
        (dsl_operator == OPR_DSLTOKENEMBEDDING ||
         dsl_operator == OPR_DSLRMSNORM ||
         dsl_operator == OPR_DSLROTARYEMBEDDING ||
         dsl_operator == OPR_DSLATTENTION ||
         dsl_operator == OPR_DSLSWIGLU) &&
        !DSL_Gatekeeper_Transformer_Result_Valid
             (dsl_operator, logical_opcode.effective_version,
              &image_node, operand_types, result_ty))
        valid = DSL_Gatekeeper_Report
                    (context, "%s.v%u transformer expression contract is "
                     "invalid", info.stable_name,
                     logical_opcode.effective_version);
    if (image_valid && dsl_operator == OPR_DSLRESIDUALADD &&
        (!DSL_Gatekeeper_Attribute_Equals
             (&image_node, "attr.broadcast_rule", "none") ||
         !DSL_Gatekeeper_Attribute_Equals
             (&image_node, "attr.shape_check", "exact") ||
         !DSL_Gatekeeper_Attribute_Equals
             (&image_node, "attr.residual_path", "true")))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLRESIDUALADD requires exact no-broadcast "
                     "residual semantics");
    if (image_valid && dsl_operator == OPR_DSLOUTPUTLOGITS &&
        (first_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Output_Logits_Result_Valid
              (&image_node, first_operand_ty, result_ty,
               logical_opcode.effective_version)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLOUTPUTLOGITS.v%u tensor or attribute "
                     "contract is invalid", logical_opcode.effective_version);
    return valid;
}

static BOOL
DSL_Gatekeeper_Verify_Tree
        (WN *wn,
         WN *parent,
         INT parent_kid,
         DSL_GATEKEEPER_CONTEXT *context)
{
    if (wn == NULL)
        return TRUE;

    BOOL valid = TRUE;
    if (DSL_WN_Is_Native(wn)) {
        if (parent == NULL || WN_operator(parent) != OPR_STID ||
            parent_kid != 0)
            valid = DSL_Gatekeeper_Report
                        (context, "%s is not the value of a result STID",
                         DSL_OPERATOR_name(DSL_WN_operator(wn)));
        else if (!DSL_Gatekeeper_Verify_Native_Node(parent, wn, context))
            valid = FALSE;
    }

    if ((WN_operator(wn) == OPR_LDA || WN_operator(wn) == OPR_LDID) &&
        DSL_Gatekeeper_Is_Result_Symbol(context, WN_st_idx(wn))) {
        BOOL direct_operand = WN_operator(wn) == OPR_LDID && parent != NULL &&
                              DSL_WN_Is_Native(parent);
        BOOL result_copy = WN_operator(wn) == OPR_LDID && parent != NULL &&
                           WN_operator(parent) == OPR_STID &&
                           ST_sclass(St_Table[WN_st_idx(parent)]) ==
                               SCLASS_FORMAL_REF;
        BOOL call_interface = WN_operator(wn) == OPR_LDA && parent != NULL &&
                              WN_operator(parent) == OPR_PARM &&
                              WN_Parm_By_Reference(parent) &&
                              WN_Parm_Passed_Not_Saved(parent) &&
                              (WN_Parm_Read_Only(parent) ||
                               WN_Parm_Out(parent));
        if (!direct_operand && !result_copy && !call_interface)
            valid = DSL_Gatekeeper_Report
                        (context, "result symbol <%u,%u> escapes through %s",
                         ST_IDX_level(WN_st_idx(wn)),
                         ST_IDX_index(WN_st_idx(wn)),
                         OPERATOR_name(WN_operator(wn)));
    }

    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
            if (!DSL_Gatekeeper_Verify_Tree(stmt, wn, -1, context))
                valid = FALSE;
        }
        return valid;
    }

    for (INT i = 0; i < WN_kid_count(wn); ++i) {
        if (!DSL_Gatekeeper_Verify_Tree(WN_kid(wn, i), wn, i, context))
            valid = FALSE;
    }
    return valid;
}

BOOL
DSL_Gatekeeper_Verify_PU
        (PU_Info *pu,
         FILE *diagnostic,
         DSL_GATEKEEPER_RESULT *result)
{
    DSL_GATEKEEPER_CONTEXT context;
    memset (&context.result, 0, sizeof(context.result));
    context.diagnostic = diagnostic;

    BOOL valid = DSL_IR_Image_Validate(diagnostic);
    if (!valid)
        ++context.result.error_count;
    if (!DSL_Effect_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++context.result.error_count;
    }
    if (!DSL_Call_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++context.result.error_count;
    }
    if (pu == NULL || PU_Info_state(pu, WT_TREE) != Subsect_InMem ||
        PU_Info_tree_ptr(pu) == NULL)
        valid = DSL_Gatekeeper_Report
                    (&context, "program unit tree is not in memory");
    else {
        if (!DSL_Gatekeeper_Collect_Results(PU_Info_tree_ptr(pu), &context))
            valid = FALSE;
        context.result.result_symbol_count = context.result_symbols.size();
        if (!DSL_Gatekeeper_Verify_Tree
                 (PU_Info_tree_ptr(pu), NULL, -1, &context))
            valid = FALSE;
    }

    if (result != NULL)
        *result = context.result;
    return valid && context.result.error_count == 0;
}

BOOL
DSL_Gatekeeper_Verify_Program
        (PU_Info *pu_tree,
         FILE *diagnostic,
         DSL_GATEKEEPER_RESULT *result)
{
    DSL_GATEKEEPER_CONTEXT context;
    memset (&context.result, 0, sizeof(context.result));
    context.diagnostic = diagnostic;

    BOOL valid = DSL_IR_Image_Validate(diagnostic);
    if (!valid)
        ++context.result.error_count;
    if (!DSL_Effect_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++context.result.error_count;
    }
    if (!DSL_Call_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++context.result.error_count;
    }

    for (PU_Info *pu = pu_tree; pu != NULL; pu = PU_Info_next(pu)) {
        if (PU_Info_state(pu, WT_TREE) != Subsect_InMem ||
            PU_Info_tree_ptr(pu) == NULL) {
            valid = DSL_Gatekeeper_Report
                        (&context, "program unit tree is not in memory");
            continue;
        }
        if (!DSL_Gatekeeper_Collect_Results(PU_Info_tree_ptr(pu), &context))
            valid = FALSE;
    }
    context.result.result_symbol_count = context.result_symbols.size();

    for (PU_Info *pu = pu_tree; pu != NULL; pu = PU_Info_next(pu)) {
        if (PU_Info_state(pu, WT_TREE) == Subsect_InMem &&
            !DSL_Gatekeeper_Verify_Tree
                 (PU_Info_tree_ptr(pu), NULL, -1, &context))
            valid = FALSE;
    }

    if (context.result.native_node_count != DSL_IR_Image_Node_Count())
        valid = DSL_Gatekeeper_Report
                    (&context, "native tree node count %u does not match "
                     "DSL image node count %u",
                     context.result.native_node_count,
                     DSL_IR_Image_Node_Count());

    if (result != NULL)
        *result = context.result;
    return valid && context.result.error_count == 0;
}
static BOOL
DSL_Gatekeeper_Decode_Profile_Error
        (FILE *diagnostic,
         const char *code,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: %s\n", code, message);
    return FALSE;
}

BOOL
DSL_Gatekeeper_Verify_Transformer_Decode_Profile
        (const DSL_TRANSFORMER_DECODE_PROFILE *profile,
         FILE *diagnostic)
{
    if (profile == NULL)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DDECODE001", "decode profile is missing");
    if (profile->version != 1)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DDECODE001",
                    "decode contract version is unsupported");
    if (profile->batch_size == 0 || profile->query_head_count == 0 ||
        profile->kv_head_count == 0 || profile->head_dimension == 0)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DKVCACHE001",
                    "cache tensor dimensions must be nonzero");
    if (profile->decode_sequence_length != 1 ||
        profile->query_head_count != profile->kv_head_count)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DATTENTION201",
                    "version 1 requires one token and equal query/KV heads");
    if (profile->cache_rank != 4 || profile->cache_sequence_axis != 2 ||
        profile->cache_update != DSL_KV_CACHE_UPDATE_FUNCTIONAL_APPEND)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DKVCACHE002",
                    "version 1 requires BHSD functional append");
    if (profile->cache_position != profile->input_cache_length)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DDECODE002",
                    "cache position must equal the input valid length");
    if (profile->output_cache_length < profile->input_cache_length ||
        profile->output_cache_length - profile->input_cache_length !=
            profile->decode_sequence_length)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DATTENTION202",
                    "output cache length must append the decode length");
    if (profile->cache_position >= profile->rope_capacity ||
        profile->output_cache_length > profile->rope_capacity)
        return DSL_Gatekeeper_Decode_Profile_Error
                   (diagnostic, "DROTARY202",
                    "cache position or result exceeds RoPE capacity");

    return TRUE;
}
