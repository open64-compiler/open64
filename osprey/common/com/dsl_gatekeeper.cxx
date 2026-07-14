/*
 * Copyright (C) 2026 Open64 Project
 */

#include <stdarg.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
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
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Count(); ++i) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(i, &value) || value.st != result_st ||
            value.producer_node_id == DSL_IR_NODE_INVALID_ID)
            continue;
        if (!DSL_IR_Image_Get_Node(value.producer_node_id, node))
            return FALSE;
        return DSL_IR_Image_Get_Opcode_Descriptor
                   (node->opcode_descriptor_id, descriptor);
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
DSL_Gatekeeper_Linear_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty)
{
    if (operands.size() != 3 ||
        !DSL_Gatekeeper_Attribute_Equals
             (node, "attr.has_bias", "true") ||
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
        !DSL_Gatekeeper_Static_Dimensions(operands[2], &bias) ||
        input.empty() || weight.size() != 2 || bias.size() != 1 ||
        input[input.size() - 1] != weight[1] || bias[0] != weight[0] ||
        !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[1]) ||
        !DSL_Gatekeeper_Tensor_Element_Type_Compatible
             (operands[0], operands[2]))
        return FALSE;
    input[input.size() - 1] = weight[0];
    return DSL_Gatekeeper_Result_Dimensions
               (operands[0], result_ty, input);
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

    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t length = end == NULL ? strlen(cursor) :
                                     (size_t)(end - cursor);
        char *name = new char[length + 1];
        memcpy (name, cursor, length);
        name[length] = '\0';
        if (length != 0 && !DSL_Gatekeeper_Node_Has_Attribute(node, name))
            valid = DSL_Gatekeeper_Report
                        (context, "%s.v%u is missing typed attribute %s",
                         Index_To_Str(descriptor->stable_name),
                         descriptor->version, name);
        delete [] name;
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    return valid;
}

static BOOL
DSL_Gatekeeper_Verify_Native_Node
        (WN *assignment,
         WN *expression,
         DSL_GATEKEEPER_CONTEXT *context)
{
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPERATOR dsl_operator = OPR_DSLUNKNOWN;
    DSL_OPERATOR_INFO info;
    DSL_IR_NODE_RECORD image_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD image_descriptor;
    ST_IDX result_st = WN_st_idx(assignment);
    TY_IDX result_ty = WN_ty(assignment);
    BOOL valid = TRUE;
    BOOL image_valid;

    ++context->result.native_node_count;
    if (!DSL_WN_Get_Logical_Opcode(expression, &logical_opcode,
                                   context->diagnostic) ||
        (dsl_operator = logical_opcode.dsl_operator) == OPR_DSLUNKNOWN ||
        !DSL_Operator_Get_Info(dsl_operator, &info) ||
        info.version != logical_opcode.effective_version ||
        (info.nkids >= 0 && (UINT32)info.nkids != WN_kid_count(expression)))
        valid = DSL_Gatekeeper_Report
                    (context, "unsupported logical operator or version");

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

    image_valid = DSL_Gatekeeper_Find_Image_Node
                      (result_st, &image_node, &image_descriptor) &&
                  image_descriptor.logical_operator == (UINT32)dsl_operator &&
                  image_descriptor.version ==
                      logical_opcode.effective_version;
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
                 dsl_operator == OPR_DSLMATMUL ||
                 dsl_operator == OPR_DSLRESIDUALADD) &&
                !DSL_Gatekeeper_Tensor_Compatible
                      (first_operand_ty, WN_ty(operand),
                       dsl_operator == OPR_DSLADD ||
                       dsl_operator == OPR_DSLRESIDUALADD))
                valid = DSL_Gatekeeper_Report
                            (context, "%s kid%u tensor is incompatible "
                             "with kid0", DSL_OPERATOR_name(dsl_operator),
                             i);
        }
    }

    if (dsl_operator == OPR_DSLADD && first_operand_ty != TY_IDX_ZERO &&
        !DSL_Gatekeeper_Tensor_Compatible
             (first_operand_ty, result_ty, TRUE))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLADD result tensor is incompatible "
                     "with its operands");
    if (dsl_operator == OPR_DSLMATMUL && first_operand_ty != TY_IDX_ZERO &&
        (second_operand_ty == TY_IDX_ZERO ||
         !DSL_Gatekeeper_Tensor_Compatible
              (first_operand_ty, result_ty, FALSE) ||
         !DSL_Gatekeeper_Matmul_Shapes_Compatible
              (first_operand_ty, second_operand_ty, result_ty)))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLMATMUL result dtype or representation "
                     "is incompatible with kid0");
    if ((dsl_operator == OPR_DSLRELU ||
         dsl_operator == OPR_DSLRESIDUALADD ||
         dsl_operator == OPR_DSLOUTPUTLOGITS) &&
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
             (&image_node, operand_types, result_ty))
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
        !DSL_Gatekeeper_Attribute_Equals
             (&image_node, "attr.semantic", "logits"))
        valid = DSL_Gatekeeper_Report
                    (context, "OPR_DSLOUTPUTLOGITS requires semantic=logits");
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
        if (!direct_operand)
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
