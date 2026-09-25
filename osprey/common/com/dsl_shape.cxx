/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <string>
#include <vector>

#include "dsl_shape.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"

static BOOL DSL_Shape_Dimension_Symbol_Owner
        (const DSL_SHAPE_FACT &, INT32, ST_IDX *);
static BOOL DSL_Shape_PU_Locators_Equivalent(ST_IDX, ST_IDX);

static BOOL
DSL_Shape_TY_Valid (TY_IDX ty)
{
    return TY_IDX_index(ty) != 0 && TY_IDX_index(ty) < TY_Table_Size();
}

BOOL
DSL_Shape_Tensor_Core_Complete (TY_IDX ty)
{
    return DSL_Shape_TY_Valid(ty) && TY_is_tensor_extension(ty) &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_KIND) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_RANK) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE) != NULL;
}
static BOOL
DSL_Shape_Tensor_Key_Equal
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
DSL_Shape_Tensor_Element_Representation_Compatible
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

    if (!DSL_Shape_Tensor_Core_Complete(ty0) ||
        !DSL_Shape_Tensor_Core_Complete(ty1) ||
        TY_tensor_element_ty(ty0) != TY_tensor_element_ty(ty1) ||
        !DSL_Shape_Tensor_Key_Equal
             (ty0, ty1, TY_TENSOR_SCHEMA_DTYPE))
        return FALSE;

    for (UINT32 i = 0;
         i < sizeof(representation_keys) / sizeof(representation_keys[0]);
         ++i) {
        if (!DSL_Shape_Tensor_Key_Equal
                 (ty0, ty1, representation_keys[i]))
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_Shape_Tensor_Compatible
        (TY_IDX ty0,
         TY_IDX ty1,
         BOOL require_shape)
{
    return DSL_Shape_Tensor_Element_Representation_Compatible
               (ty0, ty1) &&
           DSL_Shape_Tensor_Key_Equal
               (ty0, ty1, TY_TENSOR_SCHEMA_RANK) &&
           (!require_shape || DSL_Shape_Tensor_Key_Equal
                                  (ty0, ty1, TY_TENSOR_SCHEMA_SHAPE));
}

static BOOL
DSL_Shape_Tensor_Element_Type_Compatible
        (TY_IDX ty0,
         TY_IDX ty1)
{
    return DSL_Shape_Tensor_Core_Complete(ty0) &&
           DSL_Shape_Tensor_Core_Complete(ty1) &&
           TY_tensor_element_ty(ty0) == TY_tensor_element_ty(ty1) &&
           DSL_Shape_Tensor_Key_Equal
               (ty0, ty1, TY_TENSOR_SCHEMA_DTYPE);
}

static BOOL
DSL_Shape_Parse_Matrix_Shape
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
DSL_Shape_Matmul_Shapes_Compatible
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
    return DSL_Shape_Parse_Matrix_Shape
               (TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_SHAPE),
                kid0_dim0, sizeof(kid0_dim0), kid0_dim1,
                sizeof(kid0_dim1)) &&
           DSL_Shape_Parse_Matrix_Shape
               (TY_tensor_attribute(kid1_ty, TY_TENSOR_SCHEMA_SHAPE),
                kid1_dim0, sizeof(kid1_dim0), kid1_dim1,
                sizeof(kid1_dim1)) &&
           DSL_Shape_Parse_Matrix_Shape
               (TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE),
                result_dim0, sizeof(result_dim0), result_dim1,
                sizeof(result_dim1)) &&
           strcmp(kid0_dim1, kid1_dim0) == 0 &&
           strcmp(result_dim0, kid0_dim0) == 0 &&
           strcmp(result_dim1, kid1_dim1) == 0;
}
static BOOL
DSL_Shape_Node_Attribute
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
DSL_Shape_Attribute_Equals
        (const DSL_IR_NODE_RECORD *node,
         const char *name,
         const char *expected)
{
    const char *value = NULL;
    return DSL_Shape_Node_Attribute(node, name, &value) &&
           value != NULL && strcmp(value, expected) == 0;
}

static BOOL
DSL_Shape_Parse_Signed
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
DSL_Shape_Parse_Static_Shape
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

BOOL
DSL_Shape_Parse_Static_Dimensions
        (const char *shape,
         UINT64 *dimensions,
         UINT32 capacity,
         UINT32 *rank)
{
    std::vector<UINT64> parsed;
    if (rank == NULL ||
        !DSL_Shape_Parse_Static_Shape(shape, &parsed))
        return FALSE;
    if (parsed.size() > ~(UINT32)0)
        return FALSE;
    *rank = (UINT32)parsed.size();
    if (dimensions == NULL)
        return capacity == 0;
    if (capacity < parsed.size())
        return FALSE;
    for (UINT32 i = 0; i < parsed.size(); ++i)
        dimensions[i] = parsed[i];
    return TRUE;
}

BOOL
DSL_Shape_Format_Static_Dimensions
        (const UINT64 *dimensions,
         UINT32 rank,
         char *buffer,
         size_t buffer_size)
{
    if (buffer == NULL || buffer_size == 0 ||
        (rank != 0 && dimensions == NULL))
        return FALSE;

    size_t used = 0;
    int written = snprintf(buffer, buffer_size, "[");
    if (written < 0 || (size_t)written >= buffer_size)
        return FALSE;
    used = written;
    for (UINT32 i = 0; i < rank; ++i) {
        if (dimensions[i] == 0)
            return FALSE;
        written = snprintf(buffer + used, buffer_size - used,
                           i == 0 ? "%llu" : ",%llu",
                           (unsigned long long)dimensions[i]);
        if (written < 0 || (size_t)written >= buffer_size - used)
            return FALSE;
        used += written;
    }
    written = snprintf(buffer + used, buffer_size - used, "]");
    return written >= 0 && (size_t)written < buffer_size - used;
}

static BOOL
DSL_Shape_Parse_Unsigned_List
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

static BOOL DSL_Shape_Parse_Unsigned(const char *, UINT64 *);

static BOOL
DSL_Shape_Parse_Pair
        (const DSL_IR_NODE_RECORD *node,
         const char *name,
         BOOL allow_zero,
         UINT64 values[2])
{
    const char *text = NULL;
    if (!DSL_Shape_Node_Attribute(node, name, &text) || text == NULL ||
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
DSL_Shape_Static_Dimensions
        (TY_IDX ty,
         std::vector<UINT64> *dimensions)
{
    return DSL_Shape_Parse_Static_Shape
               (TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE), dimensions);
}

static BOOL
DSL_Shape_Dimensions_Equal
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
DSL_Shape_Result_Dimensions
        (TY_IDX input_ty,
         TY_IDX result_ty,
         const std::vector<UINT64> &expected)
{
    std::vector<UINT64> result;
    return DSL_Shape_Static_Dimensions(result_ty, &result) &&
           DSL_Shape_Dimensions_Equal(result, expected) &&
           DSL_Shape_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Shape_Computed_Result_Dimensions
        (TY_IDX input_ty,
         TY_IDX result_ty,
         const std::vector<UINT64> &expected)
{
    std::vector<UINT64> result;
    const char *placement =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_PLACEMENT);
    const char *memory =
        TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_MEMORY);
    return DSL_Shape_Static_Dimensions(result_ty, &result) &&
           DSL_Shape_Dimensions_Equal(result, expected) &&
           DSL_Shape_Tensor_Element_Type_Compatible
               (input_ty, result_ty) &&
           DSL_Shape_Tensor_Key_Equal
               (input_ty, result_ty, TY_TENSOR_SCHEMA_LAYOUT) &&
           DSL_Shape_Tensor_Key_Equal
               (input_ty, result_ty, TY_TENSOR_SCHEMA_QUANTIZATION) &&
           (placement == NULL || strcmp(placement, "side_file") != 0) &&
           (memory == NULL || strcmp(memory, "external_data") != 0);
}

static BOOL
DSL_Shape_Linear_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    UINT32 expected_operands = version == 2 ? 3 : 2;
    const char *expected_bias = version == 2 ? "true" : "false";
    if ((version != 2 && version != 3) ||
        operands.size() != expected_operands ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.has_bias", expected_bias) ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.transpose_input", "false") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.transpose_weight", "true") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.weight_layout", "OI"))
        return FALSE;

    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Shape_Static_Dimensions(operands[0], &input) ||
        !DSL_Shape_Static_Dimensions(operands[1], &weight) ||
        input.size() < 2 || weight.size() != 2 ||
        input[input.size() - 1] != weight[1] ||
        !DSL_Shape_Tensor_Element_Type_Compatible
             (operands[0], operands[1]))
        return FALSE;
    if (version == 2 &&
        (!DSL_Shape_Static_Dimensions(operands[2], &bias) ||
         bias.size() != 1 || bias[0] != weight[0] ||
         !DSL_Shape_Tensor_Element_Type_Compatible
             (operands[0], operands[2])))
        return FALSE;
    input[input.size() - 1] = weight[0];
    return DSL_Shape_Result_Dimensions
               (operands[0], result_ty, input);
}

static BOOL
DSL_Shape_Reshape_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    const char *target_shape = NULL;
    std::vector<UINT64> input;
    std::vector<UINT64> target;
    std::vector<UINT64> result;
    if (!DSL_Shape_Node_Attribute
             (node, "attr.target_shape", &target_shape) ||
        !DSL_Shape_Static_Dimensions(input_ty, &input) ||
        !DSL_Shape_Parse_Unsigned_List
             (target_shape, FALSE, &target) ||
        !DSL_Shape_Static_Dimensions(result_ty, &result) ||
        !DSL_Shape_Dimensions_Equal(target, result))
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
           DSL_Shape_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Shape_Transpose_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    const char *permutation_text = NULL;
    std::vector<UINT64> input;
    std::vector<UINT64> permutation;
    std::vector<UINT64> expected;
    if (!DSL_Shape_Node_Attribute
             (node, "attr.permutation", &permutation_text) ||
        !DSL_Shape_Static_Dimensions(input_ty, &input) ||
        !DSL_Shape_Parse_Unsigned_List
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
    return DSL_Shape_Result_Dimensions(input_ty, result_ty, expected);
}

static BOOL
DSL_Shape_Matmul_V2_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         TY_IDX result_ty)
{
    BOOL transpose_kid0;
    BOOL transpose_kid1;
    if (DSL_Shape_Attribute_Equals
            (node, "attr.transpose_kid0", "true"))
        transpose_kid0 = TRUE;
    else if (DSL_Shape_Attribute_Equals
                 (node, "attr.transpose_kid0", "false"))
        transpose_kid0 = FALSE;
    else
        return FALSE;
    if (DSL_Shape_Attribute_Equals
            (node, "attr.transpose_kid1", "true"))
        transpose_kid1 = TRUE;
    else if (DSL_Shape_Attribute_Equals
                 (node, "attr.transpose_kid1", "false"))
        transpose_kid1 = FALSE;
    else
        return FALSE;
    if (!DSL_Shape_Attribute_Equals
             (node, "attr.batch_rule", "exact") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.accum_dtype", "float32"))
        return FALSE;

    std::vector<UINT64> kid0;
    std::vector<UINT64> kid1;
    if (!DSL_Shape_Static_Dimensions(kid0_ty, &kid0) ||
        !DSL_Shape_Static_Dimensions(kid1_ty, &kid1) ||
        kid0.size() < 2 || kid0.size() != kid1.size() ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
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
    return DSL_Shape_Result_Dimensions(kid0_ty, result_ty, expected);
}

static BOOL
DSL_Shape_Output_Logits_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty,
         UINT16 version)
{
    if (!DSL_Shape_Tensor_Compatible(input_ty, result_ty, TRUE))
        return FALSE;
    if (version == 2)
        return DSL_Shape_Attribute_Equals
                   (node, "attr.semantic", "logits");
    if (version != 3 ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.semantic", "token_logits") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.sequence_axis", "-2") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.vocabulary_axis", "-1"))
        return FALSE;

    std::vector<UINT64> dimensions;
    return DSL_Shape_Static_Dimensions(input_ty, &dimensions) &&
           dimensions.size() == 3;
}

static BOOL
DSL_Shape_Positive_Float_Attribute
        (const DSL_IR_NODE_RECORD *node,
         const char *name)
{
    const char *text = NULL;
    if (!DSL_Shape_Node_Attribute(node, name, &text) ||
        text == NULL || text[0] == '\0')
        return FALSE;
    char *end = NULL;
    double value = strtod(text, &end);
    return end != text && *end == '\0' && value > 0.0 && value <= DBL_MAX;
}

static BOOL
DSL_Shape_Transformer_Result_Valid
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
    if (!DSL_Shape_Static_Dimensions(operands[0], &kid0) ||
        !DSL_Shape_Static_Dimensions(operands[1], &kid1))
        return FALSE;

    if (dsl_operator == OPR_DSLTOKENEMBEDDING) {
        const char *token_dtype =
            TY_tensor_attribute(operands[0], TY_TENSOR_SCHEMA_DTYPE);
        const char *weight_dtype =
            TY_tensor_attribute(operands[1], TY_TENSOR_SCHEMA_DTYPE);
        if (kid0.size() != 2 || kid1.size() != 2 ||
            token_dtype == NULL || strcmp(token_dtype, "int64") != 0 ||
            weight_dtype == NULL || strcmp(weight_dtype, "float32") != 0 ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.padding_idx", "none") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.bounds_policy", "runtime_check"))
            return FALSE;
        std::vector<UINT64> expected;
        expected.push_back(kid0[0]);
        expected.push_back(kid0[1]);
        expected.push_back(kid1[1]);
        return DSL_Shape_Computed_Result_Dimensions
                   (operands[1], result_ty, expected);
    }

    const char *activation_dtype =
        TY_tensor_attribute(operands[0], TY_TENSOR_SCHEMA_DTYPE);
    if (activation_dtype == NULL || strcmp(activation_dtype, "float32") != 0)
        return FALSE;

    if (dsl_operator == OPR_DSLRMSNORM) {
        return kid0.size() >= 2 && kid1.size() == 1 &&
               kid1[0] == kid0[kid0.size() - 1] &&
               DSL_Shape_Tensor_Element_Type_Compatible
                   (operands[0], operands[1]) &&
               DSL_Shape_Attribute_Equals(node, "attr.axis", "-1") &&
               DSL_Shape_Positive_Float_Attribute
                   (node, "attr.epsilon") &&
               DSL_Shape_Attribute_Equals
                   (node, "attr.accum_dtype", "float32") &&
               DSL_Shape_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    if (dsl_operator == OPR_DSLROTARYEMBEDDING) {
        std::vector<UINT64> position;
        if (!DSL_Shape_Static_Dimensions(operands[2], &kid2) ||
            kid0.size() != 4 || kid1.size() != 4 ||
            !DSL_Shape_Dimensions_Equal(kid1, kid2) ||
            kid1[0] != 1 || kid1[1] != 1 ||
            (version == 1 && kid1[2] != kid0[2]) ||
            (version == 2 && kid1[2] < kid0[2]) ||
            kid1[3] != kid0[3] || kid0[3] % 2 != 0 ||
            !DSL_Shape_Tensor_Element_Type_Compatible
                 (operands[0], operands[1]) ||
            !DSL_Shape_Tensor_Element_Type_Compatible
                 (operands[0], operands[2]) ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.head_layout", "BHSD") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.sequence_axis", "2") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.feature_axis", "3") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.pairing", "half_split") ||
            (version == 1 &&
             (!DSL_Shape_Attribute_Equals
                  (node, "attr.position_mode", "zero_based_static") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.position_offset", "0"))) ||
            (version == 2 &&
             (!DSL_Shape_Attribute_Equals
                  (node, "attr.position_mode", "explicit_operand") ||
              !DSL_Shape_Static_Dimensions(operands[3], &position) ||
              position.size() != 1 || position[0] != kid0[2] ||
              strcmp(TY_tensor_attribute
                         (operands[3], TY_TENSOR_SCHEMA_DTYPE),
                     "int64") != 0)))
            return FALSE;
        return DSL_Shape_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    if (dsl_operator == OPR_DSLATTENTION) {
        const char *query_heads_text = NULL;
        const char *kv_heads_text = NULL;
        const char *head_dim_text = NULL;
        UINT64 query_heads;
        UINT64 kv_heads;
        UINT64 head_dim;
        if (!DSL_Shape_Static_Dimensions(operands[2], &kid2) ||
            kid0.size() != 4 || kid1.size() != 4 || kid2.size() != 4 ||
            (version == 1 &&
             (!DSL_Shape_Dimensions_Equal(kid0, kid1) ||
              !DSL_Shape_Dimensions_Equal(kid0, kid2))) ||
            (version == 2 &&
             (kid0[0] != kid1[0] || kid0[0] != kid2[0] ||
              kid0[2] != 1 || !DSL_Shape_Dimensions_Equal(kid1, kid2))) ||
            !DSL_Shape_Tensor_Element_Representation_Compatible
                 (operands[0], operands[1]) ||
            !DSL_Shape_Tensor_Element_Representation_Compatible
                 (operands[0], operands[2]) ||
            !DSL_Shape_Node_Attribute
                 (node, "attr.query_heads", &query_heads_text) ||
            !DSL_Shape_Node_Attribute
                 (node, "attr.kv_heads", &kv_heads_text) ||
            !DSL_Shape_Node_Attribute
                 (node, "attr.head_dim", &head_dim_text) ||
            !DSL_Shape_Parse_Unsigned(query_heads_text, &query_heads) ||
            !DSL_Shape_Parse_Unsigned(kv_heads_text, &kv_heads) ||
            !DSL_Shape_Parse_Unsigned(head_dim_text, &head_dim) ||
            query_heads == 0 || query_heads != kv_heads || head_dim == 0 ||
            query_heads != kid0[1] || head_dim != kid0[3] ||
            (version == 2 &&
             (kv_heads != kid1[1] || head_dim != kid1[3])) ||
            (version == 1 &&
             (!DSL_Shape_Attribute_Equals
                  (node, "attr.execution_mode", "full_sequence") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.mask_mode", "causal") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.cache_mode", "none"))) ||
            (version == 2 &&
             (!DSL_Shape_Attribute_Equals
                  (node, "attr.execution_mode", "single_token_decode") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.mask_mode", "implicit_prefix_causal") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.cache_mode", "functional_append") ||
              !DSL_Shape_Attribute_Equals
                  (node, "attr.cache_sequence_axis", "2"))) ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.head_layout", "BHSD") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.scale_mode", "inverse_sqrt_head_dim") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.softmax_axis", "-1") ||
            !DSL_Shape_Attribute_Equals
                 (node, "attr.softmax_accum_dtype", "float32"))
            return FALSE;
        return DSL_Shape_Result_Dimensions
                   (operands[0], result_ty, kid0);
    }

    return dsl_operator == OPR_DSLSWIGLU && kid0.size() == 3 &&
           DSL_Shape_Dimensions_Equal(kid0, kid1) &&
           DSL_Shape_Tensor_Element_Representation_Compatible
               (operands[0], operands[1]) &&
           DSL_Shape_Attribute_Equals
               (node, "attr.activation", "silu") &&
           DSL_Shape_Result_Dimensions(operands[0], result_ty, kid0);
}

static BOOL
DSL_Shape_Conv2D_Result_Valid
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
        !DSL_Shape_Attribute_Equals
             (node, "attr.input_layout", "NCHW") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.weight_layout", "OIHW") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.output_layout", "NCHW") ||
        !DSL_Shape_Parse_Pair
             (node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Shape_Parse_Pair(node, "attr.stride", FALSE, stride) ||
        !DSL_Shape_Parse_Pair(node, "attr.padding", TRUE, padding) ||
        !DSL_Shape_Parse_Pair
             (node, "attr.dilation", FALSE, dilation) ||
        !DSL_Shape_Node_Attribute
             (node, "attr.groups", &groups_text) ||
        !DSL_Shape_Parse_Unsigned(groups_text, &groups) || groups == 0)
        return FALSE;

    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Shape_Static_Dimensions(operands[0], &input) ||
        !DSL_Shape_Static_Dimensions(operands[1], &weight) ||
        !DSL_Shape_Static_Dimensions(operands[2], &bias) ||
        input.size() != 4 || weight.size() != 4 || bias.size() != 1 ||
        kernel[0] != weight[2] || kernel[1] != weight[3] ||
        input[1] % groups != 0 || weight[0] % groups != 0 ||
        weight[1] > ~(UINT64)0 / groups ||
        weight[1] * groups != input[1] || bias[0] != weight[0] ||
        !DSL_Shape_Tensor_Element_Type_Compatible
             (operands[0], operands[1]) ||
        !DSL_Shape_Tensor_Element_Type_Compatible
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
    return DSL_Shape_Result_Dimensions
               (operands[0], result_ty, result);
}

static BOOL
DSL_Shape_Batch_Norm_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty)
{
    const char *epsilon_text = NULL;
    if (operands.size() != 5 ||
        !DSL_Shape_Attribute_Equals(node, "attr.training", "false") ||
        !DSL_Shape_Attribute_Equals
             (node, "attr.input_layout", "NCHW") ||
        !DSL_Shape_Attribute_Equals(node, "attr.channel_axis", "1") ||
        !DSL_Shape_Node_Attribute
             (node, "attr.epsilon", &epsilon_text))
        return FALSE;
    char *epsilon_end = NULL;
    double epsilon = strtod(epsilon_text, &epsilon_end);
    if (epsilon_end == epsilon_text || *epsilon_end != '\0' ||
        !(epsilon > 0.0 && epsilon < 1.0))
        return FALSE;

    std::vector<UINT64> input;
    if (!DSL_Shape_Static_Dimensions(operands[0], &input) ||
        input.size() != 4)
        return FALSE;
    for (UINT32 i = 1; i < operands.size(); ++i) {
        std::vector<UINT64> parameter;
        if (!DSL_Shape_Static_Dimensions(operands[i], &parameter) ||
            parameter.size() != 1 || parameter[0] != input[1] ||
            !DSL_Shape_Tensor_Element_Type_Compatible
                 (operands[0], operands[i]))
            return FALSE;
    }
    return DSL_Shape_Result_Dimensions
               (operands[0], result_ty, input);
}

static BOOL
DSL_Shape_Max_Pool_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    BOOL use_ceil;
    if (!DSL_Shape_Parse_Pair
             (node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Shape_Parse_Pair(node, "attr.stride", FALSE, stride) ||
        !DSL_Shape_Parse_Pair(node, "attr.padding", TRUE, padding) ||
        !DSL_Shape_Parse_Pair
             (node, "attr.dilation", FALSE, dilation) ||
        (!DSL_Shape_Attribute_Equals
              (node, "attr.ceil_mode", "false") &&
         !DSL_Shape_Attribute_Equals
              (node, "attr.ceil_mode", "true")))
        return FALSE;
    use_ceil = DSL_Shape_Attribute_Equals
                   (node, "attr.ceil_mode", "true");

    std::vector<UINT64> input;
    if (!DSL_Shape_Static_Dimensions(input_ty, &input) ||
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
    return DSL_Shape_Result_Dimensions(input_ty, result_ty, result);
}

static BOOL
DSL_Shape_Global_Avg_Pool_Result_Valid
        (const DSL_IR_NODE_RECORD *node,
         TY_IDX input_ty,
         TY_IDX result_ty)
{
    UINT64 output_size[2];
    std::vector<UINT64> input;
    if (!DSL_Shape_Attribute_Equals
             (node, "attr.reduction_axes", "spatial") ||
        !DSL_Shape_Parse_Pair
             (node, "attr.output_size", FALSE, output_size) ||
        output_size[0] != 1 || output_size[1] != 1 ||
        !DSL_Shape_Static_Dimensions(input_ty, &input) ||
        input.size() != 4)
        return FALSE;
    input[2] = output_size[0];
    input[3] = output_size[1];
    return DSL_Shape_Result_Dimensions(input_ty, result_ty, input);
}

static BOOL
DSL_Shape_Flatten_Result_Valid
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
        !DSL_Shape_Node_Attribute
             (node, "attr.start_dim", &start_text) ||
        !DSL_Shape_Node_Attribute
             (node, "attr.end_dim", &end_text) ||
        !DSL_Shape_Parse_Signed(start_text, &start_dim) ||
        !DSL_Shape_Parse_Signed(end_text, &end_dim))
        return FALSE;
    if (start_dim < 0)
        start_dim += rank;
    if (end_dim < 0)
        end_dim += rank;
    if (start_dim < 0 || end_dim < start_dim || end_dim >= rank)
        return FALSE;

    std::vector<UINT64> input_dimensions;
    std::vector<UINT64> result_dimensions;
    if (!DSL_Shape_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE),
              &input_dimensions) ||
        !DSL_Shape_Parse_Static_Shape
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
    return DSL_Shape_Tensor_Element_Representation_Compatible
               (input_ty, result_ty);
}

static BOOL
DSL_Shape_Parse_Unsigned
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


typedef BOOL (*DSL_SHAPE_RULE_FUNCTION)
        (const DSL_IR_NODE_RECORD *,
         const std::vector<TY_IDX> &,
         TY_IDX,
         UINT16);

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    DSL_SHAPE_RULE_FUNCTION function;
} DSL_SHAPE_RULE_ENTRY;

/* Shape contracts are selected by exact semantic version; never fall forward. */

static BOOL
DSL_Shape_Same_Result
        (const DSL_IR_NODE_RECORD *,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    if (operands.empty())
        return FALSE;
    for (UINT32 i = 1; i < operands.size(); ++i) {
        if (!DSL_Shape_Tensor_Compatible(operands[0], operands[i], TRUE))
            return FALSE;
    }
    return DSL_Shape_Tensor_Compatible(operands[0], result_ty, TRUE);
}

static BOOL
DSL_Shape_Broadcast_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    if (DSL_Shape_Attribute_Equals
            (node, "attr.broadcast_rule", "none"))
        return DSL_Shape_Same_Result(node, operands, result_ty, 0);
    if (!DSL_Shape_Attribute_Equals
             (node, "attr.broadcast_rule", "numpy") ||
        operands.size() != 2 ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (operands[0], operands[1]))
        return FALSE;

    std::vector<UINT64> left;
    std::vector<UINT64> right;
    if (!DSL_Shape_Static_Dimensions(operands[0], &left) ||
        !DSL_Shape_Static_Dimensions(operands[1], &right))
        return FALSE;
    UINT32 rank = left.size() > right.size() ? left.size() : right.size();
    std::vector<UINT64> expected(rank, 1);
    for (UINT32 offset = 0; offset < rank; ++offset) {
        UINT64 left_dimension = offset < left.size() ?
            left[left.size() - 1 - offset] : 1;
        UINT64 right_dimension = offset < right.size() ?
            right[right.size() - 1 - offset] : 1;
        if (left_dimension != right_dimension && left_dimension != 1 &&
            right_dimension != 1)
            return FALSE;
        expected[rank - 1 - offset] =
            left_dimension == 1 ? right_dimension : left_dimension;
    }
    return DSL_Shape_Result_Dimensions
               (operands[0], result_ty, expected);
}

static BOOL
DSL_Shape_Result_Matches_Kid0
        (const DSL_IR_NODE_RECORD *,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return !operands.empty() &&
           DSL_Shape_Tensor_Compatible(operands[0], result_ty, TRUE);
}

static BOOL
DSL_Shape_Matmul_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    if (operands.size() != 2 ||
        !DSL_Shape_Tensor_Compatible(operands[0], operands[1], FALSE))
        return FALSE;
    if (version == 1)
        return DSL_Shape_Tensor_Compatible(operands[0], result_ty, FALSE) &&
               DSL_Shape_Matmul_Shapes_Compatible
                   (operands[0], operands[1], result_ty);
    return version == 2 &&
           DSL_Shape_Matmul_V2_Result_Valid
               (node, operands[0], operands[1], result_ty);
}

static BOOL
DSL_Shape_Linear_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Linear_Result_Valid
               (node, operands, result_ty, version);
}

static BOOL
DSL_Shape_Flatten_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return operands.size() == 1 &&
           DSL_Shape_Flatten_Result_Valid
               (node, operands[0], result_ty);
}

static BOOL
DSL_Shape_Conv2D_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return DSL_Shape_Conv2D_Result_Valid(node, operands, result_ty);
}

static BOOL
DSL_Shape_Batch_Norm_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return DSL_Shape_Batch_Norm_Result_Valid(node, operands, result_ty);
}

static BOOL
DSL_Shape_Max_Pool_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return operands.size() == 1 &&
           DSL_Shape_Max_Pool_Result_Valid
               (node, operands[0], result_ty);
}

static BOOL
DSL_Shape_Global_Avg_Pool_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return operands.size() == 1 &&
           DSL_Shape_Global_Avg_Pool_Result_Valid
               (node, operands[0], result_ty);
}

static BOOL
DSL_Shape_Reshape_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return operands.size() == 1 &&
           DSL_Shape_Reshape_Result_Valid
               (node, operands[0], result_ty);
}

static BOOL
DSL_Shape_Transpose_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return operands.size() == 1 &&
           DSL_Shape_Transpose_Result_Valid
               (node, operands[0], result_ty);
}

static BOOL
DSL_Shape_Token_Embedding_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Transformer_Result_Valid
               (OPR_DSLTOKENEMBEDDING, version, node, operands, result_ty);
}

static BOOL
DSL_Shape_RMS_Norm_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Transformer_Result_Valid
               (OPR_DSLRMSNORM, version, node, operands, result_ty);
}

static BOOL
DSL_Shape_Rotary_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Transformer_Result_Valid
               (OPR_DSLROTARYEMBEDDING, version, node, operands, result_ty);
}

static BOOL
DSL_Shape_Attention_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Transformer_Result_Valid
               (OPR_DSLATTENTION, version, node, operands, result_ty);
}

static BOOL
DSL_Shape_SwiGLU_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return DSL_Shape_Transformer_Result_Valid
               (OPR_DSLSWIGLU, version, node, operands, result_ty);
}

static BOOL
DSL_Shape_Residual_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return DSL_Shape_Same_Result(node, operands, result_ty, 0) &&
           DSL_Shape_Attribute_Equals
               (node, "attr.broadcast_rule", "none") &&
           DSL_Shape_Attribute_Equals
               (node, "attr.shape_check", "exact") &&
           DSL_Shape_Attribute_Equals
               (node, "attr.residual_path", "true");
}

static BOOL
DSL_Shape_Output_Logits_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16 version)
{
    return operands.size() == 1 &&
           DSL_Shape_Output_Logits_Result_Valid
               (node, operands[0], result_ty, version);
}

static const DSL_SHAPE_RULE_ENTRY DSL_shape_rules[] = {
    { OPR_DSLADD, 1, DSL_Shape_Broadcast_Result },
    { OPR_DSLMATMUL, 1, DSL_Shape_Matmul_Result },
    { OPR_DSLMATMUL, 2, DSL_Shape_Matmul_Result },
    { OPR_DSLRELU, 2, DSL_Shape_Result_Matches_Kid0 },
    { OPR_DSLFLATTEN, 2, DSL_Shape_Flatten_Result },
    { OPR_DSLRESIDUALADD, 2, DSL_Shape_Residual_Result },
    { OPR_DSLLINEAR, 2, DSL_Shape_Linear_Result },
    { OPR_DSLLINEAR, 3, DSL_Shape_Linear_Result },
    { OPR_DSLOUTPUTLOGITS, 2, DSL_Shape_Output_Logits_Result },
    { OPR_DSLOUTPUTLOGITS, 3, DSL_Shape_Output_Logits_Result },
    { OPR_DSLCONV2D, 2, DSL_Shape_Conv2D_Result },
    { OPR_DSLBATCHNORMINFER, 2, DSL_Shape_Batch_Norm_Result },
    { OPR_DSLMAXPOOL2D, 2, DSL_Shape_Max_Pool_Result },
    { OPR_DSLGLOBALAVGPOOL2D, 2, DSL_Shape_Global_Avg_Pool_Result },
    { OPR_DSLRESHAPE, 1, DSL_Shape_Reshape_Result },
    { OPR_DSLTRANSPOSE, 1, DSL_Shape_Transpose_Result },
    { OPR_DSLTOKENEMBEDDING, 1, DSL_Shape_Token_Embedding_Result },
    { OPR_DSLRMSNORM, 1, DSL_Shape_RMS_Norm_Result },
    { OPR_DSLROTARYEMBEDDING, 1, DSL_Shape_Rotary_Result },
    { OPR_DSLROTARYEMBEDDING, 2, DSL_Shape_Rotary_Result },
    { OPR_DSLATTENTION, 1, DSL_Shape_Attention_Result },
    { OPR_DSLATTENTION, 2, DSL_Shape_Attention_Result },
    { OPR_DSLSWIGLU, 1, DSL_Shape_SwiGLU_Result },
    { OPR_DSLSCATTER, 1, DSL_Shape_Result_Matches_Kid0 },
    { OPR_DSLMUL, 1, DSL_Shape_Broadcast_Result },
    { OPR_DSLDIV, 1, DSL_Shape_Broadcast_Result },
    { OPR_DSLREM, 1, DSL_Shape_Broadcast_Result }
};

static const DSL_SHAPE_RULE_ENTRY *
DSL_Shape_Find_Rule
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    for (UINT32 i = 0;
         i < sizeof(DSL_shape_rules) / sizeof(DSL_shape_rules[0]);
         ++i) {
        if (DSL_shape_rules[i].dsl_operator == dsl_operator &&
            DSL_shape_rules[i].version == version)
            return &DSL_shape_rules[i];
    }
    return NULL;
}

static BOOL
DSL_Shape_Dimension_Proves_Equal
        (const DSL_SHAPE_FACT &left,
         UINT32 left_ordinal,
         const DSL_SHAPE_FACT &right,
         UINT32 right_ordinal)
{
    if (left_ordinal >= (UINT32)left.rank ||
        right_ordinal >= (UINT32)right.rank ||
        left.dimension_kind[left_ordinal] ==
            DSL_SHAPE_DIMENSION_PENDING ||
        right.dimension_kind[right_ordinal] ==
            DSL_SHAPE_DIMENSION_PENDING ||
        left.dimension_kind[left_ordinal] ==
            DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC ||
        right.dimension_kind[right_ordinal] ==
            DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC)
        return FALSE;
    if (left.dimension_kind[left_ordinal] !=
        right.dimension_kind[right_ordinal])
        return FALSE;
    const char *left_text = left.dimension_text[left_ordinal];
    const char *right_text = right.dimension_text[right_ordinal];
    if (strcmp(left_text, right_text) == 0)
        return TRUE;
    if (left.dimension_kind[left_ordinal] != DSL_SHAPE_DIMENSION_SYMBOL &&
        left.dimension_kind[left_ordinal] != DSL_SHAPE_DIMENSION_EXPRESSION)
        return FALSE;
    const char *left_qualifier = strstr(left_text, "@pu");
    const char *right_qualifier = strstr(right_text, "@pu");
    ST_IDX left_owner;
    ST_IDX right_owner;
    return left_qualifier != NULL && right_qualifier != NULL &&
           left_qualifier - left_text == right_qualifier - right_text &&
           strncmp(left_text, right_text, left_qualifier - left_text) == 0 &&
           strcmp(left_qualifier + 11, right_qualifier + 11) == 0 &&
           DSL_Shape_Dimension_Symbol_Owner
               (left, left_ordinal, &left_owner) &&
           DSL_Shape_Dimension_Symbol_Owner
               (right, right_ordinal, &right_owner) &&
           DSL_Shape_PU_Locators_Equivalent(left_owner, right_owner);
}

static BOOL
DSL_Shape_Facts_Prove_Equal
        (const DSL_SHAPE_FACT &left,
         const DSL_SHAPE_FACT &right)
{
    if (left.state != DSL_SHAPE_FACT_COMPLETE ||
        right.state != DSL_SHAPE_FACT_COMPLETE ||
        left.rank != right.rank)
        return FALSE;
    for (INT32 i = 0; i < left.rank; ++i) {
        if (!DSL_Shape_Dimension_Proves_Equal(left, i, right, i))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Check_Symbolic_Attention
        (const DSL_SHAPE_OPERATOR_INPUT *input,
         const std::vector<DSL_SHAPE_FACT> &operands,
         const DSL_SHAPE_FACT &result)
{
    if (input->version != 2 || operands.size() != 3 ||
        operands[0].rank != 4 || operands[1].rank != 4 ||
        operands[2].rank != 4 || result.rank != 4 ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->operand_types[1]) ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->operand_types[2]) ||
        !DSL_Shape_Facts_Prove_Equal(operands[1], operands[2]) ||
        !DSL_Shape_Facts_Prove_Equal(operands[0], result) ||
        !DSL_Shape_Dimension_Proves_Equal
             (operands[0], 0, operands[1], 0) ||
        operands[0].dimension_kind[1] != DSL_SHAPE_DIMENSION_STATIC ||
        operands[0].dimension_kind[2] != DSL_SHAPE_DIMENSION_STATIC ||
        operands[0].dimension[2] != 1 ||
        operands[0].dimension_kind[3] != DSL_SHAPE_DIMENSION_STATIC ||
        operands[1].dimension_kind[1] != DSL_SHAPE_DIMENSION_STATIC ||
        operands[1].dimension_kind[3] != DSL_SHAPE_DIMENSION_STATIC)
        return FALSE;

    const char *query_heads_text = NULL;
    const char *kv_heads_text = NULL;
    const char *head_dim_text = NULL;
    UINT64 query_heads;
    UINT64 kv_heads;
    UINT64 head_dim;
    return DSL_Shape_Node_Attribute
               (input->node, "attr.query_heads", &query_heads_text) &&
           DSL_Shape_Node_Attribute
               (input->node, "attr.kv_heads", &kv_heads_text) &&
           DSL_Shape_Node_Attribute
               (input->node, "attr.head_dim", &head_dim_text) &&
           DSL_Shape_Parse_Unsigned(query_heads_text, &query_heads) &&
           DSL_Shape_Parse_Unsigned(kv_heads_text, &kv_heads) &&
           DSL_Shape_Parse_Unsigned(head_dim_text, &head_dim) &&
           query_heads != 0 && query_heads == kv_heads && head_dim != 0 &&
           query_heads == operands[0].dimension[1] &&
           query_heads == operands[1].dimension[1] &&
           head_dim == operands[0].dimension[3] &&
           head_dim == operands[1].dimension[3] &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.execution_mode",
                "single_token_decode") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.mask_mode",
                "implicit_prefix_causal") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.cache_mode", "functional_append") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.cache_sequence_axis", "2") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.head_layout", "BHSD") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.scale_mode",
                "inverse_sqrt_head_dim") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.softmax_axis", "-1") &&
           DSL_Shape_Attribute_Equals
               (input->node, "attr.softmax_accum_dtype", "float32");
}

static BOOL
DSL_Shape_Matmul_Attributes
        (const DSL_IR_NODE_RECORD *node,
         UINT16 version,
         BOOL *transpose_left,
         BOOL *transpose_right)
{
    *transpose_left = FALSE;
    *transpose_right = FALSE;
    if (version == 1)
        return TRUE;
    if (version != 2)
        return FALSE;
    if (DSL_Shape_Attribute_Equals
            (node, "attr.transpose_kid0", "true"))
        *transpose_left = TRUE;
    else if (!DSL_Shape_Attribute_Equals
                 (node, "attr.transpose_kid0", "false"))
        return FALSE;
    if (DSL_Shape_Attribute_Equals
            (node, "attr.transpose_kid1", "true"))
        *transpose_right = TRUE;
    else if (!DSL_Shape_Attribute_Equals
                 (node, "attr.transpose_kid1", "false"))
        return FALSE;
    return DSL_Shape_Attribute_Equals
               (node, "attr.batch_rule", "exact") &&
           DSL_Shape_Attribute_Equals
               (node, "attr.accum_dtype", "float32");
}

static BOOL
DSL_Shape_Check_Symbolic_Matmul
        (const DSL_SHAPE_OPERATOR_INPUT *input,
         const std::vector<DSL_SHAPE_FACT> &operands,
         const DSL_SHAPE_FACT &result)
{
    BOOL transpose_left;
    BOOL transpose_right;
    if (operands.size() != 2 || operands[0].rank < 2 ||
        operands[0].rank != operands[1].rank ||
        result.rank != operands[0].rank ||
        (input->version == 1 && operands[0].rank != 2) ||
        !DSL_Shape_Matmul_Attributes
             (input->node, input->version, &transpose_left,
              &transpose_right) ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->operand_types[1]) ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->result_ty))
        return FALSE;

    UINT32 rank = operands[0].rank;
    for (UINT32 i = 0; i + 2 < rank; ++i) {
        if (!DSL_Shape_Dimension_Proves_Equal
                 (operands[0], i, operands[1], i) ||
            !DSL_Shape_Dimension_Proves_Equal
                 (operands[0], i, result, i))
            return FALSE;
    }
    UINT32 left_m = rank - (transpose_left ? 1 : 2);
    UINT32 left_k = rank - (transpose_left ? 2 : 1);
    UINT32 right_k = rank - (transpose_right ? 1 : 2);
    UINT32 right_n = rank - (transpose_right ? 2 : 1);
    return DSL_Shape_Dimension_Proves_Equal
               (operands[0], left_k, operands[1], right_k) &&
           DSL_Shape_Dimension_Proves_Equal
               (operands[0], left_m, result, rank - 2) &&
           DSL_Shape_Dimension_Proves_Equal
               (operands[1], right_n, result, rank - 1);
}

static BOOL
DSL_Shape_Check_Symbolic_Broadcast
        (const DSL_SHAPE_OPERATOR_INPUT *input,
         const std::vector<DSL_SHAPE_FACT> &operands,
         const DSL_SHAPE_FACT &result)
{
    if (operands.size() != 2 ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->operand_types[1]) ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->result_ty))
        return FALSE;
    if (DSL_Shape_Attribute_Equals
            (input->node, "attr.broadcast_rule", "none"))
        return DSL_Shape_Facts_Prove_Equal(operands[0], operands[1]) &&
               DSL_Shape_Facts_Prove_Equal(operands[0], result);
    if (!DSL_Shape_Attribute_Equals
             (input->node, "attr.broadcast_rule", "numpy"))
        return FALSE;

    UINT32 left_rank = operands[0].rank;
    UINT32 right_rank = operands[1].rank;
    UINT32 rank = left_rank > right_rank ? left_rank : right_rank;
    if ((UINT32)result.rank != rank)
        return FALSE;
    for (UINT32 offset = 0; offset < rank; ++offset) {
        BOOL has_left = offset < left_rank;
        BOOL has_right = offset < right_rank;
        UINT32 result_ordinal = rank - 1 - offset;
        if (!has_left) {
            if (!DSL_Shape_Dimension_Proves_Equal
                     (operands[1], right_rank - 1 - offset,
                      result, result_ordinal))
                return FALSE;
            continue;
        }
        if (!has_right) {
            if (!DSL_Shape_Dimension_Proves_Equal
                     (operands[0], left_rank - 1 - offset,
                      result, result_ordinal))
                return FALSE;
            continue;
        }
        UINT32 left_ordinal = left_rank - 1 - offset;
        UINT32 right_ordinal = right_rank - 1 - offset;
        const DSL_SHAPE_FACT *source = NULL;
        UINT32 source_ordinal = 0;
        if (DSL_Shape_Dimension_Proves_Equal
                (operands[0], left_ordinal,
                 operands[1], right_ordinal)) {
            source = &operands[0];
            source_ordinal = left_ordinal;
        } else if (operands[0].dimension_kind[left_ordinal] ==
                       DSL_SHAPE_DIMENSION_STATIC &&
                   operands[0].dimension[left_ordinal] == 1) {
            source = &operands[1];
            source_ordinal = right_ordinal;
        } else if (operands[1].dimension_kind[right_ordinal] ==
                       DSL_SHAPE_DIMENSION_STATIC &&
                   operands[1].dimension[right_ordinal] == 1) {
            source = &operands[0];
            source_ordinal = left_ordinal;
        } else {
            return FALSE;
        }
        if (!DSL_Shape_Dimension_Proves_Equal
                 (*source, source_ordinal, result, result_ordinal))
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_Shape_Has_Operator_Rule
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    return DSL_Shape_Find_Rule(dsl_operator, version) != NULL;
}

BOOL
DSL_Shape_Proof_Context_Init
        (DSL_SHAPE_PROOF_CONTEXT *context,
         PU_Info *pu_info,
         WN *tree,
         ST_IDX owner_pu_st,
         DSL_IR_NODE_ID node_id,
         const DSL_IR_VALUE_ID *operand_value_ids,
         UINT32 operand_value_count,
         DSL_IR_VALUE_ID result_value_id)
{
    if (context == NULL || pu_info == NULL || tree == NULL ||
        ST_IDX_index(owner_pu_st) == 0 ||
        (operand_value_count != 0 && operand_value_ids == NULL) ||
        PU_Info_proc_sym(pu_info) != owner_pu_st ||
        PU_Info_tree_ptr(pu_info) != tree ||
        WN_operator(tree) != OPR_FUNC_ENTRY ||
        WN_st_idx(tree) != owner_pu_st)
        return FALSE;
    memset(context, 0, sizeof(*context));
    context->active_boundary.pu_info = pu_info;
    context->active_boundary.tree = tree;
    context->active_boundary.owner_pu_st = owner_pu_st;
    context->node_id = node_id;
    context->operand_value_ids = operand_value_ids;
    context->operand_value_count = operand_value_count;
    context->result_value_id = result_value_id;
    return TRUE;
}

static BOOL DSL_Shape_Str_Stable (STR_IDX value);
static BOOL DSL_Shape_PU_Stable_Identity
        (ST_IDX owner_locator,
         DSL_PU_SOURCE_IDENTITY_RECORD *identity,
         UINT64 *fingerprint);

static BOOL
DSL_Shape_Proof_Context_Select_Reviewed_Interface_Mapping
        (DSL_SHAPE_PROOF_CONTEXT *context,
         DSL_CALLSITE_METADATA_ID callsite_id,
         UINT32 actual_ordinal,
         ST_IDX destination_owner_pu_st,
         UINT32 formal_ordinal)
{
    if (context == NULL || callsite_id == 0 ||
        ST_IDX_index(destination_owner_pu_st) == 0 ||
        context->active_boundary.owner_pu_st != destination_owner_pu_st)
        return FALSE;
    for (UINT32 i = 0; i < context->interface_mapping_count; ++i) {
        const DSL_SHAPE_INTERFACE_MAPPING_SELECTOR &selected =
            context->interface_mappings[i];
        if (selected.callsite_id == callsite_id &&
            selected.actual_ordinal == actual_ordinal &&
            selected.destination_owner_pu_st == destination_owner_pu_st &&
            selected.formal_ordinal == formal_ordinal)
            return TRUE;
    }
    if (context->interface_mapping_count >=
            DSL_SHAPE_MAX_INTERFACE_MAPPINGS)
        return FALSE;

    DSL_CALLSITE_METADATA_RECORD callsite;
    DSL_CALL_ARGUMENT_RECORD argument;
    DSL_CALL_ARGUMENT_RECORD unique_argument;
    DSL_PU_FORMAL_RECORD formal;
    DSL_PU_SOURCE_IDENTITY_RECORD source_identity;
    DSL_PU_SOURCE_IDENTITY_RECORD destination_identity;
    DSL_IR_VALUE_RECORD actual_value;
    DSL_IR_VALUE_RECORD formal_value;
    UINT64 source_identity_fingerprint;
    UINT64 destination_identity_fingerprint;
    if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
        !DSL_Call_ABI_Image_Find_Argument_By_Id
             (callsite_id, actual_ordinal, &argument) ||
        DSL_Call_ABI_Image_Callee_Formal_Count
            (destination_owner_pu_st, formal_ordinal) != 1 ||
        !DSL_Call_ABI_Image_Get_Callee_Formal_Argument
             (destination_owner_pu_st, formal_ordinal, 0,
              &unique_argument) ||
        !DSL_PU_Interface_Image_Find_Formal
             (destination_owner_pu_st, formal_ordinal, &formal) ||
        !DSL_Call_Image_Find_PU_Identity
             (callsite.owner_pu_st, &source_identity) ||
        !DSL_Call_Image_Find_PU_Identity
             (destination_owner_pu_st, &destination_identity) ||
        !DSL_IR_Image_Get_Value
             (argument.argument_value_id, &actual_value) ||
        !DSL_IR_Image_Get_Value(formal.formal_value_id, &formal_value) ||
        !DSL_Shape_PU_Stable_Identity
             (callsite.owner_pu_st, &source_identity,
              &source_identity_fingerprint) ||
        !DSL_Shape_PU_Stable_Identity
             (destination_owner_pu_st, &destination_identity,
              &destination_identity_fingerprint) ||
        callsite.id != callsite_id ||
        ST_IDX_index(callsite.owner_pu_st) == 0 ||
        callsite.callee_pu_st != destination_owner_pu_st ||
        callsite.flags != 0 ||
        !DSL_Shape_Str_Stable(callsite.canonical_class_name) ||
        !DSL_Shape_Str_Stable(callsite.instance_path) ||
        !DSL_Shape_Str_Stable(callsite.context_identity) ||
        argument.callsite_id != callsite_id ||
        argument.actual_ordinal != actual_ordinal ||
        argument.callee_formal_ordinal != formal_ordinal ||
        argument.argument_value_id == 0 ||
        argument.flags != 0 ||
        !DSL_Shape_Str_Stable(argument.semantic_role) ||
        unique_argument.id != argument.id ||
        unique_argument.callsite_id != argument.callsite_id ||
        unique_argument.argument_value_id != argument.argument_value_id ||
        unique_argument.actual_ordinal != argument.actual_ordinal ||
        unique_argument.callee_formal_ordinal !=
            argument.callee_formal_ordinal ||
        unique_argument.semantic_role != argument.semantic_role ||
        formal.owner_pu_st != destination_owner_pu_st ||
        formal.formal_ordinal != formal_ordinal ||
        formal.formal_value_id == 0 || formal.flags != 0 ||
        formal.reserved != 0 ||
        actual_value.id != argument.argument_value_id ||
        actual_value.flags != 0 || actual_value.reserved != 0 ||
        formal_value.id != formal.formal_value_id ||
        formal_value.st != formal.formal_st ||
        formal_value.ty != formal.formal_ty ||
        formal_value.flags != 0 || formal_value.reserved != 0)
        return FALSE;

    DSL_SHAPE_INTERFACE_MAPPING_SELECTOR &selected =
        context->interface_mappings[context->interface_mapping_count++];
    memset(&selected, 0, sizeof(selected));
    selected.callsite_id = callsite_id;
    selected.actual_ordinal = actual_ordinal;
    selected.destination_owner_pu_st =
        destination_owner_pu_st;
    selected.formal_ordinal = formal_ordinal;
    selected.snapshot_version = 1;
    selected.source_owner_pu_st = callsite.owner_pu_st;
    selected.source_identity_fingerprint = source_identity_fingerprint;
    selected.source_canonical_definition_name =
        source_identity.canonical_definition_name;
    selected.source_defining_module = source_identity.defining_module;
    selected.source_defining_file = source_identity.defining_file;
    selected.source_defining_line = source_identity.defining_line;
    selected.destination_identity_fingerprint =
        destination_identity_fingerprint;
    selected.destination_canonical_definition_name =
        destination_identity.canonical_definition_name;
    selected.destination_defining_module = destination_identity.defining_module;
    selected.destination_defining_file = destination_identity.defining_file;
    selected.destination_defining_line = destination_identity.defining_line;
    selected.callsite_owner_pu_st = callsite.owner_pu_st;
    selected.callsite_callee_pu_st = callsite.callee_pu_st;
    selected.canonical_class_name = callsite.canonical_class_name;
    selected.instance_path = callsite.instance_path;
    selected.context_identity = callsite.context_identity;
    selected.source_call_ordinal = callsite.source_call_ordinal;
    selected.argument_value_id = argument.argument_value_id;
    selected.argument_semantic_role = argument.semantic_role;
    selected.actual_st = actual_value.st;
    selected.actual_ty = actual_value.ty;
    selected.formal_owner_pu_st = formal.owner_pu_st;
    selected.formal_value_id = formal.formal_value_id;
    selected.formal_st = formal.formal_st;
    selected.formal_ty = formal.formal_ty;
    return TRUE;
}

typedef struct {
    UINT32 count;
    struct {
        ST_IDX source_owner_locator;
        UINT64 source_identity_fingerprint;
        UINT64 destination_identity_fingerprint;
        DSL_IR_VALUE_ID argument_value_id;
        DSL_IR_VALUE_ID formal_value_id;
    } entries[DSL_SHAPE_MAX_INTERFACE_MAPPINGS];
} DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE;

static BOOL DSL_Shape_Proof_Context_Validate
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_IR_NODE_RECORD *node,
         const TY_IDX *operand_types,
         UINT32 operand_count,
         TY_IDX result_ty,
         DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE *mapping);
static BOOL DSL_Shape_Fact_Admitted
        (const DSL_SHAPE_FACT &fact,
         DSL_IR_VALUE_ID value_id,
         const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE &mapping);
static BOOL DSL_Shape_Value_Provenance_Valid
        (DSL_IR_VALUE_ID value_id,
         const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE &mapping,
         std::vector<DSL_IR_VALUE_ID> *active_values);
static BOOL DSL_Shape_Merge_Equal_Fact
        (DSL_SHAPE_FACT *target,
         const DSL_SHAPE_FACT &source);
static BOOL DSL_Shape_Persisted_Fact_Proved_By_Inference
        (const DSL_SHAPE_FACT &persisted,
         const DSL_SHAPE_FACT &inferred);

DSL_SHAPE_CHECK_RESULT
DSL_Shape_Check_Operator
        (const DSL_SHAPE_OPERATOR_INPUT *input)
{
    if (input == NULL || input->proof_context == NULL)
        return DSL_SHAPE_CHECK_INVALID;
    DSL_OPERATOR_INFO info;
    if (!DSL_Operator_Get_Info_Version
             (input->dsl_operator, input->version, &info))
        return DSL_SHAPE_CHECK_UNREGISTERED;
    const DSL_SHAPE_RULE_ENTRY *rule =
        DSL_Shape_Find_Rule(input->dsl_operator, input->version);
    if (rule == NULL)
        return DSL_SHAPE_CHECK_UNREGISTERED;
    if (input->node == NULL || input->result_ty == TY_IDX_ZERO ||
        (input->operand_count != 0 && input->operand_types == NULL) ||
        (info.nkids >= 0 && (UINT32)info.nkids != input->operand_count))
        return DSL_SHAPE_CHECK_INVALID;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Opcode_Descriptor
             (input->node->opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != (UINT32)input->dsl_operator ||
        descriptor.version != input->version)
        return DSL_SHAPE_CHECK_INVALID;
    DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE mapping;
    memset(&mapping, 0, sizeof(mapping));
    if (!DSL_Shape_Proof_Context_Validate
             (input->proof_context, input->node, input->operand_types,
              input->operand_count, input->result_ty, &mapping))
        return DSL_SHAPE_CHECK_INVALID;

    std::vector<TY_IDX> operands;
    operands.reserve(input->operand_count);
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        if (input->operand_types[i] == TY_IDX_ZERO)
            return DSL_SHAPE_CHECK_INVALID;
        operands.push_back(input->operand_types[i]);
    }
    std::vector<DSL_SHAPE_FACT> operand_facts(input->operand_count);
    DSL_SHAPE_FACT result_fact;
    BOOL non_static = FALSE;
    BOOL anonymous_dynamic = FALSE;
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        if (!DSL_Shape_Fact_From_Type
                 (input->operand_types[i], &operand_facts[i]) ||
            operand_facts[i].state != DSL_SHAPE_FACT_COMPLETE ||
            !DSL_Shape_Fact_Admitted
                 (operand_facts[i],
                  input->proof_context->operand_value_ids[i],
                  input->proof_context, mapping))
            return DSL_SHAPE_CHECK_INVALID;
        for (INT32 j = 0; j < operand_facts[i].rank; ++j) {
            non_static = non_static ||
                operand_facts[i].dimension_kind[j] !=
                    DSL_SHAPE_DIMENSION_STATIC;
            anonymous_dynamic = anonymous_dynamic ||
                operand_facts[i].dimension_kind[j] ==
                    DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
        }
    }
    if (!DSL_Shape_Fact_From_Type(input->result_ty, &result_fact) ||
        result_fact.state != DSL_SHAPE_FACT_COMPLETE ||
        !DSL_Shape_Fact_Admitted
             (result_fact, input->proof_context->result_value_id,
              input->proof_context, mapping))
        return DSL_SHAPE_CHECK_INVALID;
    for (INT32 i = 0; i < result_fact.rank; ++i) {
        non_static = non_static ||
            result_fact.dimension_kind[i] != DSL_SHAPE_DIMENSION_STATIC;
        anonymous_dynamic = anonymous_dynamic ||
            result_fact.dimension_kind[i] ==
                DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
    }
    if (non_static && input->dsl_operator == OPR_DSLATTENTION)
        return DSL_Shape_Check_Symbolic_Attention
                   (input, operand_facts, result_fact) ?
               DSL_SHAPE_CHECK_VALID : DSL_SHAPE_CHECK_INVALID;
    if (non_static && input->dsl_operator == OPR_DSLMATMUL)
        return DSL_Shape_Check_Symbolic_Matmul
                   (input, operand_facts, result_fact) ?
               DSL_SHAPE_CHECK_VALID : DSL_SHAPE_CHECK_INVALID;
    if (non_static &&
        (input->dsl_operator == OPR_DSLADD ||
         input->dsl_operator == OPR_DSLMUL ||
         input->dsl_operator == OPR_DSLDIV ||
         input->dsl_operator == OPR_DSLREM))
        return DSL_Shape_Check_Symbolic_Broadcast
                   (input, operand_facts, result_fact) ?
               DSL_SHAPE_CHECK_VALID : DSL_SHAPE_CHECK_INVALID;
    if (anonymous_dynamic && input->operand_count > 1)
        return DSL_SHAPE_CHECK_INVALID;
    return rule->function(input->node, operands, input->result_ty,
                          input->version) ?
           DSL_SHAPE_CHECK_VALID : DSL_SHAPE_CHECK_INVALID;
}

static void
DSL_Shape_Fact_Init (DSL_SHAPE_FACT *fact)
{
    memset(fact, 0, sizeof(*fact));
    fact->state = DSL_SHAPE_FACT_PENDING;
    fact->rank = -1;
}

static void
DSL_Shape_Fact_Classify (DSL_SHAPE_FACT *fact)
{
    if (fact->state == DSL_SHAPE_FACT_CONTRADICTION)
        return;
    if (fact->rank < 0) {
        fact->state = DSL_SHAPE_FACT_PENDING;
        return;
    }
    for (INT32 i = 0; i < fact->rank; ++i) {
        if (fact->dimension_kind[i] == DSL_SHAPE_DIMENSION_PENDING) {
            fact->state = DSL_SHAPE_FACT_PENDING;
            return;
        }
    }
    fact->state = DSL_SHAPE_FACT_COMPLETE;
}

static BOOL
DSL_Shape_Identifier_Valid (const std::string &text)
{
    if (text.empty() ||
        (!isalpha((unsigned char)text[0]) && text[0] != '_'))
        return FALSE;
    for (size_t i = 1; i < text.size(); ++i) {
        if (!isalnum((unsigned char)text[i]) && text[i] != '_')
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Parse_Symbol
        (ST_IDX owner_pu_st,
         const std::string &input,
         std::string *canonical)
{
    size_t qualifier = input.find("@pu");
    std::string name = qualifier == std::string::npos ?
                       input : input.substr(0, qualifier);
    if (!DSL_Shape_Identifier_Valid(name))
        return FALSE;
    if (qualifier == std::string::npos) {
        if (ST_IDX_index(owner_pu_st) == 0)
            return FALSE;
        char suffix[16];
        snprintf(suffix, sizeof(suffix), "@pu%08x",
                 (unsigned int)owner_pu_st);
        *canonical = name + suffix;
        return TRUE;
    }
    if (qualifier + 11 != input.size())
        return FALSE;
    UINT32 identity = 0;
    for (size_t i = qualifier + 3; i < input.size(); ++i) {
        unsigned char character = (unsigned char)input[i];
        if (!isxdigit(character))
            return FALSE;
        identity <<= 4;
        identity += isdigit(character) ? character - '0' :
                    tolower(character) - 'a' + 10;
    }
    if (identity == 0)
        return FALSE;
    if (ST_IDX_index(owner_pu_st) != 0 &&
        identity != (UINT32)owner_pu_st)
        return FALSE;
    char suffix[16];
    snprintf(suffix, sizeof(suffix), "@pu%08x", identity);
    *canonical = name + suffix;
    return TRUE;
}

static BOOL
DSL_Shape_Parse_Dimension
        (ST_IDX owner_pu_st,
         const std::string &input,
         DSL_SHAPE_DIMENSION_KIND *kind,
         UINT64 *static_value,
         std::string *canonical)
{
    if (input == "<pending>") {
        *kind = DSL_SHAPE_DIMENSION_PENDING;
        *canonical = input;
        return TRUE;
    }
    if (input == "?") {
        *kind = DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
        *canonical = input;
        return TRUE;
    }
    if (!input.empty() && isdigit((unsigned char)input[0])) {
        UINT64 value = 0;
        for (size_t i = 0; i < input.size(); ++i) {
            if (!isdigit((unsigned char)input[i]))
                return FALSE;
            UINT64 digit = input[i] - '0';
            if (value > (~(UINT64)0 - digit) / 10)
                return FALSE;
            value = value * 10 + digit;
        }
        if (value == 0)
            return FALSE;
        char number[32];
        snprintf(number, sizeof(number), "%llu",
                 (unsigned long long)value);
        *kind = DSL_SHAPE_DIMENSION_STATIC;
        *static_value = value;
        *canonical = number;
        return TRUE;
    }

    size_t operation = std::string::npos;
    for (size_t i = 1; i < input.size(); ++i) {
        if (input[i] == '+' || input[i] == '-') {
            operation = i;
            break;
        }
    }
    std::string symbol = operation == std::string::npos ?
                         input : input.substr(0, operation);
    std::string canonical_symbol;
    if (!DSL_Shape_Parse_Symbol
             (owner_pu_st, symbol, &canonical_symbol))
        return FALSE;
    if (operation == std::string::npos) {
        *kind = DSL_SHAPE_DIMENSION_SYMBOL;
        *canonical = canonical_symbol;
        return TRUE;
    }
    std::string constant = input.substr(operation + 1);
    if (constant.empty())
        return FALSE;
    UINT64 value = 0;
    for (size_t i = 0; i < constant.size(); ++i) {
        if (!isdigit((unsigned char)constant[i]))
            return FALSE;
        UINT64 digit = constant[i] - '0';
        if (value > (~(UINT64)0 - digit) / 10)
            return FALSE;
        value = value * 10 + digit;
    }
    if (value == 0) {
        *kind = DSL_SHAPE_DIMENSION_SYMBOL;
        *canonical = canonical_symbol;
        return TRUE;
    }
    char number[32];
    snprintf(number, sizeof(number), "%llu",
             (unsigned long long)value);
    *kind = DSL_SHAPE_DIMENSION_EXPRESSION;
    *canonical = canonical_symbol + input[operation] + number;
    return TRUE;
}

static BOOL
DSL_Shape_Store_Dimension
        (DSL_SHAPE_FACT *fact,
         INT32 ordinal,
         DSL_SHAPE_DIMENSION_KIND kind,
         UINT64 static_value,
         const std::string &canonical)
{
    if (canonical.size() >= DSL_SHAPE_DIMENSION_TEXT_MAX)
        return FALSE;
    fact->dimension_kind[ordinal] = kind;
    fact->dimension_known[ordinal] =
        kind == DSL_SHAPE_DIMENSION_STATIC;
    fact->dimension[ordinal] = static_value;
    memcpy(fact->dimension_text[ordinal], canonical.c_str(),
           canonical.size() + 1);
    return TRUE;
}

static BOOL
DSL_Shape_Parse_Fact
        (ST_IDX owner_pu_st,
         const char *shape,
         INT32 expected_rank,
         DSL_SHAPE_FACT *fact)
{
    DSL_Shape_Fact_Init(fact);
    if (shape == NULL || expected_rank < -1 ||
        expected_rank > DSL_SHAPE_MAX_RANK)
        return FALSE;
    if (expected_rank < 0)
        return strcmp(shape, "<pending>") == 0;
    fact->rank = expected_rank;
    if (strcmp(shape, "<pending>") == 0) {
        for (INT32 i = 0; i < expected_rank; ++i) {
            fact->dimension_kind[i] = DSL_SHAPE_DIMENSION_PENDING;
            strcpy(fact->dimension_text[i], "<pending>");
        }
        return TRUE;
    }

    const char *cursor = shape;
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor++ != '[')
        return FALSE;
    INT32 ordinal = 0;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        if (ordinal >= expected_rank)
            return FALSE;
        const char *token_begin = cursor;
        while (*cursor != '\0' && *cursor != ',' && *cursor != ']')
            ++cursor;
        const char *token_end = cursor;
        while (token_end > token_begin &&
               isspace((unsigned char)token_end[-1]))
            --token_end;
        while (token_begin < token_end &&
               isspace((unsigned char)*token_begin))
            ++token_begin;
        std::string token(token_begin, token_end - token_begin);
        DSL_SHAPE_DIMENSION_KIND kind;
        UINT64 static_value = 0;
        std::string canonical;
        if (!DSL_Shape_Parse_Dimension
                 (owner_pu_st, token, &kind, &static_value, &canonical) ||
            !DSL_Shape_Store_Dimension
                 (fact, ordinal, kind, static_value, canonical))
            return FALSE;
        ++ordinal;
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
    if (*cursor != '\0' || ordinal != expected_rank)
        return FALSE;
    DSL_Shape_Fact_Classify(fact);
    return TRUE;
}

BOOL
DSL_Shape_Format_Fact
        (const DSL_SHAPE_FACT *fact,
         char *buffer,
         size_t buffer_size)
{
    if (fact == NULL || buffer == NULL || buffer_size == 0 ||
        fact->rank < -1 || fact->rank > DSL_SHAPE_MAX_RANK)
        return FALSE;
    if (fact->rank < 0)
        return snprintf(buffer, buffer_size, "<pending>") >= 0 &&
               strlen("<pending>") < buffer_size;
    size_t used = 0;
    int written = snprintf(buffer, buffer_size, "[");
    if (written < 0 || (size_t)written >= buffer_size)
        return FALSE;
    used = written;
    for (INT32 i = 0; i < fact->rank; ++i) {
        const char *text = fact->dimension_text[i];
        if (text[0] == '\0')
            return FALSE;
        written = snprintf(buffer + used, buffer_size - used,
                           "%s%s", i == 0 ? "" : ",", text);
        if (written < 0 || (size_t)written >= buffer_size - used)
            return FALSE;
        used += written;
    }
    written = snprintf(buffer + used, buffer_size - used, "]");
    return written >= 0 && (size_t)written < buffer_size - used;
}

BOOL
DSL_Shape_Normalize_Logical_Shape
        (ST_IDX owner_pu_st,
         const char *shape,
         INT32 expected_rank,
         char *buffer,
         size_t buffer_size)
{
    DSL_SHAPE_FACT fact;
    if (!DSL_Shape_Parse_Fact
             (owner_pu_st, shape, expected_rank, &fact))
        return FALSE;
    if (ST_IDX_index(owner_pu_st) == 0 && fact.rank >= 0) {
        for (INT32 i = 0; i < fact.rank; ++i) {
            if (fact.dimension_kind[i] == DSL_SHAPE_DIMENSION_SYMBOL ||
                fact.dimension_kind[i] == DSL_SHAPE_DIMENSION_EXPRESSION)
                return FALSE;
        }
    }
    return DSL_Shape_Format_Fact(&fact, buffer, buffer_size);
}

BOOL
DSL_Shape_Fact_From_Type (TY_IDX ty, DSL_SHAPE_FACT *fact)
{
    if (fact == NULL || !DSL_Shape_Tensor_Core_Complete(ty))
        return FALSE;
    INT32 rank = TY_tensor_rank(ty);
    const char *rank_text =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_RANK);
    const char *shape =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);
    INT32 recorded_rank;
    return DSL_Shape_Parse_Signed(rank_text, &recorded_rank) &&
           recorded_rank == rank &&
           DSL_Shape_Parse_Fact(ST_IDX_ZERO, shape, rank, fact);
}

static BOOL
DSL_Shape_Facts_Equal
        (const DSL_SHAPE_FACT &left,
         const DSL_SHAPE_FACT &right)
{
    if (left.state != right.state || left.rank != right.rank)
        return FALSE;
    if (left.rank < 0)
        return TRUE;
    for (INT32 i = 0; i < left.rank; ++i) {
        if (left.dimension_kind[i] != right.dimension_kind[i])
            return FALSE;
        if ((left.dimension_kind[i] == DSL_SHAPE_DIMENSION_SYMBOL ||
             left.dimension_kind[i] == DSL_SHAPE_DIMENSION_EXPRESSION) ?
                !DSL_Shape_Dimension_Proves_Equal(left, i, right, i) :
                strcmp(left.dimension_text[i],
                       right.dimension_text[i]) != 0)
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_Shape_Refinement_Matches_Type
        (const DSL_SHAPE_REFINEMENT *refinement,
         DSL_IR_VALUE_ID value_id,
         TY_IDX expected_old_ty,
         TY_IDX refined_ty)
{
    DSL_SHAPE_FACT refined_fact;
    return refinement != NULL &&
           refinement->value_id == value_id &&
           refinement->expected_old_ty == expected_old_ty &&
           refined_ty != TY_IDX_ZERO &&
           DSL_Shape_Fact_From_Type(refined_ty, &refined_fact) &&
           DSL_Shape_Facts_Equal(refinement->refined_fact, refined_fact);
}

static BOOL
DSL_Shape_Dimension_Symbol_Owner
        (const DSL_SHAPE_FACT &fact,
         INT32 ordinal,
         ST_IDX *owner_pu_st)
{
    if (owner_pu_st == NULL || ordinal < 0 || ordinal >= fact.rank ||
        (fact.dimension_kind[ordinal] != DSL_SHAPE_DIMENSION_SYMBOL &&
         fact.dimension_kind[ordinal] != DSL_SHAPE_DIMENSION_EXPRESSION))
        return FALSE;
    const char *qualifier = strstr(fact.dimension_text[ordinal], "@pu");
    if (qualifier == NULL)
        return FALSE;
    UINT32 identity = 0;
    for (UINT32 i = 0; i < 8; ++i) {
        unsigned char character = (unsigned char)qualifier[3 + i];
        if (!isxdigit(character))
            return FALSE;
        identity <<= 4;
        identity += isdigit(character) ? character - '0' :
                    tolower(character) - 'a' + 10;
    }
    if (identity == 0 ||
        (qualifier[11] != '\0' && qualifier[11] != '+' &&
         qualifier[11] != '-'))
        return FALSE;
    *owner_pu_st = (ST_IDX)identity;
    return TRUE;
}

static BOOL
DSL_Shape_Fact_Foreign_Owner
        (const DSL_SHAPE_FACT &fact,
         ST_IDX active_owner_pu_st,
         ST_IDX *foreign_owner_pu_st,
         BOOL *has_foreign_owner)
{
    if (foreign_owner_pu_st == NULL || has_foreign_owner == NULL ||
        ST_IDX_index(active_owner_pu_st) == 0)
        return FALSE;
    *foreign_owner_pu_st = ST_IDX_ZERO;
    *has_foreign_owner = FALSE;
    if (fact.rank < 0)
        return TRUE;
    for (INT32 i = 0; i < fact.rank; ++i) {
        if (fact.dimension_kind[i] != DSL_SHAPE_DIMENSION_SYMBOL &&
            fact.dimension_kind[i] != DSL_SHAPE_DIMENSION_EXPRESSION)
            continue;
        ST_IDX owner_pu_st;
        if (!DSL_Shape_Dimension_Symbol_Owner(fact, i, &owner_pu_st))
            return FALSE;
        if (DSL_Shape_PU_Locators_Equivalent
                (owner_pu_st, active_owner_pu_st))
            continue;
        if (*has_foreign_owner &&
            !DSL_Shape_PU_Locators_Equivalent
                 (*foreign_owner_pu_st, owner_pu_st))
            return FALSE;
        *foreign_owner_pu_st = owner_pu_st;
        *has_foreign_owner = TRUE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Value_Belongs_To_PU
        (const DSL_IR_VALUE_RECORD &value,
         ST_IDX owner_pu_st)
{
    if (value.id == DSL_IR_VALUE_INVALID_ID ||
        ST_IDX_index(value.st) == 0 || value.name == STR_IDX_ZERO ||
        ST_IDX_level(owner_pu_st) != GLOBAL_SYMTAB ||
        ST_IDX_index(owner_pu_st) >= ST_Table_Size(GLOBAL_SYMTAB))
        return FALSE;
    DSL_IR_VALUE_RECORD found;
    return DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[owner_pu_st]), &found) &&
           found.id == value.id && found.ty == value.ty &&
           found.st == value.st;
}

static BOOL
DSL_Shape_Str_Stable (STR_IDX value)
{
    return value != STR_IDX_ZERO && Index_To_Str(value) != NULL &&
           Index_To_Str(value)[0] != '\0';
}

static BOOL
DSL_Shape_Str_Content_Equal (STR_IDX left, STR_IDX right)
{
    return DSL_Shape_Str_Stable(left) && DSL_Shape_Str_Stable(right) &&
           strcmp(Index_To_Str(left), Index_To_Str(right)) == 0;
}

static UINT64
DSL_Shape_Stable_Identity_Hash_Bytes
        (UINT64 fingerprint,
         const void *data,
         size_t size)
{
    const unsigned char *bytes = (const unsigned char *)data;
    for (size_t i = 0; i < size; ++i) {
        fingerprint ^= bytes[i];
        fingerprint *= 1099511628211ULL;
    }
    return fingerprint;
}

static UINT64
DSL_Shape_Stable_Identity_Hash_String
        (UINT64 fingerprint,
         STR_IDX text)
{
    const char *value = text == STR_IDX_ZERO ? "" : Index_To_Str(text);
    size_t size = value == NULL ? 0 : strlen(value);
    fingerprint = DSL_Shape_Stable_Identity_Hash_Bytes
                      (fingerprint, value, size);
    const unsigned char separator = 0;
    return DSL_Shape_Stable_Identity_Hash_Bytes
               (fingerprint, &separator, sizeof(separator));
}

static BOOL
DSL_Shape_PU_Stable_Identity
        (ST_IDX owner_locator,
         DSL_PU_SOURCE_IDENTITY_RECORD *identity,
         UINT64 *fingerprint)
{
    if (identity == NULL || fingerprint == NULL ||
        !DSL_Call_Image_Find_PU_Identity(owner_locator, identity) ||
        identity->owner_pu_st != owner_locator ||
        !DSL_Shape_Str_Stable(identity->canonical_definition_name) ||
        !DSL_Shape_Str_Stable(identity->defining_module) ||
        !DSL_Shape_Str_Stable(identity->defining_file) ||
        identity->defining_line == 0 || identity->flags != 0 ||
        identity->reserved != 0)
        return FALSE;
    UINT64 value = 1469598103934665603ULL;
    value = DSL_Shape_Stable_Identity_Hash_String
                (value, identity->canonical_definition_name);
    value = DSL_Shape_Stable_Identity_Hash_String
                (value, identity->defining_module);
    value = DSL_Shape_Stable_Identity_Hash_String
                (value, identity->defining_file);
    unsigned char line_bytes[4];
    for (UINT32 i = 0; i < sizeof(line_bytes); ++i)
        line_bytes[i] = (identity->defining_line >> (i * 8)) & 0xff;
    value = DSL_Shape_Stable_Identity_Hash_Bytes
                (value, line_bytes, sizeof(line_bytes));
    UINT32 tuple_count = 0;
    for (UINT32 i = 1; i <= DSL_Call_Image_PU_Identity_Count(); ++i) {
        DSL_PU_SOURCE_IDENTITY_RECORD candidate;
        if (!DSL_Call_Image_Get_PU_Identity(i, &candidate) ||
            candidate.id != i ||
            ST_IDX_index(candidate.owner_pu_st) == 0 ||
            !DSL_Shape_Str_Stable
                 (candidate.canonical_definition_name) ||
            !DSL_Shape_Str_Stable(candidate.defining_module) ||
            !DSL_Shape_Str_Stable(candidate.defining_file) ||
            candidate.defining_line == 0 || candidate.flags != 0 ||
            candidate.reserved != 0)
            return FALSE;
        if (strcmp(Index_To_Str(candidate.canonical_definition_name),
                   Index_To_Str(identity->canonical_definition_name)) == 0 &&
            strcmp(Index_To_Str(candidate.defining_module),
                   Index_To_Str(identity->defining_module)) == 0 &&
            strcmp(Index_To_Str(candidate.defining_file),
                   Index_To_Str(identity->defining_file)) == 0 &&
            candidate.defining_line == identity->defining_line)
            ++tuple_count;
    }
    if (tuple_count != 1)
        return FALSE;
    *fingerprint = value;
    return TRUE;
}

static BOOL
DSL_Shape_PU_Locators_Equivalent
        (ST_IDX left,
         ST_IDX right)
{
    if (left == right)
        return TRUE;
    DSL_PU_SOURCE_IDENTITY_RECORD left_identity;
    DSL_PU_SOURCE_IDENTITY_RECORD right_identity;
    UINT64 left_fingerprint;
    UINT64 right_fingerprint;
    return DSL_Shape_PU_Stable_Identity
               (left, &left_identity, &left_fingerprint) &&
           DSL_Shape_PU_Stable_Identity
               (right, &right_identity, &right_fingerprint) &&
           left_fingerprint == right_fingerprint &&
           strcmp(Index_To_Str(left_identity.canonical_definition_name),
                  Index_To_Str(right_identity.canonical_definition_name)) == 0 &&
           strcmp(Index_To_Str(left_identity.defining_module),
                  Index_To_Str(right_identity.defining_module)) == 0 &&
           strcmp(Index_To_Str(left_identity.defining_file),
                  Index_To_Str(right_identity.defining_file)) == 0 &&
           left_identity.defining_line == right_identity.defining_line;
}

static BOOL
DSL_Shape_Find_Active_Formal
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         DSL_IR_VALUE_ID value_id,
         DSL_PU_FORMAL_RECORD *formal)
{
    if (context == NULL || formal == NULL ||
        value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;
    BOOL found = FALSE;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD candidate;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &candidate))
            return FALSE;
        if (candidate.owner_pu_st !=
                context->active_boundary.owner_pu_st ||
            candidate.formal_value_id != value_id)
            continue;
        if (found)
            return FALSE;
        *formal = candidate;
        found = TRUE;
    }
    return found;
}

static BOOL
DSL_Shape_Interface_Mapping_Validate
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_SHAPE_INTERFACE_MAPPING_SELECTOR &selector,
         DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE *evidence)
{
    if (context == NULL || evidence == NULL ||
        evidence->count >= DSL_SHAPE_MAX_INTERFACE_MAPPINGS)
        return FALSE;
    if (selector.callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
        selector.snapshot_version != 1 ||
        selector.destination_owner_pu_st !=
            context->active_boundary.owner_pu_st)
        return FALSE;

    DSL_CALLSITE_METADATA_RECORD callsite;
    DSL_CALLSITE_METADATA_RECORD callsite_by_wn;
    DSL_CALL_ARGUMENT_RECORD argument;
    DSL_CALL_ARGUMENT_RECORD unique_argument;
    DSL_PU_FORMAL_RECORD formal;
    DSL_PU_SOURCE_IDENTITY_RECORD source_identity;
    DSL_PU_SOURCE_IDENTITY_RECORD destination_identity;
    DSL_IR_VALUE_RECORD actual_value;
    DSL_IR_VALUE_RECORD formal_value;
    UINT64 source_identity_fingerprint;
    UINT64 destination_identity_fingerprint;
    const WN *call = DSL_Call_Image_Get_Call_WN(selector.callsite_id);
    const WN *parm = call == NULL ||
                     selector.actual_ordinal >= (UINT32)WN_kid_count(call) ?
                     NULL : WN_kid(call, selector.actual_ordinal);
    const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                        NULL : WN_kid0(parm);
    const WN *entry = context->active_boundary.tree;
    const WN *formal_wn = entry == NULL ||
        selector.formal_ordinal >= (UINT32)WN_num_formals(entry) ?
        NULL : WN_formal(entry, selector.formal_ordinal);
    if (!DSL_Call_Image_Get_Callsite(selector.callsite_id, &callsite) ||
        call == NULL ||
        WN_operator(call) != OPR_CALL ||
        WN_st_idx(call) != selector.destination_owner_pu_st ||
        parm == NULL || address == NULL || WN_operator(address) != OPR_LDA ||
        !WN_Parm_By_Reference(parm) || !WN_Parm_Read_Only(parm) ||
        WN_Parm_Out(parm) || !WN_Parm_Passed_Not_Saved(parm) ||
        !DSL_Call_Image_Find_Callsite(call, &callsite_by_wn) ||
        !DSL_Call_ABI_Image_Find_Argument_By_Id
             (selector.callsite_id, selector.actual_ordinal, &argument) ||
        DSL_Call_ABI_Image_Callee_Formal_Count
            (selector.destination_owner_pu_st,
             selector.formal_ordinal) != 1 ||
        !DSL_Call_ABI_Image_Get_Callee_Formal_Argument
             (selector.destination_owner_pu_st, selector.formal_ordinal, 0,
              &unique_argument) ||
        !DSL_PU_Interface_Image_Find_Formal
             (selector.destination_owner_pu_st, selector.formal_ordinal,
              &formal) ||
        !DSL_Call_Image_Find_PU_Identity
             (callsite.owner_pu_st, &source_identity) ||
        !DSL_Call_Image_Find_PU_Identity
             (selector.destination_owner_pu_st, &destination_identity) ||
        !DSL_IR_Image_Get_Value
             (argument.argument_value_id, &actual_value) ||
        !DSL_IR_Image_Get_Value(formal.formal_value_id, &formal_value))
        return FALSE;

    if (callsite.id != selector.callsite_id ||
        memcmp(&callsite, &callsite_by_wn, sizeof(callsite)) != 0 ||
        ST_IDX_index(callsite.owner_pu_st) == 0 ||
        callsite.callee_pu_st != selector.destination_owner_pu_st ||
        callsite.flags != 0 ||
        !DSL_Shape_Str_Stable(callsite.canonical_class_name) ||
        !DSL_Shape_Str_Stable(callsite.instance_path) ||
        !DSL_Shape_Str_Stable(callsite.context_identity) ||
        argument.callsite_id != selector.callsite_id ||
        argument.actual_ordinal != selector.actual_ordinal ||
        argument.callee_formal_ordinal != selector.formal_ordinal ||
        argument.argument_value_id == DSL_IR_VALUE_INVALID_ID ||
        argument.flags != 0 ||
        !DSL_Shape_Str_Stable(argument.semantic_role) ||
        strcmp(Index_To_Str(argument.semantic_role), "shape_mapping") != 0 ||
        unique_argument.id != argument.id ||
        unique_argument.callsite_id != argument.callsite_id ||
        unique_argument.argument_value_id != argument.argument_value_id ||
        unique_argument.actual_ordinal != argument.actual_ordinal ||
        unique_argument.callee_formal_ordinal !=
            argument.callee_formal_ordinal ||
        unique_argument.semantic_role != argument.semantic_role ||
        formal.owner_pu_st != selector.destination_owner_pu_st ||
        formal.formal_ordinal != selector.formal_ordinal ||
        formal.formal_value_id == DSL_IR_VALUE_INVALID_ID ||
        ST_IDX_index(formal.formal_st) == 0 ||
        formal.formal_ty == TY_IDX_ZERO ||
        formal.flags != 0 || formal.reserved != 0 ||
        formal_wn == NULL || WN_operator(formal_wn) != OPR_IDNAME ||
        WN_st_idx(formal_wn) != formal.formal_st ||
        formal_value.id != formal.formal_value_id ||
        formal_value.value_kind != DSL_IR_VALUE_SYMBOL ||
        formal_value.producer_node_id != DSL_IR_NODE_INVALID_ID ||
        formal_value.flags != 0 || formal_value.reserved != 0 ||
        formal_value.st != formal.formal_st ||
        formal_value.ty != formal.formal_ty ||
        actual_value.id != argument.argument_value_id ||
        actual_value.value_kind == DSL_IR_VALUE_UNKNOWN ||
        actual_value.flags != 0 || actual_value.reserved != 0 ||
        WN_st_idx(address) != actual_value.st ||
        WN_ty(address) != WN_ty(parm) ||
        TY_kind(WN_ty(parm)) != KIND_POINTER ||
        TY_pointed(WN_ty(parm)) != actual_value.ty ||
        actual_value.ty != formal.formal_ty ||
        !DSL_Shape_Value_Belongs_To_PU
             (actual_value, callsite.owner_pu_st) ||
        !DSL_Shape_Value_Belongs_To_PU
             (formal_value, selector.destination_owner_pu_st) ||
        source_identity.owner_pu_st != callsite.owner_pu_st ||
        destination_identity.owner_pu_st !=
            selector.destination_owner_pu_st ||
        !DSL_Shape_Str_Stable
             (source_identity.canonical_definition_name) ||
        !DSL_Shape_Str_Stable(source_identity.defining_module) ||
        !DSL_Shape_Str_Stable(source_identity.defining_file) ||
        source_identity.defining_line == 0 ||
        !DSL_Shape_Str_Stable
             (destination_identity.canonical_definition_name) ||
        !DSL_Shape_Str_Stable(destination_identity.defining_module) ||
        !DSL_Shape_Str_Stable(destination_identity.defining_file) ||
        destination_identity.defining_line == 0 ||
        !DSL_Shape_PU_Stable_Identity
             (callsite.owner_pu_st, &source_identity,
              &source_identity_fingerprint) ||
        !DSL_Shape_PU_Stable_Identity
             (selector.destination_owner_pu_st, &destination_identity,
              &destination_identity_fingerprint))
        return FALSE;

    if (selector.source_owner_pu_st != callsite.owner_pu_st ||
        selector.callsite_owner_pu_st != callsite.owner_pu_st ||
        selector.callsite_callee_pu_st != callsite.callee_pu_st ||
        selector.source_identity_fingerprint !=
            source_identity_fingerprint ||
        selector.destination_identity_fingerprint !=
            destination_identity_fingerprint ||
        !DSL_Shape_Str_Content_Equal
             (selector.source_canonical_definition_name,
              source_identity.canonical_definition_name) ||
        !DSL_Shape_Str_Content_Equal
             (selector.source_defining_module,
              source_identity.defining_module) ||
        !DSL_Shape_Str_Content_Equal
             (selector.source_defining_file,
              source_identity.defining_file) ||
        selector.source_defining_line != source_identity.defining_line ||
        !DSL_Shape_Str_Content_Equal
             (selector.destination_canonical_definition_name,
              destination_identity.canonical_definition_name) ||
        !DSL_Shape_Str_Content_Equal
             (selector.destination_defining_module,
              destination_identity.defining_module) ||
        !DSL_Shape_Str_Content_Equal
             (selector.destination_defining_file,
              destination_identity.defining_file) ||
        selector.destination_defining_line !=
            destination_identity.defining_line ||
        !DSL_Shape_Str_Content_Equal
             (selector.canonical_class_name,
              callsite.canonical_class_name) ||
        !DSL_Shape_Str_Content_Equal
             (selector.instance_path, callsite.instance_path) ||
        !DSL_Shape_Str_Content_Equal
             (selector.context_identity, callsite.context_identity) ||
        selector.source_call_ordinal != callsite.source_call_ordinal ||
        selector.argument_value_id != argument.argument_value_id ||
        !DSL_Shape_Str_Content_Equal
             (selector.argument_semantic_role,
              argument.semantic_role) ||
        selector.actual_st != actual_value.st ||
        selector.actual_ty != actual_value.ty ||
        selector.formal_owner_pu_st != formal.owner_pu_st ||
        selector.formal_value_id != formal.formal_value_id ||
        selector.formal_st != formal.formal_st ||
        selector.formal_ty != formal.formal_ty)
        return FALSE;

    DSL_SHAPE_FACT actual_fact;
    ST_IDX qualified_source_locator;
    BOOL has_foreign_owner;
    DSL_PU_SOURCE_IDENTITY_RECORD qualified_source_identity;
    UINT64 qualified_source_fingerprint;
    if (!DSL_Shape_Fact_From_Type(actual_value.ty, &actual_fact) ||
        !DSL_Shape_Fact_Foreign_Owner
             (actual_fact, selector.destination_owner_pu_st,
              &qualified_source_locator, &has_foreign_owner) ||
        !has_foreign_owner ||
        !DSL_Shape_PU_Stable_Identity
             (qualified_source_locator, &qualified_source_identity,
              &qualified_source_fingerprint) ||
        qualified_source_fingerprint != source_identity_fingerprint ||
        !DSL_Shape_PU_Locators_Equivalent
             (qualified_source_locator, callsite.owner_pu_st))
        return FALSE;

    evidence->entries[evidence->count].source_owner_locator =
        callsite.owner_pu_st;
    evidence->entries[evidence->count].source_identity_fingerprint =
        source_identity_fingerprint;
    evidence->entries[evidence->count].destination_identity_fingerprint =
        destination_identity_fingerprint;
    evidence->entries[evidence->count].argument_value_id =
        argument.argument_value_id;
    evidence->entries[evidence->count].formal_value_id =
        formal.formal_value_id;
    ++evidence->count;
    return TRUE;
}

static BOOL
DSL_Shape_Proof_Context_Validate
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_IR_NODE_RECORD *node,
         const TY_IDX *operand_types,
         UINT32 operand_count,
         TY_IDX result_ty,
         DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE *mapping)
{
    if (mapping == NULL)
        return FALSE;
    memset(mapping, 0, sizeof(*mapping));
    if (context == NULL || node == NULL || result_ty == TY_IDX_ZERO ||
        (operand_count != 0 && operand_types == NULL) ||
        context->active_boundary.pu_info == NULL ||
        context->active_boundary.tree == NULL ||
        ST_IDX_index(context->active_boundary.owner_pu_st) == 0 ||
        PU_Info_proc_sym(context->active_boundary.pu_info) !=
            context->active_boundary.owner_pu_st ||
        PU_Info_tree_ptr(context->active_boundary.pu_info) !=
            context->active_boundary.tree ||
        WN_operator(context->active_boundary.tree) != OPR_FUNC_ENTRY ||
        WN_st_idx(context->active_boundary.tree) !=
            context->active_boundary.owner_pu_st ||
        context->node_id != node->id ||
        context->operand_value_count != operand_count ||
        (operand_count != 0 && context->operand_value_ids == NULL) ||
        context->result_value_id != node->result_value_id ||
        context->interface_mapping_count >
            DSL_SHAPE_MAX_INTERFACE_MAPPINGS)
        return FALSE;

    DSL_IR_NODE_RECORD image_node;
    if (!DSL_IR_Image_Get_Node(node->id, &image_node) ||
        memcmp(node, &image_node, sizeof(image_node)) != 0 ||
        image_node.operand_count != operand_count)
        return FALSE;
    for (UINT32 i = 0; i < operand_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value_Reference
                 (image_node.first_operand_reference_id + i, &reference) ||
            reference.owner_node_id != image_node.id ||
            reference.ordinal != i ||
            reference.value_id != context->operand_value_ids[i] ||
            !DSL_IR_Image_Get_Value(reference.value_id, &value) ||
            value.ty != operand_types[i] ||
            !DSL_Shape_Value_Belongs_To_PU
                 (value, context->active_boundary.owner_pu_st))
            return FALSE;
    }
    DSL_IR_VALUE_RECORD result_value;
    if (!DSL_IR_Image_Get_Value(context->result_value_id, &result_value) ||
        result_value.producer_node_id != image_node.id ||
        result_value.ty != result_ty ||
        !DSL_Shape_Value_Belongs_To_PU
             (result_value, context->active_boundary.owner_pu_st))
        return FALSE;
    for (UINT32 i = 0; i < context->interface_mapping_count; ++i) {
        for (UINT32 j = 0; j < i; ++j) {
            if (context->interface_mappings[i].callsite_id ==
                    context->interface_mappings[j].callsite_id &&
                context->interface_mappings[i].actual_ordinal ==
                    context->interface_mappings[j].actual_ordinal &&
                context->interface_mappings[i].destination_owner_pu_st ==
                    context->interface_mappings[j].destination_owner_pu_st &&
                context->interface_mappings[i].formal_ordinal ==
                    context->interface_mappings[j].formal_ordinal)
                return FALSE;
        }
        if (!DSL_Shape_Interface_Mapping_Validate
                 (context, context->interface_mappings[i], mapping))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Fact_Admitted
        (const DSL_SHAPE_FACT &fact,
         DSL_IR_VALUE_ID value_id,
         const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE &mapping)
{
    if (context == NULL)
        return FALSE;
    ST_IDX foreign_owner_pu_st;
    BOOL has_foreign_owner;
    if (!DSL_Shape_Fact_Foreign_Owner
             (fact, context->active_boundary.owner_pu_st,
              &foreign_owner_pu_st, &has_foreign_owner))
        return FALSE;
    std::vector<DSL_IR_VALUE_ID> active_values;
    return DSL_Shape_Value_Provenance_Valid
               (value_id, context, mapping, &active_values);
}

static BOOL
DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping_For_Value
        (DSL_SHAPE_PROOF_CONTEXT *context,
         DSL_IR_VALUE_ID value_id,
         std::vector<DSL_IR_VALUE_ID> *active_values)
{
    if (active_values == NULL ||
        active_values->size() > DSL_IR_Image_Value_Count())
        return FALSE;
    for (UINT32 i = 0; i < active_values->size(); ++i) {
        if ((*active_values)[i] == value_id)
            return FALSE;
    }
    DSL_IR_VALUE_RECORD value;
    DSL_SHAPE_FACT fact;
    ST_IDX foreign_owner_pu_st;
    BOOL has_foreign_owner;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        !DSL_Shape_Fact_From_Type(value.ty, &fact) ||
        !DSL_Shape_Fact_Foreign_Owner
             (fact, context->active_boundary.owner_pu_st,
              &foreign_owner_pu_st, &has_foreign_owner))
        return FALSE;
    if (!has_foreign_owner)
        return TRUE;
    DSL_PU_FORMAL_RECORD formal;
    if (!DSL_Shape_Find_Active_Formal(context, value.id, &formal)) {
        if (value.producer_node_id == DSL_IR_NODE_INVALID_ID)
            return FALSE;
        DSL_IR_NODE_RECORD node;
        if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
            node.result_value_id != value.id)
            return FALSE;
        active_values->push_back(value.id);
        for (UINT32 i = 0; i < node.operand_count; ++i) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            if (!DSL_IR_Image_Get_Value_Reference
                     (node.first_operand_reference_id + i, &reference) ||
                reference.owner_node_id != node.id ||
                reference.ordinal != i ||
                !DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping_For_Value
                     (context, reference.value_id, active_values)) {
                active_values->pop_back();
                return FALSE;
            }
        }
        active_values->pop_back();
        return TRUE;
    }
    DSL_CALL_ARGUMENT_RECORD argument;
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (DSL_Call_ABI_Image_Callee_Formal_Count
            (context->active_boundary.owner_pu_st,
             formal.formal_ordinal) != 1 ||
        !DSL_Call_ABI_Image_Get_Callee_Formal_Argument
             (context->active_boundary.owner_pu_st,
              formal.formal_ordinal, 0, &argument) ||
        !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
        !DSL_Shape_PU_Locators_Equivalent
             (callsite.owner_pu_st, foreign_owner_pu_st))
        return FALSE;
    return DSL_Shape_Proof_Context_Select_Reviewed_Interface_Mapping
               (context, argument.callsite_id, argument.actual_ordinal,
                context->active_boundary.owner_pu_st,
                formal.formal_ordinal);
}

BOOL
DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping
        (DSL_SHAPE_PROOF_CONTEXT *context)
{
    if (context == NULL || context->active_boundary.pu_info == NULL ||
        context->active_boundary.tree == NULL ||
        (context->operand_value_count != 0 &&
         context->operand_value_ids == NULL))
        return FALSE;
    std::vector<DSL_IR_VALUE_ID> active_values;
    for (UINT32 i = 0; i < context->operand_value_count; ++i) {
        if (!DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping_For_Value
                 (context, context->operand_value_ids[i], &active_values))
            return FALSE;
    }
    return DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping_For_Value
               (context, context->result_value_id, &active_values);
}

static BOOL
DSL_Shape_Merge_Equal_Fact
        (DSL_SHAPE_FACT *target,
         const DSL_SHAPE_FACT &source)
{
    if (target->state == DSL_SHAPE_FACT_CONTRADICTION ||
        source.state == DSL_SHAPE_FACT_CONTRADICTION) {
        target->state = DSL_SHAPE_FACT_CONTRADICTION;
        return FALSE;
    }
    if (target->rank < 0)
        target->rank = source.rank;
    else if (source.rank >= 0 && target->rank != source.rank) {
        target->state = DSL_SHAPE_FACT_CONTRADICTION;
        return FALSE;
    }
    if (target->rank < 0)
        return TRUE;
    for (INT32 i = 0; i < target->rank; ++i) {
        if (source.dimension_kind[i] == DSL_SHAPE_DIMENSION_PENDING)
            continue;
        if (target->dimension_kind[i] != DSL_SHAPE_DIMENSION_PENDING &&
            (target->dimension_kind[i] != source.dimension_kind[i] ||
             ((target->dimension_kind[i] == DSL_SHAPE_DIMENSION_SYMBOL ||
               target->dimension_kind[i] ==
                   DSL_SHAPE_DIMENSION_EXPRESSION) ?
                  !DSL_Shape_Dimension_Proves_Equal
                       (*target, i, source, i) :
                  strcmp(target->dimension_text[i],
                         source.dimension_text[i]) != 0))) {
            target->state = DSL_SHAPE_FACT_CONTRADICTION;
            return FALSE;
        }
        target->dimension_kind[i] = source.dimension_kind[i];
        target->dimension_known[i] = source.dimension_known[i];
        target->dimension[i] = source.dimension[i];
        strcpy(target->dimension_text[i], source.dimension_text[i]);
    }
    DSL_Shape_Fact_Classify(target);
    return TRUE;
}

static BOOL
DSL_Shape_Persisted_Fact_Proved_By_Inference
        (const DSL_SHAPE_FACT &persisted,
         const DSL_SHAPE_FACT &inferred)
{
    DSL_SHAPE_FACT merged = persisted;
    return DSL_Shape_Merge_Equal_Fact(&merged, inferred) &&
           DSL_Shape_Facts_Equal(merged, inferred);
}

static BOOL
DSL_Shape_Fact_Complete (const DSL_SHAPE_FACT &fact)
{
    return fact.state == DSL_SHAPE_FACT_COMPLETE;
}

static BOOL
DSL_Shape_Infer_Same
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (input->operand_count == 0)
        return FALSE;
    *result = input->operand_facts[0];
    for (UINT32 i = 1; i < input->operand_count; ++i) {
        if (!DSL_Shape_Merge_Equal_Fact(result, input->operand_facts[i]))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Infer_Broadcast
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (input->operand_count != 2 ||
        !DSL_Shape_Tensor_Element_Representation_Compatible
             (input->operand_types[0], input->operand_types[1]))
        return FALSE;
    if (DSL_Shape_Attribute_Equals
            (input->node, "attr.broadcast_rule", "none"))
        return DSL_Shape_Infer_Same(input, result);
    if (!DSL_Shape_Attribute_Equals
             (input->node, "attr.broadcast_rule", "numpy"))
        return FALSE;

    const DSL_SHAPE_FACT &left = input->operand_facts[0];
    const DSL_SHAPE_FACT &right = input->operand_facts[1];
    if (!DSL_Shape_Fact_Complete(left) ||
        !DSL_Shape_Fact_Complete(right))
        return TRUE;
    UINT32 left_rank = left.rank;
    UINT32 right_rank = right.rank;
    UINT32 rank = left_rank > right_rank ? left_rank : right_rank;
    DSL_Shape_Fact_Init(result);
    result->rank = rank;
    for (UINT32 offset = 0; offset < rank; ++offset) {
        BOOL has_left = offset < left_rank;
        BOOL has_right = offset < right_rank;
        const DSL_SHAPE_FACT *source;
        UINT32 source_ordinal;
        if (!has_left) {
            source = &right;
            source_ordinal = right_rank - 1 - offset;
        } else if (!has_right) {
            source = &left;
            source_ordinal = left_rank - 1 - offset;
        } else {
            UINT32 left_ordinal = left_rank - 1 - offset;
            UINT32 right_ordinal = right_rank - 1 - offset;
            if (DSL_Shape_Dimension_Proves_Equal
                    (left, left_ordinal, right, right_ordinal)) {
                source = &left;
                source_ordinal = left_ordinal;
            } else if (left.dimension_kind[left_ordinal] ==
                           DSL_SHAPE_DIMENSION_STATIC &&
                       left.dimension[left_ordinal] == 1) {
                source = &right;
                source_ordinal = right_ordinal;
            } else if (right.dimension_kind[right_ordinal] ==
                           DSL_SHAPE_DIMENSION_STATIC &&
                       right.dimension[right_ordinal] == 1) {
                source = &left;
                source_ordinal = left_ordinal;
            } else {
                return FALSE;
            }
        }
        UINT32 result_ordinal = rank - 1 - offset;
        result->dimension_kind[result_ordinal] =
            source->dimension_kind[source_ordinal];
        result->dimension_known[result_ordinal] =
            source->dimension_known[source_ordinal];
        result->dimension[result_ordinal] =
            source->dimension[source_ordinal];
        strcpy(result->dimension_text[result_ordinal],
               source->dimension_text[source_ordinal]);
    }
    DSL_Shape_Fact_Classify(result);
    return TRUE;
}

static BOOL
DSL_Shape_Set_Complete
        (const std::vector<UINT64> &dimensions,
         DSL_SHAPE_FACT *result)
{
    if (dimensions.size() > DSL_SHAPE_MAX_RANK)
        return FALSE;
    DSL_Shape_Fact_Init(result);
    result->rank = dimensions.size();
    for (UINT32 i = 0; i < dimensions.size(); ++i) {
        if (dimensions[i] == 0)
            return FALSE;
        result->dimension_kind[i] = DSL_SHAPE_DIMENSION_STATIC;
        result->dimension_known[i] = 1;
        result->dimension[i] = dimensions[i];
        snprintf(result->dimension_text[i],
                 sizeof(result->dimension_text[i]), "%llu",
                 (unsigned long long)dimensions[i]);
    }
    result->state = DSL_SHAPE_FACT_COMPLETE;
    return TRUE;
}

static BOOL
DSL_Shape_Fact_To_Dimensions
        (const DSL_SHAPE_FACT &fact,
         std::vector<UINT64> *dimensions)
{
    if (!DSL_Shape_Fact_Complete(fact) || dimensions == NULL)
        return FALSE;
    dimensions->clear();
    for (INT32 i = 0; i < fact.rank; ++i) {
        if (fact.dimension_kind[i] != DSL_SHAPE_DIMENSION_STATIC)
            return FALSE;
        dimensions->push_back(fact.dimension[i]);
    }
    return TRUE;
}

static BOOL
DSL_Shape_Infer_Matmul
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    BOOL transpose_left;
    BOOL transpose_right;
    if (input->operand_count != 2 ||
        !DSL_Shape_Tensor_Compatible
             (input->operand_types[0], input->operand_types[1], FALSE) ||
        !DSL_Shape_Matmul_Attributes
             (input->node, input->version, &transpose_left,
              &transpose_right))
        return FALSE;
    const DSL_SHAPE_FACT &left = input->operand_facts[0];
    const DSL_SHAPE_FACT &right = input->operand_facts[1];
    if (left.rank >= 0 &&
        (left.rank < 2 || left.rank != right.rank ||
         (input->version == 1 && left.rank != 2)))
        return FALSE;
    if (!DSL_Shape_Fact_Complete(left) ||
        !DSL_Shape_Fact_Complete(right))
        return TRUE;

    for (INT32 i = 0; i + 2 < left.rank; ++i) {
        if (!DSL_Shape_Dimension_Proves_Equal(left, i, right, i))
            return FALSE;
    }
    UINT32 rank = left.rank;
    UINT32 left_m = rank - (transpose_left ? 1 : 2);
    UINT32 left_k = rank - (transpose_left ? 2 : 1);
    UINT32 right_k = rank - (transpose_right ? 1 : 2);
    UINT32 right_n = rank - (transpose_right ? 2 : 1);
    if (!DSL_Shape_Dimension_Proves_Equal
             (left, left_k, right, right_k))
        return FALSE;

    DSL_Shape_Fact_Init(result);
    result->rank = rank;
    for (UINT32 i = 0; i + 2 < rank; ++i) {
        result->dimension_kind[i] = left.dimension_kind[i];
        result->dimension_known[i] = left.dimension_known[i];
        result->dimension[i] = left.dimension[i];
        strcpy(result->dimension_text[i], left.dimension_text[i]);
    }
    result->dimension_kind[rank - 2] = left.dimension_kind[left_m];
    result->dimension_known[rank - 2] = left.dimension_known[left_m];
    result->dimension[rank - 2] = left.dimension[left_m];
    strcpy(result->dimension_text[rank - 2],
           left.dimension_text[left_m]);
    result->dimension_kind[rank - 1] = right.dimension_kind[right_n];
    result->dimension_known[rank - 1] = right.dimension_known[right_n];
    result->dimension[rank - 1] = right.dimension[right_n];
    strcpy(result->dimension_text[rank - 1],
           right.dimension_text[right_n]);
    DSL_Shape_Fact_Classify(result);
    return TRUE;
}

static BOOL
DSL_Shape_Infer_Linear
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    UINT32 expected_operands = input->version == 2 ? 3 : 2;
    if ((input->version != 2 && input->version != 3) ||
        input->operand_count != expected_operands ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.has_bias",
              input->version == 2 ? "true" : "false") ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.transpose_input", "false") ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.transpose_weight", "true") ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.weight_layout", "OI"))
        return FALSE;
    std::vector<UINT64> activation;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[0], &activation) ||
        !DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[1], &weight))
        return TRUE;
    if (activation.size() < 2 || weight.size() != 2 ||
        activation[activation.size() - 1] != weight[1])
        return FALSE;
    if (input->version == 2 &&
        (!DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[2], &bias) ||
         bias.size() != 1 || bias[0] != weight[0]))
        return FALSE;
    activation[activation.size() - 1] = weight[0];
    return DSL_Shape_Set_Complete(activation, result);
}

static BOOL
DSL_Shape_Infer_Reshape
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    const char *target_text = NULL;
    std::vector<UINT64> target;
    if (input->operand_count != 1 ||
        !DSL_Shape_Node_Attribute
             (input->node, "attr.target_shape", &target_text) ||
        !DSL_Shape_Parse_Unsigned_List(target_text, FALSE, &target))
        return FALSE;
    std::vector<UINT64> source;
    if (DSL_Shape_Fact_To_Dimensions(input->operand_facts[0], &source)) {
        UINT64 source_elements = 1;
        UINT64 target_elements = 1;
        for (UINT32 i = 0; i < source.size(); ++i) {
            if (source_elements > ~(UINT64)0 / source[i])
                return FALSE;
            source_elements *= source[i];
        }
        for (UINT32 i = 0; i < target.size(); ++i) {
            if (target_elements > ~(UINT64)0 / target[i])
                return FALSE;
            target_elements *= target[i];
        }
        if (source_elements != target_elements)
            return FALSE;
    }
    return DSL_Shape_Set_Complete(target, result);
}

static BOOL
DSL_Shape_Infer_Transpose
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    const char *text = NULL;
    std::vector<UINT64> permutation;
    if (input->operand_count != 1 ||
        !DSL_Shape_Node_Attribute
             (input->node, "attr.permutation", &text) ||
        !DSL_Shape_Parse_Unsigned_List(text, TRUE, &permutation))
        return FALSE;
    const DSL_SHAPE_FACT &source = input->operand_facts[0];
    if (source.rank < 0)
        return TRUE;
    if (permutation.size() != (UINT32)source.rank)
        return FALSE;
    DSL_Shape_Fact_Init(result);
    result->rank = source.rank;
    std::vector<BOOL> seen(source.rank, FALSE);
    for (UINT32 i = 0; i < permutation.size(); ++i) {
        if (permutation[i] >= (UINT32)source.rank || seen[permutation[i]])
            return FALSE;
        seen[permutation[i]] = TRUE;
        result->dimension_kind[i] =
            source.dimension_kind[permutation[i]];
        result->dimension_known[i] =
            source.dimension_known[permutation[i]];
        result->dimension[i] = source.dimension[permutation[i]];
        strcpy(result->dimension_text[i],
               source.dimension_text[permutation[i]]);
    }
    DSL_Shape_Fact_Classify(result);
    return TRUE;
}

static BOOL
DSL_Shape_Infer_Flatten
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (input->operand_count != 1)
        return FALSE;
    const DSL_SHAPE_FACT &source = input->operand_facts[0];
    const char *start_text = NULL;
    const char *end_text = NULL;
    INT32 start;
    INT32 end;
    if (source.rank < 0 ||
        !DSL_Shape_Node_Attribute
             (input->node, "attr.start_dim", &start_text) ||
        !DSL_Shape_Node_Attribute
             (input->node, "attr.end_dim", &end_text) ||
        !DSL_Shape_Parse_Signed(start_text, &start) ||
        !DSL_Shape_Parse_Signed(end_text, &end))
        return FALSE;
    if (start < 0)
        start += source.rank;
    if (end < 0)
        end += source.rank;
    if (start < 0 || end < start || end >= source.rank)
        return FALSE;
    DSL_Shape_Fact_Init(result);
    result->rank = source.rank - (end - start);
    INT32 output = 0;
    for (INT32 i = 0; i < start; ++i, ++output) {
        result->dimension_kind[output] = source.dimension_kind[i];
        result->dimension_known[output] = source.dimension_known[i];
        result->dimension[output] = source.dimension[i];
        strcpy(result->dimension_text[output], source.dimension_text[i]);
    }
    UINT64 flattened = 1;
    BOOL known = TRUE;
    for (INT32 i = start; i <= end; ++i) {
        if (source.dimension_kind[i] != DSL_SHAPE_DIMENSION_STATIC) {
            known = FALSE;
            continue;
        }
        if (flattened > ~(UINT64)0 / source.dimension[i])
            return FALSE;
        flattened *= source.dimension[i];
    }
    if (known) {
        result->dimension_kind[output] = DSL_SHAPE_DIMENSION_STATIC;
        result->dimension_known[output] = 1;
        result->dimension[output] = flattened;
        snprintf(result->dimension_text[output],
                 sizeof(result->dimension_text[output]), "%llu",
                 (unsigned long long)flattened);
    } else {
        result->dimension_kind[output] = DSL_SHAPE_DIMENSION_PENDING;
        strcpy(result->dimension_text[output], "<pending>");
    }
    ++output;
    for (INT32 i = end + 1; i < source.rank; ++i, ++output) {
        result->dimension_kind[output] = source.dimension_kind[i];
        result->dimension_known[output] = source.dimension_known[i];
        result->dimension[output] = source.dimension[i];
        strcpy(result->dimension_text[output], source.dimension_text[i]);
    }
    DSL_Shape_Fact_Classify(result);
    return TRUE;
}

static BOOL
DSL_Shape_Infer_Conv2D
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (input->operand_count != 3)
        return FALSE;
    std::vector<UINT64> activation;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    if (!DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[0], &activation) ||
        !DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[1], &weight) ||
        !DSL_Shape_Fact_To_Dimensions
             (input->operand_facts[2], &bias))
        return TRUE;
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    const char *groups_text = NULL;
    UINT64 groups;
    if (activation.size() != 4 || weight.size() != 4 || bias.size() != 1 ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.input_layout", "NCHW") ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.weight_layout", "OIHW") ||
        !DSL_Shape_Attribute_Equals
             (input->node, "attr.output_layout", "NCHW") ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.stride", FALSE, stride) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.padding", TRUE, padding) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.dilation", FALSE, dilation) ||
        !DSL_Shape_Node_Attribute
             (input->node, "attr.groups", &groups_text) ||
        !DSL_Shape_Parse_Unsigned(groups_text, &groups) || groups == 0 ||
        kernel[0] != weight[2] || kernel[1] != weight[3] ||
        weight[1] > ~(UINT64)0 / groups ||
        weight[1] * groups != activation[1] || bias[0] != weight[0])
        return FALSE;
    std::vector<UINT64> output(4);
    output[0] = activation[0];
    output[1] = weight[0];
    for (UINT32 i = 0; i < 2; ++i) {
        UINT64 effective = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = activation[i + 2] + 2 * padding[i];
        if (padded < effective)
            return FALSE;
        output[i + 2] = (padded - effective) / stride[i] + 1;
    }
    return DSL_Shape_Set_Complete(output, result);
}

static BOOL
DSL_Shape_Infer_Pool
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result,
         BOOL global_average)
{
    if (input->operand_count != 1)
        return FALSE;
    std::vector<UINT64> output;
    if (!DSL_Shape_Fact_To_Dimensions(input->operand_facts[0], &output))
        return TRUE;
    if (output.size() != 4)
        return FALSE;
    if (global_average) {
        UINT64 size[2];
        if (!DSL_Shape_Parse_Pair
             (input->node, "attr.output_size", FALSE, size))
            return FALSE;
        output[2] = size[0];
        output[3] = size[1];
        return DSL_Shape_Set_Complete(output, result);
    }
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    BOOL ceil_mode = DSL_Shape_Attribute_Equals
                         (input->node, "attr.ceil_mode", "true");
    if (!ceil_mode && !DSL_Shape_Attribute_Equals
                          (input->node, "attr.ceil_mode", "false"))
        return FALSE;
    if (!DSL_Shape_Parse_Pair
             (input->node, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.stride", FALSE, stride) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.padding", TRUE, padding) ||
        !DSL_Shape_Parse_Pair
             (input->node, "attr.dilation", FALSE, dilation))
        return FALSE;
    for (UINT32 i = 0; i < 2; ++i) {
        UINT64 effective = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = output[i + 2] + 2 * padding[i];
        if (padded < effective)
            return FALSE;
        UINT64 numerator = padded - effective;
        output[i + 2] = numerator / stride[i] + 1;
        if (ceil_mode && numerator % stride[i] != 0)
            ++output[i + 2];
    }
    return DSL_Shape_Set_Complete(output, result);
}

static DSL_SHAPE_INFERENCE_RESULT
DSL_Shape_Infer_Operator_Core
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    BOOL valid = FALSE;
    switch (input->dsl_operator) {
    case OPR_DSLADD:
    case OPR_DSLMUL:
    case OPR_DSLDIV:
    case OPR_DSLREM:
        valid = DSL_Shape_Infer_Broadcast(input, result);
        break;
    case OPR_DSLRESIDUALADD:
        valid = DSL_Shape_Infer_Same(input, result) &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.broadcast_rule", "none") &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.shape_check", "exact") &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.residual_path", "true");
        break;
    case OPR_DSLRELU:
    case OPR_DSLOUTPUTLOGITS:
    case OPR_DSLRMSNORM:
    case OPR_DSLROTARYEMBEDDING:
    case OPR_DSLATTENTION:
    case OPR_DSLSWIGLU:
    case OPR_DSLSCATTER:
        valid = input->operand_count != 0;
        if (valid)
            *result = input->operand_facts[0];
        break;
    case OPR_DSLBATCHNORMINFER:
        valid = input->operand_count == 5 &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.training", "false") &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.input_layout", "NCHW") &&
                DSL_Shape_Attribute_Equals
                    (input->node, "attr.channel_axis", "1");
        if (valid)
            *result = input->operand_facts[0];
        if (valid && DSL_Shape_Fact_Complete(*result)) {
            for (UINT32 i = 1; i < input->operand_count; ++i) {
                const DSL_SHAPE_FACT &parameter = input->operand_facts[i];
                if (parameter.state == DSL_SHAPE_FACT_COMPLETE &&
                    (parameter.rank != 1 || result->rank != 4 ||
                     !DSL_Shape_Dimension_Proves_Equal
                          (parameter, 0, *result, 1)))
                    valid = FALSE;
            }
        }
        break;
    case OPR_DSLMATMUL:
        valid = DSL_Shape_Infer_Matmul(input, result);
        break;
    case OPR_DSLLINEAR:
        valid = DSL_Shape_Infer_Linear(input, result);
        break;
    case OPR_DSLRESHAPE:
        valid = DSL_Shape_Infer_Reshape(input, result);
        break;
    case OPR_DSLTRANSPOSE:
        valid = DSL_Shape_Infer_Transpose(input, result);
        break;
    case OPR_DSLFLATTEN:
        valid = DSL_Shape_Infer_Flatten(input, result);
        break;
    case OPR_DSLCONV2D:
        valid = DSL_Shape_Infer_Conv2D(input, result);
        break;
    case OPR_DSLMAXPOOL2D:
        valid = DSL_Shape_Infer_Pool(input, result, FALSE);
        break;
    case OPR_DSLGLOBALAVGPOOL2D:
        valid = DSL_Shape_Infer_Pool(input, result, TRUE);
        break;
    case OPR_DSLTOKENEMBEDDING:
        if (input->operand_count == 2) {
            std::vector<UINT64> tokens;
            std::vector<UINT64> weights;
            valid = TRUE;
            if (DSL_Shape_Fact_To_Dimensions
                    (input->operand_facts[0], &tokens) &&
                DSL_Shape_Fact_To_Dimensions
                    (input->operand_facts[1], &weights)) {
                if (tokens.size() != 2 || weights.size() != 2)
                    valid = FALSE;
                else {
                    tokens.push_back(weights[1]);
                    valid = DSL_Shape_Set_Complete(tokens, result);
                }
            }
        }
        break;
    default:
        return DSL_SHAPE_INFERENCE_UNREGISTERED;
    }
    if (!valid) {
        result->state = DSL_SHAPE_FACT_CONTRADICTION;
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    }
    DSL_Shape_Fact_Classify(result);
    return result->state == DSL_SHAPE_FACT_COMPLETE ?
           DSL_SHAPE_INFERENCE_COMPLETE : DSL_SHAPE_INFERENCE_PENDING;
}

static BOOL
DSL_Shape_Value_Provenance_Valid
        (DSL_IR_VALUE_ID value_id,
         const DSL_SHAPE_PROOF_CONTEXT *context,
         const DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE &mapping,
         std::vector<DSL_IR_VALUE_ID> *active_values)
{
    if (context == NULL || active_values == NULL ||
        active_values->size() > DSL_IR_Image_Value_Count())
        return FALSE;
    for (UINT32 i = 0; i < active_values->size(); ++i) {
        if ((*active_values)[i] == value_id)
            return FALSE;
    }

    DSL_IR_VALUE_RECORD value;
    DSL_SHAPE_FACT persisted;
    ST_IDX foreign_owner_pu_st;
    BOOL has_foreign_owner;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        !DSL_Shape_Value_Belongs_To_PU
             (value, context->active_boundary.owner_pu_st) ||
        !DSL_Shape_Fact_From_Type(value.ty, &persisted) ||
        !DSL_Shape_Fact_Foreign_Owner
             (persisted, context->active_boundary.owner_pu_st,
              &foreign_owner_pu_st, &has_foreign_owner))
        return FALSE;
    if (has_foreign_owner) {
        DSL_PU_SOURCE_IDENTITY_RECORD foreign_identity;
        UINT64 foreign_identity_fingerprint;
        if (!DSL_Shape_PU_Stable_Identity
                 (foreign_owner_pu_st, &foreign_identity,
                  &foreign_identity_fingerprint))
            return FALSE;
        for (UINT32 i = 0; i < mapping.count; ++i) {
            if (mapping.entries[i].source_identity_fingerprint ==
                    foreign_identity_fingerprint &&
                DSL_Shape_PU_Locators_Equivalent
                    (mapping.entries[i].source_owner_locator,
                     foreign_owner_pu_st) &&
                mapping.entries[i].formal_value_id == value_id)
                return TRUE;
        }
    }
    if (value.producer_node_id == DSL_IR_NODE_INVALID_ID)
        return !has_foreign_owner;

    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &descriptor))
        return FALSE;
    if (!DSL_Shape_Has_Operator_Rule
             ((DSL_OPERATOR)descriptor.logical_operator,
              descriptor.version))
        return !has_foreign_owner;

    active_values->push_back(value.id);
    std::vector<TY_IDX> operand_types;
    std::vector<DSL_SHAPE_FACT> operand_facts;
    for (UINT32 i = 0; i < node.operand_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_VALUE_RECORD operand;
        DSL_SHAPE_FACT fact;
        if (!DSL_IR_Image_Get_Value_Reference
                 (node.first_operand_reference_id + i, &reference) ||
            reference.owner_node_id != node.id || reference.ordinal != i ||
            !DSL_IR_Image_Get_Value(reference.value_id, &operand) ||
            !DSL_Shape_Fact_From_Type(operand.ty, &fact) ||
            !DSL_Shape_Value_Provenance_Valid
                 (operand.id, context, mapping, active_values)) {
            active_values->pop_back();
            return FALSE;
        }
        operand_types.push_back(operand.ty);
        operand_facts.push_back(fact);
    }
    active_values->pop_back();

    DSL_SHAPE_INFERENCE_INPUT input;
    memset(&input, 0, sizeof(input));
    input.dsl_operator = (DSL_OPERATOR)descriptor.logical_operator;
    input.version = descriptor.version;
    input.node = &node;
    input.operand_types = operand_types.empty() ? NULL : &operand_types[0];
    input.operand_facts = operand_facts.empty() ? NULL : &operand_facts[0];
    input.operand_count = operand_types.size();
    input.result_ty = value.ty;
    input.proof_context = context;
    DSL_SHAPE_FACT inferred;
    DSL_Shape_Fact_Init(&inferred);
    DSL_SHAPE_INFERENCE_RESULT inference =
        DSL_Shape_Infer_Operator_Core(&input, &inferred);
    return (inference == DSL_SHAPE_INFERENCE_COMPLETE ||
            inference == DSL_SHAPE_INFERENCE_PENDING) &&
           DSL_Shape_Persisted_Fact_Proved_By_Inference
               (persisted, inferred);
}

BOOL
DSL_Shape_Proof_Context_Admit_Value
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         DSL_IR_VALUE_ID value_id,
         TY_IDX value_ty)
{
    if (context == NULL || value_id == DSL_IR_VALUE_INVALID_ID ||
        value_ty == TY_IDX_ZERO)
        return FALSE;
    std::vector<TY_IDX> operand_types;
    BOOL value_is_in_context = value_id == context->result_value_id;
    for (UINT32 i = 0; i < context->operand_value_count; ++i) {
        DSL_IR_VALUE_RECORD operand;
        if (!DSL_IR_Image_Get_Value
                 (context->operand_value_ids[i], &operand))
            return FALSE;
        operand_types.push_back(operand.ty);
        value_is_in_context = value_is_in_context || operand.id == value_id;
    }
    DSL_IR_VALUE_RECORD result_value;
    if (!value_is_in_context ||
        !DSL_IR_Image_Get_Value(context->result_value_id, &result_value))
        return FALSE;
    DSL_IR_NODE_RECORD node;
    DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE mapping;
    memset(&mapping, 0, sizeof(mapping));
    if (!DSL_IR_Image_Get_Node(context->node_id, &node) ||
        !DSL_Shape_Proof_Context_Validate
             (context, &node,
              operand_types.empty() ? NULL : &operand_types[0],
              operand_types.size(), result_value.ty, &mapping))
        return FALSE;
    DSL_IR_VALUE_RECORD value;
    DSL_SHAPE_FACT fact;
    return DSL_IR_Image_Get_Value(value_id, &value) &&
           value.ty == value_ty &&
           DSL_Shape_Fact_From_Type(value.ty, &fact) &&
           DSL_Shape_Fact_Admitted
               (fact, value_id, context, mapping);
}

BOOL
DSL_Shape_Proof_Context_Admit_Refinement
        (const DSL_SHAPE_PROOF_CONTEXT *context,
         DSL_IR_VALUE_ID value_id,
         TY_IDX expected_old_ty,
         TY_IDX refined_ty)
{
    if (context == NULL || value_id == DSL_IR_VALUE_INVALID_ID ||
        expected_old_ty == TY_IDX_ZERO || refined_ty == TY_IDX_ZERO ||
        expected_old_ty == refined_ty ||
        context->result_value_id != value_id)
        return FALSE;

    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        value.ty != expected_old_ty ||
        value.producer_node_id != context->node_id ||
        !DSL_IR_Image_Get_Node(context->node_id, &node) ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &descriptor) ||
        !DSL_Shape_Has_Operator_Rule
             ((DSL_OPERATOR)descriptor.logical_operator,
              descriptor.version) ||
        !DSL_Shape_Tensor_Core_Complete(expected_old_ty) ||
        !DSL_Shape_Tensor_Core_Complete(refined_ty))
        return FALSE;

    std::vector<TY_IDX> operand_types;
    std::vector<DSL_SHAPE_FACT> operand_facts;
    operand_types.reserve(node.operand_count);
    operand_facts.resize(node.operand_count);
    for (UINT32 i = 0; i < node.operand_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_VALUE_RECORD operand;
        if (!DSL_IR_Image_Get_Value_Reference
                 (node.first_operand_reference_id + i, &reference) ||
            reference.owner_node_id != node.id || reference.ordinal != i ||
            !DSL_IR_Image_Get_Value(reference.value_id, &operand) ||
            !DSL_Shape_Fact_From_Type(operand.ty, &operand_facts[i]))
            return FALSE;
        operand_types.push_back(operand.ty);
    }

    DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE mapping;
    memset(&mapping, 0, sizeof(mapping));
    if (!DSL_Shape_Proof_Context_Validate
             (context, &node,
              operand_types.empty() ? NULL : &operand_types[0],
              operand_types.size(), expected_old_ty, &mapping))
        return FALSE;
    for (UINT32 i = 0; i < operand_types.size(); ++i) {
        if (!DSL_Shape_Fact_Admitted
                 (operand_facts[i], context->operand_value_ids[i],
                  context, mapping))
            return FALSE;
    }

    DSL_SHAPE_FACT refined_fact;
    if (!DSL_Shape_Fact_From_Type(refined_ty, &refined_fact) ||
        refined_fact.state != DSL_SHAPE_FACT_COMPLETE)
        return FALSE;
    DSL_SHAPE_INFERENCE_INPUT input;
    memset(&input, 0, sizeof(input));
    input.dsl_operator = (DSL_OPERATOR)descriptor.logical_operator;
    input.version = descriptor.version;
    input.node = &node;
    input.operand_types =
        operand_types.empty() ? NULL : &operand_types[0];
    input.operand_facts =
        operand_facts.empty() ? NULL : &operand_facts[0];
    input.operand_count = operand_types.size();
    input.result_ty = refined_ty;
    input.proof_context = context;
    DSL_SHAPE_FACT inferred;
    DSL_Shape_Fact_Init(&inferred);
    DSL_SHAPE_INFERENCE_RESULT inference =
        DSL_Shape_Infer_Operator_Core(&input, &inferred);
    return (inference == DSL_SHAPE_INFERENCE_COMPLETE ||
            inference == DSL_SHAPE_INFERENCE_PENDING) &&
           DSL_Shape_Facts_Equal(inferred, refined_fact);
}

DSL_SHAPE_INFERENCE_RESULT
DSL_Shape_Infer_Operator
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (result == NULL)
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    DSL_Shape_Fact_Init(result);
    if (input == NULL || input->proof_context == NULL ||
        input->node == NULL ||
        input->result_ty == TY_IDX_ZERO ||
        (input->operand_count != 0 &&
         (input->operand_types == NULL || input->operand_facts == NULL)))
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    if (!DSL_Shape_Has_Operator_Rule(input->dsl_operator, input->version))
        return DSL_SHAPE_INFERENCE_UNREGISTERED;
    DSL_OPERATOR_INFO info;
    if (!DSL_Operator_Get_Info_Version
             (input->dsl_operator, input->version, &info) ||
        (info.nkids >= 0 && (UINT32)info.nkids != input->operand_count))
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Opcode_Descriptor
             (input->node->opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != (UINT32)input->dsl_operator ||
        descriptor.version != input->version)
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE mapping;
    memset(&mapping, 0, sizeof(mapping));
    if (!DSL_Shape_Proof_Context_Validate
             (input->proof_context, input->node, input->operand_types,
              input->operand_count, input->result_ty, &mapping))
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        DSL_SHAPE_FACT authoritative;
        if (input->operand_facts[i].state == DSL_SHAPE_FACT_CONTRADICTION ||
            !DSL_Shape_Tensor_Core_Complete(input->operand_types[i]) ||
            !DSL_Shape_Fact_From_Type
                 (input->operand_types[i], &authoritative) ||
            !DSL_Shape_Facts_Equal
                 (input->operand_facts[i], authoritative) ||
            !DSL_Shape_Fact_Admitted
                 (authoritative,
                  input->proof_context->operand_value_ids[i],
                  input->proof_context, mapping))
            return DSL_SHAPE_INFERENCE_CONTRADICTION;
    }
    return DSL_Shape_Infer_Operator_Core(input, result);
}

typedef struct {
    DSL_IR_VALUE_ID value_id;
    TY_IDX ty;
    DSL_SHAPE_FACT seed;
    DSL_SHAPE_FACT fact;
    BOOL has_producer;
    BOOL provenance_trusted;
    SRCPOS source_position;
} DSL_SHAPE_SOLVER_VALUE;

typedef struct {
    DSL_IR_NODE_ID node_id;
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    DSL_IR_NODE_RECORD node;
    UINT32 result_index;
    std::vector<UINT32> operand_indices;
    SRCPOS source_position;
} DSL_SHAPE_SOLVER_CONSTRAINT;

static UINT32
DSL_Shape_Solver_Find_Value
        (const std::vector<DSL_SHAPE_SOLVER_VALUE> &values,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; i < values.size(); ++i) {
        if (values[i].value_id == value_id)
            return i;
    }
    return ~(UINT32)0;
}

static UINT32
DSL_Shape_Solver_Add_Value
        (std::vector<DSL_SHAPE_SOLVER_VALUE> *values,
         const DSL_IR_VALUE_RECORD &record,
         SRCPOS source_position)
{
    UINT32 found = DSL_Shape_Solver_Find_Value(*values, record.id);
    if (found != ~(UINT32)0)
        return found;
    DSL_SHAPE_SOLVER_VALUE value;
    memset(&value, 0, sizeof(value));
    value.value_id = record.id;
    value.ty = record.ty;
    value.has_producer =
        record.producer_node_id != DSL_IR_NODE_INVALID_ID;
    value.source_position = source_position;
    if (!DSL_Shape_Fact_From_Type(record.ty, &value.seed))
        value.seed.state = DSL_SHAPE_FACT_CONTRADICTION;
    value.fact = value.seed;
    values->push_back(value);
    return values->size() - 1;
}

static BOOL
DSL_Shape_Constraint_Order
        (const DSL_SHAPE_SOLVER_CONSTRAINT &left,
         const DSL_SHAPE_SOLVER_CONSTRAINT &right)
{
    return left.node_id < right.node_id;
}

static BOOL
DSL_Shape_Find_Definition_Value
        (ST_IDX owner_pu_st,
         WN *definition,
         DSL_IR_VALUE_RECORD *value,
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor,
         DSL_LOGICAL_OPCODE *logical_opcode)
{
    if (definition == NULL || WN_operator(definition) != OPR_STID ||
        WN_kid0(definition) == NULL ||
        !DSL_WN_Get_Logical_Opcode
             (WN_kid0(definition), logical_opcode, NULL))
        return FALSE;
    ST_IDX result_st = WN_st_idx(definition);
    if (ST_IDX_index(owner_pu_st) == 0 || ST_IDX_index(result_st) == 0 ||
        ST_IDX_level(result_st) != CURRENT_SYMTAB ||
        ST_IDX_index(result_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        !DSL_IR_Image_Find_PU_Value
             (result_st, ST_name(St_Table[result_st]),
              ST_name(St_Table[owner_pu_st]), value) ||
        value->st != result_st || value->ty != WN_ty(definition) ||
        !DSL_IR_Image_Get_Node(value->producer_node_id, node) ||
        node->result_value_id != value->id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node->opcode_descriptor_id, descriptor) ||
        descriptor->logical_operator !=
            (UINT32)logical_opcode->dsl_operator ||
        descriptor->version != logical_opcode->source_version ||
        node->operand_count != (UINT32)WN_kid_count(WN_kid0(definition)))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_Shape_Collect_Constraints
        (ST_IDX owner_pu_st,
         WN *wn,
         std::vector<DSL_SHAPE_SOLVER_VALUE> *values,
         std::vector<DSL_SHAPE_SOLVER_CONSTRAINT> *constraints,
         FILE *diagnostic)
{
    if (wn == NULL)
        return TRUE;
    BOOL valid = TRUE;
    if (WN_operator(wn) == OPR_STID && WN_kid0(wn) != NULL &&
        DSL_WN_Is_Native(WN_kid0(wn))) {
        DSL_IR_VALUE_RECORD result_value;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_LOGICAL_OPCODE logical_opcode;
        if (!DSL_Shape_Find_Definition_Value
                 (owner_pu_st, wn, &result_value, &node, &descriptor,
                  &logical_opcode)) {
            SRCPOS source_position = WN_Get_Linenum(wn);
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL-SHAPE-001: malformed native definition at "
                        "line %u\n", SRCPOS_linenum(source_position));
            return FALSE;
        }
        DSL_SHAPE_SOLVER_CONSTRAINT constraint;
        constraint.node_id = node.id;
        constraint.dsl_operator = logical_opcode.dsl_operator;
        constraint.version = logical_opcode.effective_version;
        constraint.node = node;
        constraint.source_position = WN_Get_Linenum(wn);
        constraint.result_index = DSL_Shape_Solver_Add_Value
                                      (values, result_value,
                                       constraint.source_position);
        if (logical_opcode.dsl_operator == OPR_DSLMODELINPUT ||
            logical_opcode.dsl_operator == OPR_DSLTENSORCONST) {
            if (logical_opcode.dsl_operator == OPR_DSLMODELINPUT)
                (*values)[constraint.result_index].has_producer = FALSE;
            return TRUE;
        }
        for (UINT32 i = 0; i < node.operand_count; ++i) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            DSL_IR_VALUE_RECORD operand;
            if (!DSL_IR_Image_Get_Value_Reference
                    (node.first_operand_reference_id + i, &reference) ||
                reference.owner_node_id != node.id ||
                reference.ordinal != i ||
                !DSL_IR_Image_Get_Value(reference.value_id, &operand)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "DSL-SHAPE-001: malformed operand %u for node "
                            "%u at line %u\n", i, node.id,
                            SRCPOS_linenum(constraint.source_position));
                valid = FALSE;
                break;
            }
            constraint.operand_indices.push_back
                (DSL_Shape_Solver_Add_Value
                     (values, operand, constraint.source_position));
        }
        if (valid)
            constraints->push_back(constraint);
    }
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
            if (!DSL_Shape_Collect_Constraints
                     (owner_pu_st, stmt, values, constraints, diagnostic))
                valid = FALSE;
        }
        return valid;
    }
    for (INT32 i = 0; i < WN_kid_count(wn); ++i) {
        if (!DSL_Shape_Collect_Constraints
                 (owner_pu_st, WN_kid(wn, i), values, constraints,
                  diagnostic))
            valid = FALSE;
    }
    return valid;
}

static BOOL
DSL_Shape_Merge_Inference
        (DSL_SHAPE_FACT *fact,
         const DSL_SHAPE_FACT &inferred,
         BOOL *changed)
{
    DSL_SHAPE_FACT before = *fact;
    if (!DSL_Shape_Merge_Equal_Fact(fact, inferred))
        return FALSE;
    *changed = !DSL_Shape_Facts_Equal(before, *fact);
    return TRUE;
}

static UINT64
DSL_Shape_Fingerprint_Bytes
        (UINT64 fingerprint,
         const void *data,
         size_t size)
{
    const unsigned char *bytes = (const unsigned char *)data;
    for (size_t i = 0; i < size; ++i) {
        fingerprint ^= bytes[i];
        fingerprint *= 1099511628211ULL;
    }
    return fingerprint;
}

static UINT64
DSL_Shape_Image_Fingerprint (void)
{
    UINT64 fingerprint = 1469598103934665603ULL;
    DSL_IR_IMAGE_HEADER header;
    DSL_IR_Image_Get_Header(&header);
    fingerprint = DSL_Shape_Fingerprint_Bytes
                      (fingerprint, &header, sizeof(header));
    for (UINT32 i = 1; i <= header.opcode_descriptor_count; ++i) {
        DSL_IR_OPCODE_DESCRIPTOR_RECORD record;
        DSL_IR_Image_Get_Opcode_Descriptor(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.node_count; ++i) {
        DSL_IR_NODE_RECORD record;
        DSL_IR_Image_Get_Node(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD record;
        DSL_IR_Image_Get_Attribute(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.value_count; ++i) {
        DSL_IR_VALUE_RECORD record;
        DSL_IR_Image_Get_Value(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    for (UINT32 i = 1; i <= header.value_reference_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD record;
        DSL_IR_Image_Get_Value_Reference(i, &record);
        fingerprint = DSL_Shape_Fingerprint_Bytes
                          (fingerprint, &record, sizeof(record));
    }
    return fingerprint;
}

static BOOL
DSL_Shape_Solver_Preflight_Seeds
        (PU_Info *pu,
         WN *tree,
         std::vector<DSL_SHAPE_SOLVER_VALUE> *values,
         const std::vector<DSL_SHAPE_SOLVER_CONSTRAINT> &constraints,
         FILE *diagnostic)
{
    if (pu == NULL || tree == NULL || values == NULL)
        return FALSE;
    for (UINT32 i = 0; i < values->size(); ++i) {
        DSL_SHAPE_SOLVER_VALUE &solver_value = (*values)[i];
        BOOL has_constraint_producer = FALSE;
        for (UINT32 j = 0; j < constraints.size(); ++j) {
            if (constraints[j].result_index == i) {
                if (has_constraint_producer)
                    return FALSE;
                has_constraint_producer = TRUE;
            }
        }
        if (has_constraint_producer) {
            DSL_Shape_Fact_Init(&solver_value.fact);
            solver_value.provenance_trusted = FALSE;
            continue;
        }

        ST_IDX foreign_owner_pu_st;
        BOOL has_foreign_owner;
        if (!DSL_Shape_Fact_Foreign_Owner
                 (solver_value.seed, PU_Info_proc_sym(pu),
                  &foreign_owner_pu_st, &has_foreign_owner))
            return FALSE;
        if (!has_foreign_owner) {
            solver_value.provenance_trusted = TRUE;
            continue;
        }

        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(solver_value.value_id, &value))
            return FALSE;
        BOOL has_consumer = FALSE;
        for (UINT32 j = 0; j < constraints.size(); ++j) {
            const DSL_SHAPE_SOLVER_CONSTRAINT &constraint = constraints[j];
            BOOL consumes_value = FALSE;
            std::vector<DSL_IR_VALUE_ID> operand_value_ids;
            for (UINT32 k = 0; k < constraint.operand_indices.size(); ++k) {
                DSL_SHAPE_SOLVER_VALUE &operand =
                    (*values)[constraint.operand_indices[k]];
                operand_value_ids.push_back(operand.value_id);
                consumes_value = consumes_value ||
                                 constraint.operand_indices[k] == i;
            }
            if (!consumes_value)
                continue;
            has_consumer = TRUE;
            DSL_SHAPE_PROOF_CONTEXT proof_context;
            if (!DSL_Shape_Proof_Context_Init
                     (&proof_context, pu, tree, PU_Info_proc_sym(pu),
                      constraint.node_id,
                      operand_value_ids.empty() ?
                          NULL : &operand_value_ids[0],
                      operand_value_ids.size(),
                      (*values)[constraint.result_index].value_id) ||
                !DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping
                     (&proof_context) ||
                !DSL_Shape_Proof_Context_Admit_Value
                     (&proof_context, value.id, value.ty))
                return FALSE;
        }
        if (!has_consumer) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL-SHAPE-005: unproved symbolic seed for value "
                        "%u\n", value.id);
            return FALSE;
        }
        solver_value.provenance_trusted = TRUE;
    }
    return TRUE;
}

static BOOL
DSL_Shape_Analyze_PU_Internal
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_REFINEMENT_VISITOR visitor,
         void *visitor_context,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    DSL_SHAPE_SOLVER_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    DSL_IR_IMAGE_HEADER image_before;
    DSL_IR_IMAGE_HEADER image_after;
    DSL_IR_Image_Get_Header(&image_before);
    UINT64 image_fingerprint_before = DSL_Shape_Image_Fingerprint();
    UINT32 type_count_before = TY_Table_Size();

    if (pu == NULL || tree == NULL) {
        if (diagnostic != NULL)
            fprintf(diagnostic, "DSL-SHAPE-001: missing PU or WHIRL tree\n");
        local_result.contradiction_count = 1;
        local_result.diagnostic_count = 1;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    std::vector<DSL_SHAPE_SOLVER_VALUE> values;
    std::vector<DSL_SHAPE_SOLVER_CONSTRAINT> constraints;
    BOOL valid = DSL_Shape_Collect_Constraints
                     (PU_Info_proc_sym(pu), tree, &values, &constraints,
                      diagnostic);
    if (valid && !DSL_Shape_Solver_Preflight_Seeds
                     (pu, tree, &values, constraints, diagnostic))
        valid = FALSE;
    std::sort(constraints.begin(), constraints.end(),
              DSL_Shape_Constraint_Order);
    local_result.visited_node_count = constraints.size();
    local_result.value_count = values.size();

    BOOL changed = TRUE;
    UINT32 iteration_limit = values.size() + constraints.size() + 1;
    while (valid && changed && local_result.iteration_count < iteration_limit) {
        changed = FALSE;
        ++local_result.iteration_count;
        for (UINT32 i = 0; i < constraints.size(); ++i) {
            DSL_SHAPE_SOLVER_CONSTRAINT &constraint = constraints[i];
            std::vector<TY_IDX> operand_types;
            std::vector<DSL_SHAPE_FACT> operand_facts;
            std::vector<DSL_IR_VALUE_ID> operand_value_ids;
            BOOL operands_trusted = TRUE;
            for (UINT32 j = 0; j < constraint.operand_indices.size(); ++j) {
                DSL_SHAPE_SOLVER_VALUE &operand =
                    values[constraint.operand_indices[j]];
                operands_trusted = operands_trusted &&
                                   operand.provenance_trusted;
                operand_types.push_back(operand.ty);
                operand_facts.push_back(operand.fact);
                operand_value_ids.push_back(operand.value_id);
            }
            if (!operands_trusted)
                continue;
            DSL_SHAPE_PROOF_CONTEXT proof_context;
            BOOL proof_context_valid = DSL_Shape_Proof_Context_Init
                (&proof_context, pu, tree, PU_Info_proc_sym(pu),
                 constraint.node_id,
                 operand_value_ids.empty() ? NULL : &operand_value_ids[0],
                 operand_value_ids.size(),
                 values[constraint.result_index].value_id) &&
                DSL_Shape_Proof_Context_Select_Unique_Interface_Mapping
                    (&proof_context);
            DSL_SHAPE_INFERENCE_INPUT input;
            input.dsl_operator = constraint.dsl_operator;
            input.version = constraint.version;
            input.node = &constraint.node;
            input.operand_types = operand_types.empty() ?
                                  NULL : &operand_types[0];
            input.operand_facts = operand_facts.empty() ?
                                  NULL : &operand_facts[0];
            input.operand_count = operand_types.size();
            input.result_ty = values[constraint.result_index].ty;
            input.proof_context = &proof_context;
            DSL_SHAPE_FACT inferred;
            DSL_Shape_Fact_Init(&inferred);
            DSL_SHAPE_INTERFACE_MAPPING_EVIDENCE mapping;
            memset(&mapping, 0, sizeof(mapping));
            BOOL context_valid = proof_context_valid &&
                DSL_Shape_Proof_Context_Validate
                    (&proof_context, &constraint.node,
                     input.operand_types, input.operand_count,
                     input.result_ty, &mapping);
            DSL_SHAPE_INFERENCE_RESULT inference = context_valid ?
                DSL_Shape_Infer_Operator_Core(&input, &inferred) :
                DSL_SHAPE_INFERENCE_CONTRADICTION;
            BOOL all_seed_facts_complete =
                values[constraint.result_index].seed.state ==
                    DSL_SHAPE_FACT_COMPLETE;
            for (UINT32 j = 0;
                 j < constraint.operand_indices.size() &&
                 all_seed_facts_complete; ++j) {
                all_seed_facts_complete =
                    values[constraint.operand_indices[j]].seed.state ==
                        DSL_SHAPE_FACT_COMPLETE;
            }
            if (context_valid && all_seed_facts_complete) {
                DSL_SHAPE_OPERATOR_INPUT check;
                check.dsl_operator = constraint.dsl_operator;
                check.version = constraint.version;
                check.node = &constraint.node;
                check.operand_types = input.operand_types;
                check.operand_count = input.operand_count;
                check.result_ty = input.result_ty;
                check.proof_context = &proof_context;
                if (DSL_Shape_Check_Operator(&check) !=
                    DSL_SHAPE_CHECK_VALID)
                    inference = DSL_SHAPE_INFERENCE_CONTRADICTION;
            }
            if (inference == DSL_SHAPE_INFERENCE_UNREGISTERED)
                continue;
            if (inference == DSL_SHAPE_INFERENCE_CONTRADICTION) {
                values[constraint.result_index].fact.state =
                    DSL_SHAPE_FACT_CONTRADICTION;
                valid = FALSE;
            } else {
                BOOL was_trusted =
                    values[constraint.result_index].provenance_trusted;
                BOOL value_changed = FALSE;
                if (!DSL_Shape_Persisted_Fact_Proved_By_Inference
                         (values[constraint.result_index].seed, inferred) ||
                    !DSL_Shape_Merge_Inference
                         (&values[constraint.result_index].fact, inferred,
                          &value_changed))
                    valid = FALSE;
                else
                    values[constraint.result_index].provenance_trusted = TRUE;
                changed = changed || value_changed || !was_trusted;
            }
            if (!valid && diagnostic != NULL) {
                fprintf(diagnostic,
                        "DSL-SHAPE-002: %s.v%u shape contradiction for "
                        "value %u at file %u line %u\n",
                        DSL_OPERATOR_name(constraint.dsl_operator),
                        constraint.version,
                        values[constraint.result_index].value_id,
                        SRCPOS_filenum(constraint.source_position),
                        SRCPOS_linenum(constraint.source_position));
                ++local_result.diagnostic_count;
            }
            if (!valid)
                break;
        }
    }

    if (valid) {
        for (UINT32 i = 0; i < constraints.size(); ++i) {
            DSL_SHAPE_SOLVER_VALUE &produced =
                values[constraints[i].result_index];
            if (!produced.provenance_trusted) {
                produced.fact.state = DSL_SHAPE_FACT_CONTRADICTION;
                valid = FALSE;
            }
        }
    }

    for (UINT32 i = 0; i < values.size(); ++i) {
        const DSL_SHAPE_SOLVER_VALUE &value = values[i];
        if (value.fact.state == DSL_SHAPE_FACT_CONTRADICTION) {
            ++local_result.contradiction_count;
            continue;
        }
        if (value.fact.state == DSL_SHAPE_FACT_COMPLETE) {
            BOOL symbolic = FALSE;
            BOOL runtime_dynamic = FALSE;
            for (INT32 j = 0; j < value.fact.rank; ++j) {
                symbolic = symbolic ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_SYMBOL ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_EXPRESSION;
                runtime_dynamic = runtime_dynamic ||
                    value.fact.dimension_kind[j] ==
                        DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
            }
            if (symbolic)
                ++local_result.symbolic_value_count;
            if (runtime_dynamic)
                ++local_result.runtime_dynamic_value_count;
            if (DSL_Shape_Facts_Equal(value.seed, value.fact))
                ++local_result.unchanged_value_count;
            else
                ++local_result.refinable_value_count;
        } else if (value.has_producer) {
            ++local_result.unresolved_value_count;
        } else {
            ++local_result.pending_value_count;
        }
    }

    DSL_IR_Image_Get_Header(&image_after);
    if (TY_Table_Size() != type_count_before ||
        memcmp(&image_before, &image_after, sizeof(image_before)) != 0 ||
        DSL_Shape_Image_Fingerprint() != image_fingerprint_before) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL-SHAPE-003: check-only analysis mutated compiler "
                    "tables\n");
        ++local_result.contradiction_count;
        ++local_result.diagnostic_count;
        valid = FALSE;
    }
    if (valid && visitor != NULL) {
        for (UINT32 i = 0; i < values.size(); ++i) {
            const DSL_SHAPE_SOLVER_VALUE &value = values[i];
            if (value.fact.state != DSL_SHAPE_FACT_COMPLETE ||
                DSL_Shape_Facts_Equal(value.seed, value.fact))
                continue;
            DSL_SHAPE_REFINEMENT refinement;
            refinement.value_id = value.value_id;
            refinement.expected_old_ty = value.ty;
            refinement.refined_fact = value.fact;
            refinement.source_position = value.source_position;
            if (!visitor(&refinement, visitor_context)) {
                if (diagnostic != NULL)
                    fprintf(diagnostic,
                            "DSL-SHAPE-004: refinement consumer rejected "
                            "value %u at file %u line %u\n",
                            refinement.value_id,
                            SRCPOS_filenum(refinement.source_position),
                            SRCPOS_linenum(refinement.source_position));
                ++local_result.diagnostic_count;
                valid = FALSE;
                break;
            }
        }
    }
    if (result != NULL)
        *result = local_result;
    return valid && local_result.contradiction_count == 0;
}

BOOL
DSL_Shape_Analyze_PU
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    return DSL_Shape_Analyze_PU_Internal
               (pu, tree, diagnostic, NULL, NULL, result);
}

BOOL
DSL_Shape_Analyze_PU_With_Refinements
        (PU_Info *pu,
         WN *tree,
         FILE *diagnostic,
         DSL_SHAPE_REFINEMENT_VISITOR visitor,
         void *visitor_context,
         DSL_SHAPE_SOLVER_RESULT *result)
{
    return DSL_Shape_Analyze_PU_Internal
               (pu, tree, diagnostic, visitor, visitor_context, result);
}
