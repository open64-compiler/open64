/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "dsl_shape.h"
#include "symtab.h"

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
    { OPR_DSLADD, 1, DSL_Shape_Same_Result },
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
    { OPR_DSLMUL, 1, DSL_Shape_Same_Result },
    { OPR_DSLDIV, 1, DSL_Shape_Same_Result },
    { OPR_DSLREM, 1, DSL_Shape_Same_Result }
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

BOOL
DSL_Shape_Has_Operator_Rule
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    return DSL_Shape_Find_Rule(dsl_operator, version) != NULL;
}

DSL_SHAPE_CHECK_RESULT
DSL_Shape_Check_Operator
        (const DSL_SHAPE_OPERATOR_INPUT *input)
{
    if (input == NULL)
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

    std::vector<TY_IDX> operands;
    operands.reserve(input->operand_count);
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        if (input->operand_types[i] == TY_IDX_ZERO)
            return DSL_SHAPE_CHECK_INVALID;
        operands.push_back(input->operand_types[i]);
    }
    return rule->function(input->node, operands, input->result_ty,
                          input->version) ?
           DSL_SHAPE_CHECK_VALID : DSL_SHAPE_CHECK_INVALID;
}
