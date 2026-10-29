/*
 * Copyright (C) 2026 Open64 Project
 * Common local schema predicates and inference reused during ingestion.
 * PU-wide traversal/solver state lives in be/vho, not this shared service.
 * Contracts: doc/WHIRL-DSL-INFRASTRUCTURE.md;
 * row sum: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md;
 * normalization: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
 * Local facts/containers are transient scratch, not new .B records. Borrowed
 * TY/image/string identities require the caller's live compiler context;
 * no helper activates another PU or changes a descriptor, tree or image row.
 * Temporary strings/vectors may allocate and allocation exceptions propagate.
 * Structural inference does not certify full schemas, runtime storage, target
 * support or transformation permissions. Ownership split:
 * doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#include <ctype.h>
#include "../ir_image/dsl_image_service.h"
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
#include "dsl_shape_fact_internal.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"

/* Check a borrowed TY index against the active type table before dereference;
 * nonzero in-range index is not a tensor/schema validity guarantee. */
static BOOL
DSL_Shape_TY_Valid (TY_IDX ty)
{
    return TY_IDX_index(ty) != 0 && TY_IDX_index(ty) < TY_Table_Size();
}

/* Read the active TY extension's four core attributes after the bounds check.
 * Presence alone suffices here; no attribute parsing or table mutation. */
BOOL
DSL_Shape_Tensor_Core_Complete (TY_IDX ty)
{
    return DSL_Shape_TY_Valid(ty) && TY_is_tensor_extension(ty) &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_KIND) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_RANK) != NULL &&
           TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE) != NULL;
}
/* Compare one borrowed schema value by exact text; two missing values match.
 * Callers supply live tensor TYs; returned evidence retains no string pointer. */
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

/* Require core attributes, identical element TY/dtype and matching layout,
 * sharding, placement, memory, quantization and runtime-state text. Read-only;
 * rank/shape and other semantic keys are deliberately outside this subset. */
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

/* Extend the read-only element/representation subset with exact rank spelling
 * and optional exact shape spelling. This does not normalize symbolic shapes
 * or compare every descriptor key; no table ownership is transferred. */
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

/* Check core presence plus identical element TY/dtype for mixed-representation
 * domain operands. Do not require rank, shape or storage attributes to match. */
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

/* Extract two trimmed nonempty matrix dimension strings into distinct caller
 * buffers. Validate both lengths before writing, preserving both on failure.
 * This lexical helper does not parse dimensions or reject suffix text after
 * the last ']'; complete descriptor admission is the caller's responsibility. */
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

/* Compare borrowed rank-2 shape text as [M,K]*[K,N]->[M,N] using bounded
 * local buffers. Only exact trimmed dimension spellings prove equality here;
 * rank, element representation and schema checks belong to the caller. */
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
/* Scan a borrowed node's attribute range in the active managed image/string
 * tables. Return the first matching non-UNKNOWN value as a borrowed string
 * (valid only while that table context remains live). Missing/unreadable rows
 * return FALSE without writing optional value; no schema uniqueness check. */
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

/* Match a borrowed node attribute to a non-NULL expected string exactly, with
 * no retention or mutation; missing, unknown or unreadable attributes fail. */
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

/* Parse a complete strtol base-10 spelling in INT32 range, allowing its leading
 * whitespace/sign syntax. Publish optional output only on success; no errno
 * recovery or compiler-state mutation is performed. */
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

/* Parse bracketed positive UINT64 extents, including [] and a trailing comma,
 * into caller-owned vector scratch. NULL inputs preserve it; otherwise clear
 * first, then append, so later failure leaves a partial vector. Reject overflow
 * and trailing non-whitespace; scratch growth may allocate. */
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

/* Parse into a private vector before publishing rank; parse failure preserves
 * outputs. Once parsed, rank is written even on capacity/NULL-array failure;
 * extent storage is touched only after all capacity checks. No table mutation. */
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

/* Check the static representation-view subset without changing either TY.
 * Require matching storage size/alignment and listed representation keys, then
 * compare overflow-checked element counts for ranks <=16 matching TY rank.
 * Strides/runtime state and runtime alias legality are not established here. */
BOOL
DSL_Shape_Representation_Preserving_View
        (TY_IDX source_ty, TY_IDX view_ty)
{
    if (!DSL_Shape_TY_Valid(source_ty) || !DSL_Shape_TY_Valid(view_ty) ||
        !TY_is_tensor_extension(source_ty) ||
        !TY_is_tensor_extension(view_ty) ||
        TY_tensor_element_ty(source_ty) != TY_tensor_element_ty(view_ty) ||
        TY_align(source_ty) != TY_align(view_ty) ||
        TY_size(source_ty) != TY_size(view_ty))
        return FALSE;

    static const TY_TENSOR_SCHEMA_KEY representation[] = {
        TY_TENSOR_SCHEMA_KIND, TY_TENSOR_SCHEMA_DTYPE,
        TY_TENSOR_SCHEMA_LAYOUT, TY_TENSOR_SCHEMA_SHARDING,
        TY_TENSOR_SCHEMA_PLACEMENT, TY_TENSOR_SCHEMA_MEMORY,
        TY_TENSOR_SCHEMA_QUANTIZATION
    };
    for (UINT32 i = 0; i < sizeof(representation) /
                            sizeof(representation[0]); ++i) {
        const char *source = TY_tensor_attribute(source_ty, representation[i]);
        const char *view = TY_tensor_attribute(view_ty, representation[i]);
        if ((source == NULL) != (view == NULL) ||
            (source != NULL && strcmp(source, view) != 0) ||
            (i < 3 && source == NULL))
            return FALSE;
    }

    UINT64 source_dimensions[DSL_SHAPE_MAX_RANK];
    UINT64 view_dimensions[DSL_SHAPE_MAX_RANK];
    UINT32 source_rank = 0;
    UINT32 view_rank = 0;
    if (!DSL_Shape_Parse_Static_Dimensions
             (TY_tensor_attribute(source_ty, TY_TENSOR_SCHEMA_SHAPE),
              source_dimensions, DSL_SHAPE_MAX_RANK, &source_rank) ||
        !DSL_Shape_Parse_Static_Dimensions
             (TY_tensor_attribute(view_ty, TY_TENSOR_SCHEMA_SHAPE),
              view_dimensions, DSL_SHAPE_MAX_RANK, &view_rank) ||
        source_rank != (UINT32)TY_tensor_rank(source_ty) ||
        view_rank != (UINT32)TY_tensor_rank(view_ty))
        return FALSE;

    UINT64 source_count = 1;
    UINT64 view_count = 1;
    for (UINT32 i = 0; i < source_rank; ++i) {
        if (source_dimensions[i] == 0 ||
            source_count > ~0ULL / source_dimensions[i])
            return FALSE;
        source_count *= source_dimensions[i];
    }
    for (UINT32 i = 0; i < view_rank; ++i) {
        if (view_dimensions[i] == 0 ||
            view_count > ~0ULL / view_dimensions[i])
            return FALSE;
        view_count *= view_dimensions[i];
    }
    return source_count == view_count;
}

/* Recognize the exact flatten.v2 zero-motion spelling only; no type, source
 * storage or use-site evidence is consulted or changed. */
BOOL
DSL_Shape_Zero_Motion_View_Operator
        (UINT32 dsl_operator, UINT16 version)
{
    return dsl_operator == OPR_DSLFLATTEN && version == 2;
}

/* Write positive extents into caller text storage incrementally without heap
 * allocation. Invalid pointers fail before writing; zero extents or insufficient
 * capacity can leave a prefix/truncated string. Rank zero formats as []. */
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

/* Parse a nonempty comma-only list of UINT64 decimals (no whitespace), accepting
 * zero only when requested. NULL/empty preserves scratch; otherwise clear then
 * append, retaining a parsed prefix on later failure/overflow. May allocate. */
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

/* Parse all-decimal UINT64 text with checked accumulation, accepting zero;
 * optional caller output is written only after the complete text succeeds. */
static BOOL DSL_Shape_Parse_Unsigned(const char *, UINT64 *);

/* Read exactly two comma-separated strtoull values from a live node attribute.
 * Require digit-leading fields, no suffix, and optionally nonzero values;
 * publish both caller slots only after success. libc range/errno overflow is
 * not checked here, so this is not the checked unsigned-list parser. */
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

/* Parse a live TY's shape into caller vector scratch via the static parser;
 * failure can leave a cleared/partial vector. No TY mutation; may allocate. */
static BOOL
DSL_Shape_Static_Dimensions
        (TY_IDX ty,
         std::vector<UINT64> *dimensions)
{
    return DSL_Shape_Parse_Static_Shape
               (TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE), dimensions);
}

/* Compare borrowed extent vectors by length and exact UINT64 values, without
 * allocation, shape normalization or mutation. */
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

/* Check result shape against caller-derived static extents and require input/
 * result element/representation compatibility. Allocate only local parse scratch;
 * reject without modifying either borrowed TY or expected vector. */
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

/* Check computed result extents, element/dtype, layout and quantization against
 * a borrowed operand. Reject result placement=side_file or memory=external_data;
 * other representation keys need not match. Local parse allocations only. */
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

/* Validate linear.v2 (input, OI weight, bias) or v3 (no bias), with exact
 * has_bias/transpose_input=false/transpose_weight=true/weight_layout=OI tokens.
 * Static [...,K]*[N,K] produces [...,N], bias [N]; compare element types and
 * result representation. Borrow all records/TYs; local vectors only, no writes. */
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

/* Check attr.target_shape against static result extents and overflow-checked
 * source/target element products, preserving element representation. Borrow
 * node/TYs; scratch vectors may allocate, with no table/descriptor mutation. */
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

/* Validate a zero-based full permutation with no repeats and the corresponding
 * static result extents/representation. Borrow node/TYs; seen/extent vectors
 * are private scratch and rejection leaves compiler state unchanged. */
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

/* Validate v2 static matmul using exact true/false transpose tokens, exact
 * batch_rule and float32 accumulation. Require equal batch extents, matching
 * contraction K and result [...,M,N]; no broadcast, TY mutation or target choice.
 * Caller dispatch establishes version/arity; vectors are local scratch. */
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

/* Check exact result compatibility plus v2 semantic=logits, or v3 rank-3
 * semantic=token_logits with sequence_axis=-2/vocabulary_axis=-1. Borrow
 * node/TYs, allocate only local shape scratch, and reject without mutation. */
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

/* Read a borrowed attribute as a complete strtod value >0 and <=DBL_MAX;
 * this host-number predicate is not a canonical FP32-bit binding. No output,
 * descriptor mutation or target numeric-environment permission results. */
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

/* Check local static transformer contracts on live node/TY inputs using private
 * vectors: int64 tokens/[V,H] float32 embeddings; last-axis FP32 RMSNorm with
 * positive epsilon; BHSD half_split rotary v1 static positions or v2 explicit
 * int64 position operand; equal-head attention v1 full_sequence/causal/none or
 * v2 single_token_decode/implicit_prefix_causal/functional_append; rank-3 silu
 * SwiGLU with exact shapes. Enforce the branch's layout/axis/scale tokens and
 * descriptor subset only. Dispatch owns supported versions; no lowering,
 * runtime bounds proof, allocation of compiler records or target policy. */
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

/* Validate three static NCHW/OIHW convolution operands with group/channel/bias
 * consistency and positive kernel/stride/dilation, nonnegative padding. Derive
 * spatial floor extents after local arithmetic guards and compare result
 * representation. No descriptor writes or convolution algorithm selection;
 * parser/range limitations remain those of Parse_Pair and this arithmetic. */
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

/* Validate inference-only NCHW batch norm with channel_axis=1, host-parsed
 * 0<epsilon<1, input rank 4 and four matching channel vectors. Compare element
 * types/result representation without changing borrowed records or choosing
 * numerical implementation; private extent vectors may allocate. */
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

/* Check static rank-4 pooling extents from kernel/stride/padding/dilation and
 * exact true/false ceil_mode. Local guards precede floor/ceil extent arithmetic;
 * compare input/result representation without adding framework edge adjustment
 * or choosing implementation. Borrow tables, allocate only scratch vectors. */
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

/* Admit only rank-4 spatial global averaging with reduction_axes=spatial and
 * output_size=1,1; compare [N,C,1,1] and input representation. Read-only borrowed
 * node/TYs, local parse scratch; no reduction-order or target permission. */
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

/* Normalize signed start/end axes against positive TY rank, check the static
 * flattened extent product for UINT64 overflow and match unchanged outer axes.
 * Borrow types/node and preserve their representation; scratch parsing may
 * allocate. This validates shape, not runtime alias or zero-motion legality. */
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

/* Parse digit-only UINT64 text, including zero, with overflow checks. Optional
 * output is published only after full success; no whitespace/signs or mutation
 * of borrowed text/compiler context are permitted. */
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


/* Local rule callbacks borrow a live node, ordered operand TYs and result TY
 * for one exact version. Return structural acceptance without compiler-state
 * mutation; scratch allocation is allowed, but no target/optimization choice. */
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

/* Require a nonempty operand list with exact compatible rank/shape and
 * representation across all operands/result. Registry callback ignores node
 * and version; read-only, with no broadcast or symbolic normalization. */
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

/* Dispatch broadcast_rule=none to exact compatibility; numpy right-aligns two
 * static shapes and expands singleton/missing dimensions only. Borrow node/TYs;
 * local vectors allocate, while failure changes no caller or compiler state. */
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

/* Registry callback checks only first operand/result exact compatibility and
 * nonempty arity. Other operands/attributes belong to surrounding gatekeepers;
 * no mutation, allocation or runtime-value equivalence is established. */
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

/* Require two compatible operand representations/ranks; v1 compares lexical
 * matrix dimensions, v2 delegates transpose/exact-batch static checks. Unknown
 * versions fail. Borrow all inputs and change no tables; v2 may allocate scratch. */
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

/* Route the exact-version rule to linear v2/v3 static shape/attribute checks.
 * Borrow node/operand/result identities; only callee scratch is allocated and
 * FALSE leaves compiler/caller data unchanged. */
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

/* Require one operand before the flatten axis/product validator. Registry
 * supplies the version; this callback changes no borrowed node/TY data and
 * grants no runtime view/alias permission on success. */
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

/* Forward the registered convolution contract to static NCHW/OIHW shape
 * validation, which checks three operands. Borrow all data; reject without
 * table changes, with local parse allocation only. */
static BOOL
DSL_Shape_Conv2D_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return DSL_Shape_Conv2D_Result_Valid(node, operands, result_ty);
}

/* Route the registered inference batch-norm rule to its five-operand static
 * channel/epsilon validator. Borrow data for the call, allocate only scratch,
 * and leave all compiler/caller records unchanged on either result. */
static BOOL
DSL_Shape_Batch_Norm_Result
        (const DSL_IR_NODE_RECORD *node,
         const std::vector<TY_IDX> &operands,
         TY_IDX result_ty,
         UINT16)
{
    return DSL_Shape_Batch_Norm_Result_Valid(node, operands, result_ty);
}

/* Enforce one operand before static rank-4 floor/ceil pooling validation.
 * Exact version is registry-owned; no mutation or pooling implementation
 * choice, and only callee extent scratch may allocate. */
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

/* Enforce one operand before the static spatial [1,1] average-pool check.
 * Borrow node/TYs and leave them unchanged; extent scratch may allocate and
 * acceptance does not choose accumulation order. */
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

/* Enforce one operand before target-shape/checked-element-count validation.
 * Version admission is the registry's role; borrowed inputs remain unchanged
 * and success does not prove a runtime storage/alias plan. */
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

/* Enforce one operand before full static permutation/result validation.
 * The registered version is not reinterpreted here; private scratch may
 * allocate, but no node/TY mutation or physical transpose selection occurs. */
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

/* Bind the shared domain validator to token embedding, preserving the caller's
 * exact version. Check int64 indices/float32 weights and result shape, not
 * runtime index bounds; borrow inputs and mutate no compiler records. */
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

/* Bind the shared domain validator to RMSNorm's last-axis static FP32 shape
 * and positive epsilon subset. Borrow inputs, allow local scratch allocation;
 * no rsqrt accuracy, reassociation or target permission is inferred. */
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

/* Bind rotary embedding's shared validator to the exact supplied version so
 * v1 static-position and v2 explicit-position rules remain distinct. Borrow
 * node/TYs and allocate only scratch; runtime position values are not proved. */
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

/* Bind attention's static domain validator, retaining version-specific mask,
 * cache and sequence contracts. Borrow inputs, allocate scratch only; no
 * runtime cache policy, attention lowering or target support is selected. */
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

/* Bind SwiGLU to the rank-3 equal-shape silu domain check. Borrow node/TYs;
 * local scratch may allocate, with no compiler-record mutation or approximation
 * choice implied by structural acceptance. */
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

/* Require exact compatible operand/result shapes plus broadcast_rule=none,
 * shape_check=exact and residual_path=true. Borrow the registered residual node
 * and types; no allocation, graph rewrite or residual fusion permission. */
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

/* Enforce one operand before the v2 logits/v3 token-logits static contract.
 * Preserve the exact version, borrow all identities, and leave records
 * unchanged; v3 may allocate shape scratch and performs no output lowering. */
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

/* Validate the published static FP32 row-sum type/attribute contract without
 * mutating descriptors. Builder, gatekeeper and PREOPT share this predicate;
 * it does not choose a reduction order or authorize a native implementation.
 * Require F4/float32 positive static [R,N]->[R], axis=1, keepdim=false,
 * accum_dtype=float32 and reassociate=false with matching representation.
 * Borrow live TYs/strings; parser vectors may allocate, but no tables change. */
BOOL
DSL_Shape_Reduce_Sum_Types_Valid
        (TY_IDX operand_ty, TY_IDX result_ty, const char *axis,
         const char *keepdim, const char *accum_dtype,
         const char *reassociate)
{
    UINT64 input[2], output[1];
    UINT32 input_rank = 0, output_rank = 0;
    return DSL_Shape_Tensor_Core_Complete(operand_ty) &&
           DSL_Shape_Tensor_Core_Complete(result_ty) &&
           axis != NULL && strcmp(axis, "1") == 0 &&
           keepdim != NULL && strcmp(keepdim, "false") == 0 &&
           accum_dtype != NULL && strcmp(accum_dtype, "float32") == 0 &&
           reassociate != NULL && strcmp(reassociate, "false") == 0 &&
           TY_tensor_rank(operand_ty) == 2 && TY_tensor_rank(result_ty) == 1 &&
           strcmp(TY_tensor_attribute(operand_ty, TY_TENSOR_SCHEMA_DTYPE),
                  "float32") == 0 &&
           DSL_Shape_TY_Valid(TY_tensor_element_ty(operand_ty)) &&
           TY_mtype(TY_tensor_element_ty(operand_ty)) == MTYPE_F4 &&
           DSL_Shape_Tensor_Element_Representation_Compatible
               (operand_ty, result_ty) &&
           DSL_Shape_Parse_Static_Dimensions
               (TY_tensor_attribute(operand_ty, TY_TENSOR_SCHEMA_SHAPE),
                input, 2, &input_rank) && input_rank == 2 &&
           DSL_Shape_Parse_Static_Dimensions
               (TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE),
                output, 1, &output_rank) && output_rank == 1 &&
           input[0] > 0 && input[1] > 0 && output[0] == input[0];
}

/* Validate explicit ordered source meaning, independently of target support.
 * Require ascending_left_fold, positive-zero bits 00000000,
 * rne_gradual_no_traps and class_only exactly. NULL/mismatch fails without
 * allocation or mutation. See
 * doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md. */
BOOL
DSL_ordered_reduce_sum_binding_valid
    (const char *order, const char *identity_bits,
     const char *fp_environment, const char *nan_policy)
{
    return order != NULL && strcmp(order, "ascending_left_fold") == 0 &&
           identity_bits != NULL && strcmp(identity_bits, "00000000") == 0 &&
           fp_environment != NULL && strcmp(fp_environment, "rne_gradual_no_traps") == 0 &&
           nan_policy != NULL && strcmp(nan_policy, "class_only") == 0;
}

/* Adapt borrowed attributes to the row-sum check: one operand, exactly four
 * v1 fields or eight v2 fields with explicit ordered binding. Unsupported
 * versions/counts/missing fields fail without mutation; retain v1's unresolved
 * order and do not upgrade it by borrowing the v2 predicate. */
static BOOL
DSL_Shape_Reduce_Sum_Result
        (const DSL_IR_NODE_RECORD *node, const std::vector<TY_IDX> &operands,
         TY_IDX result_ty, UINT16 version)
{
    const char *axis = NULL, *keepdim = NULL;
    const char *accum = NULL, *reassociate = NULL;
    if (node == NULL) return FALSE;
    const char *order = NULL, *identity = NULL, *environment = NULL, *nan = NULL;
    BOOL binding = version == 1 ||
        (version == 2 &&
         DSL_Shape_Node_Attribute(node, "attr.order", &order) &&
         DSL_Shape_Node_Attribute(node, "attr.identity_bits", &identity) &&
         DSL_Shape_Node_Attribute(node, "attr.fp_environment", &environment) &&
         DSL_Shape_Node_Attribute(node, "attr.nan_policy", &nan) &&
         DSL_ordered_reduce_sum_binding_valid(order, identity, environment, nan));
    return node != NULL && binding &&
           node->attribute_count == (version == 1 ? 4 : 8) && operands.size() == 1 &&
           DSL_Shape_Node_Attribute(node, "attr.axis", &axis) &&
           DSL_Shape_Node_Attribute(node, "attr.keepdim", &keepdim) &&
           DSL_Shape_Node_Attribute(node, "attr.accum_dtype", &accum) &&
           DSL_Shape_Node_Attribute(node, "attr.reassociate", &reassociate) &&
           DSL_Shape_Reduce_Sum_Types_Valid
               (operands[0], result_ty, axis, keepdim, accum, reassociate);
}

/* Check only static descriptors and the exact source-recipe permissions.
 * No exp implementation, exceptional-value policy or FP merge is inferred.
 * Require equal positive static F4/float32 [R,N], axis=1, float32 accumulation,
 * reassociate/state_reformulation=false, exp_semantics=source_unresolved and
 * matching representation. Borrow live types/strings; parse scratch may allocate. */
BOOL
DSL_Shape_Stable_Softmax_Types_Valid
    (TY_IDX operand_ty, TY_IDX result_ty, const char *axis,
     const char *accum_dtype, const char *reassociate,
     const char *state_reformulation, const char *exp_semantics)
{
    UINT64 input[2], output[2];
    UINT32 input_rank = 0, output_rank = 0;
    return DSL_Shape_Tensor_Core_Complete(operand_ty) &&
           DSL_Shape_Tensor_Core_Complete(result_ty) &&
           axis != NULL && strcmp(axis, "1") == 0 &&
           accum_dtype != NULL && strcmp(accum_dtype, "float32") == 0 &&
           reassociate != NULL && strcmp(reassociate, "false") == 0 &&
           state_reformulation != NULL &&
           strcmp(state_reformulation, "false") == 0 &&
           exp_semantics != NULL &&
           strcmp(exp_semantics, "source_unresolved") == 0 &&
           TY_tensor_rank(operand_ty) == 2 && TY_tensor_rank(result_ty) == 2 &&
           strcmp(TY_tensor_attribute(operand_ty, TY_TENSOR_SCHEMA_DTYPE),
                  "float32") == 0 &&
           DSL_Shape_TY_Valid(TY_tensor_element_ty(operand_ty)) &&
           TY_mtype(TY_tensor_element_ty(operand_ty)) == MTYPE_F4 &&
           DSL_Shape_Tensor_Element_Representation_Compatible
               (operand_ty, result_ty) &&
           DSL_Shape_Parse_Static_Dimensions
               (TY_tensor_attribute(operand_ty, TY_TENSOR_SCHEMA_SHAPE),
                input, 2, &input_rank) && input_rank == 2 &&
           DSL_Shape_Parse_Static_Dimensions
               (TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE),
                output, 2, &output_rank) && output_rank == 2 &&
           input[0] > 0 && input[1] > 0 &&
           output[0] == input[0] && output[1] == input[1];
}

/* Read the five borrowed v1 Softmax attributes and one operand, then apply the
 * shared descriptor/permission validator. Exact count/version/missing fields
 * fail without image or TY changes; no numerical binding is manufactured. */
static BOOL
DSL_Shape_Stable_Softmax_Result
    (const DSL_IR_NODE_RECORD *node, const std::vector<TY_IDX> &operands,
     TY_IDX result_ty, UINT16 version)
{
    const char *axis = NULL, *accum = NULL, *reassociate = NULL;
    const char *reformulation = NULL, *exp = NULL;
    return node != NULL && node->attribute_count == 5 && version == 1 &&
           operands.size() == 1 &&
           DSL_Shape_Node_Attribute(node, "attr.axis", &axis) &&
           DSL_Shape_Node_Attribute(node, "attr.accum_dtype", &accum) &&
           DSL_Shape_Node_Attribute(node, "attr.reassociate", &reassociate) &&
           DSL_Shape_Node_Attribute(node, "attr.state_reformulation", &reformulation) &&
           DSL_Shape_Node_Attribute(node, "attr.exp_semantics", &exp) &&
           DSL_Shape_Stable_Softmax_Types_Valid
               (operands[0], result_ty, axis, accum, reassociate, reformulation, exp);
}

/* Decode canonical IEEE bits without host strtod, endianness or rounding.
 * Positive subnormals are representable; target preservation is not inferred.
 * Require non-NULL output and exactly eight lowercase hex digits with bits
 * strictly between zero and 0x7f800000. Publish only on success; no allocation. */
BOOL
DSL_positive_f32_bits_valid(const char *text, UINT32 *bits)
{
    if (text == NULL || strlen(text) != 8 || bits == NULL) return FALSE;
    UINT32 value = 0;
    for (UINT32 i = 0; i < 8; ++i) {
        char c = text[i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))) return FALSE;
        value = (value << 4) | (c <= '9' ? c - '0' : c - 'a' + 10);
    }
    if (value == 0 || value >= 0x7f800000U) return FALSE;
    *bits = value;
    return TRUE;
}

/* Share only descriptor relations and centered permissions across versions;
 * numerical binding is checked by the owning version-specific predicate.
 * Require axis=1, float32 accumulation, population_centered variance, canonical
 * positive finite epsilon, and reassociate/state_reformulation/contract=false.
 * Borrow strings only; no mutation or allocation, NULL/mismatch fails. */
static BOOL
DSL_normalization_recipe_valid
    (const char *axis, const char *accum_dtype, const char *variance_kind,
     const char *epsilon_bits, const char *reassociate,
     const char *state_reformulation, const char *contract)
{
    UINT32 bits;
    return axis != NULL && strcmp(axis, "1") == 0 &&
        accum_dtype != NULL && strcmp(accum_dtype, "float32") == 0 &&
        variance_kind != NULL && strcmp(variance_kind, "population_centered") == 0 &&
        DSL_positive_f32_bits_valid(epsilon_bits, &bits) &&
        reassociate != NULL && strcmp(reassociate, "false") == 0 &&
        state_reformulation != NULL && strcmp(state_reformulation, "false") == 0 &&
        contract != NULL && strcmp(contract, "false") == 0;
}

/* Check the shared logical descriptor relations without interpreting rsqrt
 * policy. Require F4/float32 [R,N],[N],[N]->[R,N], positive static extents and
 * matching element/representation keys. Publish optional rows/width only on
 * full success; failure preserves them. Parsing allocates temporary vectors,
 * not TYs or image records; active type/string context remains caller-owned. */
static BOOL
DSL_normalization_descriptors_valid
    (TY_IDX input_ty, TY_IDX gamma_ty, TY_IDX beta_ty, TY_IDX result_ty,
     UINT64 *rows, UINT64 *width)
{
    TY_IDX types[4] = { input_ty, gamma_ty, beta_ty, result_ty };
    UINT32 ranks[4] = { 2, 1, 1, 2 };
    UINT64 dimensions[4][2];
    for (UINT32 i = 0; i < 4; ++i) {
        UINT32 rank = 0;
        if (!DSL_Shape_Tensor_Core_Complete(types[i]) ||
            TY_tensor_rank(types[i]) != ranks[i] ||
            strcmp(TY_tensor_attribute(types[i], TY_TENSOR_SCHEMA_DTYPE), "float32") != 0 ||
            !DSL_Shape_TY_Valid(TY_tensor_element_ty(types[i])) ||
            TY_mtype(TY_tensor_element_ty(types[i])) != MTYPE_F4 ||
            !DSL_Shape_Tensor_Element_Representation_Compatible(input_ty, types[i]) ||
            !DSL_Shape_Parse_Static_Dimensions
                (TY_tensor_attribute(types[i], TY_TENSOR_SCHEMA_SHAPE),
                 dimensions[i], 2, &rank) || rank != ranks[i])
            return FALSE;
    }
    if (!(dimensions[0][0] > 0 && dimensions[0][1] > 0 &&
           dimensions[1][0] == dimensions[0][1] &&
           dimensions[2][0] == dimensions[0][1] &&
           dimensions[3][0] == dimensions[0][0] &&
           dimensions[3][1] == dimensions[0][1])) return FALSE;
    if (width != NULL) *width = dimensions[0][1];
    if (rows != NULL) *rows = dimensions[0][0];
    return TRUE;
}

/* Preserve v1's unresolved source recipe and positive-width domain exactly;
 * sharing descriptor code does not authorize an ordered mean or rsqrt.
 * Require rsqrt_semantics=source_unresolved and shared recipe/descriptor checks;
 * borrow live TYs/strings, allocate only scratch, and mutate no tables. */
BOOL
DSL_mean_variance_norm_types_valid
    (TY_IDX input_ty, TY_IDX gamma_ty, TY_IDX beta_ty, TY_IDX result_ty,
     const char *axis, const char *accum_dtype, const char *variance_kind,
     const char *epsilon_bits, const char *reassociate,
     const char *state_reformulation, const char *contract,
     const char *rsqrt_semantics)
{
    return rsqrt_semantics != NULL && strcmp(rsqrt_semantics, "source_unresolved") == 0 &&
        DSL_normalization_recipe_valid(axis, accum_dtype, variance_kind,
            epsilon_bits, reassociate, state_reformulation, contract) &&
        DSL_normalization_descriptors_valid(input_ty, gamma_ty, beta_ty, result_ty, NULL, NULL);
}

/* Exact reference fields are source meaning, not a producer-name allowlist or
 * target capability. Contradictory reference IDs never override these checks.
 * Compare all eight borrowed strings to the exact LN-REF-1 spellings below;
 * NULL/mismatch fails with no allocation, retention or compiler-state mutation. */
BOOL
DSL_mean_variance_norm_ref_binding_valid
    (const char *rsqrt_semantics, const char *mean_order,
     const char *mean_identity_bits, const char *mean_finalize,
     const char *count_conversion, const char *fp_environment,
     const char *nan_policy, const char *reference_binding)
{
    const char *actual[8] = { rsqrt_semantics, mean_order, mean_identity_bits,
        mean_finalize, count_conversion, fp_environment, nan_policy, reference_binding };
    const char *expected[8] = { "sqrt_rne_then_div_rne_f32", "ascending_left_fold",
        "00000000", "sum_then_div_rne", "exact_f32_from_static_n",
        "rne_gradual_no_traps", "class_only", "ln_ref_1" };
    for (UINT32 i = 0; i < 8; ++i)
        if (actual[i] == NULL || strcmp(actual[i], expected[i]) != 0) return FALSE;
    return TRUE;
}

/* Convert the bounded integer exactly without host FP mode, parsing or
 * target assumptions. Require 1<=count<=2^24 and non-NULL output; invalid input
 * preserves output. Integer exponent/significand construction yields exact
 * positive binary32 count bits without allocation or compiler-state mutation. */
BOOL
DSL_mean_variance_norm_ref_count_bits(UINT64 count, UINT32 *bits)
{
    if (bits == NULL || count == 0 || count > 16777216ULL) return FALSE;
    UINT32 exponent = 0;
    for (UINT64 value = count; value > 1; value >>= 1) ++exponent;
    UINT32 significand = exponent <= 23 ? (UINT32)count << (23 - exponent) :
                                         (UINT32)count >> (exponent - 23);
    *bits = ((127U + exponent) << 23) | (significand & 0x7fffffU);
    return TRUE;
}

/* V2 shares logical descriptors, not v1's unresolved numerical policy.
 * Complete numerical-field validation remains a separate required preflight.
 * Require sqrt_rne_then_div_rne_f32, shared recipe/descriptors, exact binary32
 * width in 1..2^24 and R*N*4 fitting UINT64. Borrow inputs and allocate local
 * parse scratch only; failure changes no caller/compiler state. */
BOOL
DSL_mean_variance_norm_ref_types_valid
    (TY_IDX input_ty, TY_IDX gamma_ty, TY_IDX beta_ty, TY_IDX result_ty,
     const char *axis, const char *accum_dtype, const char *variance_kind,
     const char *epsilon_bits, const char *reassociate,
     const char *state_reformulation, const char *contract,
     const char *rsqrt_semantics)
{
    UINT64 rows, width;
    UINT32 bits;
    return rsqrt_semantics != NULL &&
        strcmp(rsqrt_semantics, "sqrt_rne_then_div_rne_f32") == 0 &&
        DSL_normalization_recipe_valid(axis, accum_dtype, variance_kind,
            epsilon_bits, reassociate, state_reformulation, contract) &&
        DSL_normalization_descriptors_valid(input_ty, gamma_ty, beta_ty, result_ty, &rows, &width) &&
        DSL_mean_variance_norm_ref_count_bits(width, &bits) &&
        rows <= (~(UINT64)0 / 4) / width;
}

/* Read exactly eight v1 or fifteen v2 attributes with three borrowed operands.
 * V2 requires the entire LN-REF-1 binding before its bounded descriptor check;
 * v1 retains unresolved rsqrt semantics. Reject counts, missing fields and
 * unsupported versions without table mutation; parse scratch may allocate. */
static BOOL
DSL_mean_variance_norm_result
    (const DSL_IR_NODE_RECORD *node, const std::vector<TY_IDX> &operands,
     TY_IDX result_ty, UINT16 version)
{
    const char *a[15];
    const char *names[15] = { "attr.axis", "attr.accum_dtype", "attr.variance_kind",
        "attr.epsilon_bits", "attr.reassociate", "attr.state_reformulation",
        "attr.contract", "attr.rsqrt_semantics", "attr.mean_order", "attr.mean_identity_bits",
        "attr.mean_finalize", "attr.count_conversion", "attr.fp_environment",
        "attr.nan_policy", "attr.reference_binding" };
    UINT32 count = version == 1 ? 8 : 15;
    if (node == NULL || node->attribute_count != count || operands.size() != 3 ||
        (version != 1 && version != 2))
        return FALSE;
    for (UINT32 i = 0; i < count; ++i)
        if (!DSL_Shape_Node_Attribute(node, names[i], &a[i])) return FALSE;
    if (version == 2)
        return DSL_mean_variance_norm_ref_binding_valid
            (a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]) &&
            DSL_mean_variance_norm_ref_types_valid
            (operands[0], operands[1], operands[2], result_ty,
             a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    return DSL_mean_variance_norm_types_valid
        (operands[0], operands[1], operands[2], result_ty,
         a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
}

static const DSL_SHAPE_RULE_ENTRY DSL_shape_rules[] = {
    { OPR_DSLMEANVARNORM, 1, DSL_mean_variance_norm_result },
    { OPR_DSLMEANVARNORM, 2, DSL_mean_variance_norm_result },
    { OPR_DSLSTABLESOFTMAX, 1, DSL_Shape_Stable_Softmax_Result },
    { OPR_DSLREDUCESUM, 1, DSL_Shape_Reduce_Sum_Result },
    { OPR_DSLREDUCESUM, 2, DSL_Shape_Reduce_Sum_Result },
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

/* Borrow the immutable exact operator/version entry for process lifetime, or
 * NULL when absent. No version fallback, registration or mutable state. */
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

/* Prove one bounded canonical dimension equal by kind/text, excluding pending
 * and anonymous dynamic dimensions. Caller supplies nonnegative bounded ranks;
 * ordinal bounds are checked, numeric caches are not revalidated. Read-only. */
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
    return left.dimension_kind[left_ordinal] ==
               right.dimension_kind[right_ordinal] &&
           strcmp(left.dimension_text[left_ordinal],
                  right.dimension_text[right_ordinal]) == 0;
}

/* Require COMPLETE equal-rank borrowed facts and proof for every dimension.
 * Anonymous '?' never proves equality; rank-zero facts compare vacuously.
 * Caller guarantees bounded ranks/canonical text; no mutation/allocation. */
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

/* Validate v2 symbolic single-token attention from three complete bounded facts:
 * equal K/V and Q/result, proved shared batch, static equal head counts/width,
 * Q sequence=1 and exact decode/cache/BHSD/scale/softmax tokens. Borrow live
 * types/image and canonical facts; no runtime cache or target policy proof.
 * Caller has checked basic input context; failure changes nothing. */
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

/* Decode transpose flags into non-NULL caller scratch, defaulting both FALSE
 * even on failure. V1 needs no attributes; v2 needs explicit true/false flags,
 * batch_rule=exact and accum_dtype=float32. A later rejection may leave the
 * first flag set; borrow node/image strings and change no compiler state. */
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

/* Prove batched matmul dimensions using canonical symbolic equality and exact
 * transpose flags; v1 remains rank-2, v2 uses equal batch axes. Require matching
 * operand/result representations. Borrow checked input/facts; local flag writes
 * only, no symbol solving, runtime guards or compiler-state mutation. */
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

/* Check complete canonical facts for none/exact or numpy right-aligned
 * broadcast, permitting only proved equal dimensions or static singleton
 * expansion. Require operand/result representation compatibility. Borrow
 * bounded inputs; unknown/anonymous equality fails, with no state changes. */
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

/* Query immutable exact-version rule membership without consulting active
 * nodes/TYs, registering anything, or implying legality for a particular node. */
BOOL
DSL_Shape_Has_Operator_Rule
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    return DSL_Shape_Find_Rule(dsl_operator, version) != NULL;
}

/* Validate one caller-selected node in its live image/type/string context.
 * Resolve opcode/rule before arity/type facts: absent registration returns
 * UNREGISTERED; malformed input or incomplete facts return INVALID. Nonstatic
 * attention/matmul/broadcast use proof predicates; other multioperand anonymous
 * dimensions fail. Borrow inputs, allocate local vectors, never mutate tables,
 * activate a PU, harvest graph facts or select a target/optimization. */
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
    std::vector<DSL_SHAPE_FACT> operand_facts(input->operand_count);
    DSL_SHAPE_FACT result_fact;
    BOOL non_static = FALSE;
    BOOL anonymous_dynamic = FALSE;
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        if (!DSL_Shape_Fact_From_Type
                 (input->operand_types[i], &operand_facts[i]) ||
            operand_facts[i].state != DSL_SHAPE_FACT_COMPLETE)
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
        result_fact.state != DSL_SHAPE_FACT_COMPLETE)
        return DSL_SHAPE_CHECK_INVALID;
    for (INT32 i = 0; i < result_fact.rank; ++i) {
        non_static = non_static ||
            result_fact.dimension_kind[i] != DSL_SHAPE_DIMENSION_STATIC;
        anonymous_dynamic = anonymous_dynamic ||
            result_fact.dimension_kind[i] ==
                DSL_SHAPE_DIMENSION_ANONYMOUS_DYNAMIC;
    }
    /* Nonstatic spelling equality is not enough: domain-specific proof checks
     * preserve symbolic identity and refuse anonymous equality assumptions. */
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

/* Reset non-NULL caller scratch, including numeric/text caches, to unknown-rank
 * PENDING evidence. No allocation, table access or ownership transfer. */
static void
DSL_Shape_Fact_Init (DSL_SHAPE_FACT *fact)
{
    memset(fact, 0, sizeof(*fact));
    fact->state = DSL_SHAPE_FACT_PENDING;
    fact->rank = -1;
}



/* Accept a nonempty alpha/underscore-leading alnum/underscore identifier under
 * the active C character classification. Borrow text; no allocation/mutation
 * or compiler-symbol lookup is performed. */
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

/* Canonicalize a borrowed identifier to name@puXXXXXXXX (lowercase hex).
 * Bare names require nonzero owner index; an explicit nonzero eight-hex PU ID
 * is accepted independently of that owner, without lookup or PU activation.
 * Non-NULL canonical output is assigned only on success; strings may allocate. */
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
    char suffix[16];
    snprintf(suffix, sizeof(suffix), "@pu%08x", identity);
    *canonical = name + suffix;
    return TRUE;
}

/* Parse one logical token: <pending>, ?, positive UINT64 extent, scoped symbol,
 * or symbol +/- UINT64 offset; zero offsets canonicalize back to symbols.
 * Borrow input/owner label and require non-NULL output pointers. Failure leaves
 * outputs unchanged; success writes kind/text and writes static_value only for
 * a static token. String allocation can throw; no compiler tables are changed. */
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

/* Copy kind/cache/canonical text into one caller-bounded ordinal of non-NULL
 * scratch. Text must fit the fixed slot; failure preserves the entire fact.
 * Numeric cache validity is the caller's role, no compiler-state mutation. */
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

/* Reset non-NULL scratch before parsing rank -1/<pending> or rank 0..16 shapes.
 * Fill canonical dimensions using the owner only to qualify bare symbols;
 * complete means no pending kind, while '?' remains anonymous. Whole-shape
 * <pending> keeps PENDING state even with known rank. Whitespace and trailing
 * comma are accepted; failure leaves reset/partial scratch, not a rollback.
 * Temporary strings may allocate; no TY writes or symbol-table lookup. */
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

/* Format caller-owned bounded scratch using its terminated dimension text,
 * independent of state/caches; unknown rank becomes <pending>. No allocation
 * or table access. Invalid arguments fail before writes, but missing text,
 * formatting errors or insufficient capacity can leave partial/truncated text. */
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

/* Canonicalize borrowed shape text through a local fact, then format into caller
 * storage. Owner qualifies bare symbols without activating/validating a PU.
 * Parsing may allocate; parse failure preserves buffer, format failure can
 * partially overwrite it. No compiler identity or descriptor is constructed. */
BOOL
DSL_Shape_Normalize_Logical_Shape
        (ST_IDX owner_pu_st,
         const char *shape,
         INT32 expected_rank,
         char *buffer,
         size_t buffer_size)
{
    DSL_SHAPE_FACT fact;
    return DSL_Shape_Parse_Fact
               (owner_pu_st, shape, expected_rank, &fact) &&
           DSL_Shape_Format_Fact(&fact, buffer, buffer_size);
}

/* Extract borrowed live TY rank/shape only if core fields exist and parsed rank
 * matches TY rank. Parse with owner=zero, requiring explicit symbol qualifiers.
 * Early failures preserve output; Parse_Fact resets then may partially write it.
 * Success may be PENDING. No TY mutation; temporary parsing may allocate. */
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





/* Inspect the caller's state tag only; COMPLETE does not imply static extents,
 * nonanonymous dimensions or independently validated text/cache invariants. */
static BOOL
DSL_Shape_Fact_Complete (const DSL_SHAPE_FACT &fact)
{
    return fact.state == DSL_SHAPE_FACT_COMPLETE;
}

/* Copy the first borrowed operand fact into distinct result scratch, then merge
 * equal-shape evidence. No operands fails before writing; a later conflict
 * marks contradiction and retains prior field updates. No TY/IR mutation. */
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

/* Infer two-operand none/numpy broadcast after representation checks. Incomplete
 * facts return TRUE leaving the caller-initialized pending result; complete
 * facts fill right-aligned axes by proved equality or singleton expansion.
 * Rejection may leave partial result fields; no input/TY/IR mutation, solving
 * or runtime guard insertion. Caller supplies distinct bounded scratch. */
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

/* Materialize <=16 positive extents into distinct caller fact as COMPLETE.
 * Excess rank preserves output; otherwise reset first, so a zero extent leaves
 * partial PENDING scratch. No allocation or compiler-record mutation. */
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

/* Copy static extents from borrowed bounded COMPLETE evidence to caller vector.
 * NULL/incomplete preserves vector; otherwise clear and append, leaving a
 * prefix on the first nonstatic dimension. Growth may allocate; facts unchanged. */
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

/* Infer v1 rank-2 or v2 equal-batch matmul from borrowed canonical facts and
 * exact transpose tokens. Validate type compatibility/rank before deferring
 * incomplete facts; prove batch/K equality before copying M/N and batch axes.
 * Rejection precedes result writes; incomplete success leaves initialized
 * pending scratch. Require nonaliasing bounded result; no graph/IR mutation. */
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

/* Infer linear v2 with bias or v3 without, using exact OI/transpose/has_bias
 * tokens and static activation/weight extents. Nonstatic activation/weight
 * defers with TRUE; v2 nonstatic/invalid bias rejects once those are known.
 * Replace trailing K with N in local scratch, then Set_Complete; final failure
 * may leave partial result. This is shape inference, not full type admission. */
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

/* Infer the positive static target_shape, checking element counts/overflow only
 * when source facts are entirely static. A nonstatic source does not block
 * publishing target extents; full legality remains the checker's job. Borrow
 * input, allocate private vectors, and allow Set_Complete partial output on
 * rejection; no source descriptor or compiler tables are changed. */
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

/* Parse a zero-based permutation; unknown source rank defers without writes.
 * For known bounded rank, reset distinct result then copy permuted evidence
 * and classify it. Duplicate/out-of-range entries can fail after partial writes.
 * Private permutation/seen vectors may allocate; borrowed input is unchanged. */
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

/* Normalize signed axes against a known bounded source rank and copy outer
 * dimensions into distinct scratch. Multiply static inner extents with overflow
 * checks; any nonstatic inner extent yields one pending dimension. Failure may
 * leave partial result; unknown rank rejects rather than defers. Borrow input
 * and require consistent nonzero static caches; no TY/IR mutation. */
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

/* Infer NCHW/OIHW convolution extents from three static operand facts and exact
 * layout/group/kernel tokens. Nonstatic input defers without writing; full
 * group divisibility/type checks remain in the checker. UINT64 spatial
 * arithmetic here has no additional overflow guards, so callers must establish
 * representable intermediates. Local vectors allocate; Set_Complete can leave
 * partial scratch on failure, with no compiler-record mutation. */
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

/* Infer rank-4 static pool extents; nonstatic input defers. Global mode copies
 * positive output_size directly (the checker separately restricts it to 1,1
 * and spatial reduction); max mode uses exact ceil token and window pairs.
 * Callers establish representable UINT64 spatial intermediates here. Allocate
 * local vectors, publish via Set_Complete with possible partial failure output;
 * no descriptor writes, framework edge adjustments or policy choices. */
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

/* Infer one registered structural rule from borrowed bounded canonical facts
 * into distinct caller result. Reset output before inspecting input: malformed
 * preflight can return CONTRADICTION with state still PENDING; absent rules
 * leave initialized UNREGISTERED output. Rule failure marks contradiction but
 * retains partial fields; success reclassifies complete/pending evidence.
 * Shape-preserving cases may only copy kid0; full type/attribute/domain legality
 * is a separate Check_Operator/gatekeeper obligation. Allocate scratch only;
 * do not mutate TYs, infer FP permissions, traverse PUs or choose optimizations. */
DSL_SHAPE_INFERENCE_RESULT
DSL_Shape_Infer_Operator
        (const DSL_SHAPE_INFERENCE_INPUT *input,
         DSL_SHAPE_FACT *result)
{
    if (result == NULL)
        return DSL_SHAPE_INFERENCE_CONTRADICTION;
    DSL_Shape_Fact_Init(result);
    if (input == NULL || input->node == NULL || input->result_ty == TY_IDX_ZERO ||
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
    for (UINT32 i = 0; i < input->operand_count; ++i) {
        if (input->operand_facts[i].state == DSL_SHAPE_FACT_CONTRADICTION ||
            !DSL_Shape_Tensor_Core_Complete(input->operand_types[i]))
            return DSL_SHAPE_INFERENCE_CONTRADICTION;
    }

    /* These local propagation rules can derive a candidate shape before full
     * legality is known; the owning caller must still run structural admission. */
    BOOL valid = FALSE;
    switch (input->dsl_operator) {
    case OPR_DSLMEANVARNORM: {
        std::vector<TY_IDX> operands(input->operand_types,
                                    input->operand_types + input->operand_count);
        valid = DSL_mean_variance_norm_result
                    (input->node, operands, input->result_ty, input->version) &&
                DSL_Shape_Fact_From_Type(input->result_ty, result);
        break;
    }
    case OPR_DSLSTABLESOFTMAX: {
        std::vector<TY_IDX> operands(input->operand_types,
                                    input->operand_types + input->operand_count);
        valid = DSL_Shape_Stable_Softmax_Result
                    (input->node, operands, input->result_ty, input->version) &&
                DSL_Shape_Fact_From_Type(input->result_ty, result);
        break;
    }
    case OPR_DSLREDUCESUM: {
        std::vector<TY_IDX> operands(input->operand_types,
                                    input->operand_types + input->operand_count);
        valid = DSL_Shape_Reduce_Sum_Result
                    (input->node, operands, input->result_ty, input->version) &&
                DSL_Shape_Fact_From_Type(input->result_ty, result);
        break;
    }
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
