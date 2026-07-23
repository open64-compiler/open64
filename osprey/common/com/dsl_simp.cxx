/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "defs.h"
#include "config.h"
#include "dsl_simp.h"
#include "mtypes.h"
#include "opcode.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "wn.h"
#include "wn_simp.h"

static const char *DSL_simp_status_name[] = {
    "not_applicable",
    "unchanged",
    "engine_rewrite",
    "reject_disabled",
    "reject_malformed",
    "reject_unsupported_operator",
    "reject_effectful_operator",
    "reject_descriptor",
    "reject_numeric_policy",
    "reject_projection",
    "invalid"
};

static const char *DSL_simp_replacement_name[] = {
    "none",
    "kid0",
    "kid1",
    "new_wn"
};

static void
DSL_Simp_Set_Status
        (DSL_SIMP_BINARY_RESULT *result,
         DSL_SIMP_STATUS status)
{
    result->status = status;
    result->rejection =
        status >= DSL_SIMP_REJECT_DISABLED ? status : DSL_SIMP_NOT_APPLICABLE;
}

static BOOL
DSL_Simp_Map_Binary_Operator
        (DSL_OPERATOR dsl_operator,
         TYPE_ID mtype,
         OPCODE *opcode)
{
    OPERATOR whirl_operator;

    switch (dsl_operator) {
    case OPR_DSLADD:
        whirl_operator = OPR_ADD;
        break;
    case OPR_DSLMUL:
        whirl_operator = OPR_MPY;
        break;
    default:
        return FALSE;
    }

    if (opcode != NULL)
        *opcode = OPCODE_make_op(whirl_operator, mtype, MTYPE_V);
    return TRUE;
}

static BOOL
DSL_Simp_Tensor_Descriptors_Equivalent (TY_IDX first, TY_IDX second)
{
    return TY_is_tensor_extension(first) &&
           TY_is_tensor_extension(second) &&
           TY_tensor_is_canonical(first) &&
           TY_tensor_is_canonical(second) &&
           TY_tensor_rank(first) == TY_tensor_rank(second) &&
           TY_tensor_attributes_are_equivalent(first, second) &&
           TY_are_equivalent(TY_tensor_element_ty(first),
                             TY_tensor_element_ty(second),
                             TY_EQUIV_IGNORE_NAMES) &&
           TY_are_equivalent(first, second, TY_EQUIV_IGNORE_NAMES);
}

const char *
DSL_Simp_Status_Name (DSL_SIMP_STATUS status)
{
    UINT32 ordinal = (UINT32)status;

    if (ordinal >= sizeof(DSL_simp_status_name) /
                   sizeof(DSL_simp_status_name[0]))
        return "unknown";
    return DSL_simp_status_name[ordinal];
}

const char *
DSL_Simp_Replacement_Name (DSL_SIMP_REPLACEMENT_KIND replacement)
{
    UINT32 ordinal = (UINT32)replacement;

    if (ordinal >= sizeof(DSL_simp_replacement_name) /
                   sizeof(DSL_simp_replacement_name[0]))
        return "unknown";
    return DSL_simp_replacement_name[ordinal];
}

DSL_SIMP_STATUS
DSL_Simp_Prepare_Binary
        (const DSL_SIMP_BINARY_CANDIDATE *candidate,
         WN *stack_view,
         DSL_SIMP_BINARY_RESULT *result)
{
    DSL_OPERATOR_INFO operator_info;
    TY_IDX element_ty;
    TYPE_ID mtype;
    OPCODE opcode;

    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (candidate == NULL || stack_view == NULL || result == NULL) {
        if (result != NULL)
            DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_MALFORMED);
        return DSL_SIMP_REJECT_MALFORMED;
    }

    result->dsl_operator = candidate->dsl_operator;
    result->version = candidate->version;
    result->projected_kid[0] = candidate->projected_kid[0];
    result->projected_kid[1] = candidate->projected_kid[1];

    if (!Enable_WN_Simp) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_DISABLED);
        return result->status;
    }
    if (!DSL_Operator_Get_Info_Version(candidate->dsl_operator,
                                       candidate->version, &operator_info) ||
        !DSL_Operator_Get_Algebraic_Info(candidate->dsl_operator,
                                         candidate->version,
                                         NULL) ||
        operator_info.nkids != 2) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_UNSUPPORTED_OPERATOR);
        return result->status;
    }
    if (operator_info.effect_model != DSL_EFFECT_MODEL_PURE) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_EFFECTFUL_OPERATOR);
        return result->status;
    }
    if (!DSL_Simp_Tensor_Descriptors_Equivalent
             (candidate->result_ty, candidate->operand_ty[0]) ||
        !DSL_Simp_Tensor_Descriptors_Equivalent
             (candidate->result_ty, candidate->operand_ty[1])) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_DESCRIPTOR);
        return result->status;
    }

    element_ty = TY_tensor_element_ty(candidate->result_ty);
    mtype = TY_mtype(element_ty);
    if (!MTYPE_is_integral(mtype)) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_NUMERIC_POLICY);
        return result->status;
    }
    if (candidate->projection_kind != DSL_SIMP_PROJECTION_TEST_SCALAR ||
        candidate->projected_kid[0] == NULL ||
        candidate->projected_kid[1] == NULL ||
        !OPCODE_is_expression(WN_opcode(candidate->projected_kid[0])) ||
        !OPCODE_is_expression(WN_opcode(candidate->projected_kid[1])) ||
        WN_rtype(candidate->projected_kid[0]) != mtype ||
        WN_rtype(candidate->projected_kid[1]) != mtype ||
        !DSL_Simp_Map_Binary_Operator(candidate->dsl_operator, mtype,
                                      &opcode)) {
        DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_PROJECTION);
        return result->status;
    }

    memset(stack_view, 0, sizeof(*stack_view));
    WN_set_opcode(stack_view, opcode);
    WN_set_map_id(stack_view, -1);
    WN_kid0(stack_view) = candidate->projected_kid[0];
    WN_kid1(stack_view) = candidate->projected_kid[1];
    result->projected_opcode = opcode;
    DSL_Simp_Set_Status(result, DSL_SIMP_NOT_APPLICABLE);
    return result->status;
}

DSL_SIMP_STATUS
DSL_Simp_Apply_Binary
        (WN *stack_view,
         DSL_SIMP_BINARY_RESULT *result)
{
    if (stack_view == NULL || result == NULL ||
        result->status != DSL_SIMP_NOT_APPLICABLE ||
        result->projected_opcode != WN_opcode(stack_view)) {
        if (result != NULL)
            DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_MALFORMED);
        return DSL_SIMP_REJECT_MALFORMED;
    }

    result->engine_result =
        WN_SimplifyExp2(WN_opcode(stack_view),
                        WN_kid0(stack_view), WN_kid1(stack_view));
    result->status = result->engine_result == NULL ?
        DSL_SIMP_UNCHANGED : DSL_SIMP_ENGINE_REWRITE;
    return result->status;
}

DSL_SIMP_STATUS
DSL_Simp_Postprocess_Binary (DSL_SIMP_BINARY_RESULT *result)
{
    if (result == NULL ||
        (result->status != DSL_SIMP_UNCHANGED &&
         result->status != DSL_SIMP_ENGINE_REWRITE)) {
        if (result != NULL)
            DSL_Simp_Set_Status(result, DSL_SIMP_REJECT_MALFORMED);
        return DSL_SIMP_REJECT_MALFORMED;
    }

    if (result->status == DSL_SIMP_UNCHANGED)
        result->replacement = DSL_SIMP_REPLACEMENT_NONE;
    else if (result->engine_result == result->projected_kid[0])
        result->replacement = DSL_SIMP_REPLACEMENT_KID0;
    else if (result->engine_result == result->projected_kid[1])
        result->replacement = DSL_SIMP_REPLACEMENT_KID1;
    else
        result->replacement = DSL_SIMP_REPLACEMENT_NEW_WN;
    return result->status;
}

DSL_SIMP_STATUS
DSL_Simp_Binary
        (const DSL_SIMP_BINARY_CANDIDATE *candidate,
         DSL_SIMP_BINARY_RESULT *result)
{
    WN stack_view;
    DSL_SIMP_STATUS status =
        DSL_Simp_Prepare_Binary(candidate, &stack_view, result);

    if (status != DSL_SIMP_NOT_APPLICABLE)
        return status;
    status = DSL_Simp_Apply_Binary(&stack_view, result);
    if (status != DSL_SIMP_UNCHANGED &&
        status != DSL_SIMP_ENGINE_REWRITE)
        return status;
    return DSL_Simp_Postprocess_Binary(result);
}

void
DSL_Simp_Trace_Binary
        (FILE *file,
         const DSL_SIMP_BINARY_RESULT *result)
{
    DSL_OPERATOR_INFO operator_info;
    const char *stable_name = "unknown";

    if (file == NULL || result == NULL)
        return;
    if (DSL_Operator_Get_Info_Version(result->dsl_operator, result->version,
                                      &operator_info))
        stable_name = operator_info.stable_name;

    fprintf(file,
            "DSL simplifier: operator %s (%s.v%u), status %s, "
            "replacement %s\n",
            DSL_OPERATOR_name(result->dsl_operator),
            stable_name,
            (unsigned int)result->version,
            DSL_Simp_Status_Name(result->status),
            DSL_Simp_Replacement_Name(result->replacement));
}
