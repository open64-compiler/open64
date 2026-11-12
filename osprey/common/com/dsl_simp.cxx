/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com implements checked scalar and integral tensor preparation, WN-engine
 * dispatch and pointer-identity result classification for caller-owned scalar
 * projections. No duplicate algebraic rules, tensor publication, PU selection
 * or mapped-image changes live here; caller context and WN ownership remain
 * governed by the existing simplifier protocol.
 * Design: doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md, traditional-engine reuse.
 */

#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_opt.h"
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

/* Update only status and rejection in a nonnull caller-owned result. Rejection
 * statuses (including INVALID) are copied to rejection; other statuses clear it
 * to NOT_APPLICABLE. No pointer cleanup, allocation or WN mutation. */
static void
DSL_Simp_Set_Status
        (DSL_SIMP_BINARY_RESULT *result,
         DSL_SIMP_STATUS status)
{
    result->status = status;
    result->rejection =
        status >= DSL_SIMP_REJECT_DISABLED ? status : DSL_SIMP_NOT_APPLICABLE;
}

/* Preserve the legacy binary adapter's ADD/MUL admission while sharing the
 * scalar meaning with new element projections. Registry/version/type legality
 * remains the preparer's job. A miss leaves optional opcode output unchanged. */
static BOOL
DSL_Simp_Map_Binary_Operator
        (DSL_OPERATOR dsl_operator,
         TYPE_ID mtype,
         OPCODE *opcode)
{
    DSL_SCALAR_OPERATION operation;
    if ((dsl_operator != OPR_DSLADD && dsl_operator != OPR_DSLMUL) ||
        !DSL_scalar_operation(dsl_operator, 1, mtype, &operation))
        return FALSE;

    if (opcode != NULL)
        *opcode = operation.opcode;
    return TRUE;
}

/* Accept independently allocated scalar leaves only: no volatile memory,
 * effects, nested speculation or shared subtrees may be consumed by the
 * traditional engine. Local/formal direct loads must be nonvolatile; PREG
 * identity/type validity is caller-established. FP admission rejects PREGs
 * separately because BACK_END simplification can expose their constant homes. */
static BOOL
DSL_simp_scalar_leaf (WN *wn, TYPE_ID expected_mtype)
{
    if (wn == NULL || WN_rtype(wn) != expected_mtype)
        return FALSE;
    OPERATOR scalar_operator = WN_operator(wn);
    if (scalar_operator == OPR_INTCONST)
        return MTYPE_is_integral(expected_mtype);
    if (scalar_operator == OPR_CONST)
        return TRUE;
    if (scalar_operator != OPR_LDID)
        return FALSE;
    ST *symbol = WN_st(wn);
    if (ST_class(symbol) == CLASS_PREG)
        return TRUE;
    return ST_class(symbol) == CLASS_VAR &&
           (ST_sclass(symbol) == SCLASS_AUTO ||
            ST_sclass(symbol) == SCLASS_FORMAL) &&
           !TY_is_volatile(WN_ty(wn)) &&
           !TY_is_volatile(ST_type(symbol)) && !WN_has_side_effects(wn);
}

/* Guard the established FP engine without overriding any user control.
 * Nonconstant local arithmetic excludes its unconditional 0-x and constant-changing
 * rules; no source FP environment or transcendental binding is invented. */
static BOOL
DSL_simp_strict_fp_controls (void)
{
    return IEEE_Arithmetic == IEEE_ACCURATE &&
           Roundoff_Level == ROUNDOFF_NONE &&
           !Enable_Cfold_Reassociate && !Enable_Cfold_Aggressive &&
           !Div_Split_Allowed && Force_IEEE_Comparisons;
}

/* Prepare a disposable WN view, delegate all eligible scalar rewrites to the
 * existing engine, and retain its pool result only. Tensor indexing, shape,
 * source evaluation and final WN/ST/image publication remain caller-owned. */
DSL_SIMP_STATUS
DSL_simp_scalar (const DSL_SIMP_SCALAR_CANDIDATE *candidate,
                 DSL_SIMP_SCALAR_RESULT *result)
{
    if (result == NULL)
        return DSL_SIMP_REJECT_MALFORMED;
    memset(result, 0, sizeof(*result));
    result->status = DSL_SIMP_REJECT_MALFORMED;
    if (candidate == NULL)
        return result->status;
    if (!Enable_WN_Simp)
        return result->status = DSL_SIMP_REJECT_DISABLED;
    if (candidate->element_mtype != MTYPE_I4 &&
        candidate->element_mtype != MTYPE_I8 &&
        candidate->element_mtype != MTYPE_U4 &&
        candidate->element_mtype != MTYPE_U8 &&
        candidate->element_mtype != MTYPE_F4 &&
        candidate->element_mtype != MTYPE_F8)
        return result->status = DSL_SIMP_REJECT_PROJECTION;
    DSL_SCALAR_OPERATION operation;
    if (!DSL_scalar_operation(candidate->dsl_operator, candidate->version,
                               candidate->element_mtype, &operation))
        return result->status = DSL_SIMP_REJECT_UNSUPPORTED_OPERATOR;
    if (candidate->kid_count != operation.arity)
        return result->status;
    if (operation.requires_fp_binding)
        return result->status = DSL_SIMP_REJECT_NUMERIC_POLICY;
    if (MTYPE_is_float(candidate->element_mtype) &&
        candidate->dsl_operator == OPR_DSLSELECT &&
        (candidate->projected_kid[0] == NULL ||
         WN_operator(candidate->projected_kid[0]) != OPR_INTCONST))
        return result->status = DSL_SIMP_REJECT_NUMERIC_POLICY;

    for (UINT32 index = 0; index < candidate->kid_count; ++index) {
        TYPE_ID expected = candidate->dsl_operator == OPR_DSLSELECT && index == 0 ?
            MTYPE_I4 : candidate->element_mtype;
        WN *kid = candidate->projected_kid[index];
        if (!DSL_simp_scalar_leaf(kid, expected))
            return result->status = DSL_SIMP_REJECT_PROJECTION;
        if ((candidate->dsl_operator == OPR_DSLLOGICALNOT ||
             (candidate->dsl_operator == OPR_DSLSELECT && index == 0)) &&
            WN_operator(kid) == OPR_INTCONST &&
            WN_const_val(kid) != 0 && WN_const_val(kid) != 1)
            return result->status = DSL_SIMP_REJECT_PROJECTION;
        for (UINT32 previous = 0; previous < index; ++previous)
            if (kid == candidate->projected_kid[previous])
                return result->status = DSL_SIMP_REJECT_PROJECTION;
        if (MTYPE_is_float(candidate->element_mtype) &&
            candidate->dsl_operator != OPR_DSLSELECT &&
            (WN_operator(kid) != OPR_LDID ||
             ST_class(WN_st(kid)) != CLASS_VAR ||
             !DSL_simp_strict_fp_controls()))
            return result->status = DSL_SIMP_REJECT_NUMERIC_POLICY;
    }

    if ((candidate->dsl_operator == OPR_DSLDIV ||
         candidate->dsl_operator == OPR_DSLREM) &&
        MTYPE_is_integral(candidate->element_mtype))
        return result->status = DSL_SIMP_REJECT_NUMERIC_POLICY;

    WN stack_view;
    memset(&stack_view, 0, sizeof(stack_view));
    WN_set_opcode(&stack_view, operation.opcode);
    WN_set_map_id(&stack_view, -1);
    result->projected_opcode = operation.opcode;
    /* A plain stack WN has two kid slots; pass the ternary third separately
     * rather than writing beyond that unowned view's physical storage. */
    for (UINT32 index = 0; index < candidate->kid_count && index < 2; ++index)
        WN_kid(&stack_view, index) = candidate->projected_kid[index];

    switch (operation.arity) {
    case 1:
        result->engine_result = WN_SimplifyExp1
            (WN_opcode(&stack_view), WN_kid0(&stack_view));
        break;
    case 2:
        result->engine_result = WN_SimplifyExp2
            (WN_opcode(&stack_view), WN_kid0(&stack_view), WN_kid1(&stack_view));
        break;
    case 3:
        result->engine_result = WN_SimplifyExp3
            (WN_opcode(&stack_view), WN_kid0(&stack_view), WN_kid1(&stack_view),
             candidate->projected_kid[2]);
        break;
    }
    return result->status = result->engine_result == NULL ?
        DSL_SIMP_UNCHANGED : DSL_SIMP_ENGINE_REWRITE;
}

/* Require canonical tensor extensions with equal rank, tensor attributes,
 * element types and whole types under name-ignoring equivalence. Read caller's
 * initialized type/extension tables without sealing or rewriting descriptors;
 * this is not broadcast compatibility or projected operand-effect validation. */
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

/* Borrow a process-lifetime literal for a status ordinal. Unsigned bounds
 * checking maps negative or oversized enum values to "unknown"; no writes. */
const char *
DSL_Simp_Status_Name (DSL_SIMP_STATUS status)
{
    UINT32 ordinal = (UINT32)status;

    if (ordinal >= sizeof(DSL_simp_status_name) /
                   sizeof(DSL_simp_status_name[0]))
        return "unknown";
    return DSL_simp_status_name[ordinal];
}

/* Borrow a process-lifetime replacement spelling, using "unknown" for every
 * out-of-range ordinal. Do not inspect or transfer any projected tree. */
const char *
DSL_Simp_Replacement_Name (DSL_SIMP_REPLACEMENT_KIND replacement)
{
    UINT32 ordinal = (UINT32)replacement;

    if (ordinal >= sizeof(DSL_simp_replacement_name) /
                   sizeof(DSL_simp_replacement_name[0]))
        return "unknown";
    return DSL_simp_replacement_name[ordinal];
}

/* Clear result, preserve candidate identity for tracing and reject unsupported
 * controls/semantics before initializing the temporary physical view. Require
 * valid table/pool context and disposable strict-tree scalar kids from caller;
 * rejection never consumes them or changes stack_view. Accepted NOT_APPLICABLE
 * means ready for Apply, not that the engine has examined the expression. */
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

    /* The master switch and semantic gates precede all stack-view writes;
     * logical purity does not prove the caller's projected kids are effect-free. */
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

    /* This is an unowned stack view, never a new published WN. Apply borrows
     * its opcode/kids, and the caller discards the view without deleting it. */
    memset(stack_view, 0, sizeof(*stack_view));
    WN_set_opcode(stack_view, opcode);
    WN_set_map_id(stack_view, -1);
    WN_kid0(stack_view) = candidate->projected_kid[0];
    WN_kid1(stack_view) = candidate->projected_kid[1];
    result->projected_opcode = opcode;
    DSL_Simp_Set_Status(result, DSL_SIMP_NOT_APPLICABLE);
    return result->status;
}

/* Dispatch an unmodified prepared view/result once through WN_SimplifyExp2.
 * The engine owns normal simplification, controls, unused-tree deletion and
 * pool allocation. Record its NULL/non-NULL result; do not classify replacement
 * yet. Malformed stage/opcode changes only status/rejection, not other fields;
 * this is neither a full state revalidation nor an engine rollback boundary. */
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

    /* Reuse the existing rule engine rather than a DSL identity/folding table.
     * A rewrite may consume either input; saved kid addresses are identity only. */
    result->engine_result =
        WN_SimplifyExp2(WN_opcode(stack_view),
                        WN_kid0(stack_view), WN_kid1(stack_view));
    result->status = result->engine_result == NULL ?
        DSL_SIMP_UNCHANGED : DSL_SIMP_ENGINE_REWRITE;
    return result->status;
}

/* Classify the accepted Apply status by pointer identity only. No tree access,
 * symbol/type creation or logical/image publication occurs; caller must keep
 * the engine result valid. Bad stage/NULL input rejects without cleanup or
 * restoring previously consumed trees; other fields are not cleared. */
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

/* Sequence the three stages using one non-published stack-local WN. Propagate
 * the first rejection and the stages' output/ownership effects; only a retained
 * engine result can escape on rewrite, never the view. Caller still owns DSL
 * result construction and rejected/unchanged projection cleanup; no journal. */
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

/* Write one logical-operator diagnostic line for a borrowed result/stream,
 * using registry metadata when available and fallback names otherwise. NULL
 * arguments are silent; no state validation/mutation or I/O status is supplied. */
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
