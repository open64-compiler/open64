/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com implements integral DSL preparation, traditional WN-engine
 * dispatch and pointer-identity result classification for caller-owned scalar
 * projections. No duplicate algebraic rules, tensor publication, PU selection
 * or mapped-image changes live here; caller context and WN ownership remain
 * governed by the existing simplifier protocol.
 * Design: doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md, traditional-engine reuse.
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

/* Map only logical ADD/MUL to their scalar ADD/MPY opcode at caller-supplied
 * mtype. NULL output probes support; unsupported operators return FALSE without
 * changing output. Registry/version/type legality is the preparer's job. */
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
