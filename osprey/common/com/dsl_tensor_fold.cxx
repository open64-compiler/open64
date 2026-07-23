/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_tensor_fold.h"

static const char *DSL_tensor_fold_status_name[] = {
    "not_applicable",
    "success",
    "reject_unsupported_evaluator",
    "reject_unsupported_operator",
    "reject_malformed_candidate",
    "reject_effectful_operator",
    "reject_non_constant_operand",
    "reject_descriptor_mismatch",
    "reject_unresolved_shape",
    "reject_numeric_policy",
    "reject_result_budget",
    "reject_work_budget",
    "reject_materialization_policy",
    "reject_division_by_zero",
    "reject_target_capability",
    "reject_profitability",
    "invalid"
};

void
DSL_Tensor_Fold_Default_Policy (DSL_TENSOR_FOLD_POLICY *policy)
{
    if (policy == NULL)
        return;

    memset(policy, 0, sizeof(*policy));
    policy->preserve_compact_splats = TRUE;
}

BOOL
DSL_Tensor_Fold_Get_Evaluator
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_TENSOR_FOLD_EVALUATOR_ID *evaluator)
{
    if (evaluator != NULL)
        memset(evaluator, 0, sizeof(*evaluator));

    if (version != 1)
        return FALSE;

    switch (dsl_operator) {
    case OPR_DSLADD:
    case OPR_DSLMUL:
        if (evaluator != NULL) {
            evaluator->dsl_operator = dsl_operator;
            evaluator->version = version;
            evaluator->kind = DSL_TENSOR_FOLD_EVAL_ORDINARY;
            evaluator->max_results = 1;
        }
        return TRUE;
    default:
        return FALSE;
    }
}

UINT16
DSL_Tensor_Fold_Max_Results (DSL_TENSOR_FOLD_EVALUATOR_KIND kind)
{
    switch (kind) {
    case DSL_TENSOR_FOLD_EVAL_ORDINARY:
        return 1;
    case DSL_TENSOR_FOLD_EVAL_PROJECTABLE_DIVREM:
        return DSL_TENSOR_FOLD_MAX_RESULTS;
    default:
        return 0;
    }
}

const char *
DSL_Tensor_Fold_Status_Name (DSL_TENSOR_FOLD_STATUS status)
{
    UINT32 ordinal = (UINT32)status;

    if (ordinal >= sizeof(DSL_tensor_fold_status_name) /
                   sizeof(DSL_tensor_fold_status_name[0]))
        return "unknown";
    return DSL_tensor_fold_status_name[ordinal];
}

BOOL
DSL_Tensor_Fold_Status_Is_Rejection (DSL_TENSOR_FOLD_STATUS status)
{
    return status >= DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR &&
           status <= DSL_TENSOR_FOLD_REJECT_PROFITABILITY;
}

BOOL
DSL_Tensor_Fold_Candidate_Valid
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_STATUS *reason)
{
    DSL_TENSOR_FOLD_EVALUATOR_ID evaluator;
    DSL_OPERATOR_INFO operator_info;

    if (reason != NULL)
        *reason = DSL_TENSOR_FOLD_SUCCESS;

    if (candidate == NULL ||
        candidate->result_count == 0 ||
        candidate->result_count > DSL_TENSOR_FOLD_MAX_RESULTS ||
        candidate->result_ty == NULL ||
        (candidate->operand_count > 0 &&
         (candidate->operands == NULL || candidate->operand_ty == NULL)) ||
        (candidate->attribute_count > 0 &&
         candidate->attributes == NULL)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE;
        return FALSE;
    }

    if (!DSL_Operator_Get_Info_Version(candidate->dsl_operator,
                                       candidate->version, &operator_info)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_OPERATOR;
        return FALSE;
    }

    if (!DSL_Tensor_Fold_Get_Evaluator(candidate->dsl_operator,
                                       candidate->version, &evaluator)) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
        return FALSE;
    }

    if (candidate->result_count > evaluator.max_results) {
        if (reason != NULL)
            *reason = DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET;
        return FALSE;
    }

    return TRUE;
}

DSL_TENSOR_FOLD_STATUS
Targ_DSL_WhirlOp
        (const DSL_TENSOR_FOLD_CANDIDATE *candidate,
         DSL_TENSOR_FOLD_OUTPUT *output)
{
    DSL_TENSOR_FOLD_STATUS reason;

    if (output != NULL)
        memset(output, 0, sizeof(*output));

    if (!DSL_Tensor_Fold_Candidate_Valid(candidate, &reason)) {
        if (output != NULL) {
            output->status = reason;
            output->rejection = reason;
        }
        return reason;
    }

    if (output != NULL) {
        output->status = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
        output->rejection = DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
    }
    return DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR;
}
