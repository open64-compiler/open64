/*
 * Runtime contract checks for the M0 tensor folding handoff.
 */

#include <stdio.h>
#include <string.h>

#include "dsl_tensor_fold.h"

static int
Check_Status_Names(void)
{
    if (strcmp(DSL_Tensor_Fold_Status_Name
                   (DSL_TENSOR_FOLD_REJECT_TARGET_CAPABILITY),
               "reject_target_capability") != 0 ||
        strcmp(DSL_Tensor_Fold_Status_Name
                   (DSL_TENSOR_FOLD_REJECT_PROFITABILITY),
               "reject_profitability") != 0 ||
        !DSL_Tensor_Fold_Status_Is_Rejection
             (DSL_TENSOR_FOLD_REJECT_WORK_BUDGET) ||
        DSL_Tensor_Fold_Status_Is_Rejection
             (DSL_TENSOR_FOLD_SUCCESS)) {
        fprintf(stderr, "tensor fold status vocabulary changed\n");
        return 1;
    }

    return 0;
}

static int
Check_Evaluator_Identity(void)
{
    DSL_TENSOR_FOLD_EVALUATOR_ID evaluator;

    memset(&evaluator, 0xff, sizeof(evaluator));
    if (!DSL_Tensor_Fold_Get_Evaluator(OPR_DSLADD, 1, &evaluator) ||
        evaluator.dsl_operator != OPR_DSLADD ||
        evaluator.version != 1 ||
        evaluator.kind != DSL_TENSOR_FOLD_EVAL_ORDINARY ||
        evaluator.max_results != 1 ||
        DSL_Tensor_Fold_Max_Results(evaluator.kind) != 1) {
        fprintf(stderr, "common.add tensor fold evaluator identity changed\n");
        return 1;
    }

    if (DSL_Tensor_Fold_Max_Results
            (DSL_TENSOR_FOLD_EVAL_PROJECTABLE_DIVREM) != 2) {
        fprintf(stderr, "projectable DIVREM tensor fold bound changed\n");
        return 1;
    }

    if (DSL_Tensor_Fold_Get_Evaluator(OPR_DSLADD, 2, &evaluator) ||
        DSL_Tensor_Fold_Get_Evaluator(OPR_DSLMATMUL, 1, &evaluator)) {
        fprintf(stderr, "unsupported tensor fold evaluator was accepted\n");
        return 1;
    }

    return 0;
}

static int
Check_Candidate_Contract(void)
{
    DSL_TENSOR_FOLD_POLICY policy;
    DSL_TENSOR_FOLD_CANDIDATE candidate;
    DSL_TENSOR_FOLD_OUTPUT output;
    DSL_TENSOR_FOLD_STATUS reason;
    TCON operands[2];
    TY_IDX operand_ty[2];
    TY_IDX result_ty[DSL_TENSOR_FOLD_MAX_RESULTS];

    DSL_Tensor_Fold_Default_Policy(&policy);
    if (!policy.preserve_compact_splats) {
        fprintf(stderr, "default tensor fold policy changed\n");
        return 1;
    }

    memset(operands, 0, sizeof(operands));
    memset(operand_ty, 0, sizeof(operand_ty));
    memset(result_ty, 0, sizeof(result_ty));
    memset(&candidate, 0, sizeof(candidate));

    candidate.dsl_operator = OPR_DSLADD;
    candidate.version = 1;
    candidate.result_count = 1;
    candidate.operand_count = 2;
    candidate.operands = operands;
    candidate.operand_ty = operand_ty;
    candidate.result_ty = result_ty;
    candidate.policy = &policy;

    if (!DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_SUCCESS) {
        fprintf(stderr, "valid tensor fold candidate was rejected\n");
        return 1;
    }

    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR ||
        output.status != DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR ||
        output.result_count != 0) {
        fprintf(stderr, "M0 non-evaluator result contract changed\n");
        return 1;
    }

    candidate.result_count = 2;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET) {
        fprintf(stderr, "ordinary tensor fold result bound changed\n");
        return 1;
    }

    candidate.result_count = 1;
    candidate.operands = NULL;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE) {
        fprintf(stderr, "malformed tensor fold candidate was accepted\n");
        return 1;
    }

    candidate.operands = operands;
    candidate.operand_ty = NULL;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE) {
        fprintf(stderr, "candidate without operand types was accepted\n");
        return 1;
    }

    candidate.operand_ty = operand_ty;
    candidate.dsl_operator = OPR_DSLMATMUL;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_EVALUATOR) {
        fprintf(stderr, "unsupported tensor fold operator was accepted\n");
        return 1;
    }

    candidate.dsl_operator = OPR_DSLADD;
    candidate.version = 2;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_UNSUPPORTED_OPERATOR) {
        fprintf(stderr, "unknown tensor fold operator version was accepted\n");
        return 1;
    }

    return 0;
}

int
main(void)
{
    if (DSL_TENSOR_FOLD_MAX_RESULTS != 2) {
        fprintf(stderr, "tensor fold result bound changed\n");
        return 1;
    }

    if (Check_Status_Names() ||
        Check_Evaluator_Identity() ||
        Check_Candidate_Contract())
        return 1;

    printf("DSL tensor fold M0 contract passed\n");
    return 0;
}
