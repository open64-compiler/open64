/*
 * Runtime contract checks for the M0 tensor folding handoff.
 */

#include <stdio.h>
#include <string.h>

#include "dsl_tensor_fold.h"
#include "strtab.h"

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

    DSL_Tensor_Fold_Reset_Mock_Evaluator();
    if (DSL_Tensor_Fold_Mock_Evaluator_Enabled()) {
        fprintf(stderr, "mock evaluator did not reset\n");
        return 1;
    }

    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_NOT_APPLICABLE ||
        output.status != DSL_TENSOR_FOLD_NOT_APPLICABLE ||
        output.result_count != 0) {
        fprintf(stderr, "production tensor fold fallback changed\n");
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

static int
Check_Mock_Evaluator(void)
{
    DSL_TENSOR_FOLD_POLICY policy;
    DSL_TENSOR_FOLD_CANDIDATE candidate;
    DSL_TENSOR_FOLD_MOCK_RESPONSE response;
    DSL_TENSOR_FOLD_OUTPUT output;
    TCON operands[2];
    TY_IDX operand_ty[2];
    TY_IDX result_ty[DSL_TENSOR_FOLD_MAX_RESULTS];

    DSL_Tensor_Fold_Default_Policy(&policy);
    memset(operands, 0, sizeof(operands));
    memset(operand_ty, 0, sizeof(operand_ty));
    memset(result_ty, 0, sizeof(result_ty));
    memset(&candidate, 0, sizeof(candidate));
    memset(&response, 0, sizeof(response));

    Set_TY_IDX_index(result_ty[0], 41);
    candidate.dsl_operator = OPR_DSLADD;
    candidate.version = 1;
    candidate.result_count = 1;
    candidate.operand_count = 2;
    candidate.operands = operands;
    candidate.operand_ty = operand_ty;
    candidate.result_ty = result_ty;
    candidate.policy = &policy;

    response.status = DSL_TENSOR_FOLD_SUCCESS;
    response.result_count = 1;
    response.flags = 7;
    if (!DSL_Tensor_Fold_Set_Mock_Response(&response) ||
        !DSL_Tensor_Fold_Mock_Evaluator_Enabled()) {
        fprintf(stderr, "mock tensor fold evaluator did not install\n");
        return 1;
    }

    memset(&output, 0xff, sizeof(output));
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        output.status != DSL_TENSOR_FOLD_SUCCESS ||
        output.result_count != 1 ||
        output.results[0].kind != DSL_TENSOR_FOLD_RESULT_TCON ||
        output.results[0].result_ty != result_ty[0] ||
        output.results[0].flags != 7) {
        fprintf(stderr, "mock tensor fold success contract changed\n");
        return 1;
    }

    response.status = DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY;
    response.result_count = 0;
    if (!DSL_Tensor_Fold_Set_Mock_Response(&response) ||
        Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY ||
        output.rejection !=
            DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY ||
        output.result_count != 0) {
        fprintf(stderr, "mock tensor fold rejection contract changed\n");
        return 1;
    }

    response.status = DSL_TENSOR_FOLD_NOT_APPLICABLE;
    response.result_count = 0;
    if (!DSL_Tensor_Fold_Set_Mock_Response(&response) ||
        Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_NOT_APPLICABLE ||
        output.status != DSL_TENSOR_FOLD_NOT_APPLICABLE ||
        output.result_count != 0) {
        fprintf(stderr, "mock tensor fold unchanged path changed\n");
        return 1;
    }

    policy.max_evaluator_work = 1;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_WORK_BUDGET ||
        output.rejection != DSL_TENSOR_FOLD_REJECT_WORK_BUDGET) {
        fprintf(stderr, "tensor fold work-budget rejection changed\n");
        return 1;
    }

    policy.max_evaluator_work = 0;
    response.status = DSL_TENSOR_FOLD_SUCCESS;
    response.result_count = 2;
    if (!DSL_Tensor_Fold_Set_Mock_Response(&response) ||
        Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET ||
        output.rejection != DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET) {
        fprintf(stderr, "mock tensor fold result-budget handling changed\n");
        return 1;
    }

    DSL_Tensor_Fold_Reset_Mock_Evaluator();
    if (DSL_Tensor_Fold_Mock_Evaluator_Enabled()) {
        fprintf(stderr, "mock tensor fold evaluator remained installed\n");
        return 1;
    }

    return 0;
}

static TY_IDX
Test_Tensor_Type(UINT32 index)
{
    TY_IDX ty = TY_IDX_ZERO;
    Set_TY_IDX_index(ty, index);
    return ty;
}

static int
Check_Tensor_TCON_Storage(void)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    DSL_TENSOR_TCON_RECORD record;
    DSL_TENSOR_TCON_ID zero_id;
    DSL_TENSOR_TCON_ID one_id;
    DSL_TENSOR_TCON_ID splat_id;
    DSL_TENSOR_TCON_ID inline_id;
    DSL_TENSOR_TCON_ID side_id;
    DSL_TENSOR_TCON_ID duplicate_id;
    DSL_TENSOR_TCON_ID carrier_id;
    DSL_TENSOR_TCON_ID collision_id;
    DSL_TENSOR_TCON_ID side_unproven_id;
    TCON carrier;
    TCON scalar;
    TCON_IDX element_tcon;
    UINT64 inline_hash;
    UINT64 side_hash;
    STR_IDX side_file;
    const unsigned char inline_bytes[16] = {
        'a', 0, 'b', 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 14, 15
    };
    const unsigned char collision_bytes[16] = {
        'a', 0, 'b', 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 14, 16
    };

    Initialize_Strtab(1024);
    DSL_Tensor_TCON_Reset();

    if (sizeof(DSL_TENSOR_TCON_RECORD) != DSL_TENSOR_TCON_RECORD_SIZE) {
        fprintf(stderr, "tensor TCON fixed-row size changed\n");
        return 1;
    }

    TCON_clear(scalar);
    Set_TCON_ty(scalar, MTYPE_I4);
    if (sizeof(scalar) != sizeof(TCON) ||
        DSL_Tensor_TCON_Is_Carrier(&scalar, NULL) ||
        TCON_ty(scalar) != MTYPE_I4) {
        fprintf(stderr, "scalar TCON compatibility changed\n");
        return 1;
    }

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(101);
    info.scalar_tcon = 11;
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.scalar_integer_value = 0;
    info.element_count = 8;
    info.logical_bytes = 32;
    info.required_alignment = 4;

    if (!DSL_Tensor_TCON_Create_Zero(&info, &zero_id, &carrier) ||
        zero_id == DSL_TENSOR_TCON_INVALID_ID ||
        !DSL_Tensor_TCON_Is_Carrier(&carrier, &carrier_id) ||
        carrier_id != zero_id ||
        !DSL_Tensor_TCON_Get(zero_id, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO ||
        record.descriptor_ty != info.descriptor_ty ||
        record.scalar_tcon != info.scalar_tcon ||
        record.element_mtype != info.element_mtype ||
        record.element_size != info.element_size ||
        record.element_count != info.element_count ||
        record.logical_bytes != info.logical_bytes ||
        record.required_alignment != info.required_alignment ||
        record.carrier_token == 0) {
        fprintf(stderr, "zero tensor TCON record contract changed\n");
        return 1;
    }

    if (!DSL_Tensor_TCON_Get_Element_TCON(zero_id, 7, &element_tcon) ||
        element_tcon != info.scalar_tcon ||
        DSL_Tensor_TCON_Get_Element_TCON(zero_id, 8, &element_tcon)) {
        fprintf(stderr, "compact tensor TCON element access changed\n");
        return 1;
    }

    if (!DSL_Tensor_TCON_Create_Zero(&info, &duplicate_id, NULL) ||
        duplicate_id != zero_id ||
        DSL_Tensor_TCON_Count() != 1) {
        fprintf(stderr, "zero tensor TCON deduplication changed\n");
        return 1;
    }

    info.scalar_tcon = 12;
    info.scalar_integer_value = 1;
    if (!DSL_Tensor_TCON_Create_One(&info, &one_id, NULL) ||
        !DSL_Tensor_TCON_Get(one_id, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ONE ||
        DSL_Tensor_TCON_Semantic_Hash(one_id) == 0) {
        fprintf(stderr, "one tensor TCON record contract changed\n");
        return 1;
    }

    info.scalar_tcon = 13;
    info.scalar_integer_value = 42;
    if (!DSL_Tensor_TCON_Create_Splat(&info, &splat_id, NULL) ||
        !DSL_Tensor_TCON_Get(splat_id, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT ||
        !DSL_Tensor_TCON_Get_Element_TCON(splat_id, 3,
                                          &element_tcon) ||
        element_tcon != 13) {
        fprintf(stderr, "splat tensor TCON record contract changed\n");
        return 1;
    }

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(201);
    info.element_mtype = MTYPE_U1;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 16;
    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.checksum_hi = 0x1234;
    info.checksum_lo = 0x5678;
    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &inline_id, NULL) ||
        !DSL_Tensor_TCON_Get(inline_id, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_INLINE_DENSE ||
        record.dense_payload_ref == 0 ||
        record.dense_payload_bytes != 16 ||
        memcmp(Index_to_char_array(record.dense_payload_ref),
               inline_bytes, 16) != 0 ||
        DSL_Tensor_TCON_Get_Element_TCON(inline_id, 0,
                                         &element_tcon)) {
        fprintf(stderr, "inline dense tensor TCON contract changed\n");
        return 1;
    }
    inline_hash = DSL_Tensor_TCON_Semantic_Hash(inline_id);

    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &duplicate_id, NULL) ||
        duplicate_id != inline_id) {
        fprintf(stderr, "inline dense tensor TCON deduplication changed\n");
        return 1;
    }

    info.dense_bytes = collision_bytes;
    info.dense_bytes_length = 16;
    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &collision_id, NULL) ||
        collision_id == inline_id ||
        DSL_Tensor_TCON_Semantic_Equal(inline_id, collision_id)) {
        fprintf(stderr, "dense tensor TCON checksum collision accepted\n");
        return 1;
    }

    side_file = Save_Str("weights/tensor.bin");
    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(201);
    info.element_mtype = MTYPE_U1;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 16;
    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.side_file = side_file;
    info.byte_offset = 64;
    info.byte_length = 16;
    info.checksum_hi = 0x1234;
    info.checksum_lo = 0x5678;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_id, NULL) ||
        side_id != inline_id ||
        !DSL_Tensor_TCON_Semantic_Equal(inline_id, side_id)) {
        fprintf(stderr, "dense tensor TCON semantic identity changed\n");
        return 1;
    }
    side_hash = DSL_Tensor_TCON_Semantic_Hash(side_id);
    if (side_hash != inline_hash) {
        fprintf(stderr, "dense tensor TCON semantic hash changed\n");
        return 1;
    }

    info.dense_bytes = NULL;
    info.dense_bytes_length = 0;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_unproven_id,
                                                NULL) ||
        side_unproven_id == inline_id ||
        DSL_Tensor_TCON_Semantic_Equal(inline_id, side_unproven_id)) {
        fprintf(stderr, "dense tensor TCON storage policy changed\n");
        return 1;
    }

    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.checksum_lo = 0x5679;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_id, NULL) ||
        side_id == inline_id ||
        !DSL_Tensor_TCON_Get(side_id, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
        record.side_file != side_file ||
        record.byte_offset != 64 ||
        record.byte_length != 16) {
        fprintf(stderr, "side-file dense tensor TCON contract changed\n");
        return 1;
    }

    info.required_alignment = 3;
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "invalid tensor TCON alignment accepted\n");
        return 1;
    }
    info.required_alignment = 16;
    info.side_file = Save_Str("../bad.bin");
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "invalid tensor TCON side path accepted\n");
        return 1;
    }
    info.side_file = side_file;
    info.byte_offset = 8;
    info.byte_length = 16;
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "misaligned tensor TCON side offset accepted\n");
        return 1;
    }
    info.byte_offset = 64;
    info.byte_length = 15;
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "invalid tensor TCON side range accepted\n");
        return 1;
    }

    TCON_clear(carrier);
    Set_TCON_string_payload(carrier, MTYPE_STR,
                            Save_StrN("__WHIRL_DSL_TCON__:v1:not-id",
                                      30),
                            30);
    Set_TCON_dsl_tensor_carrier(carrier);
    if (DSL_Tensor_TCON_Is_Carrier(&carrier, NULL)) {
        fprintf(stderr, "invalid tensor TCON carrier accepted\n");
        return 1;
    }

    DSL_Tensor_TCON_Reset();
    if (DSL_Tensor_TCON_Count() != 0 ||
        DSL_Tensor_TCON_Is_Carrier(&carrier, NULL)) {
        fprintf(stderr, "tensor TCON reset behavior changed\n");
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
        Check_Candidate_Contract() ||
        Check_Mock_Evaluator() ||
        Check_Tensor_TCON_Storage())
        return 1;

    printf("DSL tensor fold M2 storage contract passed\n");
    return 0;
}
