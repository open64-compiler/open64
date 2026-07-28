/*
 * Runtime contract checks for the M0 tensor folding handoff.
 */

#include <stdio.h>
#include <string.h>

#include "dsl_tensor_fold.h"
#include "strtab.h"

extern TCON_IDX Enter_tcon (const TCON& tcon);
extern UINT32 TCON_Table_Size (void);

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
            DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND ||
        output.status != DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND ||
        output.result_count != 0) {
        fprintf(stderr, "non-constant tensor fold rejection changed\n");
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

    candidate.version = 1;
    candidate.flags = DSL_TENSOR_FOLD_CANDIDATE_EFFECTFUL;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_EFFECTFUL_OPERATOR) {
        fprintf(stderr, "effectful tensor fold candidate was accepted\n");
        return 1;
    }

    candidate.flags = DSL_TENSOR_FOLD_CANDIDATE_UNRESOLVED_SHAPE;
    if (DSL_Tensor_Fold_Candidate_Valid(&candidate, &reason) ||
        reason != DSL_TENSOR_FOLD_REJECT_UNRESOLVED_SHAPE) {
        fprintf(stderr, "unresolved tensor fold shape was accepted\n");
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

static TCON_IDX
Test_Scalar_TCON(TYPE_ID mtype)
{
    TCON scalar;

    TCON_clear(scalar);
    Set_TCON_ty(scalar, mtype);
    return Enter_tcon(scalar);
}

static int
Check_Tensor_TCON_Storage(void)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    DSL_TENSOR_TCON_RECORD record;
    DSL_TENSOR_TCON_RECORD decoded;
    TCON_IDX zero_idx;
    TCON_IDX one_idx;
    TCON_IDX splat_idx;
    TCON_IDX inline_idx;
    TCON_IDX side_idx;
    TCON_IDX duplicate_idx;
    TCON_IDX collision_idx;
    TCON_IDX side_unproven_idx;
    TCON_IDX side_distinct_idx;
    TCON_IDX zero_scalar;
    TCON_IDX one_scalar;
    TCON_IDX splat_scalar;
    TCON_IDX wrong_scalar;
    TCON carrier;
    TCON malformed_carrier;
    TCON side_carrier;
    TCON scalar;
    TCON_IDX element_tcon;
    UINT64 inline_hash;
    UINT64 side_hash;
    char *payload;
    const char *side_bytes;
    const unsigned char *dense_bytes;
    UINT32 side_length;
    UINT32 dense_length;
    const char side_path[] = "weights/tensor.bin";
    const unsigned char inline_bytes[16] = {
        'a', 0, 'b', 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 14, 15
    };
    const unsigned char collision_bytes[16] = {
        'a', 0, 'b', 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 14, 16
    };
    char malformed_payload[DSL_TENSOR_TCON_ENVELOPE_SIZE + 17];

    Initialize_Strtab(1024);
    DSL_Tensor_TCON_Reset();
    zero_scalar = Test_Scalar_TCON(MTYPE_I4);
    one_scalar = Test_Scalar_TCON(MTYPE_I4);
    splat_scalar = Test_Scalar_TCON(MTYPE_I4);
    wrong_scalar = Test_Scalar_TCON(MTYPE_F4);

    if (sizeof(DSL_TENSOR_TCON_RECORD) !=
        DSL_TENSOR_TCON_ENVELOPE_SIZE) {
        fprintf(stderr, "tensor TCON envelope size changed\n");
        return 1;
    }

    TCON_clear(scalar);
    Set_TCON_ty(scalar, MTYPE_I4);
    if (sizeof(scalar) != sizeof(TCON) ||
        DSL_Tensor_TCON_Decode_Carrier(&scalar, NULL) ||
        TCON_ty(scalar) != MTYPE_I4) {
        fprintf(stderr, "scalar TCON compatibility changed\n");
        return 1;
    }

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(101);
    info.scalar_tcon = zero_scalar;
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.scalar_integer_value = 0;
    info.element_count = 8;
    info.logical_bytes = 32;
    info.required_alignment = 4;

    if (!DSL_Tensor_TCON_Create_Zero(&info, &zero_idx, &carrier) ||
        zero_idx == TCON_IDX_ZERO ||
        !DSL_Tensor_TCON_Decode_Carrier(&carrier, &decoded) ||
        !DSL_Tensor_TCON_Get(zero_idx, &record) ||
        memcmp(&decoded, &record, sizeof(record)) != 0 ||
        record.magic != DSL_TENSOR_TCON_MAGIC ||
        record.version != DSL_TENSOR_TCON_VERSION ||
        record.header_size != DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        record.record_size != DSL_TENSOR_TCON_ENVELOPE_SIZE + 1 ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO ||
        record.descriptor_ty != info.descriptor_ty ||
        record.scalar_tcon != info.scalar_tcon ||
        record.element_mtype != info.element_mtype ||
        record.element_size != info.element_size ||
        record.element_count != info.element_count ||
        record.logical_bytes != info.logical_bytes ||
        record.required_alignment != info.required_alignment) {
        fprintf(stderr, "zero tensor TCON record contract changed\n");
        return 1;
    }

    DSL_Tensor_TCON_Reset();
    if (!DSL_Tensor_TCON_Get(zero_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO) {
        fprintf(stderr, "tensor TCON reopen lookup requires runtime state\n");
        return 1;
    }

    if (!DSL_Tensor_TCON_Get_Element_TCON(zero_idx, 7, &element_tcon) ||
        element_tcon != zero_scalar ||
        DSL_Tensor_TCON_Get_Element_TCON(zero_idx, 8, &element_tcon)) {
        fprintf(stderr, "compact tensor TCON element access changed\n");
        return 1;
    }

    if (!DSL_Tensor_TCON_Create_Zero(&info, &duplicate_idx, NULL) ||
        duplicate_idx == TCON_IDX_ZERO) {
        fprintf(stderr, "zero tensor TCON deduplication changed\n");
        return 1;
    }

    info.scalar_tcon = one_scalar;
    info.scalar_integer_value = 1;
    if (!DSL_Tensor_TCON_Create_One(&info, &one_idx, NULL) ||
        !DSL_Tensor_TCON_Get(one_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_ONE ||
        DSL_Tensor_TCON_Semantic_Hash(one_idx) == 0) {
        fprintf(stderr, "one tensor TCON record contract changed\n");
        return 1;
    }

    info.scalar_tcon = splat_scalar;
    info.scalar_integer_value = 42;
    if (!DSL_Tensor_TCON_Create_Splat(&info, &splat_idx, NULL) ||
        !DSL_Tensor_TCON_Get(splat_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT ||
        !DSL_Tensor_TCON_Get_Element_TCON(splat_idx, 3,
                                          &element_tcon) ||
        element_tcon != splat_scalar) {
        fprintf(stderr, "splat tensor TCON record contract changed\n");
        return 1;
    }

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(201);
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 16;
    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.checksum_hi = 0x1234;
    info.checksum_lo = 0x5678;
    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &inline_idx, &carrier) ||
        !DSL_Tensor_TCON_Get(inline_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_INLINE_DENSE ||
        record.dense_offset != DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        record.dense_length != 16 ||
        TCON_str_len(carrier) !=
            DSL_TENSOR_TCON_ENVELOPE_SIZE + 17 ||
        Index_to_length(TCON_str_idx(carrier)) != TCON_str_len(carrier) ||
        Index_to_char_array(TCON_str_idx(carrier))
            [TCON_str_len(carrier) - 1] != '\0' ||
        !DSL_Tensor_TCON_Get_Dense_Bytes(inline_idx, &dense_bytes,
                                         &dense_length) ||
        dense_length != 16 ||
        memcmp(dense_bytes, inline_bytes, 16) != 0 ||
        DSL_Tensor_TCON_Get_Element_TCON(inline_idx, 0,
                                         &element_tcon)) {
        fprintf(stderr, "inline dense tensor TCON contract changed\n");
        return 1;
    }
    inline_hash = DSL_Tensor_TCON_Semantic_Hash(inline_idx);
    memcpy(malformed_payload, Index_to_char_array(TCON_str_idx(carrier)),
           DSL_TENSOR_TCON_ENVELOPE_SIZE + 17);
    malformed_payload[DSL_TENSOR_TCON_ENVELOPE_SIZE + 16] = '\7';
    malformed_carrier =
        Host_To_Targ_String(MTYPE_STRING, malformed_payload,
                            DSL_TENSOR_TCON_ENVELOPE_SIZE + 17);
    if (DSL_Tensor_TCON_Decode_Carrier(&malformed_carrier, NULL)) {
        fprintf(stderr, "tensor TCON trailing payload accepted\n");
        return 1;
    }

    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &duplicate_idx, NULL) ||
        duplicate_idx != inline_idx) {
        fprintf(stderr, "inline dense tensor TCON deduplication changed\n");
        return 1;
    }

    info.dense_bytes = collision_bytes;
    info.dense_bytes_length = 16;
    if (!DSL_Tensor_TCON_Create_Inline_Dense(&info, &collision_idx, NULL) ||
        collision_idx == inline_idx ||
        DSL_Tensor_TCON_Semantic_Equal(inline_idx, collision_idx)) {
        fprintf(stderr, "dense tensor TCON checksum collision accepted\n");
        return 1;
    }

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(201);
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 16;
    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.side_path = side_path;
    info.side_path_length = strlen(side_path);
    info.byte_offset = 64;
    info.byte_length = 16;
    info.checksum_hi = 0x1234;
    info.checksum_lo = 0x5678;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_idx,
                                                &side_carrier) ||
        side_idx != inline_idx ||
        !DSL_Tensor_TCON_Semantic_Equal(inline_idx, side_idx)) {
        fprintf(stderr, "dense tensor TCON semantic identity changed\n");
        return 1;
    }
    side_hash = DSL_Tensor_TCON_Semantic_Hash(side_idx);
    if (side_hash != inline_hash) {
        fprintf(stderr, "dense tensor TCON semantic hash changed\n");
        return 1;
    }

    info.dense_bytes = NULL;
    info.dense_bytes_length = 0;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_unproven_idx,
                                                NULL) ||
        side_unproven_idx == inline_idx ||
        !DSL_Tensor_TCON_Semantic_Equal(side_unproven_idx,
                                        side_unproven_idx) ||
        DSL_Tensor_TCON_Semantic_Equal(inline_idx, side_unproven_idx)) {
        fprintf(stderr, "dense tensor TCON storage policy changed\n");
        return 1;
    }

    info.dense_bytes = inline_bytes;
    info.dense_bytes_length = 16;
    info.checksum_lo = 0x5679;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &side_distinct_idx,
                                                &side_carrier) ||
        side_distinct_idx == inline_idx ||
        !DSL_Tensor_TCON_Get(side_distinct_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
        record.side_path_offset != DSL_TENSOR_TCON_ENVELOPE_SIZE ||
        record.side_path_length != strlen(side_path) ||
        record.byte_offset != 64 ||
        record.byte_length != 16) {
        fprintf(stderr, "side-file dense tensor TCON contract changed\n");
        return 1;
    }
    payload = Index_to_char_array(TCON_str_idx(side_carrier));
    if (payload == NULL ||
        memcmp(payload + record.side_path_offset, side_path,
               record.side_path_length) != 0) {
        fprintf(stderr, "side-file tensor path was not carrier-owned\n");
        return 1;
    }
    if (!DSL_Tensor_TCON_Get_Side_Path(side_distinct_idx, &side_bytes,
                                       &side_length) ||
        side_length != strlen(side_path) ||
        memcmp(side_bytes, side_path, side_length) != 0 ||
        !DSL_Tensor_TCON_Get_Dense_Bytes(side_distinct_idx, &dense_bytes,
                                         &dense_length) ||
        dense_length != 16 ||
        memcmp(dense_bytes, inline_bytes, 16) != 0) {
        fprintf(stderr, "tensor TCON bounded byte access changed\n");
        return 1;
    }

    DSL_Tensor_TCON_Reset();
    DSL_Tensor_TCON_Rebuild_Derived_Cache(inline_idx,
                                          side_distinct_idx + 1);
    info.checksum_lo = 0x5678;
    if (!DSL_Tensor_TCON_Create_Side_File_Dense(&info, &duplicate_idx,
                                                NULL) ||
        duplicate_idx != inline_idx) {
        fprintf(stderr, "tensor TCON derived cache rebuild changed\n");
        return 1;
    }

    info.required_alignment = 3;
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "invalid tensor TCON alignment accepted\n");
        return 1;
    }
    info.required_alignment = 16;
    info.side_path = "../bad.bin";
    info.side_path_length = strlen(info.side_path);
    if (DSL_Tensor_TCON_Create_Side_File_Dense(&info, NULL, NULL)) {
        fprintf(stderr, "invalid tensor TCON side path accepted\n");
        return 1;
    }
    info.side_path = side_path;
    info.side_path_length = strlen(side_path);
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

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(201);
    info.scalar_tcon = wrong_scalar;
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.scalar_integer_value = 0;
    info.element_count = 1;
    info.logical_bytes = 4;
    info.required_alignment = 4;
    if (DSL_Tensor_TCON_Create_Zero(&info, NULL, NULL)) {
        fprintf(stderr, "wrong-type scalar tensor TCON accepted\n");
        return 1;
    }

    carrier = Host_To_Targ_String(MTYPE_STRING,
                                  "__WHIRL_DSL_TCON__:v1:not-id", 30);
    if (DSL_Tensor_TCON_Decode_Carrier(&carrier, NULL)) {
        fprintf(stderr, "invalid tensor TCON carrier accepted\n");
        return 1;
    }

    DSL_Tensor_TCON_Reset();
    if (!DSL_Tensor_TCON_Get(inline_idx, &record) ||
        record.storage_kind != DSL_TENSOR_TCON_STORAGE_INLINE_DENSE ||
        DSL_Tensor_TCON_Decode_Carrier(&carrier, NULL) ||
        DSL_Tensor_TCON_Get(TCON_Table_Size(), &record) ||
        DSL_Tensor_TCON_Semantic_Hash(TCON_Table_Size()) != 0 ||
        DSL_Tensor_TCON_Semantic_Equal(TCON_Table_Size(),
                                       TCON_Table_Size()) ||
        DSL_Tensor_TCON_Get_Dense_Bytes(TCON_Table_Size(), &dense_bytes,
                                        &dense_length) ||
        DSL_Tensor_TCON_Get_Side_Path(TCON_Table_Size(), &side_bytes,
                                      &side_length)) {
        fprintf(stderr, "tensor TCON reset behavior changed\n");
        return 1;
    }

    return 0;
}

static TCON_IDX
Test_Integer_TCON (INT64 value)
{
    TCON scalar;

    TCON_clear(scalar);
    Set_TCON_ty(scalar, MTYPE_I4);
    scalar.vals.i0 = value;
    return Enter_tcon(scalar);
}

static int
Check_Compact_Integer_Evaluator(void)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    DSL_TENSOR_TCON_RECORD result_record;
    DSL_TENSOR_FOLD_POLICY policy;
    DSL_TENSOR_FOLD_CANDIDATE candidate;
    DSL_TENSOR_FOLD_OUTPUT output;
    TCON operands[2];
    TY_IDX operand_ty[2];
    TY_IDX result_ty[DSL_TENSOR_FOLD_MAX_RESULTS];
    TCON_IDX one_idx;
    TCON_IDX three_idx;
    TCON_IDX dense_idx;
    unsigned char dense_bytes[16];

    Initialize_Strtab(1024);
    DSL_Tensor_TCON_Reset();
    memset(&info, 0, sizeof(info));
    info.descriptor_ty = Test_Tensor_Type(101);
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 4;

    info.scalar_tcon = Test_Integer_TCON(1);
    info.scalar_integer_value = 1;
    if (!DSL_Tensor_TCON_Create_One(&info, &one_idx, NULL))
        return 1;
    info.scalar_tcon = Test_Integer_TCON(3);
    info.scalar_integer_value = 3;
    if (!DSL_Tensor_TCON_Create_Splat(&info, &three_idx, NULL) ||
        !DSL_Tensor_TCON_Get_Carrier(one_idx, &operands[0]) ||
        !DSL_Tensor_TCON_Get_Carrier(three_idx, &operands[1]))
        return 1;

    operand_ty[0] = info.descriptor_ty;
    operand_ty[1] = info.descriptor_ty;
    result_ty[0] = info.descriptor_ty;
    result_ty[1] = info.descriptor_ty;
    DSL_Tensor_Fold_Default_Policy(&policy);
    memset(&candidate, 0, sizeof(candidate));
    candidate.dsl_operator = OPR_DSLADD;
    candidate.version = 1;
    candidate.result_count = 1;
    candidate.operand_count = 2;
    candidate.operands = operands;
    candidate.operand_ty = operand_ty;
    candidate.result_ty = result_ty;
    candidate.policy = &policy;
    DSL_TENSOR_FOLD_STATUS status =
        Targ_DSL_WhirlOp(&candidate, &output);
    if (status != DSL_TENSOR_FOLD_SUCCESS ||
        output.result_count != 1 ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT ||
        result_record.scalar_integer_value != 4) {
        fprintf(stderr,
                "compact integer tensor add evaluation changed: %s\n",
                DSL_Tensor_Fold_Status_Name(status));
        return 1;
    }

    policy.preserve_compact_splats = FALSE;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_MATERIALIZATION_POLICY) {
        fprintf(stderr, "compact tensor materialization policy changed\n");
        return 1;
    }
    policy.preserve_compact_splats = TRUE;

    candidate.dsl_operator = OPR_DSLMUL;
    operands[0] = operands[1];
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.scalar_integer_value != 9) {
        fprintf(stderr, "compact integer tensor multiply evaluation changed\n");
        return 1;
    }

    candidate.dsl_operator = OPR_DSLDIVREM;
    candidate.result_count = 2;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        output.result_count != 2 ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.scalar_integer_value != 1 ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[1].result, &result_record) ||
        result_record.scalar_integer_value != 0) {
        fprintf(stderr, "projectable tensor DIVREM evaluation changed\n");
        return 1;
    }

    policy.max_result_elements = 7;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_RESULT_BUDGET ||
        output.result_count != 0) {
        fprintf(stderr, "DIVREM total result budget changed\n");
        return 1;
    }
    policy.max_result_elements = 8;
    policy.max_evaluator_work = 7;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_WORK_BUDGET ||
        output.result_count != 0) {
        fprintf(stderr, "DIVREM combined work budget changed\n");
        return 1;
    }
    policy.max_result_elements = 0;
    policy.max_evaluator_work = 0;

    candidate.dsl_operator = OPR_DSLDIV;
    candidate.result_count = 1;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.scalar_integer_value != 1) {
        fprintf(stderr, "standalone tensor DIV evaluation changed\n");
        return 1;
    }

    candidate.dsl_operator = OPR_DSLREM;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.scalar_integer_value != 0) {
        fprintf(stderr, "standalone tensor REM evaluation changed\n");
        return 1;
    }

    DSL_Tensor_TCON_Get_Carrier(one_idx, &operands[1]);
    info.scalar_tcon = Test_Integer_TCON(0);
    info.scalar_integer_value = 0;
    TCON_IDX zero_idx;
    if (!DSL_Tensor_TCON_Create_Zero(&info, &zero_idx, &operands[1])) {
        fprintf(stderr, "failed to create zero divisor fixture\n");
        return 1;
    }
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_DIVISION_BY_ZERO ||
        output.result_count != 0) {
        fprintf(stderr, "tensor division-by-zero rejection changed\n");
        return 1;
    }

    info.scalar_tcon = Test_Integer_TCON(-2147483647LL - 1);
    info.scalar_integer_value = -2147483647LL - 1;
    TCON_IDX minimum_idx;
    if (!DSL_Tensor_TCON_Create_Splat
             (&info, &minimum_idx, &operands[0])) {
        fprintf(stderr, "failed to create signed-minimum fixture\n");
        return 1;
    }
    info.scalar_tcon = Test_Integer_TCON(-1);
    info.scalar_integer_value = -1;
    TCON_IDX minus_one_idx;
    if (!DSL_Tensor_TCON_Create_Splat
             (&info, &minus_one_idx, &operands[1])) {
        fprintf(stderr, "failed to create negative-one fixture\n");
        return 1;
    }
    candidate.dsl_operator = OPR_DSLDIVREM;
    candidate.result_count = 2;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_NUMERIC_POLICY ||
        output.result_count != 0) {
        fprintf(stderr, "signed DIVREM overflow risk was accepted\n");
        return 1;
    }

    info.scalar_tcon = Test_Integer_TCON(-7);
    info.scalar_integer_value = -7;
    TCON_IDX negative_idx;
    if (!DSL_Tensor_TCON_Create_Splat
             (&info, &negative_idx, &operands[0]) ||
        !DSL_Tensor_TCON_Get_Carrier(three_idx, &operands[1]) ||
        Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[0].result, &result_record) ||
        result_record.scalar_integer_value != -2 ||
        !DSL_Tensor_TCON_Decode_Carrier
             (&output.results[1].result, &result_record) ||
        result_record.scalar_integer_value != -1) {
        fprintf(stderr, "signed DIVREM quotient/remainder changed\n");
        return 1;
    }

    candidate.dsl_operator = OPR_DSLREM;
    candidate.result_count = 1;
    DSL_Tensor_TCON_Get_Carrier(three_idx, &operands[0]);
    DSL_Tensor_TCON_Get_Carrier(three_idx, &operands[1]);

    memset(dense_bytes, 1, sizeof(dense_bytes));
    memset(&info, 0, sizeof(info));
    info.descriptor_ty = result_ty[0];
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 4;
    info.dense_bytes = dense_bytes;
    info.dense_bytes_length = sizeof(dense_bytes);
    info.checksum_hi = 1;
    if (!DSL_Tensor_TCON_Create_Inline_Dense
             (&info, &dense_idx, &operands[0]) ||
        Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_NON_CONSTANT_OPERAND ||
        output.result_count != 0) {
        fprintf(stderr, "dense tensor fold rejection changed\n");
        return 1;
    }

    DSL_Tensor_TCON_Get_Carrier(three_idx, &operands[0]);
    policy.max_evaluator_work = 2;
    if (Targ_DSL_WhirlOp(&candidate, &output) !=
            DSL_TENSOR_FOLD_REJECT_WORK_BUDGET) {
        fprintf(stderr, "tensor evaluator work budget changed\n");
        return 1;
    }
    return 0;
}

static int
Check_VHO_Service_Boundary(void)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    DSL_TENSOR_FOLD_VALUE left_value;
    DSL_TENSOR_FOLD_VALUE right_value;
    DSL_TENSOR_FOLD_POLICY policy;
    DSL_TENSOR_FOLD_CANDIDATE candidate;
    DSL_TENSOR_FOLD_REPLACEMENT_CONTEXT context;
    DSL_TENSOR_FOLD_REPLACEMENT replacement;
    DSL_TENSOR_TCON_RECORD replacement_record;
    DSL_TENSOR_FOLD_STATUS reason;
    TCON operands[2];
    TY_IDX operand_ty[2];
    TY_IDX result_ty[1];
    TCON_IDX two_idx;
    TCON_IDX three_idx;
    TY_IDX descriptor_ty = Test_Tensor_Type(101);
    TY_IDX wrong_ty = Test_Tensor_Type(201);

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = descriptor_ty;
    info.element_mtype = MTYPE_I4;
    info.element_size = 4;
    info.element_count = 4;
    info.logical_bytes = 16;
    info.required_alignment = 4;
    info.scalar_tcon = Test_Integer_TCON(2);
    info.scalar_integer_value = 2;
    if (!DSL_Tensor_TCON_Create_Splat(&info, &two_idx, NULL))
        return 1;
    info.scalar_tcon = Test_Integer_TCON(3);
    info.scalar_integer_value = 3;
    if (!DSL_Tensor_TCON_Create_Splat(&info, &three_idx, NULL))
        return 1;

    if (!DSL_Tensor_Fold_Identify_Compact_TCON
             (two_idx, descriptor_ty, 11, 21, &left_value, &reason) ||
        !DSL_Tensor_Fold_Identify_Compact_TCON
             (three_idx, descriptor_ty, 12, 22, &right_value, &reason)) {
        fprintf(stderr, "M4 compact tensor identification failed\n");
        return 1;
    }

    operands[0] = left_value.carrier;
    operands[1] = right_value.carrier;
    operand_ty[0] = left_value.ty;
    operand_ty[1] = right_value.ty;
    result_ty[0] = descriptor_ty;
    DSL_Tensor_Fold_Default_Policy(&policy);
    memset(&candidate, 0, sizeof(candidate));
    candidate.dsl_operator = OPR_DSLADD;
    candidate.version = 1;
    candidate.result_count = 1;
    candidate.operand_count = 2;
    candidate.operands = operands;
    candidate.operand_ty = operand_ty;
    candidate.result_ty = result_ty;
    candidate.policy = &policy;

    memset(&context, 0, sizeof(context));
    context.result_ty = wrong_ty;
    context.result_st = 77;
    context.source_position = 0x1234;
    context.origin_node_id = 31;
    context.origin_result_value_id = 32;
    context.result_name = Save_Str("folded_splat");
    context.metadata = Save_Str("tensor_fold.origin=OPR_DSLADD");
    context.lineage = Save_Str("lineage:add-after-propagation");
    context.flags = 0x20;

    UINT32 tcon_count = TCON_Table_Size();
    const TY_IDX *saved_result_ty = candidate.result_ty;
    candidate.result_ty = NULL;
    memset(&replacement, 0xff, sizeof(replacement));
    if (DSL_Tensor_Fold_Describe_Replacement
            (&candidate, &context, &replacement) !=
            DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE ||
        replacement.status !=
            DSL_TENSOR_FOLD_REJECT_MALFORMED_CANDIDATE ||
        replacement.result_count != 0 ||
        replacement.result_tcon_idx != TCON_IDX_ZERO ||
        TCON_Table_Size() != tcon_count) {
        fprintf(stderr, "M4 malformed replacement created a TCON\n");
        return 1;
    }
    candidate.result_ty = saved_result_ty;

    memset(&replacement, 0xff, sizeof(replacement));
    if (DSL_Tensor_Fold_Describe_Replacement
            (&candidate, &context, &replacement) !=
            DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH ||
        replacement.status !=
            DSL_TENSOR_FOLD_REJECT_DESCRIPTOR_MISMATCH ||
        replacement.result_count != 0 ||
        replacement.result_tcon_idx != TCON_IDX_ZERO ||
        TCON_Table_Size() != tcon_count) {
        fprintf(stderr, "M4 descriptor rejection created a TCON\n");
        return 1;
    }

    context.result_ty = descriptor_ty;
    if (DSL_Tensor_Fold_Describe_Replacement
            (&candidate, &context, &replacement) !=
            DSL_TENSOR_FOLD_SUCCESS ||
        replacement.status != DSL_TENSOR_FOLD_SUCCESS ||
        replacement.logical_operator != OPR_DSLTENSORCONST ||
        replacement.version != 1 ||
        replacement.result_count != 1 ||
        replacement.result_tcon_idx == TCON_IDX_ZERO ||
        !DSL_Tensor_TCON_Get(replacement.result_tcon_idx,
                             &replacement_record) ||
        replacement_record.scalar_integer_value != 5 ||
        replacement.result_ty != descriptor_ty ||
        replacement.result_st != context.result_st ||
        replacement.source_position != context.source_position ||
        replacement.origin_node_id != context.origin_node_id ||
        replacement.origin_result_value_id !=
            context.origin_result_value_id ||
        replacement.result_name != context.result_name ||
        replacement.metadata != context.metadata ||
        replacement.lineage != context.lineage ||
        strcmp(replacement.compact_scalar_text, "5") != 0 ||
        (replacement.flags &
             DSL_TENSOR_FOLD_REPLACEMENT_REVISIT_PARENTS) == 0 ||
        (replacement.flags & context.flags) == 0) {
        fprintf(stderr, "M4 tensor replacement descriptor changed\n");
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
        Check_Tensor_TCON_Storage() ||
        Check_Compact_Integer_Evaluator() ||
        Check_VHO_Service_Boundary())
        return 1;

    printf("DSL tensor fold M4 VHO service contract passed\n");
    return 0;
}
