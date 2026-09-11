/*
 * Contract smoke test for the FHE-owned semantic conversion pass.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_fhe.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "fhe_convert.h"
#include "fhe_semantic_convert.h"
#include "ir_reader.h"
#include "mempool.h"
#include "pu_info.h"
#include "stab.h"
#include "strtab.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

extern BOOL VHO_FHE_Ace_Relu_Post_Refresh_Level
                                (const char *instance_path,
                                 INT32 *level);

void
Signal_Cleanup(INT sig)
{
    (void)sig;
}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    (void)kind;
    (void)parm;
    return "";
}

static void
Initialize_Test_Context(void)
{
    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();
    Init_Controls_Tbl();
    ABI_Name = "n64";
    Configure();
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
}

int
main(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_PU_SOURCE_IDENTITY source_identity;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE input;
    DSL_BUILDER_VALUE conv_weight;
    DSL_BUILDER_VALUE channel_parameter;
    DSL_BUILDER_VALUE conv;
    DSL_BUILDER_VALUE batch_norm;
    DSL_BUILDER_VALUE relu;
    DSL_BUILDER_VALUE conv_kid[3];
    DSL_BUILDER_VALUE bn_kid[5];
    DSL_BUILDER_VALUE relu_kid[1];
    DSL_BUILDER_OPERATOR_ATTRIBUTE conv_attribute[8] = {
        { "attr.kernel_shape", "1,1" },
        { "attr.stride", "1,1" },
        { "attr.padding", "0,0" },
        { "attr.dilation", "1,1" },
        { "attr.groups", "1" },
        { "attr.input_layout", "NCHW" },
        { "attr.weight_layout", "OIHW" },
        { "attr.output_layout", "NCHW" }
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE batch_norm_attribute[4] = {
        { "attr.epsilon", "0.00001" },
        { "attr.training", "false" },
        { "attr.input_layout", "NCHW" },
        { "attr.channel_axis", "1" }
    };
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD encrypted;
    DSL_FHE_ENTRY_CONTRACT_INFO entry_info;
    DSL_FHE_ENTRY_VALUE_INFO value_info;
    DSL_FHE_ENTRY_CONTRACT_ID entry_id;
    DSL_FHE_CONFIG_ID config_id;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encrypted_id;
    DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
    VHO_FHE_CONVERT_RESULT result;
    TY_IDX tensor_ty;
    TY_IDX conv_weight_ty;
    TY_IDX channel_parameter_ty;
    WN *tree;
    INT32 refresh_level;

    Initialize_Test_Context();
    /* The linked schedule accepts only a route, so table IDs cannot select it. */
    if (!VHO_FHE_Ace_Relu_Post_Refresh_Level
            ("stem.relu", &refresh_level) || refresh_level != 15 ||
        !VHO_FHE_Ace_Relu_Post_Refresh_Level
            ("layer2.2.relu2", &refresh_level) || refresh_level != 18 ||
        !VHO_FHE_Ace_Relu_Post_Refresh_Level
            ("layer3.2.relu2", &refresh_level) || refresh_level != 17 ||
        VHO_FHE_Ace_Relu_Post_Refresh_Level
            ("unknown.relu", &refresh_level)) {
        fprintf(stderr, "ACE route-specific level schedule is invalid\n");
        return 1;
    }
    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_Opcode_Register_Domain_Wrapper_Examples();

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "float32";
    descriptor.type_core.rank = 4;
    descriptor.type_core.logical_shape = "[1,4,4,4]";
    descriptor.traits.traits = "activation";
    descriptor.representation.layout = "nchw";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";
    tensor_ty = DSL_Builder_Intern_Tensor_Type
                    ("fhe_semantic_f32_1x4x4x4", MTYPE_To_TY(MTYPE_F4),
                     &descriptor);
    descriptor.type_core.logical_shape = "[4,4,1,1]";
    descriptor.traits.traits = "parameter";
    conv_weight_ty = DSL_Builder_Intern_Tensor_Type
                         ("fhe_semantic_conv_weight_f32_4x4x1x1",
                          MTYPE_To_TY(MTYPE_F4), &descriptor);
    descriptor.type_core.rank = 1;
    descriptor.type_core.logical_shape = "[4]";
    channel_parameter_ty = DSL_Builder_Intern_Tensor_Type
                               ("fhe_semantic_channel_parameter_f32_4",
                                MTYPE_To_TY(MTYPE_F4), &descriptor);
    pu = DSL_Builder_Create_Minimal_PU("fhe_semantic_relu");
    memset(&source_identity, 0, sizeof(source_identity));
    source_identity.canonical_definition_name = "FHEResNet.forward";
    source_identity.defining_module = "fhe_semantic_convert_test";
    source_identity.defining_file = __FILE__;
    source_identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(pu, &source_identity)) {
        fprintf(stderr, "failed to set PU source identity\n");
        return 1;
    }
    input = DSL_Builder_Create_Model_Input("encrypted_input", tensor_ty, 0);
    conv_weight = DSL_Builder_Create_Model_Input
                      ("conv_weight", conv_weight_ty, 1);
    channel_parameter = DSL_Builder_Create_Model_Input
                            ("channel_parameter", channel_parameter_ty, 2);
    conv_kid[0] = input;
    conv_kid[1] = conv_weight;
    conv_kid[2] = channel_parameter;
    conv = DSL_Builder_Create_Operator_With_Result
               (DSL_Opcode_Find(DSL_Domain_Find("cnn"),
                                "cnn.conv2d", 2),
                2, conv_kid, 3, conv_attribute, 8, "conv_result",
                tensor_ty);
    bn_kid[0] = conv;
    for (UINT32 i = 1; i < 5; ++i)
        bn_kid[i] = channel_parameter;
    batch_norm = DSL_Builder_Create_Operator_With_Result
                     (DSL_Opcode_Find(DSL_Domain_Find("cnn"),
                                      "cnn.batch_norm_infer", 2),
                      2, bn_kid, 5, batch_norm_attribute, 4,
                      "batch_norm_result", tensor_ty);
    relu_kid[0] = batch_norm;
    relu = DSL_Builder_Create_Operator_With_Result
               (DSL_Opcode_Find(DSL_Domain_Find("common"),
                                "common.relu", 2),
                2, relu_kid, 1, NULL, 0, "relu_result", tensor_ty);
    if (tensor_ty == TY_IDX_ZERO || conv_weight_ty == TY_IDX_ZERO ||
        channel_parameter_ty == TY_IDX_ZERO || pu == NULL ||
        input == NULL || conv_weight == NULL || channel_parameter == NULL ||
        conv == NULL || batch_norm == NULL || relu == NULL ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_PU_Value(pu, conv_weight) ||
        !DSL_Builder_Append_PU_Value(pu, channel_parameter) ||
        !DSL_Builder_Append_PU_Value(pu, conv) ||
        !DSL_Builder_Append_PU_Value(pu, batch_norm) ||
        !DSL_Builder_Append_PU_Value(pu, relu)) {
        fprintf(stderr, "failed to construct FHE semantic smoke PU\n");
        return 1;
    }

    DSL_FHE_Compilation_Config_Record_Init(&config);
    config.scheme = DSL_FHE_SCHEME_CKKS;
    config.security_level = DSL_FHE_SECURITY_128_CLASSIC;
    config.ring_dimension = 65536;
    config.multiplicative_depth_policy = DSL_FHE_POLICY_EXPLICIT;
    config.multiplicative_depth = 33;
    config.scale_bits = 56;
    config.first_modulus_bits = 60;
    config.slot_count_policy = DSL_FHE_POLICY_EXPLICIT;
    config.slot_count = 32768;
    config.bootstrap_policy = DSL_FHE_BOOTSTRAP_AUTO;
    config.backend_policy = DSL_FHE_BACKEND_OPENFHE;
    config_id = DSL_FHE_Intern_Compilation_Config(&config);

    DSL_FHE_Encryption_Descriptor_Record_Init(&encrypted);
    encrypted.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    encrypted.scheme = DSL_FHE_SCHEME_CKKS;
    encrypted.config_id = config_id;
    encrypted.key_set_name = Save_Str("fhe_semantic_key");
    encrypted.slot_count_policy = DSL_FHE_POLICY_INHERIT;
    encrypted.encoding_policy = DSL_FHE_ENCODING_CKKS_PACKED;
    encrypted.packing_policy = DSL_FHE_PACKING_AUTO;
    encrypted_id = DSL_FHE_Intern_Encryption_Descriptor(&encrypted);
    if (DSL_Builder_Bind_FHE_Tensor_Descriptor
            (tensor_ty, encrypted_id, 0) == 0) {
        fprintf(stderr, "failed to bind tensor encryption descriptor\n");
        return 1;
    }

    memset(&entry_info, 0, sizeof(entry_info));
    entry_info.config_id = config_id;
    entry_info.input_count = 1;
    entry_info.output_count = 1;
    entry_info.encrypted_io_policy = 1;
    entry_info.parameter_policy = DSL_FHE_PARAMETER_POLICY_PLAINTEXT;
    entry_id = DSL_Builder_Attach_FHE_Entry_Contract(pu, &entry_info);

    memset(&value_info, 0, sizeof(value_info));
    value_info.encryption_descriptor_id = encrypted_id;
    value_info.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    if (config_id == 0 || encrypted_id == 0 || entry_id == 0 ||
        DSL_Builder_Declare_FHE_Entry_Value
            (entry_id, input, 0, DSL_FHE_ENTRY_VALUE_INPUT,
             &value_info) == 0 ||
        DSL_Builder_Declare_FHE_Entry_Value
            (entry_id, relu, 0, DSL_FHE_ENTRY_VALUE_OUTPUT,
             &value_info) == 0) {
        fprintf(stderr, "failed to construct FHE entry contract\n");
        return 1;
    }

    VHO_FHE_Enable_Conversion = TRUE;
    VHO_FHE_Strict_O0 = TRUE;
    VHO_FHE_Convert_Reset_Passes();
    if (!VHO_FHE_Register_Default_Semantic_Conversion()) {
        fprintf(stderr, "default FHE semantic conversion did not register\n");
        return 1;
    }

    tree = PU_Info_tree_ptr(pu);
    memset(&result, 0, sizeof(result));
    if (!VHO_FHE_Convert_Program_Unit(pu, &tree, stderr, &result) ||
        result.semantic_gatekeeper_count != 2 ||
        result.conversion_pass_count != 1 ||
        result.source_disposition_count != 3 ||
        result.converted_disposition_count != 2 ||
        result.folded_batch_norm_count != 0 ||
        result.approximation_contract_count != 1 ||
        result.error_count != 0 ||
        DSL_FHE_Plan_Conversion_Disposition_Count() != 2 ||
        DSL_FHE_Plan_Approximation_Contract_Count() != 0 ||
        DSL_FHE_Plan_CKKS_Value_State_Count() != 3 ||
        DSL_FHE_Plan_BN_Fold_Provenance_Count() != 0 ||
        DSL_FHE_Approx_Profile_Count() != 1 ||
        DSL_FHE_Approx_Stage_Count() != 3 ||
        DSL_FHE_Approx_Association_Count() != 0 ||
        DSL_FHE_Context_Range_Count() != 0 ||
        !DSL_FHE_Plan_Get_Conversion_Disposition(1, &disposition) ||
        disposition.disposition != DSL_FHE_DISPOSITION_DOMAIN_WRAPPER ||
        disposition.bn_fold_count != 0) {
        fprintf(stderr, "FHE ReLU policy blocker behavior changed\n");
        return 1;
    }

    VHO_FHE_Calibration_Manifest_Path =
        const_cast<char *>("changed-after-first-callback.json");
    VHO_FHE_Calibration_Manifest_SHA256 = const_cast<char *>(
        "0000000000000000000000000000000000000000000000000000000000000000");
    memset(&result, 0, sizeof(result));
    if (VHO_FHE_Convert_Program_Unit(pu, &tree, NULL, &result)) {
        fprintf(stderr, "cross-callback calibration selection changed\n");
        return 1;
    }
    VHO_FHE_Calibration_Manifest_Path = NULL;
    VHO_FHE_Calibration_Manifest_SHA256 = NULL;
    VHO_FHE_Convert_Reset_Passes();
    if (!VHO_FHE_Register_Default_Semantic_Conversion()) {
        fprintf(stderr, "calibration failure cleanup did not re-register\n");
        return 1;
    }
    memset(&result, 0, sizeof(result));
    if (!VHO_FHE_Convert_Program_Unit(pu, &tree, stderr, &result)) {
        fprintf(stderr, "calibration failure retained callback state\n");
        return 1;
    }

    const VHO_FHE_RELU_PROFILE_MANIFEST *approved =
        VHO_FHE_Approved_Ace_Relu_Profile();
    VHO_FHE_RELU_PROFILE_MANIFEST malformed = *approved;
    if (!VHO_FHE_Validate_Relu_Profile_Manifest(approved, stderr)) {
        fprintf(stderr, "approved ACE profile manifest did not validate\n");
        return 1;
    }
    malformed.stage_count = 2;
    if (VHO_FHE_Validate_Relu_Profile_Manifest(&malformed, NULL)) {
        fprintf(stderr, "incomplete ACE profile manifest was accepted\n");
        return 1;
    }
    malformed = *approved;
    malformed.manifest_sha256 =
        "0000000000000000000000000000000000000000000000000000000000000000";
    if (VHO_FHE_Validate_Relu_Profile_Manifest(&malformed, NULL)) {
        fprintf(stderr, "ACE profile manifest hash mismatch was accepted\n");
        return 1;
    }

    VHO_FHE_RELU_STAGE_MANIFEST malformed_stages[3];
    memcpy(malformed_stages, approved->stages, sizeof(malformed_stages));
    malformed = *approved;
    malformed.stages = malformed_stages;
    malformed_stages[0].coefficient_sha256 =
        "0000000000000000000000000000000000000000000000000000000000000000";
    if (VHO_FHE_Validate_Relu_Profile_Manifest(&malformed, NULL)) {
        fprintf(stderr, "ACE profile hash mismatch was accepted\n");
        return 1;
    }
    memcpy(malformed_stages, approved->stages, sizeof(malformed_stages));
    malformed_stages[0].ordinal = 1;
    if (VHO_FHE_Validate_Relu_Profile_Manifest(&malformed, NULL)) {
        fprintf(stderr, "ACE profile stage reordering was accepted\n");
        return 1;
    }

    UINT64 changed_bits[8];
    memcpy(changed_bits, approved->stages[0].coefficient_binary64_bits,
           sizeof(changed_bits));
    changed_bits[1] ^= 1;
    memcpy(malformed_stages, approved->stages, sizeof(malformed_stages));
    malformed_stages[0].coefficient_binary64_bits = changed_bits;
    if (VHO_FHE_Validate_Relu_Profile_Manifest(&malformed, NULL)) {
        fprintf(stderr, "changed ACE coefficient bytes were accepted\n");
        return 1;
    }

    DSL_FHE_COMPOSITE_PROFILE_RECORD profile;
    DSL_FHE_CKKS_VALUE_STATE_RECORD relu_state;
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    DSL_FHE_CONTEXT_RANGE_RECORD range;
    DSL_FHE_CONVERSION_DISPOSITION_RECORD relu_disposition;
    DSL_IR_VALUE_ID relu_value_id = DSL_Builder_Get_Value_Image_Id(relu);
    DSL_FHE_COMPOSITE_PROFILE_ID profile_id;
    DSL_FHE_CONVERSION_DISPOSITION_ID relu_disposition_id;
    TCON_IDX observed_min =
        Enter_tcon(Host_To_Targ_Float(MTYPE_F8, -1.0));
    TCON_IDX observed_max =
        Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 1.0));
    TCON_IDX positive_bound =
        Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 1.25));
    if (!DSL_FHE_Approx_Profile_Find
             (config_id, VHO_FHE_ACE_RELU_PROFILE_NAME,
              VHO_FHE_ACE_RELU_PROFILE_VERSION, &profile) ||
        strcmp(Index_To_Str(profile.manifest_sha256),
               VHO_FHE_ACE_RELU_MANIFEST_SHA256) != 0 ||
        !DSL_FHE_Plan_Find_Latest_CKKS_Value_State
             (relu_value_id, &relu_state) ||
        relu_state.pending_actions != DSL_FHE_CKKS_PENDING_BOOTSTRAP ||
        relu_state.pending_bootstrap_reason !=
            DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH ||
        !DSL_Call_Image_Find_PU_Identity
             (PU_Info_proc_sym(pu), &identity)) {
        fprintf(stderr, "approved ACE profile or ReLU state is incomplete\n");
        return 1;
    }
    profile_id = profile.id;
    for (UINT32 stage_ordinal = 0; stage_ordinal < 3; ++stage_ordinal) {
        DSL_FHE_APPROX_STAGE_RECORD stage;
        const unsigned char *dense_bytes = NULL;
        UINT32 dense_length = 0;
        if (!DSL_FHE_Approx_Stage_Get
                 (profile.first_stage_id + stage_ordinal, &stage) ||
            stage.stage_ordinal != stage_ordinal ||
            strcmp(Index_To_Str(stage.coefficient_sha256),
                   approved->stages[stage_ordinal].coefficient_sha256) != 0 ||
            !DSL_Tensor_TCON_Get_Dense_Bytes
                 (stage.coefficient_tensor_tcon, &dense_bytes,
                  &dense_length) ||
            dense_length != approved->stages[stage_ordinal].coefficient_count *
                                sizeof(UINT64)) {
            fprintf(stderr, "ACE profile coefficient TCON is incomplete\n");
            return 1;
        }
        for (UINT32 coefficient = 0;
             coefficient < approved->stages[stage_ordinal].coefficient_count;
             ++coefficient) {
            UINT64 bits = approved->stages[stage_ordinal]
                              .coefficient_binary64_bits[coefficient];
            for (UINT32 byte = 0; byte < 8; ++byte) {
                if (dense_bytes[coefficient * 8 + byte] !=
                    (unsigned char)(bits >> (byte * 8))) {
                    fprintf(stderr,
                            "ACE profile coefficient TCON bytes changed\n");
                    return 1;
                }
            }
        }
    }

    DSL_FHE_Conversion_Disposition_Record_Init(&relu_disposition);
    DSL_IR_VALUE_RECORD relu_value;
    if (!DSL_IR_Image_Get_Value(relu_value_id, &relu_value)) {
        fprintf(stderr, "ReLU image value lookup failed\n");
        return 1;
    }
    relu_disposition.source_node_id = relu_value.producer_node_id;
    relu_disposition.result_value_id = relu_value_id;
    relu_disposition.disposition =
        DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION;
    relu_disposition.owner_pu_st = PU_Info_proc_sym(pu);
    relu_disposition.approximation_contract_id = profile_id;
    relu_disposition.result_ckks_value_state_id = relu_state.id;
    relu_disposition.flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
    relu_disposition_id = DSL_FHE_Plan_Add_Composite_Disposition
                              (&relu_disposition, profile_id);

    DSL_FHE_Context_Range_Record_Init(&range);
    range.profile_id = profile_id;
    range.source_relu_value_id = relu_value_id;
    range.context_pu_identity_id = identity.id;
    range.context_callsite_id = DSL_CALLSITE_METADATA_INVALID_ID;
    range.owner_pu_st = PU_Info_proc_sym(pu);
    range.positive_bound_tcon = positive_bound;
    range.observed_min_tcon = observed_min;
    range.observed_max_tcon = observed_max;
    range.out_of_range_policy = DSL_FHE_CONTEXT_RANGE_REJECT;
    range.provenance = Save_Str("deterministic-policy-fixture-not-model-data");
    range.flags = DSL_FHE_CONTEXT_RANGE_IDENTITY_IS_CALLEE;
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD context_state;
    DSL_FHE_Context_CKKS_State_Record_Init(&context_state);
    context_state.owner_pu_st = PU_Info_proc_sym(pu);
    context_state.source_value_id = relu_value_id;
    context_state.context_pu_identity_id = identity.id;
    context_state.context_callsite_id = DSL_CALLSITE_METADATA_INVALID_ID;
    context_state.state_role = DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH;
    context_state.state_version = 1;
    context_state.encryption_descriptor_id = encrypted_id;
    context_state.scheme = DSL_FHE_SCHEME_CKKS;
    context_state.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    context_state.level = 15;
    context_state.scale_bits = 56;
    context_state.component_count = 2;
    context_state.precision_bits = 30;
    context_state.slot_count = 32768;
    context_state.encrypted_layout_name = Save_Str("ckks.packed");
    context_state.pending_actions = DSL_FHE_CKKS_PENDING_BOOTSTRAP;
    context_state.pending_bootstrap_reason =
        DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH;
    if (relu_disposition_id == 0 ||
        DSL_FHE_Approx_Profile_Bind_Context_Range(&range) == 0 ||
        DSL_FHE_Context_State_Intern(&context_state) == 0 ||
        !DSL_FHE_Approx_Profile_Image_Validate(stderr) ||
        !DSL_FHE_Context_State_Image_Validate(stderr)) {
        fprintf(stderr, "deterministic ACE profile fixture did not validate\n");
        return 1;
    }

    DSL_FHE_APPROX_PROFILE_IMAGE_HEADER profile_header;
    DSL_FHE_Approx_Profile_Image_Get_Header(&profile_header);
    UINT64 image_size = DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE +
        (UINT64)profile_header.profile_count *
            DSL_FHE_COMPOSITE_PROFILE_RECORD_SIZE +
        (UINT64)profile_header.stage_count * DSL_FHE_APPROX_STAGE_RECORD_SIZE +
        (UINT64)profile_header.association_count *
            DSL_FHE_APPROX_ASSOCIATION_RECORD_SIZE +
        (UINT64)profile_header.context_range_count *
            DSL_FHE_CONTEXT_RANGE_RECORD_SIZE;
    unsigned char *image = new unsigned char[image_size];
    unsigned char *cursor = image;
    memcpy(cursor, &profile_header, sizeof(profile_header));
    cursor += sizeof(profile_header);
    for (UINT32 i = 1; i <= profile_header.profile_count; ++i) {
        DSL_FHE_Approx_Profile_Get(i, &profile);
        memcpy(cursor, &profile, sizeof(profile));
        cursor += sizeof(profile);
    }
    for (UINT32 i = 1; i <= profile_header.stage_count; ++i) {
        DSL_FHE_APPROX_STAGE_RECORD stage;
        DSL_FHE_Approx_Stage_Get(i, &stage);
        memcpy(cursor, &stage, sizeof(stage));
        cursor += sizeof(stage);
    }
    for (UINT32 i = 1; i <= profile_header.association_count; ++i) {
        DSL_FHE_APPROX_ASSOCIATION_RECORD association;
        DSL_FHE_Approx_Association_Get(i, &association);
        memcpy(cursor, &association, sizeof(association));
        cursor += sizeof(association);
    }
    for (UINT32 i = 1; i <= profile_header.context_range_count; ++i) {
        DSL_FHE_CONTEXT_RANGE_RECORD context_range;
        DSL_FHE_Context_Range_Get(i, &context_range);
        memcpy(cursor, &context_range, sizeof(context_range));
        cursor += sizeof(context_range);
    }
    DSL_FHE_Approx_Profile_Image_Reset();
    if (!DSL_FHE_Approx_Profile_Image_Load_Mapped
             (image, image_size, stderr) ||
        !DSL_FHE_Approx_Profile_Image_Validate(stderr) ||
        DSL_FHE_Approx_Profile_Count() != 1 ||
        DSL_FHE_Approx_Stage_Count() != 3 ||
        DSL_FHE_Approx_Association_Count() != 1 ||
        DSL_FHE_Context_Range_Count() != 1) {
        fprintf(stderr, "ACE profile mapped-image reopen failed\n");
        delete [] image;
        return 1;
    }
    delete [] image;

    const char *output_path = getenv("FHE_RELU_POLICY_OUTPUT");
    if (output_path != NULL && output_path[0] != '\0') {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
        memset(&request, 0, sizeof(request));
        request.path = output_path;
        (void)remove(output_path);
        if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
            fprintf(stderr, "ACE profile policy fixture output failed\n");
            return 1;
        }
    }

    DSL_Builder_Abort_Program();
    printf("FHE semantic conversion interns the approved ACE profile and "
           "defers missing model ranges\n");
    return 0;
}
