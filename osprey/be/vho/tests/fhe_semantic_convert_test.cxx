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
#include "dsl_opcode.h"
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

    Initialize_Test_Context();
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
    config.ring_dimension = 16384;
    config.multiplicative_depth_policy = DSL_FHE_POLICY_AUTO;
    config.multiplicative_depth = 8;
    config.scale_bits = 50;
    config.first_modulus_bits = 60;
    config.slot_count_policy = DSL_FHE_POLICY_AUTO;
    config.bootstrap_policy = DSL_FHE_BOOTSTRAP_AUTO;
    config.backend_policy = DSL_FHE_BACKEND_OPENFHE;
    config_id = DSL_FHE_Intern_Compilation_Config(&config);

    DSL_FHE_Encryption_Descriptor_Record_Init(&encrypted);
    encrypted.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    encrypted.scheme = DSL_FHE_SCHEME_CKKS;
    encrypted.config_id = config_id;
    encrypted.key_set_name = Save_Str("fhe_semantic_key");
    encrypted.slot_count_policy = DSL_FHE_POLICY_AUTO;
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
        result.approximation_contract_count != 0 ||
        result.error_count != 0 ||
        DSL_FHE_Plan_Conversion_Disposition_Count() != 2 ||
        DSL_FHE_Plan_Approximation_Contract_Count() != 0 ||
        DSL_FHE_Plan_CKKS_Value_State_Count() != 2 ||
        DSL_FHE_Plan_BN_Fold_Provenance_Count() != 0 ||
        !DSL_FHE_Plan_Get_Conversion_Disposition(1, &disposition) ||
        disposition.disposition != DSL_FHE_DISPOSITION_DOMAIN_WRAPPER ||
        disposition.bn_fold_count != 0) {
        fprintf(stderr, "FHE ReLU policy blocker behavior changed\n");
        return 1;
    }

    DSL_Builder_Abort_Program();
    printf("FHE semantic conversion defers unapproved ReLU policy\n");
    return 0;
}
