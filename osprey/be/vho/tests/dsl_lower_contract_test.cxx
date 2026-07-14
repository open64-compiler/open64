/*
 * Contract test for native DSL lowering before standard VHO lowering.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_lower.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
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

static TY_IDX
Create_Shaped_Tensor_Type
        (const char *name,
         const char *dtype,
         TY_IDX element_ty,
         INT32 rank,
         const char *shape,
         const char *placement,
         const char *memory)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    memset(&type_core, 0, sizeof(type_core));
    memset(&descriptor, 0, sizeof(descriptor));

    type_core.kind = "tensor";
    type_core.dtype = dtype;
    type_core.rank = rank;
    type_core.logical_shape = shape;
    TY_IDX tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                           (name, element_ty, &type_core);
    descriptor.type_core = type_core;
    descriptor.representation.layout = "contiguous";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = placement;
    descriptor.representation.memory = memory;
    descriptor.representation.quantization = "none";
    descriptor.representation.runtime_state = "static";
    if (!DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor))
        return TY_IDX_ZERO;
    return tensor_ty;
}

static TY_IDX
Create_Tensor_Type_With_Representation
        (const char *name,
         const char *placement,
         const char *memory)
{
    return Create_Shaped_Tensor_Type
               (name, "int32", MTYPE_To_TY(MTYPE_I4), 2, "[2,2]",
                placement, memory);
}

static TY_IDX
Create_Tensor_Type(void)
{
    return Create_Tensor_Type_With_Representation
               ("dsl_lower_i32_2x2", NULL, NULL);
}

static BOOL
Tree_Has_Native_DSL (WN *wn)
{
    if (wn == NULL)
        return FALSE;
    if (DSL_WN_Is_Native(wn))
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (Tree_Has_Native_DSL(statement))
                return TRUE;
        }
        return FALSE;
    }
    for (INT kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (Tree_Has_Native_DSL(WN_kid(wn, kid)))
            return TRUE;
    }
    return FALSE;
}

static BOOL
Append_Signature
        (char *signature,
         size_t signature_size,
         size_t *offset,
         const char *format,
         ...)
{
    if (*offset >= signature_size)
        return FALSE;
    va_list args;
    va_start(args, format);
    int written = vsnprintf(signature + *offset, signature_size - *offset,
                            format, args);
    va_end(args);
    if (written < 0 || (size_t)written >= signature_size - *offset)
        return FALSE;
    *offset += written;
    return TRUE;
}

static BOOL
Verify_Lowered_Body
        (WN *body,
         char *signature,
         size_t signature_size)
{
    const char *expected_calls[] = {
        "__open64_dsl_tensor_const_v1",
        "__open64_dsl_tensor_const_v1",
        "__open64_dsl_add_v1",
        "__open64_dsl_matmul_v1"
    };
    UINT32 call_count = 0;
    UINT32 comment_count = 0;
    UINT32 capture_count = 0;
    PREG_NUM result_pregs[4] = { 0, 0, 0, 0 };
    size_t signature_offset = 0;

    if (signature == NULL || signature_size == 0)
        return FALSE;
    signature[0] = '\0';

    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        OPERATOR opr = WN_operator(statement);
        if (opr == OPR_COMMENT) {
            if (!WN_Is_DSL_Comment(statement))
                return FALSE;
            ++comment_count;
            continue;
        }
        if (opr == OPR_CALL) {
            if (call_count >= 4 ||
                strcmp(ST_name(WN_st(statement)),
                       expected_calls[call_count]) != 0)
                return FALSE;

            UINT32 expected_parameters = call_count < 2 ? 2 : 4;
            if (WN_kid_count(statement) != expected_parameters)
                return FALSE;
            if (!Append_Signature(signature, signature_size,
                                  &signature_offset, "%s(",
                                  expected_calls[call_count]))
                return FALSE;
            for (UINT32 kid = 0; kid < expected_parameters; ++kid) {
                if (WN_operator(WN_kid(statement, kid)) != OPR_PARM)
                    return FALSE;
                WN *parameter = WN_kid0(WN_kid(statement, kid));
                if (WN_operator(parameter) == OPR_LDA) {
                    if (!Append_Signature
                             (signature, signature_size, &signature_offset,
                              "%sdescriptor", kid == 0 ? "" : ","))
                        return FALSE;
                } else if (WN_operator(parameter) == OPR_LDID) {
                    UINT32 producer = 0;
                    while (producer < capture_count &&
                           result_pregs[producer] != WN_load_offset(parameter))
                        ++producer;
                    if (producer == capture_count ||
                        !Append_Signature
                             (signature, signature_size, &signature_offset,
                              "%svalue%u", kid == 0 ? "" : ",", producer))
                        return FALSE;
                } else if (WN_operator(parameter) == OPR_INTCONST) {
                    if (!Append_Signature
                             (signature, signature_size, &signature_offset,
                              "%sconstant%lld", kid == 0 ? "" : ",",
                              (long long)WN_const_val(parameter)))
                        return FALSE;
                } else {
                    return FALSE;
                }
            }
            if (!Append_Signature(signature, signature_size,
                                  &signature_offset, ");"))
                return FALSE;
            if (call_count < 2) {
                if (WN_operator(WN_kid0(WN_kid(statement, 0))) != OPR_LDA ||
                    WN_operator(WN_kid0(WN_kid(statement, 1))) != OPR_LDA)
                    return FALSE;
            } else {
                if (WN_operator(WN_kid0(WN_kid(statement, 0))) != OPR_LDID ||
                    WN_operator(WN_kid0(WN_kid(statement, 1))) != OPR_LDID ||
                    WN_load_offset(WN_kid0(WN_kid(statement, 0))) !=
                        result_pregs[0] ||
                    WN_load_offset(WN_kid0(WN_kid(statement, 1))) !=
                        result_pregs[1] ||
                    WN_operator(WN_kid0(WN_kid(statement, 2))) != OPR_LDA ||
                    WN_operator(WN_kid0(WN_kid(statement, 3))) != OPR_INTCONST)
                    return FALSE;
            }
            ++call_count;
            continue;
        }
        if (opr == OPR_STID &&
            ST_class(WN_st(statement)) == CLASS_PREG) {
            if (capture_count >= 4 ||
                WN_operator(WN_kid0(statement)) != OPR_LDID ||
                WN_st(WN_kid0(statement)) != Return_Val_Preg)
                return FALSE;
            result_pregs[capture_count++] = WN_store_offset(statement);
            continue;
        }
        return FALSE;
    }

    if (call_count != 4 || comment_count != 4 || capture_count != 4 ||
        Tree_Has_Native_DSL(body))
        return FALSE;
    for (UINT32 i = 0; i < 4; ++i) {
        for (UINT32 j = i + 1; j < 4; ++j) {
            if (result_pregs[i] == result_pregs[j])
                return FALSE;
        }
    }
    return Append_Signature(signature, signature_size, &signature_offset,
                            "comments=%u", comment_count);
}

static DSL_BUILDER_PROGRAM_UNIT
Create_Compatibility_PU(void)
{
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_lower_compatibility");
    if (pu == NULL)
        return NULL;
    WN *body = WN_func_body(PU_Info_tree_ptr(pu));

    const char *zero_payload =
        "name=tensor_zero;dtype=int32;rank=2;shape=[2,2];"
        "value_kind=splat;value=0";
    const char *one_payload =
        "name=tensor_one;dtype=int32;rank=2;shape=[2,2];"
        "value_kind=splat;value=1";
    WN *tensor_zero = DSL_WN_Create_Logical_Opcode
        (OPR_DSLTENSORCONST, 1, zero_payload, NULL, 0,
         DSL_OPCODE_OUTPUT_LEGACY_XPRAGMA);
    WN *tensor_one = DSL_WN_Create_Logical_Opcode
        (OPR_DSLTENSORCONST, 1, one_payload, NULL, 0,
         DSL_OPCODE_OUTPUT_COMMENT_PROJECTION);
    WN *operands[2] = { tensor_zero, tensor_one };
    WN *add = DSL_WN_Create_Logical_Opcode
        (OPR_DSLADD, 1,
         "kid0=tensor_zero;kid1=tensor_one;attr.broadcast_rule=none",
         operands, 2, DSL_OPCODE_OUTPUT_LEGACY_EVAL);
    WN *matmul = DSL_WN_Create_Logical_Opcode
        (OPR_DSLMATMUL, 1,
         "kid0=tensor_zero;kid1=tensor_one;"
         "attr.transpose_kid0=false;attr.transpose_kid1=false",
         operands, 2, DSL_OPCODE_OUTPUT_COMMENT_PROJECTION);
    if (tensor_zero == NULL || tensor_one == NULL || add == NULL ||
        matmul == NULL)
        return NULL;

    WN_INSERT_BlockLast(body, tensor_zero);
    WN_INSERT_BlockLast(body, tensor_one);
    WN_INSERT_BlockLast(body, add);
    WN_INSERT_BlockLast(body, matmul);
    return pu;
}

static BOOL
Check_Canonical_Boundary_Rejection(void)
{
    const char *payload =
        "name=boundary_tensor;dtype=int32;rank=2;shape=[2,2];"
        "value_kind=splat;value=0";
    WN *block = WN_CreateBlock();
    WN_INSERT_BlockLast
        (block, WN_CreateEval
                    (DSL_WN_Create_Native
                         (OPR_DSLTENSORCONST, 1, payload, NULL, 0)));
    WN_INSERT_BlockLast
        (block, DSL_WN_Create_Opcode
                    (DSL_OPCODE_COMMON_TENSOR_CONST, 1, payload));
    WN_INSERT_BlockLast
        (block, DSL_WN_Create_Opcode_Marker
                    (DSL_OPCODE_COMMON_TENSOR_CONST, 1, payload));

    UINT32 remaining = 0;
    return !VHO_DSL_Lowered_Tree_Is_Canonical(block, NULL, &remaining) &&
           remaining == 2;
}

static BOOL
Check_Value_Source_Gatekeeper_Rejection(void)
{
    DSL_BUILDER_PROGRAM_UNIT duplicate_pu =
        DSL_Builder_Create_Minimal_PU("dsl_duplicate_model_inputs");
    TY_IDX input_ty = Create_Tensor_Type_With_Representation
                          ("dsl_duplicate_input_type", "host", "host");
    DSL_BUILDER_VALUE input0 = DSL_Builder_Create_Model_Input
                                   ("model_input0", input_ty, 0);
    DSL_BUILDER_VALUE input1 = DSL_Builder_Create_Model_Input
                                   ("model_input1", input_ty, 0);
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    if (duplicate_pu == NULL || input0 == NULL || input1 == NULL ||
        !DSL_Builder_Append_PU_Value(duplicate_pu, input0) ||
        !DSL_Builder_Append_PU_Value(duplicate_pu, input1) ||
        DSL_Gatekeeper_Verify_Program
            (duplicate_pu, NULL, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0)
        return FALSE;

    DSL_BUILDER_PROGRAM_UNIT external_pu =
        DSL_Builder_Create_Minimal_PU("dsl_bad_external_reference");
    TY_IDX external_ty = Create_Tensor_Type_With_Representation
                             ("dsl_bad_external_type", "side_file",
                              "external_data");
    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE reference;
    memset(&reference, 0, sizeof(reference));
    reference.storage_format = "safetensors";
    reference.side_file = "resnet.safetensors";
    reference.tensor_key = "layer.weight";
    reference.byte_offset = 64;
    reference.byte_length = 16;
    DSL_BUILDER_VALUE weight = DSL_Builder_Create_External_Tensor_Constant
                                   ("weight0", external_ty, &reference);
    if (external_pu == NULL || weight == NULL ||
        !DSL_Builder_Append_PU_Value(external_pu, weight))
        return FALSE;
    ST_tensor_bind_metadata
        (WN_st_idx(weight), "storage_byte_length", "0");
    return !DSL_Gatekeeper_Verify_Program
                (external_pu, NULL, &gatekeeper_result) &&
           gatekeeper_result.error_count != 0;
}

static BOOL
Check_Value_Source_Lowering(void)
{
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_value_source_lowering");
    if (pu == NULL)
        return FALSE;

    TY_IDX input_ty = Create_Tensor_Type_With_Representation
                          ("dsl_model_input_type", "host", "host");
    TY_IDX external_ty = Create_Tensor_Type_With_Representation
                             ("dsl_external_type", "side_file",
                              "external_data");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("model_input0", input_ty, 0);
    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE reference;
    memset(&reference, 0, sizeof(reference));
    reference.storage_format = "safetensors";
    reference.side_file = "resnet.safetensors";
    reference.tensor_key = "layer.weight";
    reference.byte_offset = 64;
    reference.byte_length = 16;

    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE invalid_reference = reference;
    invalid_reference.byte_length = 0;
    if (DSL_Builder_Create_External_Tensor_Constant
            ("invalid_length", external_ty, &invalid_reference) != NULL)
        return FALSE;
    invalid_reference = reference;
    invalid_reference.byte_offset = ~(UINT64)0 - 7;
    if (DSL_Builder_Create_External_Tensor_Constant
            ("invalid_range", external_ty, &invalid_reference) != NULL)
        return FALSE;
    invalid_reference = reference;
    invalid_reference.checksum = "not-a-sha256-checksum";
    if (DSL_Builder_Create_External_Tensor_Constant
            ("invalid_checksum", external_ty, &invalid_reference) != NULL)
        return FALSE;

    DSL_BUILDER_VALUE weight = DSL_Builder_Create_External_Tensor_Constant
        ("weight0", external_ty, &reference);
    if (input == NULL || weight == NULL ||
        strcmp(ST_tensor_metadata(WN_st_idx(weight), "storage_format"),
               "safetensors") != 0 ||
        strcmp(ST_tensor_metadata(WN_st_idx(weight), "storage_file"),
               "resnet.safetensors") != 0 ||
        strcmp(ST_tensor_metadata(WN_st_idx(weight), "storage_tensor_key"),
               "layer.weight") != 0 ||
        strcmp(ST_tensor_metadata(WN_st_idx(weight), "storage_byte_offset"),
               "64") != 0 ||
        strcmp(ST_tensor_metadata(WN_st_idx(weight), "storage_byte_length"),
               "16") != 0 ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_PU_Value(pu, weight))
        return FALSE;

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    if (!DSL_Gatekeeper_Verify_Program(pu, NULL, &gatekeeper_result) ||
        gatekeeper_result.native_node_count != 2 ||
        gatekeeper_result.error_count != 0)
        return FALSE;

    VHO_DSL_LOWER_RESULT result;
    WN *tree = PU_Info_tree_ptr(pu);
    if (!VHO_DSL_Lower_Verified_Program_Unit
             (pu, tree, NULL, &result) ||
        result.native_node_count != 2 || result.lowered_node_count != 2 ||
        result.remaining_executable_carrier_count != 0)
        return FALSE;

    const char *expected_calls[] = {
        "__open64_dsl_model_input_v1",
        "__open64_dsl_external_tensor_v1"
    };
    UINT32 call_count = 0;
    UINT32 capture_count = 0;
    UINT32 comment_count = 0;
    WN *body = WN_func_body(tree);
    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_COMMENT) {
            ++comment_count;
        } else if (WN_operator(statement) == OPR_CALL) {
            if (call_count >= 2 || WN_kid_count(statement) != 2 ||
                strcmp(ST_name(WN_st(statement)),
                       expected_calls[call_count]) != 0 ||
                WN_operator(WN_kid0(WN_kid(statement, 0))) != OPR_LDA)
                return FALSE;
            WN *source = WN_kid0(WN_kid(statement, 1));
            if ((call_count == 0 && WN_operator(source) != OPR_INTCONST) ||
                (call_count == 1 && WN_operator(source) != OPR_LDA))
                return FALSE;
            ++call_count;
        } else if (WN_operator(statement) == OPR_STID &&
                   ST_class(WN_st(statement)) == CLASS_PREG) {
            ++capture_count;
        } else {
            return FALSE;
        }
    }
    return call_count == 2 && capture_count == 2 && comment_count == 2;
}

static BOOL
Check_Simple_ResNet_Gatekeeper_Rejection(void)
{
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_bad_residual_add");
    TY_IDX tensor_ty = Create_Tensor_Type_With_Representation
                           ("dsl_bad_residual_type", "host", "host");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("model_input0", tensor_ty, 0);
    DSL_Opcode_Register_Common_Substrate();
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_OPCODE_ID residual_id = DSL_Opcode_Find
                                    (common, "common.residual_add", 2);
    DSL_BUILDER_VALUE kids[2] = { input, input };
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[3];
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "numpy";
    attrs[1].name = "attr.shape_check";
    attrs[1].value = "exact";
    attrs[2].name = "attr.residual_path";
    attrs[2].value = "true";
    DSL_BUILDER_VALUE residual = DSL_Builder_Create_Operator
                                     (residual_id, 2, kids, 2, attrs, 3);
    DSL_GATEKEEPER_RESULT result;
    return pu != NULL && tensor_ty != TY_IDX_ZERO && input != NULL &&
           residual_id != DSL_OPCODE_INVALID_ID && residual != NULL &&
           DSL_Builder_Append_PU_Value(pu, input) &&
           DSL_Builder_Append_PU_Value(pu, residual) &&
           !DSL_Gatekeeper_Verify_Program(pu, NULL, &result) &&
           result.error_count != 0;
}

static BOOL
Check_Simple_ResNet_Vertical_Slice (BOOL artifact_only)
{
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU
            (artifact_only ? "dsl_simple_resnet_artifact" :
             "dsl_simple_resnet_lowering");
    TY_IDX tensor_ty = Create_Tensor_Type_With_Representation
                           ("dsl_simple_resnet_type", "host", "host");
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("model_input0", tensor_ty, 0);
    DSL_Opcode_Register_Common_Substrate();
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_OPCODE_ID relu_id = DSL_Opcode_Find(common, "common.relu", 2);
    DSL_OPCODE_ID residual_id = DSL_Opcode_Find
                                    (common, "common.residual_add", 2);
    DSL_OPCODE_ID flatten_id = DSL_Opcode_Find
                                   (common, "common.flatten", 2);
    DSL_OPCODE_ID output_id = DSL_Opcode_Find
                                  (common, "common.output_logits", 2);

    DSL_BUILDER_VALUE unary_kid[1] = { input };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator
                                 (relu_id, 2, unary_kid, 1, NULL, 0);
    DSL_BUILDER_VALUE residual_kids[2] = { relu, input };
    DSL_BUILDER_OPERATOR_ATTRIBUTE residual_attrs[3];
    residual_attrs[0].name = "attr.broadcast_rule";
    residual_attrs[0].value = "none";
    residual_attrs[1].name = "attr.shape_check";
    residual_attrs[1].value = "exact";
    residual_attrs[2].name = "attr.residual_path";
    residual_attrs[2].value = "true";
    DSL_BUILDER_VALUE residual = DSL_Builder_Create_Operator
                                     (residual_id, 2, residual_kids, 2,
                                      residual_attrs, 3);
    DSL_BUILDER_VALUE flatten_kid[1] = { residual };
    DSL_BUILDER_OPERATOR_ATTRIBUTE flatten_attrs[2];
    flatten_attrs[0].name = "attr.start_dim";
    flatten_attrs[0].value = "0";
    flatten_attrs[1].name = "attr.end_dim";
    flatten_attrs[1].value = "1";
    DSL_BUILDER_VALUE flatten = DSL_Builder_Create_Operator
                                    (flatten_id, 2, flatten_kid, 1,
                                     flatten_attrs, 2);
    DSL_BUILDER_VALUE output_kid[1] = { flatten };
    DSL_BUILDER_OPERATOR_ATTRIBUTE output_attr;
    output_attr.name = "attr.semantic";
    output_attr.value = "logits";
    DSL_BUILDER_VALUE output = DSL_Builder_Create_Operator
                                   (output_id, 2, output_kid, 1,
                                    &output_attr, 1);

    if (pu == NULL || tensor_ty == TY_IDX_ZERO || input == NULL ||
        relu_id == DSL_OPCODE_INVALID_ID ||
        residual_id == DSL_OPCODE_INVALID_ID ||
        flatten_id == DSL_OPCODE_INVALID_ID ||
        output_id == DSL_OPCODE_INVALID_ID || relu == NULL ||
        residual == NULL || flatten == NULL || output == NULL ||
        TY_tensor_rank(WN_ty(flatten)) != 1 ||
        strcmp(TY_tensor_attribute
                   (WN_ty(flatten), TY_TENSOR_SCHEMA_SHAPE), "[4]") != 0) {
        fprintf(stderr, "simple ResNet construction or flatten shape failed\n");
        return FALSE;
    }

    DSL_BUILDER_VALUE values[5] = {
        input, relu, residual, flatten, output
    };
    for (UINT32 i = 0; i < 5; ++i) {
        if (!DSL_Builder_Append_PU_Value(pu, values[i])) {
            fprintf(stderr, "simple ResNet PU append %u failed\n", i);
            return FALSE;
        }
    }

    DSL_IR_IMAGE_HEADER header;
    DSL_IR_Image_Get_Header(&header);
    if (header.opcode_descriptor_count != 5 || header.node_count != 5 ||
        header.attribute_count != 7 || header.value_count != 5 ||
        header.value_reference_count != 5 ||
        !DSL_IR_Image_Validate(stderr)) {
        fprintf(stderr,
                "simple ResNet image counts are %u/%u/%u/%u/%u\n",
                header.opcode_descriptor_count, header.node_count,
                header.attribute_count, header.value_count,
                header.value_reference_count);
        return FALSE;
    }

    FILE *dump = tmpfile();
    if (dump == NULL)
        return FALSE;
    fdump_tree(dump, PU_Info_tree_ptr(pu));
    DSL_IR_Image_Print(dump);
    rewind(dump);
    char dump_text[32768];
    size_t dump_size = fread(dump_text, 1, sizeof(dump_text) - 1, dump);
    dump_text[dump_size] = '\0';
    fclose(dump);
    if (strstr(dump_text, "OPR_DSLRELU") == NULL ||
        strstr(dump_text, "OPR_DSLRESIDUALADD") == NULL ||
        strstr(dump_text, "OPR_DSLFLATTEN") == NULL ||
        strstr(dump_text, "OPR_DSLOUTPUTLOGITS") == NULL ||
        strstr(dump_text, "stable_name=common.relu version=2") == NULL ||
        strstr(dump_text, "stable_name=common.residual_add version=2") ==
            NULL ||
        strstr(dump_text, "stable_name=common.flatten version=2") == NULL ||
        strstr(dump_text, "stable_name=common.output_logits version=2") ==
            NULL ||
        strstr(dump_text, "shape=[4]") == NULL ||
        strstr(dump_text, "no_alias=true") == NULL ||
        strstr(dump_text, "OPR_DSL ") != NULL ||
        strstr(dump_text, "MDSL ") != NULL) {
        fprintf(stderr, "simple ResNet logical dump evidence is incomplete\n");
        return FALSE;
    }

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    if (!DSL_Gatekeeper_Verify_Program(pu, stderr, &gatekeeper_result) ||
        gatekeeper_result.native_node_count != 5 ||
        gatekeeper_result.error_count != 0) {
        fprintf(stderr, "simple ResNet gatekeeper counts are invalid\n");
        return FALSE;
    }

    if (artifact_only) {
        const char *retained_artifact =
            getenv("OPEN64_DSL_M2_TEST_ARTIFACT");
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
        request.path = retained_artifact == NULL ||
                       retained_artifact[0] == '\0' ?
                       "simple_resnet_contract_test.B" : retained_artifact;
        request.flags = 0;
        (void)unlink(request.path);
        if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
            access(request.path, F_OK) != 0) {
            fprintf(stderr, "simple ResNet mapped-image output failed\n");
            return FALSE;
        }
        if (retained_artifact == NULL || retained_artifact[0] == '\0')
            (void)unlink(request.path);
        return TRUE;
    }

    VHO_DSL_LOWER_RESULT lower_result;
    WN *tree = PU_Info_tree_ptr(pu);
    if (!VHO_DSL_Lower_Verified_Program_Unit
             (pu, tree, stderr, &lower_result) ||
        lower_result.native_node_count != 5 ||
        lower_result.lowered_node_count != 5 ||
        lower_result.remaining_executable_carrier_count != 0) {
        fprintf(stderr, "simple ResNet lowering counts are invalid\n");
        return FALSE;
    }

    const char *expected_calls[] = {
        "__open64_dsl_model_input_v1",
        "__open64_dsl_relu_v1",
        "__open64_dsl_residual_add_v1",
        "__open64_dsl_flatten_v1",
        "__open64_dsl_output_logits_v1"
    };
    const UINT32 expected_parameters[] = { 2, 2, 3, 4, 3 };
    UINT32 call_count = 0;
    UINT32 capture_count = 0;
    UINT32 comment_count = 0;
    WN *body = WN_func_body(tree);
    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_COMMENT) {
            ++comment_count;
        } else if (WN_operator(statement) == OPR_CALL) {
            if (call_count >= 5 ||
                strcmp(ST_name(WN_st(statement)),
                       expected_calls[call_count]) != 0 ||
                WN_kid_count(statement) !=
                    expected_parameters[call_count]) {
                fprintf(stderr, "simple ResNet runtime call %u is invalid\n",
                        call_count);
                return FALSE;
            }
            ++call_count;
        } else if (WN_operator(statement) == OPR_STID &&
                   ST_class(WN_st(statement)) == CLASS_PREG) {
            ++capture_count;
        } else {
            fprintf(stderr, "simple ResNet lowered body has unexpected %s\n",
                    OPERATOR_name(WN_operator(statement)));
            return FALSE;
        }
    }
    return call_count == 5 && capture_count == 5 && comment_count == 5;
}

static DSL_BUILDER_VALUE
Create_ResNet_External_Tensor
        (const char *name,
         TY_IDX tensor_ty,
         const char *tensor_key,
         UINT64 byte_offset,
         UINT64 byte_length)
{
    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE reference;
    memset(&reference, 0, sizeof(reference));
    reference.storage_format = "safetensors";
    reference.side_file = "resnet_parameters.safetensors";
    reference.tensor_key = tensor_key;
    reference.byte_offset = byte_offset;
    reference.byte_length = byte_length;
    return DSL_Builder_Create_External_Tensor_Constant
               (name, tensor_ty, &reference);
}

static BOOL
Check_Complete_ResNet_Vertical_Slice (BOOL artifact_only)
{
    DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU
        (artifact_only ? "dsl_complete_resnet_artifact" :
         "dsl_complete_resnet_lowering");
    TY_IDX input_ty = Create_Shaped_Tensor_Type
        ("resnet_input_type", "float32", MTYPE_To_TY(MTYPE_F4), 4,
         "[1,3,8,8]", "host", "host");
    TY_IDX conv_weight_ty = Create_Shaped_Tensor_Type
        ("resnet_conv_weight_type", "float32", MTYPE_To_TY(MTYPE_F4), 4,
         "[4,3,3,3]", "side_file", "external_data");
    TY_IDX channel_ty = Create_Shaped_Tensor_Type
        ("resnet_channel_type", "float32", MTYPE_To_TY(MTYPE_F4), 1,
         "[4]", "side_file", "external_data");
    TY_IDX linear_weight_ty = Create_Shaped_Tensor_Type
        ("resnet_linear_weight_type", "float32", MTYPE_To_TY(MTYPE_F4), 2,
         "[10,4]", "side_file", "external_data");
    TY_IDX linear_bias_ty = Create_Shaped_Tensor_Type
        ("resnet_linear_bias_type", "float32", MTYPE_To_TY(MTYPE_F4), 1,
         "[10]", "side_file", "external_data");
    if (pu == NULL || input_ty == TY_IDX_ZERO ||
        conv_weight_ty == TY_IDX_ZERO || channel_ty == TY_IDX_ZERO ||
        linear_weight_ty == TY_IDX_ZERO || linear_bias_ty == TY_IDX_ZERO) {
        fprintf(stderr, "complete ResNet tensor type construction failed\n");
        return FALSE;
    }

    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("model_input0", input_ty, 0);
    DSL_BUILDER_VALUE conv_weight = Create_ResNet_External_Tensor
        ("conv_weight", conv_weight_ty, "conv.weight", 0, 432);
    DSL_BUILDER_VALUE conv_bias = Create_ResNet_External_Tensor
        ("conv_bias", channel_ty, "conv.bias", 432, 16);
    DSL_BUILDER_VALUE bn_scale = Create_ResNet_External_Tensor
        ("bn_scale", channel_ty, "bn.weight", 448, 16);
    DSL_BUILDER_VALUE bn_bias = Create_ResNet_External_Tensor
        ("bn_bias", channel_ty, "bn.bias", 464, 16);
    DSL_BUILDER_VALUE bn_mean = Create_ResNet_External_Tensor
        ("bn_mean", channel_ty, "bn.running_mean", 480, 16);
    DSL_BUILDER_VALUE bn_variance = Create_ResNet_External_Tensor
        ("bn_variance", channel_ty, "bn.running_variance", 496, 16);
    DSL_BUILDER_VALUE linear_weight = Create_ResNet_External_Tensor
        ("linear_weight", linear_weight_ty, "fc.weight", 512, 160);
    DSL_BUILDER_VALUE linear_bias = Create_ResNet_External_Tensor
        ("linear_bias", linear_bias_ty, "fc.bias", 672, 40);
    if (input == NULL || conv_weight == NULL || conv_bias == NULL ||
        bn_scale == NULL || bn_bias == NULL || bn_mean == NULL ||
        bn_variance == NULL || linear_weight == NULL || linear_bias == NULL) {
        fprintf(stderr, "complete ResNet value source construction failed\n");
        return FALSE;
    }

    DSL_Opcode_Register_Common_Substrate();
    DSL_Opcode_Register_Domain_Wrapper_Examples();
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_DOMAIN_ID cnn = DSL_Domain_Find("cnn");
    DSL_OPCODE_ID conv_id = DSL_Opcode_Find(cnn, "cnn.conv2d", 2);
    DSL_OPCODE_ID batch_norm_id = DSL_Opcode_Find
                                      (cnn, "cnn.batch_norm_infer", 2);
    DSL_OPCODE_ID relu_id = DSL_Opcode_Find(common, "common.relu", 2);
    DSL_OPCODE_ID residual_id = DSL_Opcode_Find
                                    (common, "common.residual_add", 2);
    DSL_OPCODE_ID max_pool_id = DSL_Opcode_Find
                                    (cnn, "cnn.max_pool2d", 2);
    DSL_OPCODE_ID global_pool_id = DSL_Opcode_Find
                                       (cnn, "cnn.global_avg_pool2d", 2);
    DSL_OPCODE_ID flatten_id = DSL_Opcode_Find
                                   (common, "common.flatten", 2);
    DSL_OPCODE_ID linear_id = DSL_Opcode_Find
                                  (common, "common.linear", 2);
    DSL_OPCODE_ID output_id = DSL_Opcode_Find
                                  (common, "common.output_logits", 2);

    DSL_BUILDER_OPERATOR_ATTRIBUTE conv_attrs[8] = {
        { "attr.kernel_shape", "3,3" },
        { "attr.stride", "1,1" },
        { "attr.padding", "1,1" },
        { "attr.dilation", "1,1" },
        { "attr.groups", "1" },
        { "attr.input_layout", "NCHW" },
        { "attr.weight_layout", "OIHW" },
        { "attr.output_layout", "NCHW" }
    };
    DSL_BUILDER_VALUE conv_kids[3] = {
        input, conv_weight, conv_bias
    };
    DSL_BUILDER_VALUE conv = DSL_Builder_Create_Operator
        (conv_id, 2, conv_kids, 3, conv_attrs, 8);

    DSL_BUILDER_OPERATOR_ATTRIBUTE batch_norm_attrs[4] = {
        { "attr.epsilon", "0.00001" },
        { "attr.training", "false" },
        { "attr.input_layout", "NCHW" },
        { "attr.channel_axis", "1" }
    };
    DSL_BUILDER_VALUE batch_norm_kids[5] = {
        conv, bn_scale, bn_bias, bn_mean, bn_variance
    };
    DSL_BUILDER_VALUE batch_norm = DSL_Builder_Create_Operator
        (batch_norm_id, 2, batch_norm_kids, 5, batch_norm_attrs, 4);

    DSL_BUILDER_VALUE unary_kid[1] = { batch_norm };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator
        (relu_id, 2, unary_kid, 1, NULL, 0);
    DSL_BUILDER_VALUE residual_kids[2] = { relu, batch_norm };
    DSL_BUILDER_OPERATOR_ATTRIBUTE residual_attrs[3] = {
        { "attr.broadcast_rule", "none" },
        { "attr.shape_check", "exact" },
        { "attr.residual_path", "true" }
    };
    DSL_BUILDER_VALUE residual = DSL_Builder_Create_Operator
        (residual_id, 2, residual_kids, 2, residual_attrs, 3);

    DSL_BUILDER_VALUE max_pool_kid[1] = { residual };
    DSL_BUILDER_OPERATOR_ATTRIBUTE max_pool_attrs[5] = {
        { "attr.kernel_shape", "2,2" },
        { "attr.stride", "2,2" },
        { "attr.padding", "0,0" },
        { "attr.dilation", "1,1" },
        { "attr.ceil_mode", "false" }
    };
    DSL_BUILDER_VALUE max_pool = DSL_Builder_Create_Operator
        (max_pool_id, 2, max_pool_kid, 1, max_pool_attrs, 5);

    DSL_BUILDER_VALUE global_pool_kid[1] = { max_pool };
    DSL_BUILDER_OPERATOR_ATTRIBUTE global_pool_attrs[2] = {
        { "attr.output_size", "1,1" },
        { "attr.reduction_axes", "spatial" }
    };
    DSL_BUILDER_VALUE global_pool = DSL_Builder_Create_Operator
        (global_pool_id, 2, global_pool_kid, 1, global_pool_attrs, 2);

    DSL_BUILDER_VALUE flatten_kid[1] = { global_pool };
    DSL_BUILDER_OPERATOR_ATTRIBUTE flatten_attrs[2] = {
        { "attr.start_dim", "1" },
        { "attr.end_dim", "-1" }
    };
    DSL_BUILDER_VALUE flatten = DSL_Builder_Create_Operator
        (flatten_id, 2, flatten_kid, 1, flatten_attrs, 2);

    DSL_BUILDER_VALUE linear_kids[3] = {
        flatten, linear_weight, linear_bias
    };
    DSL_BUILDER_OPERATOR_ATTRIBUTE linear_attrs[4] = {
        { "attr.has_bias", "true" },
        { "attr.transpose_input", "false" },
        { "attr.transpose_weight", "true" },
        { "attr.weight_layout", "OI" }
    };
    DSL_BUILDER_VALUE linear = DSL_Builder_Create_Operator
        (linear_id, 2, linear_kids, 3, linear_attrs, 4);

    DSL_BUILDER_VALUE output_kid[1] = { linear };
    DSL_BUILDER_OPERATOR_ATTRIBUTE output_attr = {
        "attr.semantic", "logits"
    };
    DSL_BUILDER_VALUE output = DSL_Builder_Create_Operator
        (output_id, 2, output_kid, 1, &output_attr, 1);

    if (conv_id == DSL_OPCODE_INVALID_ID ||
        batch_norm_id == DSL_OPCODE_INVALID_ID ||
        relu_id == DSL_OPCODE_INVALID_ID ||
        residual_id == DSL_OPCODE_INVALID_ID ||
        max_pool_id == DSL_OPCODE_INVALID_ID ||
        global_pool_id == DSL_OPCODE_INVALID_ID ||
        flatten_id == DSL_OPCODE_INVALID_ID ||
        linear_id == DSL_OPCODE_INVALID_ID ||
        output_id == DSL_OPCODE_INVALID_ID || conv == NULL ||
        batch_norm == NULL || relu == NULL || residual == NULL ||
        max_pool == NULL || global_pool == NULL || flatten == NULL ||
        linear == NULL || output == NULL ||
        strcmp(TY_tensor_attribute
                   (WN_ty(conv), TY_TENSOR_SCHEMA_SHAPE), "[1,4,8,8]") != 0 ||
        strcmp(TY_tensor_attribute
                   (WN_ty(max_pool), TY_TENSOR_SCHEMA_SHAPE), "[1,4,4,4]") != 0 ||
        strcmp(TY_tensor_attribute
                   (WN_ty(global_pool), TY_TENSOR_SCHEMA_SHAPE),
               "[1,4,1,1]") != 0 ||
        strcmp(TY_tensor_attribute
                   (WN_ty(flatten), TY_TENSOR_SCHEMA_SHAPE), "[1,4]") != 0 ||
        strcmp(TY_tensor_attribute
                   (WN_ty(linear), TY_TENSOR_SCHEMA_SHAPE), "[1,10]") != 0) {
        fprintf(stderr,
                "complete ResNet operator construction failed: "
                "ids=%u/%u/%u/%u/%u/%u/%u/%u/%u "
                "values=%p/%p/%p/%p/%p/%p/%p/%p/%p\n",
                conv_id, batch_norm_id, relu_id, residual_id, max_pool_id,
                global_pool_id, flatten_id, linear_id, output_id,
                conv, batch_norm, relu, residual, max_pool, global_pool,
                flatten, linear, output);
        return FALSE;
    }

    DSL_BUILDER_VALUE values[18] = {
        input, conv_weight, conv_bias, bn_scale, bn_bias, bn_mean,
        bn_variance, linear_weight, linear_bias, conv, batch_norm, relu,
        residual, max_pool, global_pool, flatten, linear, output
    };
    for (UINT32 i = 0; i < 18; ++i) {
        if (!DSL_Builder_Append_PU_Value(pu, values[i])) {
            fprintf(stderr, "complete ResNet PU append %u failed\n", i);
            return FALSE;
        }
    }

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    TY_tensor_bind_attribute
        (WN_ty(conv), TY_TENSOR_SCHEMA_SHAPE, "[1,4,7,7]");
    if (DSL_Gatekeeper_Verify_Program
            (pu, NULL, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "invalid convolution result shape was accepted\n");
        return FALSE;
    }
    TY_tensor_bind_attribute
        (WN_ty(conv), TY_TENSOR_SCHEMA_SHAPE, "[1,4,8,8]");
    if (!DSL_Gatekeeper_Verify_Program(pu, stderr, &gatekeeper_result) ||
        gatekeeper_result.native_node_count != 18 ||
        gatekeeper_result.error_count != 0)
        return FALSE;

    FILE *dump = tmpfile();
    if (dump == NULL)
        return FALSE;
    fdump_tree(dump, PU_Info_tree_ptr(pu));
    DSL_IR_Image_Print(dump);
    rewind(dump);
    char dump_text[65536];
    size_t dump_size = fread(dump_text, 1, sizeof(dump_text) - 1, dump);
    dump_text[dump_size] = '\0';
    fclose(dump);
    const char *dump_evidence[] = {
        "OPR_DSLCONV2D", "OPR_DSLBATCHNORMINFER", "OPR_DSLMAXPOOL2D",
        "OPR_DSLGLOBALAVGPOOL2D", "OPR_DSLLINEAR",
        "stable_name=cnn.conv2d version=2",
        "stable_name=cnn.batch_norm_infer version=2",
        "stable_name=common.linear version=2", "shape=[1,10]",
        "no_alias=true"
    };
    for (UINT32 i = 0;
         i < sizeof(dump_evidence) / sizeof(dump_evidence[0]); ++i) {
        if (strstr(dump_text, dump_evidence[i]) == NULL)
            return FALSE;
    }
    if (strstr(dump_text, "OPR_DSL ") != NULL ||
        strstr(dump_text, "MDSL ") != NULL)
        return FALSE;

    if (artifact_only) {
        const char *retained_artifact =
            getenv("OPEN64_DSL_RESNET_TEST_ARTIFACT");
        DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
        request.path = retained_artifact == NULL ||
                       retained_artifact[0] == '\0' ?
                       "complete_resnet_contract_test.B" : retained_artifact;
        request.flags = 0;
        (void)unlink(request.path);
        if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
            access(request.path, F_OK) != 0)
            return FALSE;
        if (retained_artifact == NULL || retained_artifact[0] == '\0')
            (void)unlink(request.path);
        return TRUE;
    }

    VHO_DSL_LOWER_RESULT lower_result;
    if (!VHO_DSL_Lower_Verified_Program_Unit
             (pu, PU_Info_tree_ptr(pu), stderr, &lower_result) ||
        lower_result.native_node_count != 18 ||
        lower_result.lowered_node_count != 18 ||
        lower_result.remaining_executable_carrier_count != 0)
        return FALSE;
    const char *expected_calls[18] = {
        "__open64_dsl_model_input_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_external_tensor_v1",
        "__open64_dsl_conv2d_v1",
        "__open64_dsl_batch_norm_infer_v1",
        "__open64_dsl_relu_v1",
        "__open64_dsl_residual_add_v1",
        "__open64_dsl_max_pool2d_v1",
        "__open64_dsl_global_avg_pool2d_v1",
        "__open64_dsl_flatten_v1",
        "__open64_dsl_linear_v1",
        "__open64_dsl_output_logits_v1"
    };
    const UINT32 expected_parameters[18] = {
        2, 2, 2, 2, 2, 2, 2, 2, 2, 16, 9, 2, 3, 11, 5, 4, 6, 3
    };
    UINT32 call_count = 0;
    UINT32 capture_count = 0;
    UINT32 comment_count = 0;
    WN *body = WN_func_body(PU_Info_tree_ptr(pu));
    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_COMMENT) {
            ++comment_count;
        } else if (WN_operator(statement) == OPR_CALL) {
            if (call_count >= 18 ||
                strcmp(ST_name(WN_st(statement)),
                       expected_calls[call_count]) != 0 ||
                WN_kid_count(statement) != expected_parameters[call_count])
                return FALSE;
            ++call_count;
        } else if (WN_operator(statement) == OPR_STID &&
                   ST_class(WN_st(statement)) == CLASS_PREG) {
            ++capture_count;
        } else {
            return FALSE;
        }
    }
    return call_count == 18 && capture_count == 18 && comment_count == 18;
}

int
main(void)
{
    PU_Info empty_pu;
    VHO_DSL_LOWER_RESULT result;
    memset(&empty_pu, 0, sizeof(empty_pu));

    Initialize_Test_Context();
    WN *baseline = WN_CreateBlock();
    if (!VHO_DSL_Lower_Verified_Program_Unit
             (&empty_pu, baseline, NULL, &result) ||
        result.native_node_count != 0 ||
        result.lowered_node_count != 0 ||
        result.unsupported_node_count != 0) {
        fprintf(stderr, "DSL lowerer changed baseline WHIRL behavior\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_lower_contract");
    TY_IDX tensor_ty = Create_Tensor_Type();
    DSL_BUILDER_VALUE tensor_zero = DSL_Builder_Create_Tensor_Constant
        ("tensor_zero", tensor_ty, "int32", 2, "[2,2]", "splat", "0");
    DSL_BUILDER_VALUE tensor_one = DSL_Builder_Create_Tensor_Constant
        ("tensor_one", tensor_ty, "int32", 2, "[2,2]", "splat", "1");

    DSL_Opcode_Register_Common_Substrate();
    DSL_DOMAIN_ID common = DSL_Domain_Find("common");
    DSL_OPCODE_ID add_id = DSL_Opcode_Find(common, DSL_OPCODE_COMMON_ADD, 1);
    DSL_OPCODE_ID matmul_id = DSL_Opcode_Find
                                  (common, DSL_OPCODE_COMMON_MATMUL, 1);
    DSL_BUILDER_VALUE operands[2] = { tensor_zero, tensor_one };
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator
        (add_id, 1, operands, 2, &attribute, 1);
    DSL_BUILDER_OPERATOR_ATTRIBUTE matmul_attributes[2];
    matmul_attributes[0].name = "attr.transpose_kid0";
    matmul_attributes[0].value = "false";
    matmul_attributes[1].name = "attr.transpose_kid1";
    matmul_attributes[1].value = "false";
    DSL_BUILDER_VALUE matmul = DSL_Builder_Create_Operator
        (matmul_id, 1, operands, 2, matmul_attributes, 2);

    if (pu == NULL || tensor_ty == TY_IDX_ZERO || tensor_zero == NULL ||
        tensor_one == NULL || add == NULL || matmul == NULL ||
        !DSL_Builder_Append_PU_Value(pu, tensor_zero) ||
        !DSL_Builder_Append_PU_Value(pu, tensor_one) ||
        !DSL_Builder_Append_PU_Value(pu, add) ||
        !DSL_Builder_Append_PU_Value(pu, matmul)) {
        fprintf(stderr, "could not construct native DSL lowering fixture\n");
        return 1;
    }

    WN *tree = PU_Info_tree_ptr(pu);
    FILE *diagnostic = tmpfile();
    BOOL valid = VHO_DSL_Lower_Verified_Program_Unit
                     (pu, tree, diagnostic, &result);
    if (diagnostic != NULL)
        fclose(diagnostic);

    char native_signature[1024];
    if (!valid || result.native_node_count != 4 ||
        result.compatibility_node_count != 0 ||
        result.lowered_node_count != 4 ||
        result.unsupported_node_count != 0 ||
        result.malformed_node_count != 0 ||
        result.remaining_executable_carrier_count != 0 ||
        !Verify_Lowered_Body(WN_func_body(tree), native_signature,
                             sizeof(native_signature))) {
        fprintf(stderr, "native DSL topological lowering contract changed\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT compatibility_pu = Create_Compatibility_PU();
    if (compatibility_pu == NULL) {
        fprintf(stderr, "could not construct compatibility lowering fixture\n");
        return 1;
    }
    WN *compatibility_tree = PU_Info_tree_ptr(compatibility_pu);
    valid = VHO_DSL_Lower_Verified_Program_Unit
                (compatibility_pu, compatibility_tree, NULL, &result);
    char compatibility_signature[1024];
    if (!valid || result.native_node_count != 4 ||
        result.compatibility_node_count != 4 ||
        result.lowered_node_count != 4 ||
        result.unsupported_node_count != 0 ||
        result.malformed_node_count != 0 ||
        result.remaining_executable_carrier_count != 0 ||
        !Verify_Lowered_Body
             (WN_func_body(compatibility_tree), compatibility_signature,
              sizeof(compatibility_signature)) ||
        strcmp(native_signature, compatibility_signature) != 0) {
        fprintf(stderr,
                "native and compatibility lowering signatures differ\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT malformed_pu = DSL_Builder_Create_Minimal_PU
        ("dsl_lower_malformed_compatibility");
    WN *malformed_add = DSL_WN_Create_Opcode_Marker
        (DSL_OPCODE_COMMON_ADD, 1,
         "kid0=missing;kid1=also_missing;attr.broadcast_rule=none");
    if (malformed_pu == NULL || malformed_add == NULL) {
        fprintf(stderr, "could not construct malformed compatibility fixture\n");
        return 1;
    }
    WN_INSERT_BlockLast
        (WN_func_body(PU_Info_tree_ptr(malformed_pu)), malformed_add);
    if (VHO_DSL_Lower_Verified_Program_Unit
            (malformed_pu, PU_Info_tree_ptr(malformed_pu), NULL, &result) ||
        result.compatibility_node_count != 0 ||
        result.malformed_node_count == 0) {
        fprintf(stderr, "malformed compatibility input was not rejected\n");
        return 1;
    }

    const char *newer_payload =
        "name=newer_tensor;dtype=int32;rank=2;shape=[2,2];"
        "value_kind=splat;value=0";
    DSL_BUILDER_PROGRAM_UNIT newer_pu = DSL_Builder_Create_Minimal_PU
        ("dsl_lower_newer_compatibility");
    WN *newer = DSL_WN_Create_Opcode_Marker
        (DSL_OPCODE_COMMON_TENSOR_CONST, 2, newer_payload);
    if (newer_pu == NULL || newer == NULL) {
        fprintf(stderr, "could not construct newer compatibility fixture\n");
        return 1;
    }
    WN_INSERT_BlockLast(WN_func_body(PU_Info_tree_ptr(newer_pu)), newer);
    if (VHO_DSL_Lower_Verified_Program_Unit
            (newer_pu, PU_Info_tree_ptr(newer_pu), NULL, &result) ||
        result.malformed_node_count == 0) {
        fprintf(stderr, "newer compatibility version was not rejected\n");
        return 1;
    }

    if (!Check_Canonical_Boundary_Rejection()) {
        fprintf(stderr, "executable DSL carrier boundary check changed\n");
        return 1;
    }

    if (!Check_Value_Source_Gatekeeper_Rejection()) {
        fprintf(stderr, "native DSL value-source gatekeeper changed\n");
        return 1;
    }

    if (!Check_Value_Source_Lowering()) {
        fprintf(stderr, "native DSL value-source lowering contract changed\n");
        return 1;
    }

    if (!Check_Simple_ResNet_Gatekeeper_Rejection()) {
        fprintf(stderr, "simple ResNet gatekeeper contract changed\n");
        return 1;
    }

    if (!Check_Simple_ResNet_Vertical_Slice(FALSE)) {
        fprintf(stderr, "simple ResNet lowering contract changed\n");
        return 1;
    }

    if (!Check_Complete_ResNet_Vertical_Slice(FALSE)) {
        fprintf(stderr, "complete ResNet lowering contract changed\n");
        return 1;
    }

    /* Mapped-image finalization is terminal for this producer-style fixture. */
    if (!Check_Complete_ResNet_Vertical_Slice(TRUE)) {
        fprintf(stderr, "complete ResNet artifact contract changed\n");
        return 1;
    }

    return 0;
}
