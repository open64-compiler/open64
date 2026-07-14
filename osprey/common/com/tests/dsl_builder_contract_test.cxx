/*
 * Native contract test for the minimal DSL builder boundary.
 *
 * This deliberately exercises the future Python-facing construction surface
 * without adding Python bindings or backend lowering dependencies.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "elf_stuff.h"
#include <sys/elf_whirl.h>
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "srcpos.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"

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

static int
Check_Tensor_Type_And_Descriptor(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    TY_IDX tensor_ty;
    TENSOR_DESCRIPTOR_RECORD record;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    memset(&descriptor, 0, sizeof(descriptor));

    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core("builder_tensor_type",
                                                    MTYPE_To_TY(MTYPE_I4),
                                                    &type_core);

    if (TY_kind(tensor_ty) != KIND_TENSOR ||
        !TY_is_tensor_extension(tensor_ty) ||
        TY_tensor_element_ty(tensor_ty) != MTYPE_To_TY(MTYPE_I4) ||
        TY_tensor_rank(tensor_ty) != 2) {
        fprintf(stderr, "builder did not create a native tensor type core\n");
        failed = 1;
    }

    descriptor.type_core = type_core;
    descriptor.traits.traits = "dense";
    descriptor.representation.layout = "row_major";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";
    descriptor.representation.runtime_state = "static";
    descriptor.lineage.lineage = "builder_contract_test";

    if (!DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor)) {
        fprintf(stderr, "builder failed to attach tensor descriptor\n");
        failed = 1;
    }

    if (!TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_DTYPE) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_DTYPE),
               "int32") != 0 ||
        !TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_LAYOUT),
               "row_major") != 0 ||
        !TY_tensor_attribute_is_bound(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE) ||
        strcmp(TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_LINEAGE),
               "builder_contract_test") != 0) {
        fprintf(stderr, "builder tensor descriptor attributes changed\n");
        failed = 1;
    }

    TENSOR_DESCRIPTOR_RECORD_Init(&record);
    if (!TY_get_tensor_descriptor_record(tensor_ty, &record) ||
        record.rank != 2 ||
        record.attribute_count != TY_tensor_attribute_count(tensor_ty)) {
        fprintf(stderr, "builder tensor descriptor record is inconsistent\n");
        failed = 1;
    }

    if (DSL_Builder_Attach_Tensor_Descriptor(TY_IDX_ZERO, &descriptor)) {
        fprintf(stderr, "builder accepted descriptor for non-tensor type\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Upgraded_Ingestion_APIs(void)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR different_descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR observed;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    DSL_BUILDER_COMPILER_METADATA metadata;
    DSL_BUILDER_SOURCE_POSITION source_position;
    DSL_BUILDER_VERIFY_RESULT verify_result;
    DSL_BUILDER_VALUE kids[2];
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    TY_IDX tensor_ty;
    TY_IDX duplicate_ty;
    TY_IDX contextual_duplicate_ty;
    TY_IDX different_ty;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE add;
    ST_IDX add_st;
    UINT32 file_id;
    char diagnostic[1024];
    USRCPOS observed_position;
    int failed = 0;

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "int32";
    descriptor.type_core.rank = 2;
    descriptor.type_core.logical_shape = "[2,2]";
    descriptor.traits.traits = "activation";
    descriptor.representation.layout = "row_major";
    descriptor.representation.sharding = "replicated";
    descriptor.representation.placement = "host";
    descriptor.representation.memory = "contiguous";
    descriptor.representation.quantization = "none";
    descriptor.representation.runtime_state = "static";
    descriptor.lineage.lineage = "canonical_contract";

    tensor_ty = DSL_Builder_Intern_Tensor_Type
                    ("canonical_tensor_a", MTYPE_To_TY(MTYPE_I4),
                     &descriptor);
    duplicate_ty = DSL_Builder_Intern_Tensor_Type
                       ("canonical_tensor_b", MTYPE_To_TY(MTYPE_I4),
                        &descriptor);
    different_descriptor = descriptor;
    different_descriptor.representation.runtime_state = "dynamic";
    different_descriptor.lineage.lineage = "different_value_lineage";
    contextual_duplicate_ty = DSL_Builder_Intern_Tensor_Type
                                  ("canonical_tensor_contextual",
                                   MTYPE_To_TY(MTYPE_I4),
                                   &different_descriptor);
    different_descriptor = descriptor;
    different_descriptor.type_core.logical_shape = "[4,2]";
    different_ty = DSL_Builder_Intern_Tensor_Type
                       ("canonical_tensor_c", MTYPE_To_TY(MTYPE_I4),
                        &different_descriptor);

    if (tensor_ty == TY_IDX_ZERO || duplicate_ty != tensor_ty ||
        contextual_duplicate_ty != tensor_ty ||
        different_ty == TY_IDX_ZERO || different_ty == tensor_ty ||
        !DSL_Builder_Tensor_Type_Is_Canonical(tensor_ty) ||
        !TY_tensor_attributes_are_equivalent(tensor_ty, duplicate_ty) ||
        TY_tensor_attributes_are_equivalent(tensor_ty, different_ty)) {
        fprintf(stderr, "canonical tensor interning contract changed\n");
        failed = 1;
    }

    memset(&observed, 0, sizeof(observed));
    if (!DSL_Builder_Get_Tensor_Descriptor(tensor_ty, &observed) ||
        observed.type_core.rank != 2 ||
        observed.type_core.logical_shape == NULL ||
        strcmp(observed.type_core.logical_shape, "[2,2]") != 0 ||
        observed.representation.runtime_state != NULL ||
        observed.lineage.lineage != NULL ||
        DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor)) {
        fprintf(stderr, "canonical tensor descriptor query changed\n");
        failed = 1;
    }

    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    pu = DSL_Builder_Create_Minimal_PU("upgraded_ingestion_contract");
    file_id = DSL_Builder_Register_Source_File(pu, "model.py");

    kids[0] = DSL_Builder_Create_Model_Input("input0", tensor_ty, 0);
    kids[1] = DSL_Builder_Create_Model_Input("input1", tensor_ty, 1);
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    add = DSL_Builder_Create_Operator_With_Result
              (add_id, 1, kids, 2, &attribute, 1, "explicit_add", tensor_ty);
    add_st = DSL_Builder_Get_Value_Result_Symbol(add);

    metadata.name = "source_layer_name";
    metadata.value = "residual_add";
    memset(&source_position, 0, sizeof(source_position));
    source_position.file_id = file_id;
    source_position.line = 27;
    source_position.column = 9;
    source_position.statement_begin = 1;

    if (pu == NULL || file_id == 0 || kids[0] == NULL || kids[1] == NULL ||
        add == NULL || DSL_Builder_Get_Value_Type(add) != tensor_ty ||
        ST_IDX_index(add_st) == 0 ||
        !DSL_Builder_Attach_Value_Metadata(add, &metadata, 1) ||
        !DSL_Builder_Attach_Value_Lineage(add, "residual_path") ||
        !DSL_Builder_Set_Value_Source_Position(add, &source_position)) {
        fprintf(stderr, "upgraded value-oriented builder API failed\n");
        failed = 1;
    }

    observed_position.srcpos = WN_Get_Linenum(add);
    if (USRCPOS_filenum(observed_position) != file_id ||
        USRCPOS_linenum(observed_position) != 27 ||
        USRCPOS_column(observed_position) != 9 ||
        !USRCPOS_stmt_begin(observed_position) ||
        !ST_tensor_metadata_is_bound(add_st, "source_layer_name") ||
        strcmp(ST_tensor_metadata(add_st, "source_layer_name"),
               "residual_add") != 0 ||
        !ST_tensor_attribute_is_bound
             (add_st,
              TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LINEAGE)) ||
        strcmp(ST_tensor_attribute
                   (add_st,
                    TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LINEAGE)),
               "residual_path") != 0) {
        fprintf(stderr, "value source position or metadata changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Append_PU_Value(pu, kids[1]) ||
        !DSL_Builder_Append_PU_Value(pu, add)) {
        fprintf(stderr, "upgraded program construction failed\n");
        failed = 1;
    }

    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (!DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.native_node_count != 3 ||
        verify_result.result_symbol_count != 3 ||
        verify_result.error_count != 0 || diagnostic[0] != '\0') {
        fprintf(stderr, "structured builder verification failed: %s\n",
                diagnostic);
        failed = 1;
    }

    DSL_Builder_Abort_Program();
    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (DSL_Builder_Get_Value_Type(add) != TY_IDX_ZERO ||
        DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.error_count != 1 ||
        strstr(diagnostic, "no program unit") == NULL) {
        fprintf(stderr, "builder abort did not isolate program state\n");
        failed = 1;
    }

    DSL_Builder_Begin_Program();
    pu = DSL_Builder_Create_Minimal_PU("second_ingestion_contract");
    kids[0] = DSL_Builder_Create_Model_Input("second_input", tensor_ty, 0);
    memset(&verify_result, 0, sizeof(verify_result));
    verify_result.diagnostic = diagnostic;
    verify_result.diagnostic_capacity = sizeof(diagnostic);
    if (pu == NULL || kids[0] == NULL ||
        !DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Verify_Program(&verify_result) ||
        verify_result.native_node_count != 1 ||
        verify_result.result_symbol_count != 1 ||
        verify_result.error_count != 0) {
        fprintf(stderr, "second builder program inherited stale state\n");
        failed = 1;
    }
    DSL_Builder_Abort_Program();

    return failed;
}

static int
Check_Symbol_Metadata(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_COMPILER_METADATA metadata[2];
    TY_IDX tensor_ty;
    ST_IDX st;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core("builder_symbol_type",
                                                    MTYPE_To_TY(MTYPE_I4),
                                                    &type_core);
    st = DSL_Builder_Create_Symbol("builder_tensor", tensor_ty, CLASS_VAR,
                                   SCLASS_UGLOBAL, EXPORT_LOCAL);

    metadata[0].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME);
    metadata[0].value = "builder_contract_test";
    metadata[1].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_LOWERING_HINT);
    metadata[1].value = "native_builder";

    if (!DSL_Builder_Attach_Metadata(st, metadata, 2)) {
        fprintf(stderr, "builder failed to attach compiler metadata\n");
        failed = 1;
    }

    if (!ST_tensor_metadata_is_bound(st, TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME) ||
        strcmp(ST_tensor_metadata(st, TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME),
               "builder_contract_test") != 0 ||
        !ST_tensor_metadata_is_bound(st, TY_TENSOR_SCHEMA_LOWERING_HINT) ||
        strcmp(ST_tensor_metadata(st, TY_TENSOR_SCHEMA_LOWERING_HINT),
               "native_builder") != 0) {
        fprintf(stderr, "builder compiler metadata changed\n");
        failed = 1;
    }

    if (DSL_Builder_Attach_Metadata(ST_IDX_ZERO, metadata, 2) ||
        DSL_Builder_Attach_Metadata(st, NULL, 1)) {
        fprintf(stderr, "builder accepted invalid metadata request\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Operator_Creation(void)
{
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_OPCODE_ID matmul_id;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[1];
    DSL_BUILDER_OPERATOR op;
    DSL_BUILDER_OPERATOR matmul;
    DSL_BUILDER_OPERATOR legacy_eval;
    DSL_OPCODE_ANNOTATION annotation;
    DSL_WHIRL_NODE_RECORD op_record;
    DSL_WHIRL_NODE_RECORD marker_record;
    DSL_WHIRL_NODE_RECORD recreated_record;
    DSL_WHIRL_NODE_RECORD projected_record;
    DSL_WHIRL_NODE_RECORD xpragma_record;
    DSL_WHIRL_OPERAND_RECORD operand_record;
    DSL_BUILDER_VALUE_INFO operand_info;
    int failed = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    matmul_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_MATMUL, 1);

    kids[0] = DSL_WN_Create_Tensor_Const("kid_lhs", "int32", 2, "[2,2]",
                                         "splat", "0");
    kids[1] = DSL_WN_Create_Tensor_Const("kid_rhs", "int32", 2, "[2,2]",
                                         "splat", "1");
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "none";

    op = DSL_Builder_Create_Operator(add_id, 1, kids, 2, attrs, 1);

    if (op == NULL || !DSL_WN_Get_Opcode_Annotation(op, &annotation)) {
        fprintf(stderr, "builder failed to create annotated DSL value\n");
        return 1;
    }

    if (WN_operator(op) == OPR_COMMENT ||
        !DSL_WN_Is_Opcode_Node(op) ||
        DSL_WN_Is_Opcode_Marker(op) ||
        DSL_WN_Opcode_Carrier_Kind(op) != DSL_OPCODE_CARRIER_XPRAGMA ||
        strcmp(DSL_WN_Opcode_Carrier_Name
                   (DSL_WN_Opcode_Carrier_Kind(op)),
               "xpragma_lda_string") != 0) {
        fprintf(stderr, "builder did not create a non-eval DSL node\n");
        failed = 1;
    }

    if (WN_operator(op) != OPR_XPRAGMA ||
        WN_kid_count(op) != 1 ||
        WN_kid0(op) == NULL ||
        WN_operator(WN_kid0(op)) != OPR_COMMA ||
        DSL_WN_Opcode_Operand_Count(op) != 2) {
        fprintf(stderr, "builder DSL node carrier shape changed\n");
        failed = 1;
    }

    WN *xpragma_operand_block =
        WN_kid0(op) != NULL && WN_operator(WN_kid0(op)) == OPR_COMMA ?
        WN_kid0(WN_kid0(op)) : NULL;
    if (xpragma_operand_block == NULL ||
        WN_operator(xpragma_operand_block) != OPR_BLOCK ||
        WN_first(xpragma_operand_block) == NULL ||
        WN_operator(WN_first(xpragma_operand_block)) != OPR_XPRAGMA ||
        WN_next(WN_first(xpragma_operand_block)) == NULL ||
        WN_operator(WN_next(WN_first(xpragma_operand_block))) != OPR_XPRAGMA) {
        fprintf(stderr, "migrated operand evidence still uses EVAL\n");
        failed = 1;
    }

    if (WN_operator(kids[0]) != OPR_XPRAGMA ||
        WN_operator(kids[1]) != OPR_XPRAGMA ||
        !DSL_WN_Is_Opcode_Node(kids[0]) ||
        !DSL_WN_Is_Opcode_Node(kids[1]) ||
        DSL_WN_Opcode_Carrier_Kind(kids[0]) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_WN_Opcode_Carrier_Kind(kids[1]) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_WN_Opcode_Operand_Count(kids[0]) != 0 ||
        DSL_WN_Opcode_Operand_Count(kids[1]) != 0) {
        fprintf(stderr, "builder tensor value carrier is not non-eval\n");
        failed = 1;
    }

    matmul = DSL_Builder_Create_Operator(matmul_id, 1, kids, 2, NULL, 0);
    if (matmul == NULL || WN_operator(matmul) != OPR_XPRAGMA ||
        DSL_WN_Opcode_Carrier_Kind(matmul) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        !DSL_WN_Verify_Opcode_Carrier(matmul, NULL)) {
        fprintf(stderr, "builder matmul carrier is not non-eval\n");
        failed = 1;
    }

    WN *matmul_operand_block =
        matmul != NULL && WN_kid0(matmul) != NULL &&
        WN_operator(WN_kid0(matmul)) == OPR_COMMA ?
        WN_kid0(WN_kid0(matmul)) : NULL;
    if (matmul_operand_block == NULL ||
        WN_operator(matmul_operand_block) != OPR_BLOCK ||
        WN_first(matmul_operand_block) == NULL ||
        WN_operator(WN_first(matmul_operand_block)) != OPR_XPRAGMA) {
        fprintf(stderr, "builder matmul operand evidence still uses EVAL\n");
        failed = 1;
    }

    legacy_eval = DSL_WN_Create_Opcode_With_Operands
                      (DSL_OPCODE_COMMON_MATMUL, 1,
                       "kid0=kid_lhs;kid1=kid_rhs", kids, 2);
    WN *eval_operand_block =
        legacy_eval != NULL && WN_kid0(legacy_eval) != NULL &&
        WN_operator(WN_kid0(legacy_eval)) == OPR_COMMA ?
        WN_kid0(WN_kid0(legacy_eval)) : NULL;
    if (legacy_eval == NULL || WN_operator(legacy_eval) != OPR_EVAL ||
        DSL_WN_Opcode_Carrier_Kind(legacy_eval) != DSL_OPCODE_CARRIER_NODE ||
        eval_operand_block == NULL ||
        WN_operator(eval_operand_block) != OPR_BLOCK ||
        WN_first(eval_operand_block) == NULL ||
        WN_operator(WN_first(eval_operand_block)) != OPR_EVAL ||
        !DSL_WN_Decode_Opcode_Operand_Record
             (legacy_eval, 0, &operand_record)) {
        fprintf(stderr, "legacy EVAL operand evidence compatibility changed\n");
        failed = 1;
    }

    if (!DSL_WN_Verify_Opcode_Carrier(op, NULL) ||
        !DSL_WN_Verify_Opcode_Carrier(kids[0], NULL) ||
        !DSL_WN_Verify_Opcode_Carrier(kids[1], NULL)) {
        fprintf(stderr, "builder DSL opcode carrier verifier rejected valid node\n");
        failed = 1;
    }

    WN *bad_carrier = WN_Create(OPC_EVAL, 1);
    WN_kid0(bad_carrier) =
        WN_CreateIntconst(OPR_INTCONST, MTYPE_I4, MTYPE_V, 0);
    if (DSL_WN_Verify_Opcode_Carrier(bad_carrier, NULL)) {
        fprintf(stderr, "builder DSL opcode carrier verifier accepted malformed node\n");
        failed = 1;
    }

    if (annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(annotation.name, DSL_OPCODE_COMMON_ADD,
                annotation.name_len) != 0 ||
        annotation.version != 1 ||
        strcmp(annotation.payload,
               "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none") != 0) {
        fprintf(stderr, "builder common.add annotation changed\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Operand_Record(op, 0, &operand_record) ||
        operand_record.ordinal != 0 ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        operand_record.version != 1 ||
        strstr(operand_record.payload, "name=kid_lhs") == NULL) {
        fprintf(stderr, "builder DSL operand kid0 record changed\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Operand_Record(op, 1, &operand_record) ||
        operand_record.ordinal != 1 ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        operand_record.version != 1 ||
        strstr(operand_record.payload, "name=kid_rhs") == NULL) {
        fprintf(stderr, "builder DSL operand kid1 record changed\n");
        failed = 1;
    }

    if (DSL_WN_Decode_Opcode_Operand_Record(op, 2, &operand_record)) {
        fprintf(stderr, "builder accepted invalid DSL operand record lookup\n");
        failed = 1;
    }

    if (DSL_Builder_Count_Value_Operands(op) != 2 ||
        DSL_Builder_Count_Value_Operands(kids[0]) != 0) {
        fprintf(stderr, "builder DSL operand count API changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_Value_Operand(op, 0, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        operand_info.version != 1 ||
        strstr(operand_info.payload, "name=kid_lhs") == NULL) {
        fprintf(stderr, "builder DSL operand info API changed\n");
        failed = 1;
    }

    if (DSL_Builder_Get_Value_Operand(op, 2, &operand_info) ||
        DSL_Builder_Get_Value_Operand(kids[0], 0, &operand_info) ||
        DSL_Builder_Get_Value_Operand(op, 0, NULL)) {
        fprintf(stderr, "builder accepted invalid DSL operand info lookup\n");
        failed = 1;
    }

    if (DSL_Builder_Create_Operator(DSL_OPCODE_INVALID_ID, 1,
                                    kids, 2, attrs, 1) != NULL) {
        fprintf(stderr, "builder accepted invalid opcode id\n");
        failed = 1;
    }

    WN *compat_marker = DSL_WN_Create_Opcode_Marker
                            (DSL_OPCODE_COMMON_ADD, 1,
                             "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none");
    if (!DSL_WN_Is_Opcode_Marker(compat_marker) ||
        WN_operator(compat_marker) != OPR_COMMENT ||
        DSL_WN_Opcode_Carrier_Kind(compat_marker) !=
            DSL_OPCODE_CARRIER_MARKER ||
        !DSL_WN_Get_Opcode_Annotation(compat_marker, &annotation) ||
        annotation.name_len != strlen(DSL_OPCODE_COMMON_ADD)) {
        fprintf(stderr, "compatibility DSL marker no longer decodes\n");
        failed = 1;
    }

    if (!DSL_WN_Verify_Opcode_Carrier(compat_marker, NULL)) {
        fprintf(stderr, "compatibility DSL marker verifier rejected valid marker\n");
        failed = 1;
    }

    if (DSL_WN_Decode_Opcode_Operand_Record(compat_marker, 0,
                                            &operand_record)) {
        fprintf(stderr, "compatibility marker unexpectedly had operand record\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Record(op, &op_record) ||
        !DSL_WN_Decode_Opcode_Record(compat_marker, &marker_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &marker_record)) {
        fprintf(stderr, "DSL node and compatibility marker records differ\n");
        failed = 1;
    }

    WN *xpragma_op = DSL_WN_Create_Opcode_Xpragma
                         (DSL_OPCODE_COMMON_ADD, 1,
                          "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none",
                          kids, 2);
    if (xpragma_op == NULL ||
        WN_operator(xpragma_op) != OPR_XPRAGMA ||
        WN_operator(xpragma_op) == OPR_EVAL ||
        !DSL_WN_Is_Opcode_Node(xpragma_op) ||
        DSL_WN_Is_Opcode_Marker(xpragma_op) ||
        DSL_WN_Opcode_Carrier_Kind(xpragma_op) !=
            DSL_OPCODE_CARRIER_XPRAGMA ||
        strcmp(DSL_WN_Opcode_Carrier_Name
                   (DSL_WN_Opcode_Carrier_Kind(xpragma_op)),
               "xpragma_lda_string") != 0 ||
        DSL_WN_Opcode_Operand_Count(xpragma_op) != 2 ||
        !DSL_WN_Verify_Opcode_Carrier(xpragma_op, NULL) ||
        !DSL_WN_Decode_Opcode_Record(xpragma_op, &xpragma_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &xpragma_record)) {
        fprintf(stderr, "non-eval DSL node carrier changed semantics\n");
        failed = 1;
    }

    if (!DSL_WN_Decode_Opcode_Operand_Record
            (xpragma_op, 1, &operand_record) ||
        operand_record.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_record.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_record.opcode_name_len) != 0 ||
        strstr(operand_record.payload, "name=kid_rhs") == NULL) {
        fprintf(stderr, "non-eval DSL node operand record changed\n");
        failed = 1;
    }

    FILE *xpragma_dump = tmpfile();
    if (xpragma_dump == NULL) {
        fprintf(stderr, "failed to create temporary xpragma dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(xpragma_dump, xpragma_op);
        rewind(xpragma_dump);
        count = fread(text, 1, sizeof(text) - 1, xpragma_dump);
        text[count] = '\0';
        fclose(xpragma_dump);

        if (strstr(text, "XPRAGMA") == NULL ||
            strstr(text, "EVAL") != NULL ||
            strstr(text, "dsl_node=common.add.v1") == NULL ||
            strstr(text, "dsl_carrier=xpragma_lda_string") == NULL ||
            strstr(text, "dsl_operand_count=2") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL) {
            fprintf(stderr, "fdump_tree missing non-eval DSL node evidence\n");
            failed = 1;
        }
    }

    WN *recreated = DSL_WN_Create_Opcode_From_Record(&op_record);
    if (recreated == NULL ||
        !DSL_WN_Is_Opcode_Node(recreated) ||
        !DSL_WN_Decode_Opcode_Record(recreated, &recreated_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &recreated_record)) {
        fprintf(stderr, "DSL record recreate path changed semantics\n");
        failed = 1;
    }

    WN *projection = DSL_WN_Create_Opcode_Comment_Projection(op);
    if (projection == NULL ||
        WN_operator(projection) != OPR_COMMENT ||
        !DSL_WN_Is_Opcode_Marker(projection) ||
        DSL_WN_Opcode_Carrier_Kind(projection) !=
            DSL_OPCODE_CARRIER_MARKER ||
        !DSL_WN_Decode_Opcode_Record(projection, &projected_record) ||
        !DSL_WN_Opcode_Records_Equivalent(&op_record, &projected_record)) {
        fprintf(stderr, "DSL comment projection changed logical record\n");
        failed = 1;
    }

    DSL_LOGICAL_OPCODE logical_opcode;
    if (!DSL_WN_Get_Logical_Opcode(op, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 1 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition != DSL_OPCODE_VERSION_EXACT ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_XPRAGMA ||
        DSL_Operator_Find_Current(DSL_OPCODE_COMMON_ADD,
                                  strlen(DSL_OPCODE_COMMON_ADD)) !=
            OPR_DSLADD) {
        fprintf(stderr, "logical opcode accessor rejected exact XPRAGMA input\n");
        failed = 1;
    }

    if (!DSL_WN_Get_Logical_Opcode(legacy_eval, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLMATMUL ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_NODE ||
        !DSL_WN_Get_Logical_Opcode(compat_marker, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.carrier != DSL_OPCODE_CARRIER_MARKER) {
        fprintf(stderr, "legacy carriers do not share logical opcode access\n");
        failed = 1;
    }

    WN *older = DSL_WN_Create_Opcode_Marker
                    (DSL_OPCODE_COMMON_ADD, 0, "attr.broadcast_rule=none");
    if (DSL_WN_Get_Logical_Opcode(older, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 0 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_OLDER) {
        fprintf(stderr, "older DSL opcode version was not deterministically rejected\n");
        failed = 1;
    }

    WN *newer = DSL_WN_Create_Opcode
                    (DSL_OPCODE_COMMON_ADD, 2, "attr.broadcast_rule=none");
    if (DSL_WN_Get_Logical_Opcode(newer, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        logical_opcode.source_version != 2 ||
        logical_opcode.effective_version != 1 ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_NEWER) {
        fprintf(stderr, "newer DSL opcode version was not deterministically rejected\n");
        failed = 1;
    }

    WN *unknown = DSL_WN_Create_Opcode_Marker
                      ("example.unknown", 1, "");
    if (DSL_WN_Get_Logical_Opcode(unknown, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLUNKNOWN ||
        logical_opcode.version_disposition !=
            DSL_OPCODE_VERSION_REJECTED_UNKNOWN_OPERATOR) {
        fprintf(stderr, "unknown DSL opcode did not receive stable rejection\n");
        failed = 1;
    }

    const char *add_payload =
        "kid0=kid_lhs;kid1=kid_rhs;attr.broadcast_rule=none";
    WN *native_output = DSL_WN_Create_Logical_Opcode
                            (OPR_DSLADD, 1, add_payload, kids, 2,
                             DSL_OPCODE_OUTPUT_NATIVE);
    WN *eval_output = DSL_WN_Create_Logical_Opcode
                          (OPR_DSLADD, 1, add_payload, kids, 2,
                           DSL_OPCODE_OUTPUT_LEGACY_EVAL);
    WN *xpragma_output = DSL_WN_Create_Logical_Opcode
                             (OPR_DSLADD, 1, add_payload, kids, 2,
                              DSL_OPCODE_OUTPUT_LEGACY_XPRAGMA);
    WN *comment_output = DSL_WN_Create_Logical_Opcode
                             (OPR_DSLADD, 1, add_payload, kids, 2,
                              DSL_OPCODE_OUTPUT_COMMENT_PROJECTION);
    if (native_output == NULL || !DSL_WN_Is_Native(native_output) ||
        eval_output == NULL || WN_operator(eval_output) != OPR_EVAL ||
        DSL_WN_Opcode_Operand_Count(eval_output) != 2 ||
        xpragma_output == NULL || WN_operator(xpragma_output) != OPR_XPRAGMA ||
        DSL_WN_Opcode_Operand_Count(xpragma_output) != 2 ||
        comment_output == NULL || WN_operator(comment_output) != OPR_COMMENT ||
        !DSL_WN_Get_Logical_Opcode(native_output, &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != OPR_DSLADD ||
        strcmp(DSL_Opcode_Output_Mode_Name(DSL_OPCODE_OUTPUT_LEGACY_EVAL),
               "legacy_eval") != 0 ||
        strcmp(DSL_Opcode_Version_Disposition_Name
                   (DSL_OPCODE_VERSION_REJECTED_NEWER),
               "rejected_newer") != 0) {
        fprintf(stderr, "explicit DSL compatibility output mode failed\n");
        failed = 1;
    }

    if (DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 2, add_payload, kids, 2,
             DSL_OPCODE_OUTPUT_LEGACY_EVAL) != NULL ||
        DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 1, add_payload, kids, 1,
             DSL_OPCODE_OUTPUT_LEGACY_EVAL) != NULL ||
        DSL_WN_Create_Logical_Opcode
            (OPR_DSLADD, 1, add_payload, kids, 2,
             (DSL_OPCODE_OUTPUT_MODE)99) != NULL) {
        fprintf(stderr, "compatibility output accepted invalid policy request\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Program_Unit_Value_Attach(void)
{
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[1];
    DSL_BUILDER_OPERATOR op;
    DSL_BUILDER_VALUE_INFO info;
    DSL_BUILDER_VALUE_INFO operand_info;
    int failed = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);

    kids[0] = DSL_WN_Create_Tensor_Const("pu_lhs", "int32", 2, "[2,2]",
                                         "splat", "0");
    kids[1] = DSL_WN_Create_Tensor_Const("pu_rhs", "int32", 2, "[2,2]",
                                         "splat", "1");
    attrs[0].name = "attr.broadcast_rule";
    attrs[0].value = "none";

    op = DSL_Builder_Create_Operator(add_id, 1, kids, 2, attrs, 1);
    pu = DSL_Builder_Create_Minimal_PU("builder_value_contract");

    if (!DSL_Builder_Append_PU_Value(pu, kids[0]) ||
        !DSL_Builder_Append_PU_Value(pu, op)) {
        fprintf(stderr, "builder failed to append DSL PU values\n");
        return 1;
    }

    if (DSL_Builder_Count_PU_Values(pu) != 2 ||
        DSL_Builder_Count_PU_Markers(pu) != 2) {
        fprintf(stderr, "builder PU value count changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_PU_Value(pu, 1, &info) ||
        info.opcode_name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(info.opcode_name, DSL_OPCODE_COMMON_ADD,
                info.opcode_name_len) != 0 ||
        strcmp(info.payload,
               "kid0=pu_lhs;kid1=pu_rhs;attr.broadcast_rule=none") != 0) {
        fprintf(stderr, "builder PU value inspection changed\n");
        failed = 1;
    }

    if (DSL_Builder_Count_Value_Operands(op) != 2 ||
        !DSL_Builder_Get_Value_Operand(op, 1, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        strstr(operand_info.payload, "name=pu_rhs") == NULL) {
        fprintf(stderr, "builder PU operand inspection changed\n");
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create temporary dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, op);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);

        if (strstr(text, "XPRAGMA") == NULL ||
            strstr(text, "EVAL") != NULL ||
            strstr(text, "LDA") == NULL ||
            strstr(text, "dsl_node=common.add.v1") == NULL ||
            strstr(text, "dsl_carrier=xpragma_lda_string") == NULL ||
            strstr(text, "dsl_operand_count=2") == NULL ||
            strstr(text, "__WHIRL_DSL_OPERAND__:kid0") == NULL ||
            strstr(text, "__WHIRL_DSL_OPERAND__:kid1") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL ||
            strstr(text, "__WHIRL_DSL__:opcode:common.add:v1") == NULL) {
            fprintf(stderr, "fdump_tree missing non-comment DSL node evidence\n");
            failed = 1;
        }
    }

    return failed;
}

static int
Check_Mapped_Image_Finalizer(void)
{
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    void *input_handle;
    OFFSET_AND_SIZE revision_section(0, 0);
    int failed = 0;

    request.path = "builder_contract_test.B";
    request.flags = 0;

    (void) unlink(request.path);

    if (!DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer failed valid request\n");
        failed = 1;
    }

    if (access(request.path, F_OK) != 0) {
        fprintf(stderr, "mapped image finalizer did not create output file\n");
        failed = 1;
    }

    input_handle = Open_Input_Info((char *)request.path);
    revision_section = input_handle == NULL ||
        input_handle == (void *)-1 ? OFFSET_AND_SIZE(0, 0) :
        get_section(input_handle, SHT_PROGBITS, 0);
    if (input_handle == NULL || input_handle == (void *)-1 ||
        revision_section.offset == 0 ||
        strcmp((const char *)input_handle + revision_section.offset,
               WHIRL_REVISION) != 0 ||
        get_section(input_handle, SHT_MIPS_WHIRL,
                    WT_DSL_IR_IMAGE).offset != 0 ||
        WN_get_dsl_ir_image(input_handle) != 0 ||
        DSL_IR_Image_Has_Records()) {
        fprintf(stderr, "legacy mapped image unexpectedly has DSL tables\n");
        failed = 1;
    }
    if (input_handle != NULL && input_handle != (void *)-1)
        Free_Input_Info();

    if (DSL_Builder_Finalize_Mapped_Image(NULL)) {
        fprintf(stderr, "mapped image finalizer accepted null request\n");
        failed = 1;
    }

    request.path = NULL;
    if (DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer accepted null path\n");
        failed = 1;
    }

    request.path = "builder_contract_test.B";
    request.flags = 1;
    if (DSL_Builder_Finalize_Mapped_Image(&request)) {
        fprintf(stderr, "mapped image finalizer accepted unknown flags\n");
        failed = 1;
    }

    (void) unlink("builder_contract_test.B");

    return failed;
}

static int
Check_Production_Native_Builder(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_OPERATOR_ATTRIBUTE add_attr;
    DSL_BUILDER_OPERATOR_ATTRIBUTE matmul_attrs[2];
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_VALUE add;
    DSL_BUILDER_VALUE matmul;
    DSL_BUILDER_VALUE_INFO info;
    DSL_BUILDER_VALUE_INFO operand_info;
    DSL_OPCODE_ANNOTATION annotation;
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID add_id;
    DSL_OPCODE_ID matmul_id;
    DSL_IR_IMAGE_HEADER header;
    TY_IDX tensor_ty;
    WN *add_expression;
    WN *matmul_expression;
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    int failed = 0;

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";
    tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("production_native_tensor", MTYPE_To_TY(MTYPE_I4),
                     &type_core);

    kids[0] = DSL_Builder_Create_Tensor_Constant
                  ("native_zero", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "0");
    kids[1] = DSL_Builder_Create_Tensor_Constant
                  ("native_one", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "1");

    common_id = DSL_Domain_Find("common");
    add_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_ADD, 1);
    matmul_id = DSL_Opcode_Find(common_id, DSL_OPCODE_COMMON_MATMUL, 1);
    add_attr.name = "attr.broadcast_rule";
    add_attr.value = "none";
    matmul_attrs[0].name = "attr.transpose_kid0";
    matmul_attrs[0].value = "false";
    matmul_attrs[1].name = "attr.transpose_kid1";
    matmul_attrs[1].value = "false";
    add = DSL_Builder_Create_Operator(add_id, 1, kids, 2, &add_attr, 1);
    matmul = DSL_Builder_Create_Operator
                 (matmul_id, 1, kids, 2, matmul_attrs, 2);

    if (kids[0] == NULL || kids[1] == NULL || add == NULL || matmul == NULL) {
        fprintf(stderr, "production native builder rejected valid values\n");
        return 1;
    }

    add_expression = WN_kid0(add);
    matmul_expression = WN_kid0(matmul);
    if (WN_operator(kids[0]) != OPR_STID ||
        WN_operator(kids[1]) != OPR_STID ||
        WN_operator(add) != OPR_STID ||
        WN_operator(matmul) != OPR_STID ||
        !DSL_WN_Is_Native(WN_kid0(kids[0])) ||
        DSL_WN_operator(WN_kid0(kids[0])) != OPR_DSLTENSORCONST ||
        !DSL_WN_Is_Native(add_expression) ||
        DSL_WN_operator(add_expression) != OPR_DSLADD ||
        !DSL_WN_Is_Native(matmul_expression) ||
        DSL_WN_operator(matmul_expression) != OPR_DSLMATMUL ||
        WN_kid_count(add_expression) != 2 ||
        WN_operator(WN_kid0(add_expression)) != OPR_LDID ||
        WN_operator(WN_kid1(add_expression)) != OPR_LDID ||
        WN_st_idx(WN_kid0(add_expression)) != WN_st_idx(kids[0]) ||
        WN_st_idx(WN_kid1(add_expression)) != WN_st_idx(kids[1])) {
        fprintf(stderr, "production native builder tree shape changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(kids[0])) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(kids[1])) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(add)) ||
        !DSL_Builder_Tensor_Has_Unique_Ownership(WN_st_idx(matmul)) ||
        !ST_is_temp_var(St_Table[WN_st_idx(add)]) ||
        WN_ty(add) != tensor_ty) {
        fprintf(stderr, "production native result ownership changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Get_Value_Operand(add, 1, &operand_info) ||
        operand_info.opcode_name_len !=
            strlen(DSL_OPCODE_COMMON_TENSOR_CONST) ||
        strncmp(operand_info.opcode_name, DSL_OPCODE_COMMON_TENSOR_CONST,
                operand_info.opcode_name_len) != 0 ||
        strstr(operand_info.payload, "name=native_one") == NULL ||
        !DSL_WN_Get_Opcode_Annotation(add_expression, &annotation)) {
        fprintf(stderr, "production native value inspection changed\n");
        failed = 1;
    }

    pu = DSL_Builder_Create_Minimal_PU("builder_value_contract");
    BOOL append_kid0 = DSL_Builder_Append_PU_Value(pu, kids[0]);
    BOOL append_kid1 = DSL_Builder_Append_PU_Value(pu, kids[1]);
    BOOL append_add = DSL_Builder_Append_PU_Value(pu, add);
    BOOL append_matmul = DSL_Builder_Append_PU_Value(pu, matmul);
    UINT32 pu_value_count = DSL_Builder_Count_PU_Values(pu);
    BOOL got_add = DSL_Builder_Get_PU_Value(pu, 4, &info);
    if (!append_kid0 || !append_kid1 || !append_add || !append_matmul ||
        pu_value_count != 6 || !got_add ||
        info.opcode_name_len != strlen(DSL_OPCODE_COMMON_ADD) ||
        strncmp(info.opcode_name, DSL_OPCODE_COMMON_ADD,
                info.opcode_name_len) != 0) {
        fprintf(stderr,
                "production native PU value API changed: append=%d/%d/%d/%d "
                "count=%u get=%d\n",
                append_kid0, append_kid1, append_add, append_matmul,
                pu_value_count, got_add);
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create production native dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, add);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);
        if (strstr(text, "OPR_DSLADD") == NULL ||
            strstr(text, "dsl_comment_projection=OPR_COMMENT") == NULL ||
            strstr(text, "OPR_DSL ") != NULL ||
            strstr(text, "MDSL ") != NULL) {
            fprintf(stderr, "production native dump exposed storage detail\n");
            failed = 1;
        }
    }

    DSL_IR_Image_Get_Header(&header);
    if (header.opcode_descriptor_count != 3 || header.node_count != 4 ||
        header.attribute_count != 7 || header.value_count != 4 ||
        header.value_reference_count != 4 ||
        !DSL_IR_Image_Validate(stderr)) {
        fprintf(stderr, "production builder DSL image rows changed\n");
        failed = 1;
    }

    FILE *gatekeeper_dump = tmpfile();
    if (!DSL_Gatekeeper_Verify_Program
             (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.native_node_count != 4 ||
        gatekeeper_result.result_symbol_count != 4 ||
        gatekeeper_result.error_count != 0) {
        fprintf(stderr, "gatekeeper rejected a valid native program\n");
        failed = 1;
    }

    ST_tensor_bind_attribute
        (WN_st_idx(add),
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "false");
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted missing no-alias ownership\n");
        failed = 1;
    }
    ST_tensor_bind_attribute
        (WN_st_idx(add),
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "true");

    DSL_BUILDER_TENSOR_TYPE_CORE incompatible_core = type_core;
    incompatible_core.dtype = "int64";
    TY_IDX incompatible_ty = DSL_Builder_Create_Tensor_Type_Core
                                 ("incompatible_native_tensor",
                                  MTYPE_To_TY(MTYPE_I8), &incompatible_core);
    TY_IDX saved_operand_ty = WN_ty(WN_kid1(add_expression));
    WN_set_ty(WN_kid1(add_expression), incompatible_ty);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted incompatible tensor operands\n");
        failed = 1;
    }
    WN_set_ty(WN_kid1(add_expression), saved_operand_ty);

    WN *saved_operand = WN_kid1(add_expression);
    WN_kid1(add_expression) = WN_Intconst(MTYPE_I4, 1);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted a non-LDID native operand\n");
        failed = 1;
    }
    WN_kid1(add_expression) = saved_operand;

    WN_OFFSET saved_record = WN_offset(add_expression);
    WN_offset(add_expression) = Save_Str
        ("__WHIRL_DSL__:opcode:common.add:v2:attr.broadcast_rule=none");
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted an unsupported operator version\n");
        failed = 1;
    }
    WN_offset(add_expression) = saved_record;

    WN *body = WN_func_body(PU_Info_tree_ptr(pu));
    WN *escape = WN_CreateEval
                     (WN_Lda(Pointer_Mtype, 0,
                             &St_Table[WN_st_idx(add)], 0));
    WN_INSERT_BlockLast(body, escape);
    if (DSL_Gatekeeper_Verify_Program
            (pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted a tensor-result address escape\n");
        failed = 1;
    }
    WN_EXTRACT_FromBlock(body, escape);

    if (gatekeeper_dump != NULL)
        fclose(gatekeeper_dump);

    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    const char *retained_artifact = getenv("OPEN64_DSL_TEST_ARTIFACT");
    request.path = retained_artifact == NULL || retained_artifact[0] == '\0' ?
                   "gatekeeper_contract_test.B" : retained_artifact;
    request.flags = 0;
    (void) unlink(request.path);
    if (!DSL_Builder_Finalize_Mapped_Image(&request) ||
        access(request.path, F_OK) != 0) {
        fprintf(stderr, "gatekeeper blocked valid mapped-image output\n");
        failed = 1;
    }
    if (retained_artifact == NULL || retained_artifact[0] == '\0')
        (void) unlink(request.path);

    DSL_BUILDER_PROGRAM_UNIT bad_pu =
        DSL_Builder_Create_Minimal_PU("gatekeeper_missing_attribute");
    DSL_BUILDER_VALUE bad_kids[2];
    bad_kids[0] = DSL_Builder_Create_Tensor_Constant
                  ("bad_attr_kid0", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "1");
    bad_kids[1] = DSL_Builder_Create_Tensor_Constant
                  ("bad_attr_kid1", tensor_ty, "int32", 2, "[2,2]",
                   "splat", "1");
    DSL_BUILDER_VALUE bad_matmul = DSL_Builder_Create_Operator
                                       (matmul_id, 1, bad_kids, 2, NULL, 0);
    DSL_Builder_Append_PU_Value(bad_pu, bad_kids[0]);
    DSL_Builder_Append_PU_Value(bad_pu, bad_kids[1]);
    DSL_Builder_Append_PU_Value(bad_pu, bad_matmul);
    gatekeeper_dump = tmpfile();
    if (DSL_Gatekeeper_Verify_Program
            (bad_pu, gatekeeper_dump, &gatekeeper_result) ||
        gatekeeper_result.error_count == 0) {
        fprintf(stderr, "gatekeeper accepted missing typed attributes\n");
        failed = 1;
    }
    if (gatekeeper_dump != NULL)
        fclose(gatekeeper_dump);

    return failed;
}

static int
Check_DSL_IR_Image_Tables(void)
{
    DSL_OPERATOR_INFO logical_info;
    DSL_IR_IMAGE_HEADER header;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD saved_descriptor;
    DSL_IR_NODE_RECORD node;
    DSL_IR_NODE_RECORD saved_node;
    DSL_IR_ATTRIBUTE_RECORD attribute;
    DSL_IR_ATTRIBUTE_RECORD saved_attribute;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_VALUE_RECORD saved_value;
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    DSL_IR_VALUE_REFERENCE_RECORD saved_reference;
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id;
    DSL_IR_NODE_ID node_id;
    DSL_IR_ATTRIBUTE_ID attribute_id;
    DSL_IR_VALUE_ID kid0_value_id;
    DSL_IR_VALUE_ID kid1_value_id;
    DSL_IR_VALUE_ID result_value_id;
    DSL_IR_VALUE_REFERENCE_ID kid0_reference_id;
    DSL_IR_VALUE_REFERENCE_ID kid1_reference_id;
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
    void *input_handle;
    OFFSET_AND_SIZE revision_section(0, 0);
    int failed = 0;

    DSL_IR_Image_Reset();
    DSL_IR_Image_Get_Header(&header);
    if (header.version != DSL_IR_IMAGE_VERSION ||
        header.record_kind_count != DSL_IR_IMAGE_RECORD_VALUE_REFERENCE ||
        header.opcode_descriptor_count != 0 ||
        header.node_count != 0 ||
        header.attribute_count != 0 ||
        header.value_count != 0 ||
        header.value_reference_count != 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_NODE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_VALUE) == 0 ||
        (header.capabilities & DSL_IR_IMAGE_CAP_VALUE_REFERENCE) == 0) {
        fprintf(stderr, "DSL image header initialization changed\n");
        failed = 1;
    }

    if (!DSL_Operator_Get_Info(OPR_DSLADD, &logical_info)) {
        fprintf(stderr, "common.add logical descriptor is unavailable\n");
        return 1;
    }

    DSL_IR_Opcode_Descriptor_Record_Init(&descriptor);
    descriptor.logical_operator = logical_info.dsl_operator;
    descriptor.version = logical_info.version;
    descriptor.operand_count = logical_info.nkids;
    descriptor.category = logical_info.category;
    descriptor.level = logical_info.level;
    descriptor.shape_rule = logical_info.shape_rule;
    descriptor.effect_model = logical_info.effect_model;
    descriptor.lowering_model = logical_info.lowering_model;
    descriptor.logical_name = Save_Str(logical_info.logical_name);
    descriptor.stable_name = Save_Str(logical_info.stable_name);
    descriptor.attribute_schema = Save_Str(logical_info.attribute_schema);
    descriptor.diagnostic_prefix = Save_Str(logical_info.diagnostic_prefix);
    descriptor_id = DSL_IR_Image_Add_Opcode_Descriptor(&descriptor);

    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = descriptor_id;
    node.payload = Save_Str("attr.broadcast_rule=none");
    node_id = DSL_IR_Image_Add_Node(&node);

    DSL_IR_Value_Record_Init(&value);
    value.value_kind = DSL_IR_VALUE_CONSTANT;
    value.ty = MTYPE_To_TY(MTYPE_I4);
    value.name = Save_Str("kid0");
    kid0_value_id = DSL_IR_Image_Add_Value(&value);
    value.name = Save_Str("kid1");
    kid1_value_id = DSL_IR_Image_Add_Value(&value);
    value.value_kind = DSL_IR_VALUE_OPERATOR_RESULT;
    value.producer_node_id = node_id;
    value.name = Save_Str("result");
    result_value_id = DSL_IR_Image_Add_Value(&value);

    DSL_IR_Value_Reference_Record_Init(&reference);
    reference.owner_node_id = node_id;
    reference.ordinal = 0;
    reference.value_id = kid0_value_id;
    kid0_reference_id = DSL_IR_Image_Add_Value_Reference(&reference);
    reference.ordinal = 1;
    reference.value_id = kid1_value_id;
    kid1_reference_id = DSL_IR_Image_Add_Value_Reference(&reference);

    DSL_IR_Attribute_Record_Init(&attribute);
    attribute.owner_node_id = node_id;
    attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
    attribute.name = Save_Str("attr.broadcast_rule");
    attribute.value = Save_Str("none");
    attribute_id = DSL_IR_Image_Add_Attribute(&attribute);

    if (!DSL_IR_Image_Set_Node_Links
            (node_id, kid0_reference_id, 2, attribute_id, 1,
             result_value_id)) {
        fprintf(stderr, "DSL image node links were rejected\n");
        failed = 1;
    }

    DSL_IR_Image_Get_Header(&header);
    if (descriptor_id != 1 || node_id != 1 || attribute_id != 1 ||
        kid0_value_id != 1 || kid1_value_id != 2 || result_value_id != 3 ||
        kid0_reference_id != 1 || kid1_reference_id != 2 ||
        header.opcode_descriptor_count != 1 || header.node_count != 1 ||
        header.attribute_count != 1 || header.value_count != 3 ||
        header.value_reference_count != 2 ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (descriptor_id, &saved_descriptor) ||
        saved_descriptor.id != descriptor_id ||
        strcmp(Index_To_Str(saved_descriptor.stable_name), "common.add") != 0 ||
        !DSL_IR_Image_Get_Node(node_id, &saved_node) ||
        saved_node.first_operand_reference_id != kid0_reference_id ||
        saved_node.operand_count != 2 ||
        saved_node.first_attribute_id != attribute_id ||
        saved_node.attribute_count != 1 ||
        saved_node.result_value_id != result_value_id ||
        !DSL_IR_Image_Get_Attribute(attribute_id, &saved_attribute) ||
        saved_attribute.value_kind != DSL_IR_ATTRIBUTE_VALUE_STRING ||
        !DSL_IR_Image_Get_Value(result_value_id, &saved_value) ||
        saved_value.producer_node_id != node_id ||
        !DSL_IR_Image_Get_Value_Reference
             (kid1_reference_id, &saved_reference) ||
        saved_reference.ordinal != 1 ||
        saved_reference.value_id != kid1_value_id) {
        fprintf(stderr, "DSL fixed-row image table contract changed\n");
        failed = 1;
    }

    DSL_IR_Opcode_Descriptor_Record_Init(&descriptor);
    DSL_IR_Node_Record_Init(&node);
    node.opcode_descriptor_id = 99;
    DSL_IR_Attribute_Record_Init(&attribute);
    attribute.owner_node_id = node_id;
    attribute.name = Save_Str("invalid");
    DSL_IR_Value_Reference_Record_Init(&reference);
    reference.owner_node_id = node_id;
    reference.value_id = 99;
    if (DSL_IR_Image_Add_Opcode_Descriptor(&descriptor) !=
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        DSL_IR_Image_Add_Node(&node) != DSL_IR_NODE_INVALID_ID ||
        DSL_IR_Image_Add_Attribute(&attribute) !=
            DSL_IR_ATTRIBUTE_INVALID_ID ||
        DSL_IR_Image_Add_Value_Reference(&reference) !=
            DSL_IR_VALUE_REFERENCE_INVALID_ID ||
        DSL_IR_Image_Set_Node_Links
            (node_id, kid0_reference_id, 1, attribute_id, 1,
             result_value_id) ||
        DSL_IR_Image_Get_Node(99, NULL)) {
        fprintf(stderr, "DSL image tables accepted an invalid row or range\n");
        failed = 1;
    }

    request.path = "dsl_ir_image_contract_test.B";
    request.flags = 0;
    (void) unlink(request.path);
    Irb_File_Name = (char *)request.path;
    if (Open_Output_Info(Irb_File_Name) == NULL) {
        fprintf(stderr, "DSL image mapped output failed\n");
        failed = 1;
    } else {
        Write_Global_Info(NULL);
        Close_Output_Info();
    }

    DSL_IR_Image_Reset();
    input_handle = Open_Input_Info((char *)request.path);
    revision_section = input_handle == NULL || input_handle == (void *)-1 ?
        OFFSET_AND_SIZE(0, 0) : get_section(input_handle, SHT_PROGBITS, 0);
    if (input_handle == NULL || input_handle == (void *)-1 ||
        revision_section.offset == 0 ||
        strcmp((const char *)input_handle + revision_section.offset,
               WHIRL_DSL_REVISION) != 0 ||
        get_section(input_handle, SHT_MIPS_WHIRL,
                    WT_DSL_IR_IMAGE).offset == 0 ||
        WN_get_dsl_ir_image(input_handle) != 0 ||
        DSL_IR_Image_Opcode_Descriptor_Count() != 1 ||
        DSL_IR_Image_Node_Count() != 1 ||
        DSL_IR_Image_Attribute_Count() != 1 ||
        DSL_IR_Image_Value_Count() != 3 ||
        DSL_IR_Image_Value_Reference_Count() != 2 ||
        !DSL_IR_Image_Get_Node(node_id, &saved_node) ||
        saved_node.result_value_id != result_value_id) {
        fprintf(stderr, "DSL image mapped round trip changed\n");
        failed = 1;
    }
    if (input_handle != NULL && input_handle != (void *)-1)
        Free_Input_Info();
    (void) unlink(request.path);

    DSL_IR_IMAGE_HEADER malformed_header;
    memset(&malformed_header, 0, sizeof(malformed_header));
    malformed_header.version = DSL_IR_IMAGE_VERSION + 1;
    malformed_header.record_kind_count =
        DSL_IR_IMAGE_RECORD_VALUE_REFERENCE;
    malformed_header.capabilities =
        DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR |
        DSL_IR_IMAGE_CAP_NODE |
        DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE |
        DSL_IR_IMAGE_CAP_VALUE |
        DSL_IR_IMAGE_CAP_VALUE_REFERENCE;
    if (DSL_IR_Image_Load_Mapped
            (&malformed_header, sizeof(malformed_header), NULL)) {
        fprintf(stderr, "DSL image accepted an unknown version\n");
        failed = 1;
    }

    DSL_IR_Image_Get_Header(&header);
    if (header.opcode_descriptor_count != 0 || header.node_count != 0 ||
        header.attribute_count != 0 || header.value_count != 0 ||
        header.value_reference_count != 0 ||
        DSL_IR_Image_Get_Node(node_id, NULL)) {
        fprintf(stderr, "DSL image table reset changed\n");
        failed = 1;
    }

    return failed;
}

static int
Check_Native_DSL_Node_Layout(void)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_OPERATOR_INFO operator_info;
    WN *kids[2];
    WN *node;
    WN *assignment;
    TY_IDX tensor_ty;
    ST_IDX temp_st;
    int failed = 0;

    if (!DSL_Operator_Get_Info(OPR_DSLADD, &operator_info) ||
        operator_info.dsl_operator != OPR_DSLADD ||
        strcmp(operator_info.logical_name, "OPR_DSLADD") != 0 ||
        strcmp(operator_info.stable_name, "common.add") != 0 ||
        operator_info.version != 1 ||
        operator_info.category != DSL_OPCODE_CATEGORY_EXECUTABLE ||
        operator_info.level != DSL_OPCODE_LEVEL_2_NUMERIC ||
        operator_info.nkids != 2 ||
        operator_info.shape_rule != DSL_SHAPE_RULE_BROADCAST ||
        operator_info.effect_model != DSL_EFFECT_MODEL_PURE ||
        operator_info.lowering_model != DSL_LOWERING_MODEL_RUNTIME_CALL ||
        strcmp(operator_info.diagnostic_prefix, "DOPC_COMMON_ADD") != 0 ||
        strcmp(operator_info.attribute_schema,
               "attr.broadcast_rule") != 0 ||
        DSL_Operator_Find("common.add", strlen("common.add"), 1) !=
            OPR_DSLADD ||
        DSL_Operator_Find("common.add", strlen("common.add"), 2) !=
            OPR_DSLUNKNOWN ||
        DSL_Operator_Get_Info(OPR_DSLUNKNOWN, NULL) ||
        strcmp(DSL_OPERATOR_name((DSL_OPERATOR)99),
               "OPR_DSLUNKNOWN") != 0) {
        fprintf(stderr, "logical DSL operator descriptor changed\n");
        failed = 1;
    }

    memset(&type_core, 0, sizeof(type_core));
    type_core.kind = "tensor";
    type_core.dtype = "int32";
    type_core.rank = 2;
    type_core.logical_shape = "[2,2]";

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                    ("native_dsl_result_type", MTYPE_To_TY(MTYPE_I4),
                     &type_core);
    temp_st = DSL_Builder_Create_Tensor_Result_Symbol
                  ("native_dsl_temp", tensor_ty, SCLASS_AUTO, EXPORT_LOCAL);
    if (ST_IDX_index(temp_st) == 0) {
        fprintf(stderr, "native DSL result symbol was not created\n");
        failed = 1;
    }

    kids[0] = WN_Intconst(MTYPE_I4, 0);
    kids[1] = WN_Intconst(MTYPE_I4, 1);
    node = DSL_WN_Create_Native
               (OPR_DSLADD, 1, "attr.broadcast_rule=none", kids, 2);
    assignment = WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, temp_st,
                               tensor_ty, node);

#ifdef TARG_X8664
    if (OPR_ZDLBR != 147 || OPR_DSL != 148) {
        fprintf(stderr, "native DSL opcode renumbered an existing operator\n");
        failed = 1;
    }
#endif

    if (OPERATOR_nkids(OPR_DSL) != -1 ||
        !OPERATOR_is_expression(OPR_DSL) ||
        OPERATOR_is_stmt(OPR_DSL) ||
        OPERATOR_has_next_prev(OPR_DSL) ||
        OPERATOR_has_sym(OPR_DSL) ||
        !OPERATOR_has_offset(OPR_DSL) ||
        !OPERATOR_is_not_executable(OPR_DSL)) {
        fprintf(stderr, "native DSL operator properties changed\n");
        failed = 1;
    }

    if (node == NULL ||
        WN_operator(node) != OPR_DSL ||
        WN_operator(node) == OPR_COMMENT ||
        WN_operator(node) == OPR_EVAL ||
        !DSL_WN_Is_Native(node) ||
        DSL_WN_operator(node) != OPR_DSLADD ||
        strcmp(DSL_OPERATOR_name(DSL_WN_operator(node)),
               "OPR_DSLADD") != 0 ||
        DSL_WN_Opcode_Carrier_Kind(node) != DSL_OPCODE_CARRIER_NATIVE ||
        WN_rtype(node) != MTYPE_M ||
        WN_desc(node) != MTYPE_V ||
        WN_kid_count(node) != 2 ||
        WN_kid0(node) != kids[0] ||
        WN_kid1(node) != kids[1]) {
        fprintf(stderr, "native DSL WN layout changed\n");
        failed = 1;
    }

    FILE *dump = tmpfile();
    if (dump == NULL) {
        fprintf(stderr, "failed to create native DSL dump file\n");
        failed = 1;
    } else {
        char text[4096];
        size_t count;

        fdump_tree(dump, node);
        rewind(dump);
        count = fread(text, 1, sizeof(text) - 1, dump);
        text[count] = '\0';
        fclose(dump);

        if (strstr(text, "OPR_DSLADD") == NULL ||
            strstr(text, "version=1") == NULL ||
            strstr(text, "payload=attr.broadcast_rule=none") == NULL ||
            strstr(text, "OPR_DSL ") != NULL ||
            strstr(text, "MDSL ") != NULL ||
            strstr(text, "dsl_carrier=native") != NULL) {
            fprintf(stderr, "native DSL dump exposed escape representation\n");
            failed = 1;
        }
    }

    if (assignment == NULL ||
        WN_operator(assignment) != OPR_STID ||
        WN_st_idx(assignment) != temp_st ||
        WN_ty(assignment) != tensor_ty ||
        WN_kid0(assignment) != node) {
        fprintf(stderr, "native DSL expression assignment shape changed\n");
        failed = 1;
    }

    if (!DSL_Builder_Tensor_Has_Unique_Ownership(temp_st) ||
        ST_class(St_Table[temp_st]) != CLASS_VAR ||
        !ST_is_temp_var(St_Table[temp_st]) ||
        !TY_is_tensor_extension(ST_type(St_Table[temp_st])) ||
        !ST_tensor_attribute_is_bound
             (temp_st,
              TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)) ||
        strcmp(ST_tensor_attribute
                   (temp_st,
                    TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS)),
               "true") != 0 ||
        ST_pt_to_unique_mem(St_Table[temp_st])) {
        fprintf(stderr, "native DSL result no-alias contract changed\n");
        failed = 1;
    }

    if (DSL_WN_Create_Native
            (OPR_DSLUNKNOWN, 1, NULL, NULL, 0) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 0, NULL, NULL, 0) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 2, NULL, kids, 2) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 1, NULL, kids, 1) != NULL ||
        DSL_WN_Create_Native(OPR_DSLADD, 1, NULL, NULL, 1) != NULL) {
        fprintf(stderr, "native DSL WN accepted an invalid record\n");
        failed = 1;
    }

    return failed;
}

int
main(void)
{
    int failed = 0;

    Initialize_Test_Context();
    if (getenv("OPEN64_DSL_INGESTION_API_ONLY") != NULL)
        return Check_Upgraded_Ingestion_APIs();

    failed |= Check_Tensor_Type_And_Descriptor();
    failed |= Check_Symbol_Metadata();
    failed |= Check_Operator_Creation();
    failed |= Check_Mapped_Image_Finalizer();
    failed |= Check_Program_Unit_Value_Attach();
    failed |= Check_Production_Native_Builder();
    failed |= Check_Native_DSL_Node_Layout();
    failed |= Check_DSL_IR_Image_Tables();

    return failed;
}
